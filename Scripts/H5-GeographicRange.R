#load libraries
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)

col <- c("#2ca25f", "#99d8c9", "#e5f5f9")
plot_theme <- theme(panel.grid = element_blank(), 
                    aspect.ratio = .75, #adjust as needed
                    axis.text = element_text(size = 21, color = "black"), 
                    axis.ticks.length=unit(0.2,"cm"),
                    axis.title = element_text(size = 21),
                    axis.title.y = element_text(margin = margin(r = 10)),
                    axis.title.x = element_text(margin = margin(t = 10)),
                    axis.title.x.top = element_text(margin = margin(b = 5)),
                    plot.title = element_text(size = 21, face = "plain", hjust = 10),
                    panel.border = element_rect(colour = "black", fill=NA, size=1),
                    panel.background = element_blank(),
                    legend.position = "none",
                    text = element_text(family = 'Helvetica'))

#load data
options(stringsAsFactors = FALSE)

## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data <- read.table("MOM.global.mammals.csv", header = TRUE, sep = ",")

data$n.cont[data$n.cont == 4] <- "3+"
data$n.cont[data$n.cont == 3] <- "3+"
data$n.cont <- as.factor(data$n.cont)

####H5. Sp. w greater geographic ranges are found on more continents ####

#Eurasia = 54.752349 km2
#Africa = 30.380561 km2
#North America = 24.70849 km2
#South America = 17.839838 km2
#Australia = 7.692265 km2

#want to add "tot.area" column
for(i in 1:length(data$binomial)){
  if(data$continent[i] == "Eurasia"){
    data$tot.area[i] <- (54.75*10^6)
  }else if(data$continent[i] == "Africa"){
    data$tot.area[i] <- (30.38*10^6)
  }else if(data$continent[i] == "North.America"){
    data$tot.area[i] <- (24.70*10^6)
  }else if(data$continent[i] == "South.America"){
    data$tot.area[i] <- (17.83*10^6)
  }else{
    data$tot.area[i] <- (7.69*10^6)
  }
}

#want to combine to unique spp
df <- data %>%
  dplyr::group_by(binomial) %>%
  dplyr::summarise(cont.tot.area = sum(tot.area), pan.gr.area = X26.1_GR_Area_km2[1], 
                   faurby.nat.range = present.natural.range.km2[1], faurby.current.range = current.range.km2[1],
                   num.cont = n.cont[1], size = mean(mass)) %>%
  as.data.frame()

#get cleanest dataset
df.pan <- subset(df, !is.na(df$pan.gr.area) & !is.na(df$size)) #3084 spp (out of 3497 w/ bs est)

df.faurby <- subset(df, !is.na(df$faurby.nat.range) & !is.na(df$size) & df$faurby.nat.range != 0) #2629 spp (out of 3497 w/ bs est)

length(unique(df.pan$binomial)) #3084
length(unique(df.pan$binomial[df.pan$num.cont == 1])) #2841
length(unique(df.pan$binomial[df.pan$num.cont == 2])) #238
length(unique(df.pan$binomial[df.pan$num.cont == "3+"])) #5

length(unique(df.faurby$binomial)) # 2629
length(unique(df.faurby$binomial[df.faurby$num.cont == 1])) #2517
length(unique(df.faurby$binomial[df.faurby$num.cont == 2])) #108
length(unique(df.faurby$binomial[df.faurby$num.cont == "3+"])) #4

#get ratio of geog range out of total area
df.pan$ratio <- df.pan$pan.gr.area/df.pan$cont.tot.area
df.faurby$ratio <- df.faurby$faurby.nat.range/df.faurby$cont.tot.area

#change to log units
df.pan$logSize <- log10(df.pan$size)
df.pan$logRatio <- log10(df.pan$ratio)

df.faurby$logSize <- log10(df.faurby$size)
df.faurby$logRatio <- log10(df.faurby$ratio)

#Do species on multiple continents occupy a larger area than available when taking body size into account? Yes!
summary(glm(lm(log10(df.pan$ratio) ~ log10(df.pan$size) + as.factor(df.pan$num.cont))))
summary(lm(log10(df.pan$ratio[df.pan$num.cont == "1"]) ~ log10(df.pan$size[df.pan$num.cont == "1"])))
summary(lm(log10(df.pan$ratio[df.pan$num.cont == "2"]) ~ log10(df.pan$size[df.pan$num.cont == "2"])))
summary(lm(log10(df.pan$ratio[df.pan$num.cont == "3+"]) ~ log10(df.pan$size[df.pan$num.cont == "3+"])))

summary(glm(lm(log10(df.faurby$ratio) ~ log10(df.faurby$size) + as.factor(df.faurby$num.cont))))
summary(lm(log10(df.faurby$ratio[df.faurby$num.cont == "1"]) ~ log10(df.faurby$size[df.faurby$num.cont == "1"])))
summary(lm(log10(df.faurby$ratio[df.faurby$num.cont == "2"]) ~ log10(df.faurby$size[df.faurby$num.cont == "2"])))
summary(lm(log10(df.faurby$ratio[df.faurby$num.cont == "3+"]) ~ log10(df.faurby$size[df.faurby$num.cont == "3+"])))

#Do species have a lager geo-range for their body size? Yes!
summary(glm(lm(log10(df.pan$pan.gr.area) ~ log10(df.pan$size) + as.factor(df.pan$num.cont))))
summary(glm(lm(log10(df.faurby$faurby.nat.range) ~ log10(df.faurby$size) + as.factor(df.faurby$num.cont))))

# ranges
ggplot(data = df.pan, aes(x = logSize, y = ratio)) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = col) +
  labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Home~Range/Geographic~Range), color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "top")

ggplot(data = df.faurby, aes(x = logSize, y = ratio)) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = col) +
  labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Home~Range/Geographic~Range), color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "top")
