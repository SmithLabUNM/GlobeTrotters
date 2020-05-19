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

data <- read.table("MOM.global.mammals.csv", header = TRUE, sep = ",")
## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data$n.cont[data$n.cont == 4] <- "3+"
data$n.cont[data$n.cont == 3] <- "3+"
data$n.cont <- as.factor(data$n.cont)


######H3 spp on mult cont are larger
clean.data <- subset(data, data$mass >= 0) 
length(clean.data$binomial) #3768

unique.data <- clean.data %>% 
  group_by(binomial) %>% 
  dplyr::summarise(mass = mean(mass), n.cont = n.cont[1]) %>%
  as.data.frame()

unique.data$logMass <- log10(unique.data$mass)
unique.data$global <- as.factor(unique.data$n.cont)

length(unique.data$binomial) #3505

#1 v 2+
length(unique.data$mass[unique.data$n.cont == 1]) #3241
length(unique.data$mass[unique.data$n.cont == 2 | unique.data$n.cont == "3+"]) #264
median(unique.data$mass[unique.data$n.cont == 1]) #85
median(unique.data$mass[unique.data$n.cont == 2 | unique.data$n.cont == "3+"]) #32.43
ks.test(unique.data$mass[unique.data$n.cont == 1], 
        clean.data$mass[clean.data$n.cont == 2 | clean.data$n.cont == "3+"]) #p-value = 7.772e-16

#1+2 v 3+
length(unique.data$mass[unique.data$n.cont == 1 | unique.data$n.cont == 2]) #3499
length(unique.data$mass[unique.data$n.cont == "3+"]) #6
median(unique.data$mass[unique.data$n.cont == 1 | unique.data$n.cont == 2]) #78
median(unique.data$mass[unique.data$n.cont == "3+"]) #92753.02
ks.test(unique.data$mass[unique.data$n.cont == 1 | unique.data$n.cont == 2], 
        unique.data$mass[unique.data$n.cont == "3+"]) #p = 0.09532

#for continent pairs
#NA and SA
N_S_America <- subset(clean.data, clean.data$continent == "North.America" | clean.data$continent == "South.America")
N_S_America1 <- subset(N_S_America, N_S_America$n.cont == 1)
length(N_S_America1$binomial) #1359

two.cont_NS <- subset(N_S_America, N_S_America$n.cont == 2) 

two.cont_NS <- plyr::ddply(two.cont_NS, 'binomial', function(.df){
  size <- mean(.df$mass)
})
colnames(two.cont_NS)[2] <- "size"
length(two.cont_NS$binomial) #175

ks.test(N_S_America$mass, two.cont_NS$size, alternative = "two.sided") #p = 9.739e-09
ks.test(N_S_America$mass, two.cont_NS$size, alternative = "less") #p = 4.87e-09
ks.test(N_S_America$mass, two.cont_NS$size, alternative = "greater") #p = 0.5249 x is statistically smaller than y

#EA and AF
EA_AF <- subset(clean.data, clean.data$continent == "Eurasia" | clean.data$continent == "Africa")
EA_AF1 <- subset(EA_AF, EA_AF$n.cont == 1) #1577 spp
length(EA_AF1$binomial) #1583

two.cont_EF <- subset(EA_AF, EA_AF$n.cont == 2)

two.cont_EF <- plyr::ddply(two.cont_EF, 'binomial', function(.df){
  size <- mean(.df$mass)
})
colnames(two.cont_EF)[2] <- "size"
length(two.cont_EF$binomial) #97

ks.test(EA_AF$mass, two.cont_EF$size, alternative = "two.sided") #p = 0.3435

#EA and NA
EA_NA <- subset(clean.data, clean.data$continent == "North.America" | clean.data$continent == "Eurasia")
EA_NA1 <- subset(EA_NA, EA_NA$n.cont == 1) #length 1337
length(EA_NA1$binomial) #1347

two.cont_EN <- subset(EA_NA, EA_NA$n.cont == 2)

two.cont_EN <- plyr::ddply(two.cont_EN, 'binomial', function(.df){
  size <- mean(.df$mass)
})
colnames(two.cont_EN)[2] <- "size"
length(two.cont_EN$binomial) #255

ks.test(EA_NA$mass, two.cont_EN$size, alternative = "two.sided") # p = 4.048e-05
ks.test(EA_NA$mass, two.cont_EN$size, alternative = "less") #p = 2.024e-05
ks.test(EA_NA$mass, two.cont_EN$size, alternative = "greater") #p = 0.6614

## body mass
ggplot() +
  geom_density(data = clean.data, aes(x = log10(mass), fill = n.cont), alpha = 0.7) +
  scale_fill_manual(values = col, 
                    name="Continents") +
  plot_theme + theme(legend.position = c(0.85, 0.82))+
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                     breaks = seq(-1, 7.5, 1),
                     limits = c(-0, 7.5),
                     expand=c(0,0))+
  scale_y_continuous(limits = c(0, 0.65),breaks = c(0,0.2,0.4,0.6),expand=c(0,0), 
                     name = 'Probability')
