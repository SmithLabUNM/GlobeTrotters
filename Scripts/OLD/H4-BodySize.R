#Are certain sized animals more likely to be on more than one continent?
#meghan.balk@gmail.com
#Rasmus Ø. Pedersen

#### LOAD DATA ----

options(stringsAsFactors = FALSE)

## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data <- read.table("../Data/MOM.global.mammals.csv", 
                   header = TRUE, sep = ",")

#### LOAD LIBRARIES ----
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)

#### SET GRAPHING THEME ----

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


#### TEST: certain sized species are more likely to be wide-ranging ----

clean.data <- subset(data, data$mass >= 0) 
length(clean.data$binomial) #616

unique.data <- clean.data %>% 
  group_by(binomial) %>% 
  dplyr::summarise(mass = mean(mass), n.cont = n.cont[1]) %>%
  as.data.frame()

unique.data$logMass <- log10(unique.data$mass)
unique.data$global <- as.factor(unique.data$n.cont)

length(unique.data$binomial) #3355
length(unique.data$binomial[unique.data$n.cont == 1]) #3094
length(unique.data$binomial[unique.data$n.cont == 2]) #255
length(unique.data$binomial[unique.data$n.cont == "3+"]) #6

##### 1 v 2+ -----
length(unique.data$mass[unique.data$n.cont == 1]) #3094
length(unique.data$mass[unique.data$n.cont == 2 | unique.data$n.cont == "3+"]) #261
median(unique.data$mass[unique.data$n.cont == 1]) #92.75
median(unique.data$mass[unique.data$n.cont == 2 | unique.data$n.cont == "3+"]) #33.21
ks.test(unique.data$mass[unique.data$n.cont == 1], 
        clean.data$mass[clean.data$n.cont == 2 | clean.data$n.cont == "3+"]) #p-value < 2.2e-16
ks.test(unique.data$mass[unique.data$n.cont == 1], 
        clean.data$mass[clean.data$n.cont == 2 | clean.data$n.cont == "3+"],
        alternative = "greater") #p-value = 0.2475
ks.test(unique.data$mass[unique.data$n.cont == 1], 
        clean.data$mass[clean.data$n.cont == 2 | clean.data$n.cont == "3+"],
        alternative = "less") #p-value < 2.2e-16 (i.e., 1 cont is not "less" than 2+ cont)

##### 1+2 v 3+ -----
length(unique.data$mass[unique.data$n.cont == 1 | unique.data$n.cont == 2]) #3349
length(unique.data$mass[unique.data$n.cont == "3+"]) #6
median(unique.data$mass[unique.data$n.cont == 1 | unique.data$n.cont == 2]) #89
median(unique.data$mass[unique.data$n.cont == "3+"]) #92753.02
ks.test(unique.data$mass[unique.data$n.cont == 1 | unique.data$n.cont == 2], 
        unique.data$mass[unique.data$n.cont == "3+"]) #p-value = 0.1055
ks.test(unique.data$mass[unique.data$n.cont == 1 | unique.data$n.cont == 2], 
        unique.data$mass[unique.data$n.cont == "3+"],
        alternative = "greater") #p-value = 0.05277 (i.e., 1 or 2 cont is not "greater" than 3+)

##### for continent pairs -----

###### NA and SA ------
N_S_America <- subset(clean.data, clean.data$continent == "North.America" | clean.data$continent == "South.America")
N_S_America1 <- subset(N_S_America, N_S_America$n.cont == 1)
length(N_S_America1$binomial) #1327

two.cont_NS <- subset(N_S_America, N_S_America$n.cont == 2) 

two.cont_NS <- plyr::ddply(two.cont_NS, 'binomial', function(.df){
  size <- mean(.df$mass)
})
colnames(two.cont_NS)[2] <- "size"
length(two.cont_NS$binomial) #173

ks.test(N_S_America$mass, two.cont_NS$size, alternative = "two.sided") #p-value = 1.671e-08
ks.test(N_S_America$mass, two.cont_NS$size, alternative = "less") #p-value = 8.356e-09
ks.test(N_S_America$mass, two.cont_NS$size, alternative = "greater") #p-value = 0.5457 x is statistically smaller than y

###### EA and AF ------
EA_AF <- subset(clean.data, clean.data$continent == "Eurasia" | clean.data$continent == "Africa")
EA_AF1 <- subset(EA_AF, EA_AF$n.cont == 1) 
length(EA_AF1$binomial) #1468

two.cont_EF <- subset(EA_AF, EA_AF$n.cont == 2)

two.cont_EF <- plyr::ddply(two.cont_EF, 'binomial', function(.df){
  size <- mean(.df$mass)
})
colnames(two.cont_EF)[2] <- "size"
length(two.cont_EF$binomial) #97

ks.test(EA_AF$mass, two.cont_EF$size, alternative = "two.sided") #p-value = 0.552

###### EA and NA ------
EA_NA <- subset(clean.data, clean.data$continent == "North.America" | clean.data$continent == "Eurasia")
EA_NA1 <- subset(EA_NA, EA_NA$n.cont == 1)
length(EA_NA1$binomial) #1301

two.cont_EN <- subset(EA_NA, EA_NA$n.cont == 2)

two.cont_EN <- plyr::ddply(two.cont_EN, 'binomial', function(.df){
  size <- mean(.df$mass)
})
colnames(two.cont_EN)[2] <- "size"
length(two.cont_EN$binomial) #255

ks.test(EA_NA$mass, two.cont_EN$size, alternative = "two.sided") #p-value = 4.711e-05
ks.test(EA_NA$mass, two.cont_EN$size, alternative = "less") #p-value = 2.356e-05
ks.test(EA_NA$mass, two.cont_EN$size, alternative = "greater") #p-value = 0.7488

#### PLOT ----
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

#### WHICH SIZES ARE UNIQUE? ----
##limited dispersers
lim.true <- subset(df.family, df.family$signif.sidak.lim == TRUE, select = c(family,
                                                                             null.N,
                                                                             limited.N,
                                                                             prop.null,
                                                                             prop.lim,
                                                                             signif.sidak.lim))

lim.false <- subset(df.family, df.family$signif.sidak.lim == FALSE, select = c(family,
                                                                               null.N,
                                                                               limited.N,
                                                                               prop.null,
                                                                               prop.lim,
                                                                               signif.sidak.lim))

family.lim.true <- df[df$family %in% lim.true$family,]
hist(log10(family.lim.true$avg.mass))

family.lim.false <- df[df$family %in% lim.false$family,]
hist(log10(family.lim.false$avg.mass))

ks.test(family.lim.true$avg.mass, family.lim.false$avg.mass) #sig diff
ks.test(family.lim.true$avg.mass, family.lim.false$avg.mass, alternative = "greater") ##sig
ks.test(family.lim.true$avg.mass, family.lim.false$avg.mass, alternative = "less") #not sig

##globe trotters
trot.true <- subset(df.family, df.family$signif.sidak.trot == TRUE, select = c(family,
                                                                               null.N,
                                                                               trotter.N,
                                                                               prop.null,
                                                                               prop.trot,
                                                                               signif.sidak.trot))
