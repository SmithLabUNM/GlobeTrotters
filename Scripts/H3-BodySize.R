#load libraries
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)

#load data
options(stringsAsFactors = FALSE)

data <- read.table("MOM.global.mammals.csv", header = TRUE, sep = ",")
## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data$num.conts <- data$n.cont
data$num.conts[data$num.conts == 4] <- "3+"
data$num.conts[data$num.conts == 3] <- "3+"
data$num.conts <- as.factor(data$num.conts)

pan <- read.table("pantheria.csv", header = TRUE, sep = ",")
pan1 <- subset(pan, select = c("MSW05_Binomial", "X26.1_GR_Area_km2", "X22.1_HomeRange_km2"))
colnames(pan1)[1] <- "binomial"
colnames(pan1)[2] <- "GR_Area_km2"
colnames(pan1)[3] <- "HomeRange_km2"

ranges <- read.table("ranges.csv", header = TRUE, sep = ",")
colnames(ranges)[1] <- "binomial"

######H4 spp on mult cont are larger
clean.data <- subset(data, data$mass >= 0) #3808

unique.data <- clean.data %>% 
  group_by(binomial) %>% 
  dplyr::summarise(mass <- mean(mass), n.cont = n.cont[1]) %>%
  as.data.frame()

unique.data$logMass <- log10(unique.data$mass)
unique.data$global <- as.factor(unique.data$n.cont)
levels(unique.data$global) <- c("one", "more than one", "more than one", "more than one")

#1 v 2
median(unique.data$mass[unique.data$n.cont == 1]) #85.62
median(unique.data$mass[unique.data$n.cont == 2]) #28.725
ks.test(unique.data$mass[unique.data$n.cont == 1], clean.data$mass[clean.data$n.cont == 2]) #p < 2.2e-16
length(unique.data$mass[unique.data$n.cont == 1]) #3232
length(unique.data$mass[unique.data$n.cont == 2]) #280

#1 v all
ks.test(unique.data$mass[unique.data$n.cont == 1], unique.data$mass[unique.data$n.cont >= 2]) #p < 2.2e-16
length(unique.data$mass[unique.data$n.cont == 1]) #3232
length(unique.data$mass[unique.data$n.cont >= 2]) #576

#1+2 v all
median(unique.data$mass[unique.data$n.cont <= 2]) #78
median(unique.data$mass[unique.data$n.cont > 2]) #92753.02
ks.test(unique.data$mass[unique.data$n.cont <= 2], unique.data$mass[unique.data$n.cont > 2]) #p = 0.09561
length(unique.data$mass[unique.data$n.cont <= 2]) #3512
length(unique.data$mass[unique.data$n.cont > 2]) #6

#for continent pairs
#NA and SA
N_S_America1 <- subset(clean.data, clean.data$continent == "North.America" | clean.data$continent == "South.America")
N_S_America <- subset(N_S_America1, N_S_America1$n.cont == 1) #1354

two.cont_NS1 <- subset(N_S_America1, N_S_America1$n.cont == 2 | N_S_America1$n.cont == 3 | N_S_America1$n.cont == 4) #349
#two.cont_NS2 <- two.cont_NS1[duplicated(two.cont_NS1$binomial),] #length 166
two.cont_NS <- plyr::ddply(two.cont_NS1, 'binomial', function(.df){
  size <- mean(.df$mass)
})
colnames(two.cont_NS)[2] <- "size"

# two.cont_NS <- two.cont_NS1 %>%
#   group_by(binomial) %>%
#   summarise(size = mean(mass)) %>%
#   as.data.frame()

ks.test(N_S_America$mass, two.cont_NS$size, alternative = "two.sided") #p = 1.223e-13
ks.test(N_S_America$mass, two.cont_NS$size, alternative = "less") #p < 6.115e-14
ks.test(N_S_America$mass, two.cont_NS$size, alternative = "greater") #p = 0.3801 x is statistically smaller than y

N_S_America3 <- subset(N_S_America, select = c("binomial", "mass"))
colnames(N_S_America3)[2] <- "size"
N_S_America3$cont <- "one"
two.cont_NS$cont <- "two"

NS <- rbind(N_S_America3, two.cont_NS)

median(N_S_America$mass) #89
length(N_S_America$mass) #1354
median(two.cont_NS$size) #26.68
length(two.cont_NS$size) #188

#EA and AF
EA_AF1 <- subset(clean.data, clean.data$continent == "Eurasia" | clean.data$continent == "Africa")
EA_AF <- subset(EA_AF1, EA_AF1$n.cont == 1) #1577 spp

two.cont_EF1 <- subset(EA_AF1, EA_AF1$n.cont == 2)
#two.cont_EF2 <- two.cont_EF1[duplicated(two.cont_EF1$binomial),] #length 88
#two.cont_EF <- two.cont_EF2 %>%
#  group_by(binomial) %>%
#  summarise(size = mean(mass)) %>%
#  as.data.frame()

two.cont_EF <- plyr::ddply(two.cont_EF1, 'binomial', function(.df){
  size <- mean(.df$mass)
})
colnames(two.cont_EF)[2] <- "size"

ks.test(EA_AF$mass, two.cont_EF$size, alternative = "two.sided") #p = 0.3001
ks.test(EA_AF$mass, two.cont_EF$size, alternative = "less") # p = 0.3181
ks.test(EA_AF$mass, two.cont_EF$size, alternative = "greater") #p = 0.1505

EA_AF3 <- subset(EA_AF, select = c("binomial", "mass"))
colnames(EA_AF3)[2] <- "size"
EA_AF3$cont <- "one"
two.cont_EF$cont <- "two"

EF <- rbind(EA_AF3, two.cont_EF)

median(EA_AF$mass) #74.6
length(EA_AF$mass) #1577
median(two.cont_EF$size) #55.9
length(two.cont_EF$size) #112

#EA and NA
EA_NA1 <- subset(clean.data, clean.data$continent == "North.America" | clean.data$continent == "Eurasia")
EA_NA <- subset(EA_NA1, EA_NA1$n.cont == 1) #length 1337
#only duplicated spp; no spp inhabiting two continents that are share with a different continent
two.cont_EN1 <- subset(EA_NA1, EA_NA1$n.cont == 2)
#two.cont_EN2 <- two.cont_EN1[duplicated(two.cont_EN1$binomial),] #26 spp
#two.cont_EN <- two.cont_EN2 %>% #don't want to double count spp
#  group_by(binomial) %>%
#  summarise(size = mean(mass)) %>%
#  as.data.frame #15 spp

two.cont_EN <- plyr::ddply(two.cont_EN1, 'binomial', function(.df){
  size <- mean(.df$mass)
})
colnames(two.cont_EN)[2] <- "size"

ks.test(EA_NA$mass, two.cont_EN$size, alternative = "two.sided") # p = 2.335e-07
ks.test(EA_NA$mass, two.cont_EN$size, alternative = "less") #p = 1.167e-07
ks.test(EA_NA$mass, two.cont_EN$size, alternative = "greater") #p = 0.5561

EA_NA3 <- subset(EA_NA, select = c("binomial", "mass"))
colnames(EA_NA3)[2] <- "size"
EA_NA3$cont <- "one"
two.cont_EN$cont <- "two"

EN <- rbind(EA_NA3, two.cont_EN)

median(EA_NA$mass) #80
length(EA_NA$mass) #1337
median(two.cont_EN$size) #28.875
length(two.cont_EN$size) #280

## body mass
ggplot() +
  geom_density(data = clean.data, aes(x = log10(mass), fill = num.conts), alpha = 0.7) +
  scale_fill_manual(values = col, 
                    name="Continents") +
  theme_jmg + theme(legend.position = c(0.85, 0.82))+
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                     breaks = seq(-1, 7.5, 1),
                     limits = c(-0, 7.5),
                     expand=c(0,0))+
  scale_y_continuous(limits = c(0, 0.65),breaks = c(0,0.2,0.4,0.6),expand=c(0,0), 
                     name = 'Probability')

#North vs South America
ggplot() +
  geom_density(data = NS, aes(x = log10(size), fill = cont), alpha = 0.7) +
  scale_fill_manual(values = col,
                    name="N. & S. America",
                    labels=c("1", "2")) +
  theme_jmg +  theme(legend.position = c(0.77, 0.8))+
  scale_x_continuous(name = expression(log[10]~Body~Mass),
                     breaks = seq(0, 7.5, 1),
                     limits = c(0, 7.5), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 0.7),breaks=seq(0,0.7,0.2),
                     name="Probability Density", expand=c(0,0))

#Africa vs Eurasia
ggplot() +
  geom_density(data = EF, aes(x = log10(size), fill = cont), alpha = 0.7) +
  scale_fill_manual(values = col,
                    name="Africa & Eurasia",
                    labels=c("1", "2")) +
  theme_jmg +  theme(legend.position = c(0.77, 0.8))+
  scale_x_continuous(name = expression(log[10]~Body~Mass),
                     breaks = seq(0, 7.5, 1),
                     limits = c(0, 7.5), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 0.7),breaks=seq(0,0.7,0.2),
                     name="Probability Density", expand=c(0,0))

#Africa vs Eurasia
ggplot() +
  geom_density(data = EN, aes(x = log10(size), fill = cont), alpha = 0.7) +
  scale_fill_manual(values = col,
                    name="Eurasia & North America",
                    labels=c("One", "Both")) +
  theme_jmg +  theme(legend.position = c(0.77, 0.8))+
  scale_x_continuous(name = expression(log[10]~Body~Mass),
                     breaks = seq(0, 7.5, 1),
                     limits = c(0, 7.5), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 0.7),breaks=seq(0,0.7,0.2),
                     name="Probability Density", expand=c(0,0))
