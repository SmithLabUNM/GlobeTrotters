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

#H1 spp that are on mult cont are older
fossil <- subset(data, data$foss.age > 0)
no_age <- subset(data, is.na(data$foss.age))

#need to gather data by unique binomials
fossil$num.conts <- as.factor(fossil$num.conts)
fossil.unique <- fossil %>% 
  dplyr::group_by(binomial) %>% 
  dplyr::summarise(n.cont = n.cont[1], num.conts = num.conts[1], min.age = min(foss.age), max.age = max(foss.age), min.mass = min(mass), max.mass = max(mass), med.mass = median(mass), avg.mass = mean(mass), std.mass = sd(mass)) %>%
  as.data.frame

no.age.unique <- no_age %>% 
  group_by(binomial) %>% 
  summarise(n.cont = n.cont[1], min.mass = min(mass), max.mass = max(mass), med.mass = median(mass), avg.mass = mean(mass), std.mass = sd(mass)) %>%
  as.data.frame

length(unique(fossil$binomial)) #668
length(fossil$binomial[fossil$continent == "North.America"]) #280 (42%) (compared to 18.5% in full dataset)
length(fossil$binomial[fossil$continent == "South.America"]) #147 (22%) (compared to 28% in full dataset)
length(fossil$binomial[fossil$continent == "Eurasia"]) #153 (23%) (compared to 27% in full dataset)
length(fossil$binomial[fossil$continent == "Australia"]) #73 (11%) (compared to 8% in full dataset)
length(fossil$binomial[fossil$continent == "Africa"]) #116 (17%) (compared to 26% in full dataset)

fossil %>%
  group_by(order) %>%
  summarise(n = length(unique(binomial))) %>%
  as.data.frame()

max(fossil.unique$max.age, na.rm = TRUE) #22.185

#lookign for biases in the data
min(fossil.unique$min.mass, na.rm = TRUE) #2.575
min(no.age.unique$min.mass, na.rm = TRUE) #1.75

max(fossil.unique$max.mass, na.rm = TRUE) #13300000
max(no.age.unique$max.mass, na.rm = TRUE) #12700000

median(fossil.unique$med.mass, na.rm = TRUE) #2336.239
median(no.age.unique$med.mass, na.rm = TRUE) #62.5 #VERY DIFFERENT

mean(fossil.unique$avg.mass, na.rm = TRUE) #227695.5
mean(no.age.unique$avg.mass, na.rm = TRUE) #26672.47

sd(fossil.unique$avg.mass, na.rm = TRUE) #992258.8
sd(no.age.unique$avg.mass, na.rm = TRUE) #343089

sd(fossil.unique$std.mass, na.rm = TRUE) #199904.5 #greater spread
sd(no.age.unique$std.mass, na.rm = TRUE) #20600.6

#test age of things
#1 v 2
ks.test(fossil.unique$max.age[fossil.unique$n.cont == 1], fossil.unique$max.age[fossil.unique$n.cont == 2])
#p = 0.02916
length(fossil.unique$max.age[fossil.unique$n.cont == 1]) #573
length(fossil.unique$max.age[fossil.unique$n.cont == 2]) #90

#1 v all
ks.test(fossil.unique$max.age[fossil.unique$n.cont == 1], fossil.unique$max.age[fossil.unique$n.cont >= 2])
#p = 0.01243
length(fossil.unique$max.age[fossil.unique$n.cont >= 2]) #95

#1+2 v all
ks.test(fossil.unique$max.age[fossil.unique$n.cont <= 2], fossil.unique$max.age[fossil.unique$n.cont > 2])
#p = 0.07039
length(fossil.unique$max.age[fossil.unique$n.cont <= 2]) #663
length(fossil.unique$max.age[fossil.unique$n.cont > 2]) #5

fossil.unique$global 
?levels
fossil.unique$global <- as.factor(fossil.unique$n.cont)
levels(fossil.unique$global) <- c("one", "more than one", "more than one", "more than one")

## continents by age  of family
ggplot() +
  geom_density(data = fossil.unique, aes(x = max.age, fill = num.conts), alpha = 0.7) +
  scale_fill_manual(values = col,
                    name="Continents") +
  theme_jmg + theme(panel.border = element_rect(fill=NA),
                    strip.background = element_rect(fill=NA),
                    legend.position = c(0.85, 0.8))+
  scale_x_continuous(name = "Age of Family",
                     breaks = seq(0, 25, 2),
                     limits = c(0, 8),
                     expand=c(0,0))+
  scale_y_continuous(name="Probability Density", expand=c(0,0), breaks=seq(0,0.6,0.2),limits=c(0,0.7))

