#load libraries
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)

#load data
options(stringsAsFactors = FALSE)

## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data <- read.table("MOM.global.mammals.csv", header = TRUE, sep = ",")

data$n.cont[data$n.cont == 4] <- "3+"
data$n.cont[data$n.cont == 3] <- "3+"
data$n.cont <- as.factor(data$n.cont)

#how big will the datasets be?
length(unique(data$binomial[data$foss.age > 0]))
length(unique(data$binomial[data$age.median > 0]))

# range of dates
max(data$foss.age, na.rm = TRUE) #22.185
min(data$foss.age, na.rm = TRUE) #0.00585

#H1 spp that are on mult cont are older

#foss.age
data.foss.age <- data %>%
  dplyr::select(binomial, foss.age, n.cont, continent)

data.foss.age <- data.foss.age %>%
  na.omit()

length(unique(data.foss.age$binomial))
length(unique(data.foss.age$binomial[data.foss.age$n.cont == 1]))
length(unique(data.foss.age$binomial[data.foss.age$n.cont == 2]))
length(unique(data.foss.age$binomial[data.foss.age$n.cont == "3+"]))

#1 v 3
ks.test(data.foss.age$foss.age[data.foss.age$n.cont == 1], 
        data.foss.age$foss.age[data.foss.age$n.cont == 2 | data.foss.age$n.cont == "3+"]) 

#1+2 v 3+
ks.test(data.foss.age$foss.age[data.foss.age$n.cont == 2 | data.foss.age$n.cont == 1], 
        data.foss.age$foss.age[data.foss.age$n.cont == "3+"]) 

## continents by age  of family
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
ggplot() +
  geom_density(data = data.foss.age, aes(x = foss.age, fill = n.cont), alpha = 0.7) +
  scale_fill_manual(values = col, 
                    name="Continents") + 
  plot_theme + theme(panel.border = element_rect(fill=NA),
                    strip.background = element_rect(fill=NA),
                    legend.position = c(0.85, 0.8))+
  scale_x_continuous(name = "Age of Family",
                     breaks = seq(0, 25, 2),
                     limits = c(0, 8),
                     expand=c(0,0))+
  scale_y_continuous(name="Probability Density", expand=c(0,0), breaks=seq(0,0.6,0.2),limits=c(0,0.7))

# faurby
data.faurby <- data %>%
  dplyr::select(binomial, age.median, n.cont, continent)

data.faurby <- data.faurby %>%
  na.omit()

length(unique(data.faurby$binomial))
length(unique(data.faurby$binomial[data.faurby$n.cont == 1]))
length(unique(data.faurby$binomial[data.faurby$n.cont == 2]))
length(unique(data.faurby$binomial[data.faurby$n.cont == "3+"]))

#1 v 2+
ks.test(data.faurby$age.median[data.faurby$n.cont == 1], 
        data.faurby$age.median[data.faurby$n.cont == 2 | data.faurby$n.cont == "3+"]) 

#1+2 v 3+
ks.test(data.faurby$age.median[data.faurby$n.cont == 1 | data.faurby$n.cont == 2], 
        data.faurby$age.median[data.faurby$n.cont == "3+"]) 

ggplot() +
  geom_density(data = data.faurby, aes(x = age.median, fill = n.cont), alpha = 0.7) +
  scale_fill_manual(values = col, 
                    name="Continents") + 
  plot_theme + theme(panel.border = element_rect(fill=NA),
                     strip.background = element_rect(fill=NA),
                     legend.position = c(0.85, 0.8))+
  scale_x_continuous(name = "Age of Species",
                     breaks = seq(0, 25, 2),
                     limits = c(0, 8),
                     expand=c(0,0))+
  scale_y_continuous(name="Probability Density", expand=c(0,0), breaks=seq(0,0.6,0.2),limits=c(0,0.7))

## what about continent connectivity and aage of family to disperse?
#for continent pairs
#NA and SA
N_S_America <- subset(data.foss.age, data.foss.age$continent == "North.America" | data.foss.age$continent == "South.America")
N_S_America1 <- subset(N_S_America, N_S_America$n.cont == 1)

two.cont_NS <- subset(N_S_America, N_S_America$n.cont == 2)

length(two.cont_NS$binomial) #104
length(N_S_America1$binomial) #317

ks.test(N_S_America1$foss.age, two.cont_NS$foss.age, alternative = "two.sided") #p = 0.0008218
ks.test(N_S_America1$foss.age, two.cont_NS$foss.age, alternative = "less") #p = 0.0004109 x is statically younger than y
ks.test(N_S_America1$foss.age, two.cont_NS$foss.age, alternative = "greater") #p = 0.4979

#NA and EA
N_E <- subset(data.foss.age, data.foss.age$continent == "North.America" | data.foss.age$continent == "Eurasia")
N_E1 <- subset(N_E, N_E$n.cont == 1)

two.cont_NE <- subset(N_E, N_E$n.cont == 2)

length(two.cont_NE$binomial) #100
length(N_E1$binomial) #324

ks.test(N_E1$foss.age, two.cont_NE$foss.age, alternative = "two.sided") #p = 0.03584
ks.test(N_E1$foss.age, two.cont_NE$foss.age, alternative = "less") #p = 0.01792 x is statically younger than y
ks.test(N_E1$foss.age, two.cont_NE$foss.age, alternative = "greater") #p = 0.04871


#EA and AF
E_A <- subset(data.foss.age, data.foss.age$continent == "Eurasia" | data.foss.age$continent == "Africa")
E_A1 <- subset(E_A, E_A$n.cont == 1)

two.cont_EA <- subset(E_A, E_A$n.cont == 2)

length(two.cont_EA$binomial) #68
length(E_A$binomial) #265

ks.test(E_A1$foss.age, two.cont_EA$foss.age, alternative = "two.sided") #p = 0.0005699
ks.test(E_A1$foss.age, two.cont_EA$foss.age, alternative = "less") #p = 0.9747 
ks.test(E_A1$foss.age, two.cont_EA$foss.age, alternative = "greater") #p = 0.0002849 x is statically older than y
