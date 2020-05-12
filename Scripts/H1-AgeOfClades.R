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

#1 v 2
ks.test(data.foss.age$foss.age[data.foss.age$n.cont == 1], 
        data.foss.age$foss.age[data.foss.age$n.cont == 2]) 

#2 v 3+
ks.test(data.foss.age$foss.age[data.foss.age$n.cont == 2], 
        data.foss.age$foss.age[data.foss.age$n.cont == "3+"]) 

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

# faurby
data.faurby <- data %>%
  dplyr::select(binomial, age.median, n.cont, continent)

data.faurby <- data.faurby %>%
  na.omit()

length(unique(data.faurby$binomial))
length(unique(data.faurby$binomial[data.faurby$n.cont == 1]))
length(unique(data.faurby$binomial[data.faurby$n.cont == 2]))
length(unique(data.faurby$binomial[data.faurby$n.cont == "3+"]))

#1 v 2
ks.test(data.faurby$age.median[data.faurby$n.cont == 1], 
        data.faurby$age.median[data.faurby$n.cont == 2]) 

#2 v 3+
ks.test(data.faurby$age.median[data.faurby$n.cont == 2], 
        data.faurby$age.median[data.faurby$n.cont == "3+"]) 

