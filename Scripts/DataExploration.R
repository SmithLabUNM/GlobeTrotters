#Data exploration
#meghan.balk@gmail.com
#April 18, 2019

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
data$num.conts <- as.factor(data$num.conts)

#about data
length(unique(data$order)) #29
length(unique(data$family)) #135
length(unique(data$genus)) #1069
length(unique(data$binomial)) #4403

data %>%
  group_by(order) %>%
  summarise(n = length(unique(binomial))) %>%
  as.data.frame()

length(unique(data$order[data$foss.age >= 0])) 
length(unique(data$family[data$foss.age >= 0]))
length(unique(data$genus[data$foss.age >= 0]))
length(unique(data$binomial[data$foss.age >= 0]))

length(unique(data$binomial[data$continent == "North.America"]))
length(unique(data$binomial[data$continent == "South.America"]))
length(unique(data$binomial[data$continent == "Eurasia"]))
length(unique(data$binomial[data$continent == "Africa"]))
length(unique(data$binomial[data$continent == "Australia"]))

