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

## Q1: How many spp are on ea continent
length(unique(data$binomial[data$n.cont == 1])) #4148
length(unique(data$binomial[data$n.cont == 2])) #272
length(unique(data$binomial[data$n.cont == "3+"])) #6

unique(data[which(data$n.cont == "3+"), "binomial"])

#about the spp on 2 continents
two.cont <- subset(data, data$n.cont == 2)
two.cont %>%
  group_by(order) %>%
  summarise(n = length(unique(binomial))) %>%
  as.data.frame

length(unique(two.cont$order)) #14
length(unique(two.cont$family)) #54
length(unique(two.cont$genus)) #175
length(unique(two.cont$binomial)) #272

#chiroptera
151/272 #(55.5%)
length(unique(data$binomial[data$order == "Chiroptera"]))/4148 #compared to total dataset (19%)
