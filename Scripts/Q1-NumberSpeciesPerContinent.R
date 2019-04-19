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

## Q1: How many spp are on ea continent
length(unique(data$binomial[data$n.cont == 1])) #4105
length(unique(data$binomial[data$n.cont == 2])) #292
length(unique(data$binomial[data$n.cont == 3])) #5
length(unique(data$binomial[data$n.cont == 4])) #1

data[which(data$n.cont == 3), "binomial"]
data[which(data$n.cont == 4), "binomial"]

#about the spp on 2 continents
two.cont <- subset(data, data$n.cont == 2)
two.cont %>%
  group_by(order) %>%
  summarise(n = length(unique(binomial))) %>%
  as.data.frame

length(unique(two.cont$order)) #14
length(unique(two.cont$family)) #55
length(unique(two.cont$genus)) #186

#chiroptera
161/292 #(55%)
798/4403 #compared to total dataset (18%)


#list of bats on two continents
bats <- subset(data, data$order == "Chiroptera")
bats_two <- subset(bats, bats$n.cont == 2) #322 spp
