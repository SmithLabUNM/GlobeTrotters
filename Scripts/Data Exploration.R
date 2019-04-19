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

