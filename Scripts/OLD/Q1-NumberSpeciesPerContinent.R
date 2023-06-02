#Tally number of species on 1, 2, or 3+ continents
#meghan.balk@gmail.com

#### LOAD LIBRARIES ----
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)

#### LOAD DATA ----
options(stringsAsFactors = FALSE)

## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data <- read.table("../Data/MOM.global.mammals.csv", 
                   header = TRUE, sep = ",")

#### DATA WRANGLE ----

data$n.cont[data$n.cont == 4] <- "3+"
data$n.cont[data$n.cont == 3] <- "3+"
data$n.cont <- as.factor(data$n.cont)

#### Q1: How many spp are on each continent? ----

tot <- length(unique(data$binomial)) #4426
n.one <- length(unique(data$binomial[data$n.cont == 1])) #4148
per.one <- n.one/tot #93.69%
n.two <- length(unique(data$binomial[data$n.cont == 2])) #272
per.two <- n.two/tot #6.14%
n.more <- length(unique(data$binomial[data$n.cont == "3+"])) #6
per.more <- n.more/tot #0.14%

unique(data[which(data$n.cont == "3+"), c("order", "family", "binomial")])
# "Miniopterus schreibersii" "Mustela nivalis"          "Vulpes vulpes"           
# "Ursus arctos"             "Cervus elaphus"           "Panthera leo" 

## ARTIODACTYLA
length(unique(data$binomial[data$order == "Artiodactyla"]))/4148 #compared to total dataset (6.15%)

##### LIMITED DISPERSERS -----

two.cont <- subset(data, data$n.cont == 2)
two.cont %>%
  group_by(order) %>%
  summarise(n = length(unique(binomial)),
            per = n/272) %>%
  as.data.frame

length(unique(two.cont$order)) #14
length(unique(two.cont$family)) #54
length(unique(two.cont$genus)) #176
length(unique(two.cont$binomial)) #272

## CARNIVORA
unique(two.cont$family[two.cont$order == "Carnivora"])
unique(two.cont$genus[two.cont$order == "Carnivora"])
length(unique(data$binomial[data$order == "Carnivora"]))/4148 #compared to total dataset (5.95%)

## CHIROPTERA
length(unique(data$binomial[data$order == "Chiroptera"]))/4148 #compared to total dataset (19.38%)

## RODENTS
length(unique(data$binomial[data$order == "Rodentia"]))/4148 #compared to total dataset (45.46%)
