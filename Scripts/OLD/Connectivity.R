#How many species are shared between continents
#meghan.balk@gmail.com

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