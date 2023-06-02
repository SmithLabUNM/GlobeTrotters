#Data exploration of full MOMv11 dataset
#meghan.balk@gmail.com

#### LOAD LIBRARIES ----
require(tidyverse)

#### LOAD DATA ----
mm.df <- read.csv("../Data/MOMv11.csv",
                  header = TRUE)

#### FIX DIET ----
#from Init.R

mm.df$trophic[which(mm.df$trophic == "")] <- NA
sort(table(mm.df$trophic))


invertivore <- c("ainsect", "Ainsect", "ainsect/carn", "ainsect/ginsect",
                 "browse/frug/ginsect", "Browse/frug/ginsect", "browse/ginsect",
                 "browse/ginsect/carn", "carn/ginsect", "carn/ginsect/frug", "carn/invert",
                 "carn/ginsect/frug", "carn/invert", "carn/invert/frug", "frug/Ainsect",
                 "frug/ginsect", "Frug/ginsect", "frug/ginsect/browse", "frug/ginsect/carn",
                 "frug/browse/ginsect", "frug/invert", "ginsect", "Ginsect", "ginsect (earthworms)",
                 "ginsect/browse", "ginsect/browse/frug", "ginsect/carn", "Ginsect/carn", "ginsect/carn/frug",
                 "ginsect/frug", "Ginsect/frug", "ginsect/frug/browse", "Ginsect/frug/carn",
                 "graze/ginsect", "herb/invert", "insect", "invert", "invert/browse", "invert/carn",
                 "invert/carn/frug", "invert/piscivore", "inverts/carn/frug", "piscivore/invert", "piscivore/invert/carn")
carnivore <- c("ainsect/carn", "browse/ginsect/carn", "carn", "Carn", "carn/frug", "carn/ginsect",
               "carn/ginsect/frug", "carn/invert", "carn/invert/frug", "carn/omnivore", "Carn/piscivore",
               "frug/browse/carn", "ginsect/carn", "Ginsect/carn", "ginsect/carn/frug", "Ginsect/frug/carn",
               "Graze/carn", "invert/carn", "invert/carn/frug", "inverts/carn/frug", "piscivore/invert/carn", "frug/carn")
browser <- c("browse", "Browse", "browse (bamboo)", "browse (roots & tubers)", "browse/frug", "Browse/frug",
             "browse/frug/ginsect", "Browse/frug/ginsect", "browse/ginsect", "browse/ginsect/carn",
             "browse/graze", "Browse/graze", "browse/graze/frug", "frug/browse", "Frug/browse", "frug/browse/carn",
             "frug/browse/ginsect", "frug/browse/graze", "frug/ginsect/browse", "ginsect/browse", "ginsect/browse/frug",
             "ginsect/frug/browse", "graze/brower", "graze/browse", "Graze/browse", "graze/browse/frug", "invert/browse")
grazer <- c("browse/graze", "Browse/graze", "browse/graze/frug", "frug/browse/graze", "frug/graze", "graze", "Graze",
            "graze/brower", "graze/browse", "Graze/browse", "graze/browse/frug", "Graze/carn", "graze/frug", "Graze/frug",
            "graze/ginsect")
frugivore <- c("browse/frug", "Browse/frug", "browse/frug/ginsect", "Browse/frug/ginsect", "browse/graze/frug", "carn/frug",
               "carn/ginsect/frug", "carn/invert/frug", "frug", "Frug", "frug/Ainsect", "frug/browse", "Frug/browse", "frug/browse/carn",
               "frug/browse/ginsect", "frug/browse/graze", "frug/carn", "frug/ginsect", "Frug/ginsect", "frug/ginsect/browse", "frug/ginsect/carn",
               "frug/graze", "frug/herb", "frug/invert", "ginsect/browse/frug", "ginsect/carn/frug", "ginsect/frug", "Ginsect/frug",
               "ginsect/frug/browse", "Ginsect/frug/carn", "graze/browse/frug", "graze/frug", "Graze/frug", "invert/carn/frug", "inverts/carn/frug")
piscivore <- c("Carn/piscivore", "invert/piscivore", "piscivore", "piscivore/invert", "piscivore/invert/carn")


troph.diet <- which(mm.df$trophic %in% c(invertivore, carnivore, browser, grazer, frugivore, piscivore))

mm.df <- mm.df %>% mutate(diet.invertivore = trophic %in% invertivore,
                          diet.carnivore = trophic %in% carnivore,
                          diet.browser = trophic %in% browser,
                          diet.grazer = trophic %in% grazer,
                          diet.frugivore = trophic %in% frugivore,
                          diet.piscivore = trophic %in% piscivore)

##### DIET BREADTH -----
mm.df$diet.breadth <- select(mm.df, diet.invertivore:diet.piscivore) %>% rowSums()
table(mm.df$diet.breadth)

#### TABLES ----

##### TOTAL RECORDS -----

nrow(mm.df) #6695

length(unique(mm.df$order)) #31
length(unique(mm.df$family)) #170
length(unique(mm.df$genus)) #1360
length(unique(mm.df$binomial)) #5736

##### BY CONTINENT -----

mm.df %>% 
  group_by(continent) %>%
  summarise(n.order = length(unique(order)),
            n.family = length(unique(family)),
            n.genus = length(unique(genus)),
            n.sp = length(unique(binomial)))

##### BY DIET -----

length(unique(mm.df$binomial[mm.df$diet.browser == TRUE])) #1265
length(unique(mm.df$binomial[mm.df$diet.grazer == TRUE])) #501
length(unique(mm.df$binomial[mm.df$diet.frugivore == TRUE])) #1535
length(unique(mm.df$binomial[mm.df$diet.carnivore == TRUE])) #291
length(unique(mm.df$binomial[mm.df$diet.piscivore == TRUE])) #102
length(unique(mm.df$binomial[mm.df$diet.invertivore == TRUE])) #1668

table(mm.df$diet.breadth[!duplicated(mm.df$binomial)])
#0    1    2     3 
#2195 2002 1390  149

##### N INVALID MASS -----

nrow(mm.df[mm.df$mass.status == "invalid",]) #73 records

length(unique(mm.df$order[mm.df$mass.status == "invalid"])) #6
length(unique(mm.df$family[mm.df$mass.status == "invalid"])) #15
length(unique(mm.df$genus[mm.df$mass.status == "invalid"])) #35
length(unique(mm.df$binomial[mm.df$mass.status == "invalid"])) #44

unique(mm.df$continent[mm.df$mass.status == "invalid"]) #all continents

###### SIZE BIAS? ------

ggplot(data = mm.df[mm.df$mass.status == "invalid",]) +
  geom_density(aes(x = log10(mass)))

mm.df$mass[mm.df$mass.status == "invalid"]
#10 under 10 g
#50 between 10-99g
#5 between 100-999g
#7 1000g or above

##### N DATA DEFICIENT (IUCN) -----

nrow(mm.df[mm.df$iucn.status == "DD",]) #684 records

length(unique(mm.df$order[mm.df$iucn.status == "DD"])) #17
length(unique(mm.df$family[mm.df$iucn.status == "DD"])) #56
length(unique(mm.df$genus[mm.df$iucn.status == "DD"])) #267
length(unique(mm.df$binomial[mm.df$iucn.status == "DD"])) #616

unique(mm.df$continent[mm.df$iucn.status == "DD"]) #all continents

###### SIZE BIAS? ------

ggplot(data = mm.df[mm.df$iucn.status == "DD",]) + 
  geom_density(aes(x = log10(mass)))

sort(mm.df$mass[mm.df$iucn.status == "DD"])
#389 without mass estimates
#36 below 10g
#128 between 10-99g
#84 between 100-999g
#18 between 1000-9999g
#5 between 10000-99999g
#3 between 100000-999999g
#16 between 1000000-9999999g
#1 above 10000000g


