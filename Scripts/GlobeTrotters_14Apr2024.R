# Meghan A. Balk
# meghan.balk@gmail.com
# Rasmus Ø. Pedersen

#### LOAD PACKAGES ----
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)
require(stringr)
library(gcookbook)
library(scales)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(MASS)
library(mfx)
library(stargazer)

#### PLOT THEME ----

## COLOR SCHEME
#South America = #E2C9F2; dark #9A8AA6
#North America = #B4D9C8; dark #748C81
#Africa = #C2D991; dark #7E8C5E
#Eurasia = #F2CDA0; dark #A68C6D
#Australia = #D9967E; dark #8C6151

cont_bw <- c("black", "gray47", "red")

plot_theme <- theme(panel.grid = element_blank(), 
                    aspect.ratio = .75, #adjust as needed
                    axis.text = element_text(size = 26, color = "black"), 
                    axis.ticks.length = unit(0.2,"cm"),
                    axis.title = element_text(size = 32),
                    #axis.title.y = element_text(margin = margin(r = 10)),
                    #axis.title.x = element_text(margin = margin(t = 10)),
                    #axis.title.x.top = element_text(margin = margin(b = 5)),
                    plot.title = element_text(size = 21, face = "plain", hjust = 10),
                    panel.border = element_rect(colour = "black", fill = NA, size = 1),
                    panel.background = element_blank(),
                    text = element_text(family = 'Helvetica'),
                    plot.background = element_rect(fill = 'transparent', color = NA))

#### LOAD DATA ----

options(stringsAsFactors = FALSE)

## MOM database
mm.df <- read.csv("./Data/MOMv11.1.csv", 
                  header = TRUE)

mm.df.old <- read.csv("./Data/MOMv11.csv",
                      header = TRUE)

## Pacifici et al. 2013 data for dispersal
pacifici <- read.csv("./Data/Generation Length for Mammals.csv", 
                     header = TRUE)

## Family origin data collected for this paper
origin <- read.csv("./Data/familyOrigin.csv", 
                   header = TRUE)

## PBDB first fossil occurrence data
pbdb <- read.csv("./Data/pbdb.data.csv", 
                 as.is = T)

## Faurby et al. Phylocene tree data for ages
faurby.ages <- read.csv("./Data/species.age_Faurby.csv", 
                        header = TRUE, row.names = 1)

## Faurby et al. present natural ranges
ranges <- read.csv("./Data/ranges.csv", 
                   header = TRUE)

## Jones et al. PanTHERIA dataset
pantheria <- read.csv("./Data/pantheria.csv", 
                      header = TRUE)

#### FIX DIET ----

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

mm.df <- mm.df %>% 
  mutate(diet.invertivore = trophic %in% invertivore,
         diet.carnivore = trophic %in% carnivore,
         diet.browser = trophic %in% browser,
         diet.grazer = trophic %in% grazer,
         diet.frugivore = trophic %in% frugivore,
         diet.piscivore = trophic %in% piscivore)

length(unique(mm.df$binomial[mm.df$diet.browser == TRUE])) #1265
length(unique(mm.df$binomial[mm.df$diet.grazer == TRUE])) #501
length(unique(mm.df$binomial[mm.df$diet.frugivore == TRUE])) #1535
length(unique(mm.df$binomial[mm.df$diet.carnivore == TRUE])) #291
length(unique(mm.df$binomial[mm.df$diet.piscivore == TRUE])) #102
length(unique(mm.df$binomial[mm.df$diet.invertivore == TRUE])) #1668

mm.df$diet.breadth <- dplyr::select(mm.df, diet.invertivore:diet.piscivore) %>% rowSums()

#### MAKE GENERIC & FAMILY AVERAGES ----
mm.df$diet.src <- NA
mm.df$diet.src[troph.diet] <- "troph.diet"

diet <- mm.df %>%
  dplyr::select(order, family, genus, binomial, starts_with("diet"))

species.diet <- filter(diet, !is.na(diet.src)) %>% 
  group_by(binomial) %>%
  dplyr::summarise(family = family[1],
                   genus = genus[1],
                   diet.invertivore = sum(diet.invertivore, na.rm = TRUE) >= 1,
                   diet.carnivore = sum(diet.carnivore, na.rm = TRUE) >= 1,
                   diet.browser = sum(diet.browser, na.rm = TRUE) >= 1,
                   diet.grazer = sum(diet.grazer, na.rm = TRUE) >= 1,
                   diet.frugivore = sum(diet.frugivore, na.rm = TRUE) >= 1,
                   diet.piscivore = sum(diet.piscivore, na.rm = TRUE) >= 1,
                   diet.src = "species.diet")

genus.diet <- group_by(species.diet, genus) %>%
  dplyr::summarise(family = family[1],
                   diet.invertivore = mean(diet.invertivore, na.rm = TRUE) > 0.5,
                   diet.carnivore = mean(diet.carnivore, na.rm = TRUE) > 0.5,
                   diet.browser = mean(diet.browser, na.rm = TRUE) > 0.5,
                   diet.grazer = mean(diet.grazer, na.rm = TRUE) > 0.5,
                   diet.frugivore = mean(diet.frugivore, na.rm = TRUE) > 0.5,
                   diet.piscivore = mean(diet.piscivore, na.rm = TRUE) > 0.5,
                   diet.src = "mean.genus")

family.diet <- group_by(genus.diet, family) %>%
  dplyr::summarise(diet.invertivore = mean(diet.invertivore, na.rm = TRUE) > 0.5,
                   diet.carnivore = mean(diet.carnivore, na.rm = TRUE) > 0.5,
                   diet.browser = mean(diet.browser, na.rm = TRUE) > 0.5,
                   diet.grazer = mean(diet.grazer, na.rm = TRUE) > 0.5,
                   diet.frugivore = mean(diet.frugivore, na.rm = TRUE) > 0.5,
                   diet.piscivore = mean(diet.piscivore, na.rm = TRUE) > 0.5,
                   diet.src = "mean.family")

for(i in 1:nrow(mm.df)) {
  if(mm.df$binomial[i] %in% species.diet$binomial) {
    k <- which(species.diet$binomial == mm.df$binomial[i])
    mm.df[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      species.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else if(mm.df$genus[i] %in% genus.diet$genus) {
    k <- which(genus.diet$genus == mm.df$genus[i])
    mm.df[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      genus.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else if(mm.df$family[i] %in% family.diet$family) {
    k <- which(family.diet$family == mm.df$family[i])
    mm.df[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      family.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else {
    print("Cannot find diet")
  }
}

table(mm.df$diet.src, useNA = "always")

# mean.family   mean.genus   species.diet  <NA> 
# 460           1557         4488          190 

length(unique(mm.df$binomial[is.na(mm.df$diet.src)]))

mm.df$diet.breadth <- dplyr::select(mm.df, diet.invertivore:diet.piscivore) %>% rowSums()
table(mm.df$diet.breadth[!duplicated(mm.df$binomial)])
# 0    1    2    3 
# 214 3266 1995  261 

length(unique(mm.df$binomial[mm.df$diet.breadth == 0]))

setdiff(unique(mm.df$binomial[mm.df$diet.breadth == 0]), 
        unique(mm.df$binomial[is.na(mm.df$diet.src)]))
View(mm.df[mm.df$diet.breadth == 0 &!(is.na(mm.df$diet.src)),]) #all diets are FALSE

#### ABOUT FULL DATASET ----

nrow(mm.df) #6695

length(unique(mm.df$order)) #31
length(unique(mm.df$family)) #170
length(unique(mm.df$genus)) #1360
length(unique(mm.df$binomial)) #5736

##### INVASIVES & DOMESTICATES -----

# get species that are introduced or domesticated
# keep records for where they are native to
total <- length(unique(mm.df$binomial[mm.df$continent != "Insular"])) #4552

sp.intro <- unique(mm.df$binomial[mm.df$extant.status == "introduction" |
                                  mm.df$extant.status == "domesticated"])
length(sp.intro) #50 species total

intro.df <- mm.df[mm.df$binomial %in% sp.intro,]

setdiff(intro.df$binomial, sp.intro) #no diff
setdiff(sp.intro, intro.df$binomial) #no diff

#how many of these introduced sp also on islands?
length(unique(intro.df$binomial[intro.df$continent == "Insular"])) #42
View(intro.df[intro.df$continent == "Insular",])
sp.intro.islands <- unique(intro.df$binomial[intro.df$continent == "Insular"])
intro.df <- intro.df[intro.df$continent != "Insular",]

invasive <- length(unique(intro.df$binomial[intro.df$extant.status == "introduction"])) #49
domesticated <- length(unique(intro.df$binomial[intro.df$extant.status == "domesticated"])) #3

(invasive/total)*100 #0.615%

(domesticated/total)*100 #0.0659%

#which sp are domesticated and which are invasives?
unique(intro.df$binomial[intro.df$extant.status == "introduction"])
unique(intro.df$binomial[intro.df$extant.status == "domesticated"])
#"Bubalus bubalis" domesticated on Africa and South America; introduced to Australia; from Eurasia; grazer
#"Capra hircus" domesticated on North Aermica, introduced to Australia, from Eurasia, browser and grazer
#"Equus caballus" extinct North America?, domesticated on South America, Australia, and Eurasia; grazer

mean(intro.df$mass, na.rm = TRUE)
range(intro.df$mass, na.rm = TRUE)

intro.df <- intro.df %>% 
    mutate(Africa = continent == "Africa",
           North.America = continent == "North.America",
           South.America = continent == "South.America",
           Eurasia = continent == "Eurasia",
           Australia = continent == "Australia")

intro.sums <- intro.df %>%
    group_by(binomial) %>%
    dplyr::summarise(diet.invertivore.tot = isTRUE(sum(diet.invertivore) > 0),
                     diet.carnivore.tot = isTRUE(sum(diet.carnivore) > 0),
                     diet.browser.tot = isTRUE(sum(diet.browser) > 0), 
                     diet.grazer.tot = isTRUE(sum(diet.grazer) > 0),
                     diet.piscivore.tot = isTRUE(sum(diet.piscivore) > 0),
                     diet.frugivore.tot = isTRUE(sum(diet.frugivore) > 0),
                     avg.mass = mean(mass, na.rm = TRUE),
                     n.cont = length(unique(continent)),
                     iucn = iucn.status[1]) %>%
    as.data.frame()

intro.continent <- intro.df %>%
    group_by(binomial) %>%
    dplyr::summarise(continent.Africa = as.logical(sum(Africa)),
                     continent.North.America = as.logical(sum(North.America)),
                     continent.South.America = as.logical(sum(South.America)),
                     continent.Eurasia = as.logical(sum(Eurasia)),
                     continent.Australia = as.logical(sum(Australia))) %>%
    as.data.frame()

intro.taxa <- intro.df[!duplicated(intro.df$binomial),] %>%
    dplyr::select(order,
                  family,
                  genus,
                  species,
                  binomial)

intro.sumTaxa <- left_join(intro.taxa, intro.sums,
                           by = "binomial")
intro.contTaxaSums <- left_join(intro.sumTaxa, intro.continent,
                                by = "binomial")

intro.contTaxaSums$diet.breadth <- intro.contTaxaSums %>%
    dplyr::select(starts_with("diet.")) %>% 
    rowSums()
table(intro.contTaxaSums$diet.breadth)
# 1  2   3 
# 29 19  2 

intro.contTaxaSums$n.cont <- intro.contTaxaSums %>%
    dplyr::select(starts_with("continent.")) %>%
    rowSums()
table(intro.contTaxaSums$n.cont)
# 1   2   3  4  5 
# 18  15  6  8  2 
intro.contTaxaSums$binomial[intro.contTaxaSums$n.cont == 5] #Felis catus, Mus musculus

length(unique(intro.df$binomial)) #50
nrow(intro.contTaxaSums) #50

write.csv(intro.contTaxaSums,
          "./Results/invasive.species.csv",
          row.names = FALSE)

unique(intro.contTaxaSums$binomial)
range(intro.contTaxaSums$avg.mass, na.rm = TRUE)
#11.25 900000.0
median(intro.contTaxaSums$avg.mass, na.rm = TRUE) #5500.572 (log10: 3.740408)
mean(intro.contTaxaSums$avg.mass, na.rm = TRUE) #86993.59 (log10: 4.939487)
ggplot() + #do histogram; .25 log 
    geom_histogram(aes(log10(intro.contTaxaSums$avg.mass)), 
                   colour = "gray", fill = "gray",
                   binwidth = .25) +
    plot_theme +
    theme(legend.position = c(0.8, 0.6)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)))
#sampling from most bins, more in bins 4.25 to 6; most in 4.75

intro.contTaxaSums %>%
    dplyr::group_by(n.cont, diet.breadth) %>%
    dplyr::summarise(n = n())
#all groups have dietary breadth of 1 or 2 not being really diff (except those on 4)
#those on 2 cont are the only ones that also have dietary breadth 3
#those on 4 cont tend to have dietary breadth of 1 rather than 2

intro.contTaxaSums %>%
    dplyr::group_by(n.cont) %>%
    dplyr::summarise(n.carn = sum(diet.carnivore.tot == TRUE),
                     n.invert = sum(diet.invertivore.tot == TRUE),
                     n.frug = sum(diet.frugivore.tot == TRUE),
                     n.graz = sum(diet.grazer.tot == TRUE),
                     n.brows = sum(diet.browser.tot == TRUE),
                     n.pisc = sum(diet.piscivore.tot == TRUE))
#1 continent: highest is brows (9) and frug (9), followed by carn (1); no pisc
#2 continent: most brows (8), then even for invert, frug, and graz (4), then even for carn and pisc (2); only group with pisc
#3 continent: most are brows (4); no frug or pisc
#4 continent: most are graz (4) or brows (3); no pisc
#5 continent: no invert, graz, or pisc

table(intro.contTaxaSums$family)
# Bovidae    Camelidae   Canidae Castoridae  Cercopithecidae 
# 7          1           2       1           1 
# Cervidae   Cuniculidae Equidae Felidae     Herpestidae 
# 9          1           2       1           3 
# Leporidae  Macropodidae    Muridae     Mustelidae  Petauridae 
# 3          1               6           3           1 
# Phalangeridae  Procyonidae     Pteropodidae    Sciuridae   Soricidae 
# 1              1               1               1           1 
# Suidae Viverridae 
# 1      2 

table(intro.contTaxaSums$order)
# Artiodactyla    Carnivora      Lagomorpha     Perissodactyla    Rodentia   Soricomorpha 
# 18              12             3              2                 9          1 
# Chiroptera    Diprotodontia   Primates
# 1             2               1

intro.contTaxaSums %>%
    dplyr::group_by(n.cont, family) %>%
    dplyr::summarise(n = n()) %>%
    as.data.frame()
#1 continent: grps with the most sp (3) are Cervids, Herpestidae, Muridae
#2 continent: grps with the most sp (3) is Cervidae
#4 continent: grps with the most sp (2) are Bovidae, Cervidae

intro.contTaxaSums %>%
    dplyr::group_by(n.cont, order) %>%
    dplyr::summarise(n = n()) %>%
    as.data.frame()

###### BY CONTINENT ------

mm.df %>% 
  group_by(continent) %>%
  summarise(n.order = length(unique(order)),
            n.family = length(unique(family)),
            n.genus = length(unique(genus)),
            n.sp = length(unique(binomial)))

#continent     n.order n.family n.genus  n.sp
# Africa             16       56     303  1168
# Australia          13       43     140   374
# Eurasia            14       60     360  1201
# Insular            22      100     535  1738
# Marine              3       13      42    86
# North.America      13       60     262   841
# South.America      17       62     345  1229

##### BY DIET -----

length(unique(mm.df$binomial[mm.df$diet.browser == TRUE])) #1933
length(unique(mm.df$binomial[mm.df$diet.grazer == TRUE])) #658
length(unique(mm.df$binomial[mm.df$diet.frugivore == TRUE])) #2529
length(unique(mm.df$binomial[mm.df$diet.carnivore == TRUE])) #357
length(unique(mm.df$binomial[mm.df$diet.piscivore == TRUE])) #114
length(unique(mm.df$binomial[mm.df$diet.invertivore == TRUE])) #2448

table(mm.df$diet.breadth[!duplicated(mm.df$binomial)])
# 0    1    2    3 
# 214 3266 1995  261 

##### N INVALID MASS -----

nrow(mm.df[mm.df$mass.status == "invalid",]) #73 records

length(unique(mm.df$order[mm.df$mass.status == "invalid"])) #6
length(unique(mm.df$family[mm.df$mass.status == "invalid"])) #15
length(unique(mm.df$genus[mm.df$mass.status == "invalid"])) #35
length(unique(mm.df$binomial[mm.df$mass.status == "invalid"])) #44

unique(mm.df$continent[mm.df$mass.status == "invalid"]) #all continents

mm.df$size.bin <- round(log10(mm.df$mass), digits =  0) #log10 size bins

table(mm.df$size.bin[mm.df$mass.status == "invalid"])
# 1  2   3  4 
# 41 22  4  5 

##### DATA DEFICIENT (IUCN) -----

nrow(mm.df[mm.df$iucn.status == "DD",]) #684 records

length(unique(mm.df$order[mm.df$iucn.status == "DD"])) #17
length(unique(mm.df$family[mm.df$iucn.status == "DD"])) #56
length(unique(mm.df$genus[mm.df$iucn.status == "DD"])) #267
length(unique(mm.df$binomial[mm.df$iucn.status == "DD"])) #616

unique(mm.df$continent[mm.df$iucn.status == "DD"]) #all continents

##### SIZE -----
table(mm.df$size.bin[mm.df$iucn.status == "DD"])
# 0  1   2    3    4   5   6    7 
# 1  95  127  42   9   2   11   8 

#### TRIM DATA ----

#remove humans
mm.df <- mm.df[mm.df$binomial != "Homo sapiens",]

#remove invalid mass estimates
mm.df <- mm.df[mm.df$mass.status != "invalid",]

##remove marine species
mm.df <- mm.df %>%
  # Order Cetacea (Whales s.l.)
  filter(order != "Cetacea") %>%
  # Order Sirenia (Sea cows s.l.):
  filter(order != "Sirenia") %>%
  # Families in the clade Pinnipedia (Seal s.l.):
  # Odobenidae (walruses)
  # Otariidae (fur seals and sea lions)
  # Phocidae (true seals)
  filter(!family %in% c("Odobenidae", "Otariidae", "Phocidae")) %>%
  # Other marine mammals:
  # Marine otter (Lontra felina)
  filter(binomial != "Lontra felina")
# Sea otter (Enhydra lutris)

##fix continents
# Search for continental mistakes
table(mm.df$continent, useNA = "always")

# Africa    Australia    Eurasia    Insular   North.America   South.America   <NA>
# 1157      358          1173       1768      813             1208            0 

# Remove NA's, Insulars, and Marine
mm.df <- filter(mm.df, !is.na(continent) &
                       continent != "Insular" &
                       continent != "Marine")

# Remove introduced and domesticated species
table(mm.df$extant.status)

#domesticated       extant      extinct   historical   introduction 
#6                  4403        226       30           44 

mm.df <- filter(mm.df, extant.status != "introduction",
                       extant.status != "domesticated")

# Checking for accidents
stopifnot(!any(str_trim(mm.df$genus) != mm.df$genus))
stopifnot(!any(str_trim(mm.df$species) != mm.df$species))

##remove continental duplicates
mm.df[(duplicated(mm.df[c("binomial", "continent")])),]

##check habitat.mode
#check that all bats are volant
unique(mm.df$habitat.mode)
unique(mm.df$order[mm.df$habitat.mode == "volant"])
unique(mm.df$habitat.mode[mm.df$order == "Chiroptera"])
mm.df$habitat.mode[mm.df$order == "Chiroptera"] <- "volant"
unique(mm.df$habitat.mode[mm.df$order == "Chiroptera"])

#remove marine
mm.df[mm.df$habitat.mode == "marine- births on land",] #enhydra lutris; remove
mm.df <- mm.df[mm.df$binomial != "Enhydra lutris",]

#fix redundancy
mm.df$habitat.mode[mm.df$habitat.mode == "terr; freshwater inland"] <- "terr _ aquatic"
mm.df$habitat.mode[mm.df$habitat.mode == "aquatic"] <- "terr _ aquatic"

nrow(mm.df[mm.df$habitat.mode == "",]) #missing 607

##### WHICH LEVEL IS DIET DATA CREATED? ----
unique(mm.df$diet.src)
nrow(mm.df[mm.df$diet.src == "species.diet",]) #3737
nrow(mm.df[mm.df$diet.src == "mean.genus",]) #849
nrow(mm.df[mm.df$diet.src == "mean.family",]) #73

##### CREATE LONG CONTINENT VERSION -----

mm.df <- mm.df %>% 
  mutate(Africa = continent == "Africa",
         North.America = continent == "North.America",
         South.America = continent == "South.America",
         Eurasia = continent == "Eurasia",
         Australia = continent == "Australia")

##### GROUP_BY SPECIES -----

#diets differ by continent, taking the widest diet possible
df.sums <- mm.df %>%
  group_by(binomial) %>%
  dplyr::summarise(diet.invertivore.tot = isTRUE(sum(diet.invertivore) > 0),
                   diet.carnivore.tot = isTRUE(sum(diet.carnivore) > 0),
                   diet.browser.tot = isTRUE(sum(diet.browser) > 0), 
                   diet.grazer.tot = isTRUE(sum(diet.grazer) > 0),
                   diet.piscivore.tot = isTRUE(sum(diet.piscivore) > 0),
                   diet.frugivore.tot = isTRUE(sum(diet.frugivore) > 0),
                   avg.mass = mean(mass, na.rm = TRUE),
                   n.cont = length(unique(continent)),
                   iucn = iucn.status[1])

df.continent <- mm.df %>%
  group_by(binomial) %>%
  dplyr::summarise(continent.Africa = as.logical(sum(Africa)),
                   continent.North.America = as.logical(sum(North.America)),
                   continent.South.America = as.logical(sum(South.America)),
                   continent.Eurasia = as.logical(sum(Eurasia)),
                   continent.Australia = as.logical(sum(Australia)))

df.taxa <- mm.df[!duplicated(mm.df$binomial),] %>%
  dplyr::select(order,
                family,
                genus,
                species,
                binomial,
                habitat.mode)

df.sumTaxa <- left_join(df.taxa, df.sums,
                        by = "binomial")
df.contTaxaSums <- left_join(df.sumTaxa, df.continent,
                             by = "binomial")
    
df.contTaxaSums$diet.breadth <- df.contTaxaSums %>%
  dplyr::select(starts_with("diet.")) %>% 
  rowSums()
table(df.contTaxaSums$diet.breadth)
# 1    2    3 
# 2463 1737  186 

df.contTaxaSums$n.cont <- df.contTaxaSums %>%
  dplyr::select(starts_with("continent.")) %>%
  rowSums()
table(df.contTaxaSums$n.cont)
# 1    2    3    4 
# 4120  260    5    1 

length(unique(mm.df$binomial)) #4385
nrow(df.contTaxaSums) #4385

##### ADD AGES -----

### Fossil age
#age data fossil = PBDB min & max occurence estiamtes. 
#This provides different fossil data at different resolutions. 
#The ages extracted here are based only on species level identifications of fossils. 
#All fossils are provided with a maximum and minimum estimated age. 
#To get the most likely age of species origin we found the oldest minimum species age, and the oldest maximum species age for each species. 
#The midpoint of this range was used as species age. Because of species name mismatches and missing species the following analysis includes 693 species out of 4443 possible.

foss.ages <- pbdb %>%
  group_by(accepted_name) %>%
  dplyr::summarise(binomial = accepted_name[1],
                   lw.range = max(min_ma),
                   hi.range = max(max_ma),
                   foss.age = (hi.range+lw.range)/2) 

### Faurby ages
#age data phyl = from Faurby tree estimates. 
#This source provides 1000 equally likely trees. 
#The species ages were extracted as branch length to the parent node of each species for all trees. 
#The estimate used for species ages were the median ages found by this method. Because of species name mismatches the following analysis includes 4019 species out of 4443 possible.

species.age.summary <- function(x) {
  c(age.mean = mean(x),
    age.median = median(x),
    age.lower.range = range(x)[1],
    age.upper.range = range(x)[2],
    age.q95 = quantile(x, .95),
    age.q05 = quantile(x, .05),
    age.sd = sd(x))
}
species.age <- apply(faurby.ages, 1, species.age.summary)
str(species.age)

species.age[1:7,1:5]

phyl.ages <- as.data.frame(t(species.age))
phyl.ages$binomial <- rownames(phyl.ages)
phyl.ages$binomial <- gsub("_", " ", phyl.ages$binomial)

phyl.ages <- phyl.ages %>%
  dplyr::select(binomial, age.median) 

# genus level
# age <- read.csv("age.csv", header = TRUE) #match on genus
# age <- age %>%
#   dplyr::select(-cont)
# genera <- unique(mom$genus)
# age <- age[(age$genus %in% genera),]

##### COMBINE DATA -----

###### TRIM DATASETS ------
##trim data
colnames(pacifici)
pacifici.trim <- pacifici %>%
  dplyr::select(binomial = Scientific_name, 
                Max_longevity_d, 
                Rspan_d, 
                AFR_d, 
                Calculated_GL_d, 
                GenerationLength_d)


colnames(origin)
origin.trim <- origin %>%
  dplyr::select(family,
                continent.family = final.continent)

colnames(ranges)
ranges.trim <- ranges %>%
  dplyr::select(binomial = Binomial.1.2,
                current.range,
                present.natural.range,
                current.range.km2,
                present.natural.range.km2)
ranges.trim$binomial <- gsub("_", " ", ranges.trim$binomial)

colnames(pantheria)
pantheria.trim <- pantheria %>%
  dplyr::select(binomial = MSW05_Binomial,
                home.range.km2 = X22.1_HomeRange_km2,
                gr.area.km2 = X26.1_GR_Area_km2,
                indiv.home.range.km2 = X22.2_HomeRange_Indiv_km2,
                dispersal.age.d = X7.1_DispersalAge_d)

###### MERGE TRIMMED DATASETS ------
  
df.origin <- left_join(df.contTaxaSums, origin.trim,
                       by = "family")

df.origin.gen <- left_join(df.origin, pacifici.trim,
                           by = "binomial") #why is it adding 2 rows?

df.origin.gen.foss <- left_join(df.origin.gen, foss.ages,
                                by = "binomial")

df.origin.gen.foss.phyl <- left_join(df.origin.gen.foss, phyl.ages,
                                     by = "binomial")

df.origin.gen.foss.phyl.ranges <- left_join(df.origin.gen.foss.phyl, ranges.trim,
                                            by = "binomial")

df.origin.gen.foss.phyl.ranges.pan <- left_join(df.origin.gen.foss.phyl.ranges, pantheria.trim,
                                                by = "binomial")

df <- df.origin.gen.foss.phyl.ranges.pan

df$n.cont[df$n.cont >= 3] <- "3+"
colnames(df)[colnames(df) == "continent.family"] <- "family.origin"

##### SIZE -----

## LOG MASS
df$log.mass <- log10(df$avg.mass)

df[c('log.mass')][sapply(df[c('log.mass')], is.infinite)] <- NA

## SIZE BINS
df$log.size.bin <- trunc(df$log.mass) #log10 size bins
df <- mutate(df, qtr.bin = cut(log.mass, breaks = seq(0, 7.25, .25)))

##### WRITE DATA FOR ANALYSES -----
write.csv(df, 
          "./Data/data.for.analyses.csv",
          row.names = FALSE)

#df <- read.csv("./Data/data.for.analyses.csv",
#               header = TRUE)

#### ABOUT DATA ----

## continent of origin
nrow(origin) #168
nrow(origin[origin$continent != "",]) #159

length(unique(df$family)) #135
length(unique(df$family[df$family.origin != ""])) #128

## dispersal information
# dispersal age from PanTHERIA
length(unique(df$binomial[!is.na(df$dispersal.age.d)])) #109
# generation length from Pacifici
length(unique(df$binomial[!is.na(df$GenerationLength_d)])) #3850

## fossil ages
# pbdb
length(unique(df$genus[!is.na(df$foss.age)])) #389
length(unique(df$binomial[df$foss.age > 0])) #661
# faurby
length(unique(df$genus[!is.na(df$age.median)])) #1026
length(unique(df$binomial[!is.na(df$age.median)])) #3967

#### BIASES IN DATA FOR ANALYSES ----

nrow(df) #4385
length(unique(df$order)) #28
length(unique(df$family)) #135
length(unique(df$genus)) #1071
length(unique(df$binomial)) #4385

## TABLE FOR COUNTS PER ORDER
df %>%
  group_by(order) %>%
  summarise(n = length(unique(binomial))) %>%
  as.data.frame()

##### SIZE -----

## GRAPH OF BODY SIZE DISTRIBUTION BY CONTINENT
bs.cont <- ggplot() +
  geom_density(aes(df$log.mass[!is.na(df$log.mass)]), colour = "black") +
  geom_density(aes(df$log.mass[df$continent.North.America == TRUE & !is.na(df$log.mass)]), colour = "#B4D9C8") +
  geom_density(aes(df$log.mass[df$continent.South.America == TRUE & !is.na(df$log.mass)]), colour = "#E2C9F2") +
  geom_density(aes(df$log.mass[df$continent.Eurasia == TRUE & !is.na(df$log.mass)]), colour = "#F2CDA0") +
  geom_density(aes(df$log.mass[df$continent.Australia == TRUE & !is.na(df$log.mass)]), colour = "#D9967E") +
  geom_density(aes(df$log.mass[df$continent.Africa == TRUE & !is.na(df$log.mass)]), colour = "#C2D991") +
  plot_theme +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_y_continuous(name = "Density")

ggsave(bs.cont, file = paste0("./Figures/bodyMassDensityByContinent",".png"), 
       width = 14, height = 10, units = "cm")

#qualitatively similar

## COUNT OF BODY MASS
nrow(df[!is.na(df$log.mass),]) #3316
nrow(df[is.na(df$log.mass),]) #1069

## BY CLADE

#from which groups are we missing things from?

## FAMILY LEVEL COUNTS OF MISSING MASS DATA
df.clade.mass <- df %>% 
  group_by(order) %>% 
  summarise(#order = order[1],
            n.mass = sum(!is.na(avg.mass)),
            n.na = sum(is.na(avg.mass)),
            percent = n.na/(n.na + n.mass),
            avg.size = mean(avg.mass, na.rm = TRUE)) %>% 
  as.data.frame()

## HOW MANY ORDERS?
unique(df.clade.mass$order) #29

write.csv(df.clade.mass,
          "./Results/clade.missing.mass.csv",
          row.names = FALSE)

## BY CONTINENT
## are certain continents more affected by this missing data?
nrow(df[df$continent.Africa == TRUE,]) #1150
nrow(df[df$continent.Africa == TRUE & is.na(df$log.mass),]) #357 (out of 1150; 31%)

nrow(df[df$continent.North.America == TRUE,]) #807
nrow(df[df$continent.North.America == TRUE & is.na(df$log.mass),]) #92 (out of 807; 11.4%)

nrow(df[df$continent.South.America == TRUE,]) #1200
nrow(df[df$continent.South.America == TRUE & is.na(df$log.mass),]) #277 (out of 1200; 14.8%)

nrow(df[df$continent.Eurasia == TRUE,]) #1165
nrow(df[df$continent.Eurasia == TRUE & is.na(df$log.mass),]) #325 (out of 1165; 28%

nrow(df[df$continent.Australia == TRUE,]) #336
nrow(df[df$continent.Australia == TRUE & is.na(df$log.mass),]) #35 (out of 336; 10%)

##### FOSSIL AGES ----
## PBDB COUNTS
length(unique(df$order[df$foss.age >= 0]))  #21
length(unique(df$family[df$foss.age >= 0])) #93
length(unique(df$genus[df$foss.age >= 0])) #390
length(unique(df$binomial[df$foss.age >= 0])) #661
nrow(df[is.na(df$foss.age),]) #3725

length(unique(df$binomial[df$foss.age >= 0 &
                          df$n.cont == 1])) #573
length(unique(df$binomial[df$foss.age >= 0 &
                            df$n.cont == 2])) #84
length(unique(df$binomial[df$foss.age >= 0 &
                            df$n.cont == "3+"])) #6

## FAURBY COUNTS
length(unique(df$order[df$age.median >= 0]))  #30
length(unique(df$family[df$age.median >= 0])) #136
length(unique(df$genus[df$age.median >= 0])) #1027
length(unique(df$binomial[df$age.median >= 0])) #3968
nrow(df[is.na(df$age.median),]) #418

length(unique(df$binomial[df$age.median >= 0 &
                            df$n.cont == 1])) #3711
length(unique(df$binomial[df$age.median >= 0 &
                            df$n.cont == 2])) #252
length(unique(df$binomial[df$age.median >= 0 &
                            df$n.cont == "3+"])) #6

## BY SIZE

## WHICH BODY SIZES ARE WE MISSING FOSSIL AGES FOR?
# PBDB
foss.na <- ggplot() +
  geom_histogram(aes(x = df$foss.age[is.na(df$log.mass) & !is.na(df$foss.age)])) + 
  plot_theme +
  scale_x_continuous(name = "Fossil Age (Ma)") +
  scale_y_continuous(name = "Number of Missing Mass Values")
#missing a lot of younger fossils, actually

ggsave(foss.na, file = paste0("./Figures/PBDBfossilAgeMissing",".png"), 
       width = 14, height = 10, units = "cm")

# FAURBY
phyl.na <- ggplot() +
  geom_histogram(aes(x = df$age.median[!is.na(df$age.median) & is.na(df$log.mass)])) + 
  plot_theme +
  scale_x_continuous(name = "Phylogenetic Age (Ma)") +
  scale_y_continuous(name = "Number of Missing Mass Values")
#missing a lot of younger fossils, actually

ggsave(phyl.na, file = paste0("./Figures/PhyloAgeMissing",".png"), 
       width = 14, height = 10, units = "cm")

## WHICH SIZES DO WE HAVE FOSSIL AGES FOR?
# PBDB
size.fossil.na <- ggplot() +
  geom_histogram(aes(x = df$log.mass[is.na(df$foss.age) & !is.na(df$log.mass)])) + 
  plot_theme +
  scale_x_continuous(name = "Log Body Size (g)") +
  scale_y_continuous(name = "Number of Missing Age Values")
#missing a lot of smaller bodied organisms, of course

ggsave(size.fossil.na, file = paste0("./Figures/FossilSizeMissing",".png"), 
       width = 14, height = 10, units = "cm")

# FAURBY
size.phylo.na <- ggplot() +
  geom_histogram(aes(x = df$log.mass[is.na(df$age.median) & !is.na(df$log.mass)])) + 
  plot_theme +
  scale_x_continuous(name = "Log Body Size (g)") +
  scale_y_continuous(name = "Number of Missing Age Values")
#missing a size classes from all over...looks weird

ggsave(size.phylo.na, file = paste0("./Figures/PhyloSizeMissing",".png"), 
       width = 14, height = 10, units = "cm")

## COUNTS BY LOG SIZE BIN
df.mass.age <- df %>% 
  group_by(log.size.bin) %>% 
  summarise(n.foss.age = sum(!is.na(foss.age)),
            n.foss.na = sum(is.na(foss.age)),
            foss.per = n.foss.na/(n.foss.na + n.foss.age),
            n.phyl.age = sum(!is.na(age.median)),
            n.phyl.na = sum(is.na(age.median)),
            phyl.per = n.phyl.na/(n.phyl.na + n.phyl.age)) %>% 
  as.data.frame()

df$age.TF <- FALSE
df$age.TF[!is.na(df$age.median)] <- TRUE
df$age.TF[!is.na(df$foss.age)] <- TRUE

anova(lm(age.TF ~ log.size.bin,
         data = df)) #sig difference in which sizes we have ages for

ggplot(df) +
    geom_bar(aes(x = log.size.bin, fill = age.TF))

write.csv(df.mass.age,
          "./Results/age.missing.mass.csv",
          row.names = FALSE)

## CORRELATION OF BODY MASS AND AGE?
#want to look for coverage in species (genus) age
#species (genus) age by body size
foss.age.lm <- ggplot() +
  geom_point(aes(x = df$log.mass[!is.na(df$log.mass) & !is.na(df$foss.age)], 
                 y = df$foss.age[!is.na(df$log.mass) & !is.na(df$foss.age)])) +
  geom_smooth(aes(x = df$log.mass[!is.na(df$log.mass) & !is.na(df$foss.age)], 
                  y = df$foss.age[!is.na(df$log.mass) & !is.na(df$foss.age)])) +
  plot_theme + 
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_y_continuous(name = "Fossil Age (Genus)")

ggsave(foss.age.lm , file = paste0("./Figures/lm.fossil.age",".png"), 
       width = 14, height = 10, units = "cm")

summary(lm(df$foss.age ~ df$avg.mass)) #p-value: 3.173e-07; Adjusted R-squared: 0.03849
#baseically 0, no explanitory power

phyl.age.lm <- ggplot() +
  geom_point(aes(x = df$log.mass[!is.na(df$log.mass) & !is.na(df$age.median)], 
                 y = df$age.median[!is.na(df$log.mass) & !is.na(df$age.median)])) +
  geom_smooth(aes(x = df$log.mass[!is.na(df$log.mass) & !is.na(df$age.median)], 
                  y = df$age.median[!is.na(df$log.mass) & !is.na(df$age.median)])) +
  plot_theme + 
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_y_continuous(name = "Median Phylogenetic Age (Genus)")

ggsave(phyl.age.lm , file = paste0("./Figures/lm.phylo.age",".png"), 
       width = 14, height = 10, units = "cm")

summary(lm(df$age.median ~ df$avg.mass)) #p-value: 0.001473; Adjusted R-squared: 0.002964
#basically 0, no explanatory power

## BY CLADE
#from which groups are we missing things from?

df.clade.foss <- df %>% 
  group_by(family) %>% 
  summarise(order = order[1],
            n.foss.fam = sum(!is.na(foss.age)),
            n.foss.na = sum(is.na(foss.age)),
            foss.per = n.foss.na/(n.foss.na + n.foss.fam),
            n.phyl.fam = sum(!is.na(age.median)),
            n.phyl.na = sum(is.na(age.median)),
            phyl.per = n.phyl.fam/(n.phyl.fam + n.phyl.na),
            avg.size = mean(avg.mass, na.rm = TRUE)) %>% 
  as.data.frame()

write.csv(df.clade.foss,
          "./Results/foss.missing.clade.csv",
          row.names = FALSE)

## BY CONTINENT

foss.Africa <- sum(!is.na(df$foss.age[df$continent.Africa == TRUE]))
foss.Africa.na <- sum(is.na(df$foss.age[df$continent.Africa == TRUE]))
per.foss.missing.Africa <- foss.Africa.na/(foss.Africa+foss.Africa.na)
  
phyl.Africa <- sum(!is.na(df$age.median[df$continent.Africa == TRUE]))
phyl.Africa.na <- sum(is.na(df$age.median[df$continent.Africa == TRUE]))
per.phyl.missing.Africa <- phyl.Africa.na/(phyl.Africa+phyl.Africa.na)
  
foss.Eurasia <- sum(!is.na(df$foss.age[df$continent.Eurasia == TRUE]))
foss.Eurasia.na <- sum(is.na(df$foss.age[df$continent.Eurasia == TRUE]))
per.foss.missing.Eurasia <- foss.Eurasia.na/(foss.Eurasia+foss.Eurasia.na)
  
phyl.Eurasia <- sum(!is.na(df$age.median[df$continent.Eurasia == TRUE]))
phyl.Eurasia.na <- sum(is.na(df$age.median[df$continent.Eurasia == TRUE]))
per.phyl.missing.Eurasia <- phyl.Eurasia.na/(phyl.Eurasia+phyl.Eurasia.na)
  
foss.Australia <- sum(!is.na(df$foss.age[df$continent.Australia == TRUE]))
foss.Australia.na <- sum(is.na(df$foss.age[df$continent.Australia == TRUE]))
per.foss.missing.Australia <- foss.Australia.na/(foss.Australia+foss.Australia.na)
  
phyl.Australia <- sum(!is.na(df$age.median[df$continent.Australia == TRUE]))
phyl.Australia.na <- sum(is.na(df$age.median[df$continent.Australia == TRUE]))
per.phyl.missing.Australia <- phyl.Australia.na/(phyl.Australia+phyl.Australia.na)
  
foss.North.America <- sum(!is.na(df$foss.age[df$continent.North.America == TRUE]))
foss.North.America.na <- sum(is.na(df$foss.age[df$continent.North.America == TRUE]))
per.foss.missing.North.America <- foss.North.America.na/(foss.North.America+foss.North.America.na)
  
phyl.North.America <- sum(!is.na(df$age.median[df$continent.North.America == TRUE]))
phyl.North.America.na <- sum(is.na(df$age.median[df$continent.North.America == TRUE]))
per.phyl.missing.North.America <- phyl.North.America.na/(phyl.North.America+phyl.North.America.na)
  
foss.South.America <- sum(!is.na(df$foss.age[df$continent.South.America == TRUE]))
foss.South.America.na <- sum(is.na(df$foss.age[df$continent.South.America == TRUE]))
per.foss.missing.South.America <- foss.South.America.na/(foss.South.America+foss.South.America.na)

phyl.South.America <- sum(!is.na(df$age.median[df$continent.South.America == TRUE]))
phyl.South.America.na <- sum(is.na(df$age.median[df$continent.South.America == TRUE]))
per.phyl.missing.South.America <- phyl.South.America.na/(phyl.South.America+phyl.South.America.na)
  
df.cont.foss <- cbind(foss.Africa, foss.Africa.na, per.foss.missing.Africa,
                      phyl.Africa, phyl.Africa.na, per.phyl.missing.Africa,
                      foss.Eurasia, foss.Eurasia.na, per.foss.missing.Eurasia,
                      phyl.Eurasia, phyl.Eurasia.na, per.phyl.missing.Eurasia,
                      foss.Australia, foss.Australia.na, per.foss.missing.Australia,
                      phyl.Australia, phyl.Australia.na, per.phyl.missing.Australia,
                      foss.North.America, foss.North.America.na, per.foss.missing.North.America,
                      phyl.North.America, phyl.North.America.na, per.phyl.missing.North.America,
                      foss.South.America, foss.South.America.na, per.foss.missing.South.America,
                      phyl.South.America, phyl.South.America.na, per.phyl.missing.South.America)

write.csv(df.cont.foss,
          "./Results/foss.missing.cont.csv",
          row.names = FALSE)

##### DISPERSAL ------
nrow(df[!is.na(df$dispersal.age.d),])

table(df$log.size.bin[!is.na(df$dispersal.age.d)])
# 1  2  3  4  5  6 
# 3 24 44 23 12  2 
table(df$log.size.bin[is.na(df$dispersal.age.d)])
# 0    1    2    3    4    5    6    7 
# 385 1312  696  392  229  148   44    2 

nrow(df[!is.na(df$dispersal.age.d),]) #108
nrow(df[!is.na(df$GenerationLength_d),]) #3850

##### CONTINENT OF ORIGIN ------
## FAMILY ORIGIN

length(unique(df$family[df$family.origin != ""])) #128
nrow(df[df$family.origin != "",]) #3571

length(unique(df$family[df$family.origin == ""])) #7
length(df$binomial[df$family.origin == ""]) #814
table(df$order[df$family.origin == ""])
# "Chiroptera"  "Rodentia" "Paucituberculata" "Carnivora"  "Artiodactyla"  
#  87            713        6                  1            7

#which groups have the most species that we are missing data for and that would 
#affect our family origin results?
length(unique(df$family[df$family.origin == "" & df$order == "Chiroptera"])) #2
unique(df$family[df$family.origin == "" & df$order == "Chiroptera"]) 
#Molossidae and Nycteridae
nrow(df[df$family.origin == "" &
          df$family == "Molossidae",])
View(df[df$family == "Molossidae" & df$family.origin == "" & df$n.cont == 2,])

nrow(df[df$family.origin == "" &
          df$family == "Nycteridae",])

length(unique(df$family[df$family.origin == "" & df$order == "Rodentia"])) #2
unique(df$family[df$family.origin == "" & df$order == "Rodentia"]) 
#Muridae and Sciuridae
nrow(df[df$family.origin == "" &
          df$family == "Muridae",])
nrow(df[df$family.origin == "" &
          df$family == "Sciuridae",])

table(df$n.cont[df$family == "Muridae"])
table(df$n.cont[df$family == "Sciuridae"])

## COUNTS BY CONTINENT
nrow(df[df$family.origin != "",]) #3571
df %>%
  filter(family.origin != "") %>%
  group_by(family.origin) %>%
  summarise(N = n())
# family.origin     N
# Africa          319 (9%)
# Australia       205 (5.7%)
# Eurasia        1753 (49%)
# North.America   636 (17.8%)
# South.America   658 (18.4%)
#total is 3571 for %

## SIZE
nrow(df[!is.na(df$family.origin) & is.na(df$log.mass),]) #1069

##### CONTINENT -----

## counts
length(unique(df$binomial[df$continent.North.America == TRUE])) #807
length(unique(df$binomial[df$continent.South.America == TRUE])) #1200
length(unique(df$binomial[df$continent.Eurasia == TRUE])) #1165
length(unique(df$binomial[df$continent.Africa == TRUE])) #1150
length(unique(df$binomial[df$continent.Australia == TRUE])) #336

#want to check for taxonomic and geographic coverage
cont.counts <- df %>%
  dplyr::select(starts_with("continent.")) %>%
  dplyr::summarise_all(sum)
#  Africa   North.America   South.America   Eurasia   Australia
#  1150     807             1200            1165      336

## BY SIZE
# which continents are missing the most mass data?
mass.Africa <- sum(!is.na(df$avg.mass[df$continent.Africa == TRUE]))
mass.Africa.na  <- sum(is.na(df$avg.mass[df$continent.Africa == TRUE]))
per.Africa.missing.mass <- mass.Africa.na/(mass.Africa.na+mass.Africa)

mass.Eurasia <- sum(!is.na(df$avg.mass[df$continent.Eurasia == TRUE]))
mass.Eurasia.na  <- sum(is.na(df$avg.mass[df$continent.Eurasia == TRUE]))
per.Eurasia.missing.mass <- mass.Eurasia.na/(mass.Eurasia.na+mass.Eurasia)

mass.Australia <- sum(!is.na(df$avg.mass[df$continent.Australia == TRUE]))
mass.Australia.na  <- sum(is.na(df$avg.mass[df$continent.Australia == TRUE]))
per.Australia.missing.mass <- mass.Australia.na/(mass.Australia.na+mass.Australia)

mass.North.America <- sum(!is.na(df$avg.mass[df$continent.North.America == TRUE]))
mass.North.America.na  <- sum(is.na(df$avg.mass[df$continent.North.America == TRUE]))
per.North.America.missing.mass <- mass.North.America.na/(mass.North.America.na+mass.North.America)

mass.South.America <- sum(!is.na(df$avg.mass[df$continent.South.America == TRUE]))
mass.South.America.na  <- sum(is.na(df$avg.mass[df$continent.South.America == TRUE]))
per.South.America.missing.mass <- mass.South.America.na/(mass.South.America.na+mass.South.America)

df.cont.mass <- cbind(mass.Africa, mass.Africa.na, per.Africa.missing.mass,
                      mass.Eurasia, mass.Eurasia.na, per.Eurasia.missing.mass,
                      mass.Australia, mass.Australia.na, per.Australia.missing.mass,
                      mass.North.America, mass.North.America.na, per.North.America.missing.mass,
                      mass.South.America, mass.South.America.na, per.South.America.missing.mass)

write.csv(df.cont.mass,
          "./Results/cont.missing.mass.csv",
          row.names = FALSE)

##### DIET TYPE ----

## missing diet for?
diet.counts <- df %>%
  dplyr::select(starts_with("diet.")) %>%
  dplyr::summarise_all(sum)

unique(df$diet.breadth)
table(df$diet.breadth)
#1    2    3 
#2462 1737  186 

## BY SIZE
# which diet types are missing the most mass data?
nrow(df[is.na(df$log.mass),]) #1069

df$mass.TF <- !is.na(df$avg.mass) #TRUE = mass, FALSE = no mass

df.melt <- melt(df, measure.vars = c("diet.invertivore.tot", 
                                     "diet.carnivore.tot",
                                     "diet.browser.tot",
                                     "diet.grazer.tot",
                                     "diet.piscivore.tot",
                                     "diet.frugivore.tot"),
                variable.name = "diet.type",
                value.name = "has.diet")
df.diet <- df.melt[df.melt$has.diet == TRUE,]

df.diet %>%
  group_by(diet.type) %>%
  summarise(n.mass = sum(mass.TF == TRUE),
            n.na = sum(mass.TF == FALSE))
#diet.type              n.mass  n.na
# diet.invertivore.tot   1423   549
# diet.carnivore.tot      282    48
# diet.browser.tot       1240   380
# diet.grazer.tot         495   119
# diet.piscivore.tot       28     1
# diet.frugivore.tot     1462   467

nrow(df[!is.na(df$diet.breadth) & is.na(df$log.mass),]) #1069

ggplot(data = df.diet) + 
  geom_bar(aes(fill = mass.TF, x = diet.type),
           position = 'stack', stat = 'count') 

df.diet.mass <- df.diet %>%
  group_by(diet.type) %>%
  summarise(n.mass = sum(!is.na(avg.mass)),
            n.na = sum(is.na(avg.mass)),
            per = n.na/(n.na+n.mass)) %>% 
  as.data.frame()

write.csv(df.diet.mass,
          "./Results/diet.missing.mass.csv",
          row.names = FALSE)

##### DIET BREADTH ----

table(df$diet.breadth)
#1     2      3 
#2462  1737   186 

##### N DATA DEFICIENT (IUCN) -----

dd <- df[df$iucn == "DD",]
#[2 rows are NA]

nrow(dd) #434 records (minus 2 NA rows)

## What size range are these records?
table(dd$log.size.bin)
# 0   1   2   3   4 
# 25  84  59  14  3 

## Which continent are most of these records on?
table(dd$continent.Africa) #137 TRUE; 295 FALSE (32%)
table(dd$continent.Eurasia) #105 TRUE; 327 FALSE (24%)
table(dd$continent.Australia) #6 TRUE; 426 FALSE (1%)
table(dd$continent.North.America) #25 TRUE: 407 FALSE (6%)
table(dd$continent.South.America) #166 TRUE; 266 FALSE (38%)

## Which clades are most of these records from?
table(dd$order)
#Afrosoricida   Artiodactyla        Carnivora           Chiroptera 
#3              10                  3                   103 
#Cingulata      Dasyuromorphia      Didelphimorphia     Erinaceomorpha 
#5              2                   10                  1 
#Lagomorpha     Macroscelidea       Primates            Rodentia 
#3              3                   4                   222 
#Soricomorpha 
#63 
#most Chiroptera, Rodents
#may be why we don't have information for ranges?

##what is overlap between DD and other missing information?

#missing size
nrow(dd) #434
nrow(dd[is.na(dd$avg.mass),]) #249 (out of 434; 57%); 2 rows NA

#missing diet
View(dd[is.na(dd$diet.breadth),]) #0 (all NA)

#missing continent
View(dd[is.na(dd$n.cont),]) #0
View(dd[is.na(dd$family.origin),]) #0

#missing age
nrow(dd[is.na(dd$foss.age),]) #430
nrow(dd[is.na(dd$age.median),]) #32

##### DISPERSAL DATA -----

## make dataset of what need to calculate dispersal

df$AFR_d <- as.numeric(df$AFR_d)

df.dispersal <- df %>%
  dplyr::group_by(binomial) %>%
  dplyr::summarise(n = n(),
                   foss.avg.age = foss.age*1000000, #in yrs
                   age = age.median*1000000, #in yrs
                   avg.mass = avg.mass,
                   hmrg = home.range.km2,
                   disp.age = dispersal.age.d,
                   gen.length = GenerationLength_d,
                   repro.age = AFR_d, 
                   n.cont = n.cont,
                   family = family,
                   order = order,
                   family.origin = family.origin,
                   carn = isTRUE(sum(diet.piscivore.tot + diet.invertivore.tot + diet.carnivore.tot) >= 1 & sum(diet.browser.tot + diet.grazer.tot + diet.frugivore.tot) == 0)) %>%
  as.data.frame()

## missing dispersal
df.dispersal %>% 
  drop_na() %>%
  summarise(n = n()) #65 records that have everything

df.dispersal %>%
  dplyr::select(-foss.avg.age) %>%
  drop_na() %>%
  summarise(n = n()) #80 for phylo age

table(df$qtr.bin[is.na(df$dispersal.age.d)])
#small size bins are bigger, should do percent
unique(df$dispersal.age.d[df$avg.mass < 10]) #only NA

#### Q1: NUM SP PER CONTINENT ----
#Tally number of species on 1, 2, or 3+ continents

tot <- length(unique(df$binomial)) #4385
n.one <- length(unique(df$binomial[df$n.cont == 1])) #4119
per.one <- n.one/tot #93.94%
n.two <- length(unique(df$binomial[df$n.cont == 2])) #260
per.two <- n.two/tot #5.93%
n.more <- length(unique(df$binomial[df$n.cont == "3+"])) #6
per.more <- n.more/tot #0.14%

unique(df[which(df$n.cont == "3+"), c("order", "family", "binomial")])
# "Miniopterus schreibersii" "Mustela nivalis"          "Vulpes vulpes"           
# "Ursus arctos"             "Cervus elaphus"           "Panthera leo" 

#how many on each continent?
nrow(df[df$continent.North.America == TRUE,]) #807
nrow(df[df$continent.North.America == TRUE,])/tot #18.4%

nrow(df[df$continent.South.America == TRUE,]) #1200
nrow(df[df$continent.South.America == TRUE,])/tot #27.4%

nrow(df[df$continent.Eurasia == TRUE,]) #1165
nrow(df[df$continent.Eurasia == TRUE,])/tot #26.6

nrow(df[df$continent.Africa == TRUE,]) #1150
nrow(df[df$continent.Africa == TRUE,])/tot #26.2

nrow(df[df$continent.Australia == TRUE,]) #336
nrow(df[df$continent.Australia == TRUE,])/tot #7.7

#how many between continents
nrow(df[df$n.cont != 1 & 
        df$continent.North.America == TRUE &
        df$continent.South.America == TRUE,]) #163
nrow(df[df$n.cont != 1 & 
        df$continent.North.America == TRUE &
        df$continent.South.America == TRUE,])/tot #3.7%
nrow(df[df$n.cont != 1 & 
        df$continent.North.America == TRUE &
        df$continent.South.America == TRUE,])/nrow(df[df$continent.North.America == TRUE |
                                                      df$continent.South.America == TRUE,]) #8.8% are on both

nrow(df[df$n.cont != 1 & 
        df$continent.North.America == TRUE &
        df$continent.Eurasia == TRUE,]) #20
nrow(df[df$n.cont != 1 & 
        df$continent.North.America == TRUE &
        df$continent.Eurasia == TRUE,])/tot #.5%
nrow(df[df$n.cont != 1 & 
        df$continent.North.America == TRUE &
        df$continent.Eurasia == TRUE,])/nrow(df[df$continent.North.America == TRUE |
                                                df$continent.Eurasia == TRUE,]) #.1%

nrow(df[df$n.cont != 1 & 
        df$continent.North.America == TRUE &
        df$continent.Africa == TRUE,]) #5
nrow(df[df$n.cont != 1 & 
        df$continent.North.America == TRUE &
        df$continent.Africa == TRUE,])/tot #0.1%

nrow(df[df$n.cont != 1 & 
        df$continent.North.America == TRUE &
        df$continent.Australia == TRUE,]) #0

nrow(df[df$n.cont != 1 & 
        df$continent.South.America == TRUE &
        df$continent.Eurasia == TRUE,]) #1
nrow(df[df$n.cont != 1 & 
        df$continent.South.America == TRUE &
        df$continent.Eurasia == TRUE,])/tot #0.02

nrow(df[df$n.cont != 1 & 
        df$continent.South.America == TRUE &
        df$continent.Africa == TRUE,]) #1
nrow(df[df$n.cont != 1 & 
        df$continent.South.America == TRUE &
        df$continent.Africa == TRUE,])/tot #.02

nrow(df[df$n.cont == 2 & 
        df$continent.South.America == TRUE &
        df$continent.Australia == TRUE,]) #0

nrow(df[df$n.cont != 1 & 
        df$continent.Eurasia == TRUE &
        df$continent.Africa == TRUE,]) #85
nrow(df[df$n.cont != 1 & 
        df$continent.Eurasia == TRUE &
        df$continent.Africa == TRUE,])/tot #1.9%
nrow(df[df$n.cont != 1 & 
        df$continent.Eurasia == TRUE &
        df$continent.Africa == TRUE,])/nrow(df[df$continent.Eurasia == TRUE |
                                               df$continent.Africa == TRUE,]) #3.8%

nrow(df[df$n.cont != 1 & 
        df$continent.Eurasia == TRUE &
        df$continent.Australia == TRUE,]) #5
nrow(df[df$n.cont != 1 & 
        df$continent.Eurasia == TRUE &
        df$continent.Australia == TRUE,])/tot #0.1%
nrow(df[df$n.cont != 1 & 
        df$continent.Eurasia == TRUE &
        df$continent.Australia == TRUE,])/nrow(df[df$continent.Eurasia == TRUE |
                                                  df$continent.Australia == TRUE,]) #0.3%

nrow(df[df$n.cont != 1 & 
        df$continent.Africa == TRUE &
        df$continent.Australia == TRUE,]) #1
nrow(df[df$n.cont != 1 & 
        df$continent.Africa == TRUE &
        df$continent.Australia == TRUE,])/tot #0.02

## Which groups are most common in each group?
sort(table(df$order[df$n.cont == "1"])) #Rodentia
sort(table(df$family[df$n.cont == "1"])) #Cricetidae
head(sort(table(df$genus[df$n.cont == "1"]), decreasing = TRUE)) #Crocidura

sort(table(df$order[df$n.cont == "2"])) #Chiroptera
sort(table(df$family[df$n.cont == "2" & df$order == "Chiroptera"])) #Phyllostomidae
sort(table(df$genus[df$n.cont == "2" & df$order == "Chiroptera"])) #Myotis

sort(table(df$order[df$n.cont == "2" & df$habitat.mode != "volant"])) #Carnivora with Rodentia as a close second
sort(table(df$family[df$n.cont == "2" & df$habitat.mode != "volant"])) #Muridae then Felidae
sort(table(df$genus[df$n.cont == "2" & df$habitat.mode != "volant"])) #Sylvilagus, Felis, Leopardus, Mustela, Vulpes

##### WHAT IS SPECIAL ABOUT THESE 6? -----
##what is so special about these six?
## WHAT ELSE HAS SAME METADATA (diet, family origin, but NOT family or order; discount dispersal because everything can get everywhere)
## see what is similar to red fox or ursus; include log.size.bin

## Vulpes vulpes
nrow(df[df$family.origin == "North.America" & 
          df$log.size.bin == 3 & 
          df$diet.carnivore.tot == TRUE & 
          df$diet.invertivore.tot == TRUE &
          df$diet.breadth == 2,])
#8 species similar to fox (including fox) (1 row NA)
# only 1, the golden jackal, on 2 continents; the rest on 1
# maybe low speciation for foxes or high enough pop density to maintain and not separate
fox.sp <- df$binomial[df$family.origin == "North.America" & 
               df$log.size.bin == 3 & 
               df$diet.carnivore.tot == TRUE & 
               df$diet.invertivore.tot == TRUE &
               df$diet.breadth == 2]
View(mm.df[mm.df$binomial %in% fox.sp,])

## Ursus arctos
nrow(df[df$family.origin == "North.America" & 
          df$log.size.bin == 5 & 
          df$diet.carnivore.tot == TRUE & 
          df$diet.frugivore.tot == TRUE &
          df$diet.breadth == 2,] %>%
         drop_na(order))
#2 (including Ursus arctos)
#other one is cave bear and Ursus americanus; both on 1

#if allow for fish too
xx <- df[df$family.origin == "North.America" & 
           df$log.size.bin == 5 & 
           df$diet.breadth >= 2,]

xx[xx$diet.carnivore.tot == TRUE | 
     xx$diet.frugivore.tot == TRUE |
     xx$diet.piscivore.tot == TRUE,] %>%
    drop_na(order) 
#JUST BEARS

## Mustela nivalis
nrow(df[df$family.origin == "Eurasia" & 
          df$log.size.bin == 2 & 
          df$diet.carnivore.tot == TRUE & 
          df$diet.invertivore.tot == TRUE &
          df$diet.breadth == 2,] %>%
         drop_na(order))
# 13 species (including Mustela are similar)
# 1 on two; the rest on 1

must.sp <- df$binomial[df$family.origin == "Eurasia" & 
                df$log.size.bin == 2 & 
                df$diet.carnivore.tot == TRUE & 
                df$diet.invertivore.tot == TRUE &
                df$diet.breadth == 2]
View(df[df$binomial %in% must.sp,])

## Cervus elaphus
nrow(df[df$family.origin == "Eurasia" & 
          df$log.size.bin == 5 & 
          df$diet.browser.tot == TRUE & 
          df$diet.breadth == 1,] %>%
         drop_na(order))
# 16 things similar (including Cervus elephas)
cerv.sp <- df$binomial[df$family.origin == "Eurasia" & 
                df$log.size.bin == 5 & 
                df$diet.browser.tot == TRUE & 
                df$diet.breadth == 1]
View(df[df$binomial %in% cerv.sp,])

## Panthera leo
df[df$family.origin == "Eurasia" & 
   df$log.size.bin == 5 & 
   df$diet.carnivore.tot == TRUE & 
   df$diet.breadth == 1,] %>%
    drop_na(order)
#6 things including P. leo similar; all Felids

fel.sp <- df$binomial[df$family.origin == "Eurasia" & 
               df$log.size.bin == 5 & 
               df$diet.carnivore.tot == TRUE & 
               df$diet.breadth == 1]
View(df[df$binomial %in% fel.sp,])

## Miniopterus schreibersii
yy <- df[df$family.origin == "North.America" & 
         df$log.size.bin == 1 & 
         df$diet.invertivore.tot == TRUE & 
         df$diet.breadth == 1,] %>%
    drop_na(order)
nrow(yy) #70 incl. bat
table(yy$n.cont)
# 1  2   3+ 
# 58 11  1 
nrow(yy[yy$continent.Africa == TRUE,]) #18
nrow(yy[yy$continent.North.America == TRUE,]) #16
nrow(yy[yy$continent.South.America == TRUE,]) #9
nrow(yy[yy$continent.Eurasia == TRUE,]) #32
nrow(yy[yy$continent.Australia == TRUE,]) #8
bat.sp <- yy$binomial
View(df[df$binomial %in% bat.sp,])

#### H1: CONNECTIVITY ----

#calculate sørensen index
sorensen <- function(x,y) {
  index = (2*(length(intersect(x, y))))/(length(x) + length(y))
  return(index)
}

continents <- c("North.America", "South.America", "Eurasia", "Africa", "Australia")
indeces <- matrix(nrow = 5, ncol = 5, dimnames = list(continents, continents))

indeces[1,2] <- sorensen(x = df$binomial[df$continent.North.America == TRUE], 
                         y = df$binomial[df$continent.South.America == TRUE])
indeces[1,3] <- sorensen(x = df$binomial[df$continent.North.America == TRUE], 
                         y = df$binomial[df$continent.Eurasia == TRUE])
indeces[1,4] <- sorensen(x = df$binomial[df$continent.North.America == TRUE], 
                         y = df$binomial[df$continent.Africa == TRUE])
indeces[1,5] <- sorensen(x = df$binomial[df$continent.North.America == TRUE], 
                         y = df$binomial[df$continent.Australia == TRUE])
indeces[2, 3] <- sorensen(x = df$binomial[df$continent.South.America == TRUE], 
                          y = df$binomial[df$continent.Eurasia == TRUE])
indeces[2,4] <- sorensen(x = df$binomial[df$continent.South.America == TRUE], 
                         y = df$binomial[df$continent.Africa == TRUE])
indeces[2,5] <- sorensen(x = df$binomial[df$continent.South.America == TRUE], 
                         y = df$binomial[df$continent.Australia == TRUE])
indeces[3,4] <- sorensen(x = df$binomial[df$continent.Eurasia == TRUE], 
                         y = df$binomial[df$continent.Africa == TRUE])
indeces[3,5] <- sorensen(x = df$binomial[df$continent.Eurasia == TRUE], 
                         y = df$binomial[df$continent.Australia == TRUE])
indeces[4,5] <- sorensen(x = df$binomial[df$continent.Africa == TRUE], 
                         y = df$binomial[df$continent.Australia == TRUE])
write.csv(indeces, 
          "./Results/sorensen.index.csv",
          row.names = FALSE)

## look at which groups are unique to each continent
setdiff(unique(df$family[df$n.cont == 1 & df$continent.Africa == TRUE]),
        unique(df$family[df$n.cont == 1 & df$continent.Africa == FALSE]))
setdiff(unique(df$family[df$n.cont == 1 & df$continent.North.America == TRUE]),
        unique(df$family[df$n.cont == 1 & df$continent.North.America == FALSE]))
setdiff(unique(df$family[df$n.cont == 1 & df$continent.South.America == TRUE]),
        unique(df$family[df$n.cont == 1 & df$continent.South.America == FALSE]))
setdiff(unique(df$family[df$n.cont == 1 & df$continent.Eurasia == TRUE]),
        unique(df$family[df$n.cont == 1 & df$continent.Eurasia == FALSE]))
setdiff(unique(df$family[df$n.cont == 1 & df$continent.Australia == TRUE]),
        unique(df$family[df$n.cont == 1 & df$continent.Australia == FALSE]))

setdiff(unique(df$order[df$n.cont == 1 & df$continent.Africa == TRUE]),
        unique(df$order[df$n.cont == 1 & df$continent.Africa == FALSE]))
setdiff(unique(df$order[df$n.cont == 1 & df$continent.North.America == TRUE]), #No orders unique to North America!
        unique(df$order[df$n.cont == 1 & df$continent.North.America == FALSE]))
setdiff(unique(df$order[df$n.cont == 1 & df$continent.South.America == TRUE]),
        unique(df$order[df$n.cont == 1 & df$continent.South.America == FALSE]))
setdiff(unique(df$order[df$n.cont == 1 & df$continent.Eurasia == TRUE]),
        unique(df$order[df$n.cont == 1 & df$continent.Eurasia == FALSE]))
setdiff(unique(df$order[df$n.cont == 1 & df$continent.Australia == TRUE]),
        unique(df$order[df$n.cont == 1 & df$continent.Australia == FALSE]))

## which families are unique to certain pairs of continents?
setdiff(unique(df$family[df$n.cont == 2 & df$continent.Africa == TRUE & df$continent.Eurasia == TRUE]),
        unique(df$family[df$n.cont == 2 & df$continent.Africa == FALSE & df$continent.Eurasia == FALSE]))
setdiff(unique(df$family[df$n.cont == 2 & df$continent.South.America == TRUE & df$continent.North.America == TRUE]),
        unique(df$family[df$n.cont == 2 & df$continent.South.America == FALSE & df$continent.North.America == FALSE]))
setdiff(unique(df$family[df$n.cont == 2 & df$continent.North.America == TRUE & df$continent.Eurasia == TRUE]),
        unique(df$family[df$n.cont == 2 & df$continent.North.America == FALSE & df$continent.Eurasia == FALSE]))
setdiff(unique(df$family[df$n.cont == 2 & df$continent.Australia == TRUE & df$continent.Eurasia == TRUE]),
        unique(df$family[df$n.cont == 2 & df$continent.Australia == FALSE & df$continent.Eurasia == FALSE]))

setdiff(unique(df$order[df$n.cont == 2 & df$continent.Africa == TRUE & df$continent.Eurasia == TRUE]),
        unique(df$order[df$n.cont == 2 & df$continent.Africa == FALSE & df$continent.Eurasia == FALSE]))
setdiff(unique(df$order[df$n.cont == 2 & df$continent.South.America == TRUE & df$continent.North.America == TRUE]),
        unique(df$order[df$n.cont == 2 & df$continent.South.America == FALSE & df$continent.North.America == FALSE]))
setdiff(unique(df$order[df$n.cont == 2 & df$continent.North.America == TRUE & df$continent.Eurasia == TRUE]),
        unique(df$order[df$n.cont == 2 & df$continent.North.America == FALSE & df$continent.Eurasia == FALSE]))
setdiff(unique(df$order[df$n.cont == 2 & df$continent.Australia == TRUE & df$continent.Eurasia == TRUE]),
        unique(df$order[df$n.cont == 2 & df$continent.Australia == FALSE & df$continent.Eurasia == FALSE]))

##which two continents are species on?
df[df$n.cont == 2,] %>%
  summarise(n.AF = sum(continent.Africa == TRUE),
            n.EA = sum(continent.Eurasia == TRUE),
            n.NA = sum(continent.North.America == TRUE),
            n.SA = sum(continent.South.America == TRUE),
            n.AU = sum(continent.Australia == TRUE))
#most on NA and SA; followed by AF and EA
# n.AF n.EA n.NA n.SA n.AU
# 79   98   177  162    4

##which groups of limited dispersers are groups on?
on2 <- df[df$n.cont == 2,]
on2.cont <- on2 %>%
  group_by(order) %>%
  dplyr::summarise(N = n(),
                   N.Africa.Eurasia = length(continent.Africa[continent.Africa == TRUE & continent.Eurasia == TRUE]),
                   N.Africa.Australia = length(continent.Africa[continent.Africa == TRUE & continent.Australia == TRUE]),
                   N.Africa.North.America = length(continent.Africa[continent.Africa == TRUE & continent.North.America == TRUE]),
                   N.Africa.South.America = length(continent.Africa[continent.Africa == TRUE & continent.South.America == TRUE]),
                   N.Australia.Eurasia = length(continent.Australia[continent.Australia == TRUE & continent.Eurasia == TRUE]),
                   N.Australia.South.America = length(continent.Australia[continent.Australia == TRUE & continent.South.America == TRUE]),
                   N.Australia.North.America = length(continent.Australia[continent.Australia == TRUE & continent.North.America == TRUE]),
                   N.Eurasia.North.America = length(continent.Eurasia[continent.Eurasia == TRUE & continent.North.America == TRUE]),
                   N.Eurasia.South.America = length(continent.Eurasia[continent.Eurasia == TRUE & continent.South.America == TRUE]),
                   N.South.America.North.America = length(continent.South.America[continent.South.America == TRUE & continent.North.America == TRUE])) %>%
  as.data.frame()

##### CLOSER LOOK AT LIMITED DISPERSERS -----
#eutherians v marsupial limited dispersals
on2.n <- on2 %>%
  group_by(order, n.cont) %>%
  dplyr::summarise(count = n()) %>%
  as.data.frame()

marsup <- c("Didelphimorphia", "Paucituberculata", "Microbiotheria",
            "Dasyuromorphia", "Peramelemorphia", "Notoryctemorphia",
            "Diprotodontia")
on2.n[on2.n$order %in% marsup,] #only didelmorphia
length(unique(df$binomial[df$order == "Didelphimorphia"])) #87 total
length(unique(on2.n$binomial[on2.n$order == "Didelphimorphia"])) #5 (5.755 of total Didelmorphia diversity (5/87))
length(unique(df$binomial[df$order == "Didelphimorphia" &
                          df$continent.South.America == TRUE])) #83 (represent 6%; 83/1200)
length(unique(df$binomial[df$continent.South.America == TRUE])) #1200
length(unique(df$binomial[df$continent.South.America == TRUE &
                            df$n.cont == 2])) #162

## are things in trees not wide-ranging?
nrow(df[df$habitat.mode == "arboreal",]) #480
nrow(df[df$habitat.mode == "arboreal" & df$n.cont == "1",]) #470
#which are on 2? are they from the new world?
df[df$habitat.mode == "arboreal" & df$n.cont == "2",]
#yes, all on North and South America

table(df$habitat.mode[df$n.cont == "2"])
#aquatic            arboreal        terr    terr _ aquatic 
#2                  2               10      100               2 
#terr- fossorial    volant 
#2                  142 
#most volant, then terr
df$binomial[df$habitat.mode == "terr _ aquatic" & df$n.cont == "2"]
#artiodactyla and rodentia; hippo and Hoplyomys gymnurus

table(df$order[df$habitat.mode == "terr" & df$n.cont == "2"])
#carnivora and rodents, of course
table(df$log.size.bin[df$habitat.mode == "terr" & df$n.cont == "2"])
#most from 10^3 or 10^4 (similar to islands...?)

#are bats restricted by connectivity?
bat.cont <- df[df$order == "Chiroptera" & df$n.cont == 2,] %>%
    dplyr::select(continent.Africa, continent.Eurasia, 
                  continent.North.America, continent.South.America,
                  continent.Australia) %>%
    as.data.frame()
#want to know how many on Aus
nrow(bat.cont) #142
nrow(bat.cont[bat.cont$continent.Australia == TRUE,]) #4; not many
bat.cont[bat.cont$continent.Australia == TRUE,] #if on Australia, also one Eurasia
#how many on South America without being on North America?
nrow(bat.cont[bat.cont$continent.South.America == TRUE &
              bat.cont$continent.North.America == FALSE,]) #none
#how many on Africa without being on Eurasia?
nrow(bat.cont[bat.cont$continent.Africa == TRUE &
              bat.cont$continent.Eurasia == FALSE,]) #none

#### H2: FAMILY ORIGIN ----
#jumpers (no longer living where family originated) and spreaders (living where family originated and other continents too)

length(unique(df$order[df$family.origin != ""])) #28
length(df$family.origin[df$family.origin != ""]) #3571

#gather data

df %>% group_by(family.origin) %>% summarise(n.sp = n())
#family.origin    n.sp
# ""                814
# "Africa"          319
# "Australia"       205
# "Eurasia"        1754
# "North.America"   636
# "South.America"   658

## do we see more limited disperses from highly connected continents than not?
#1. looking at species level
df %>% 
  group_by(n.cont) %>%
  summarise(n.sp.af = sum(continent.Africa == TRUE),
            n.sp.na = sum(continent.North.America == TRUE),
            n.sp.aus = sum(continent.Australia == TRUE),
            n.sp.sa = sum(continent.South.America == TRUE),
            n.sp.ea = sum(continent.Eurasia == TRUE))
#n.cont     n.sp.af     n.sp.na     n.sp.aus    n.sp.sa     n.sp.ea
#1          1065        625         331         1037        1061
#2          79          177         4           162         98
#3+          6          5           1           1           6


#2. looking at family level
nrow(df[df$family.origin == "Eurasia",])
df %>% 
  group_by(n.cont) %>%
  summarise(n.sp.af = sum(family.origin == "Africa"), #316 have Africa as origin
            n.sp.na = sum(family.origin == "North.America"), #636 have as origin
            n.sp.aus = sum(family.origin == "Australia"), #205 have as origin
            n.sp.sa = sum(family.origin == "South.America"), #658 have as origin
            n.sp.ea = sum(family.origin == "Eurasia")) #1754
#n.cont     n.sp.af     n.sp.na     n.sp.aus    n.sp.sa     n.sp.ea
#1          299         587         205         575         1675
#2          20          46          0           83          75
#3+         0           3           0           0           3


unique(df$family[df$family.origin == ""]) #only 7 families; most small groups but really diverse
# Molossidae, Muridae, and Sciuridae are super diverse
length(df$binomial[df$family == "Muridae"]) #486
length(df$binomial[df$family == "Sciuridae"]) #227

nrow(df[df$n.cont == 2 & df$family == "Molossidae",]) #19
#3 Molossidae on Africa and Eurasia, the rest are on N & S america
nrow(df[df$n.cont == 2 & df$family == "Sciuridae",]) #2
nrow(df[df$n.cont == 2 & df$family == "Muridae",]) #14
#all Muridae on Africa and Eurasia

##### FAMILY ORIGIN BAR CHART -----
df$family.origin <- factor(df$family.origin,
                           levels = c("Australia",
                                      "Africa",
                                      "Eurasia",
                                      "North.America",
                                      "South.America",
                                      ""))

df.origin.counts <- df %>%
    group_by(n.cont, family.origin) %>%
    summarize(n = n()) %>%
    as.data.frame()
two.add <- c("2", "Australia", 0)
three.add.aus <- c("3+", "Australia", 0)
three.add.sa <- c("3+", "South.America", 0)
three.add.af <- c("3+", "Africa", 0)
df.origin.counts <- rbind(df.origin.counts, two.add, three.add.af,
                          three.add.aus, three.add.sa)

p.origin.2 <- ggplot() +  
    geom_col(aes(df.origin.counts$family.origin[df.origin.counts$family.origin != "" & df.origin.counts$n.cont == "2"],
                 as.numeric(df.origin.counts$n[df.origin.counts$family.origin != "" & df.origin.counts$n.cont == "2"])), #df$family.origin[df$n.cont == "2" & df$family.origin != ""]
             colour = "gray47", fill = "gray47") +
    plot_theme +
    theme(axis.text.x = element_text(hjust = 1, size = 26)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Continent of Family Origin",
                     labels = c("Australia" = "AU", "Africa" = "AF", 
                                "Eurasia" = "EA", "North.America" = "NA", 
                                "South.America" = "SA"))
#sample size
sum(as.numeric(df.origin.counts$n[df.origin.counts$family.origin != "" & df.origin.counts$n.cont == "2"])) 
#224

ggsave(p.origin.2, 
       file = "./Figures/continent.origin.two.png", 
       width = 20, height = 10, units = "cm")

df.vol <- df[df$habitat.mode == "volant",]
df.nonvol <- df[df$habitat.mode != "volant",]

df.origin.counts.nonvol <- df.nonvol %>%
    group_by(n.cont, family.origin) %>%
    dplyr::summarize(n = n()) %>%
    as.data.frame()
df.origin.counts.nonvol <- rbind(df.origin.counts.nonvol, two.add, three.add.af,
                                 three.add.aus, three.add.sa)

df.origin.counts.vol <- df.vol %>%
    dplyr::group_by(n.cont, family.origin) %>%
    dplyr::summarize(n = n()) %>%
    as.data.frame()
df.origin.counts.vol <- rbind(df.origin.counts.vol, two.add, three.add.af,
                              three.add.aus, three.add.sa)

p.origin.2.nonvol <- ggplot() +  
    geom_col(aes(df.origin.counts.nonvol$family.origin[df.origin.counts.nonvol$family.origin != "" & df.origin.counts.nonvol$n.cont == "2"],
                 as.numeric(df.origin.counts.nonvol$n[df.origin.counts.nonvol$family.origin != "" & df.origin.counts.nonvol$n.cont == "2"])), #df$family.origin[df$n.cont == "2" & df$family.origin != ""]
             colour = "gray47", fill = "gray47") +
    plot_theme +
    theme(axis.text.x = element_text(hjust = 1, size = 26)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Nonvolant Continent of Family Origin",
                     labels = c("Australia" = "AU", "Africa" = "AF", 
                                "Eurasia" = "EA", "North.America" = "NA", 
                                "South.America" = "SA"))
#sample size
sum(as.numeric(df.origin.counts.nonvol$n[df.origin.counts.nonvol$family.origin != "" & df.origin.counts.nonvol$n.cont == "2"])) 
#102

ggsave(p.origin.2.nonvol, 
       file = "./Figures/continent.origin.two.nonvol.png", 
       width = 20, height = 10, units = "cm")

p.origin.2.vol <- ggplot() +  
    geom_col(aes(df.origin.counts.vol$family.origin[df.origin.counts.vol$family.origin != "" & df.origin.counts.vol$n.cont == "2"],
                 as.numeric(df.origin.counts.vol$n[df.origin.counts.vol$family.origin != "" & df.origin.counts.vol$n.cont == "2"])), #df$family.origin[df$n.cont == "2" & df$family.origin != ""]
             colour = "gray47", fill = "gray47") +
    plot_theme +
    theme(axis.text.x = element_text(hjust = 1, size = 26)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Volant Continent of Family Origin",
                     labels = c("Australia" = "AU", "Africa" = "AF", 
                                "Eurasia" = "EA", "North.America" = "NA", 
                                "South.America" = "SA"))
#sample size
sum(as.numeric(df.origin.counts.vol$n[df.origin.counts.vol$family.origin != "" & df.origin.counts.vol$n.cont == "2"])) 
#122

ggsave(p.origin.2.vol, 
       file = "./Figures/continent.origin.two.vol.png", 
       width = 20, height = 10, units = "cm")

p.origin.1 <- ggplot() +  
    geom_col(aes(df.origin.counts$family.origin[df.origin.counts$family.origin != "" & df.origin.counts$n.cont == "1"],
                 as.numeric(df.origin.counts$n[df.origin.counts$family.origin != "" & df.origin.counts$n.cont == "1"])), #df$family.origin[df$n.cont == "2" & df$family.origin != ""]
             colour = "black", fill = "black") +
    plot_theme +
    theme(axis.text.x = element_text(hjust = 1, size = 26)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Continent of Family Origin",
                     labels = c("Australia" = "AU", "Africa" = "AF", 
                                "Eurasia" = "EA", "North.America" = "NA", 
                                "South.America" = "SA"))
#sample size
sum(as.numeric(df.origin.counts$n[df.origin.counts$family.origin != "" & df.origin.counts$n.cont == "1"])) 
#3342

ggsave(p.origin.1, 
       file = "./Figures/continent.origin.one.png", 
       width = 20, height = 10, units = "cm")

p.origin.3 <- ggplot() +  
    geom_col(aes(df.origin.counts$family.origin[df.origin.counts$family.origin != "" & df.origin.counts$n.cont == "3+"],
                 as.numeric(df.origin.counts$n[df.origin.counts$family.origin != "" & df.origin.counts$n.cont == "3+"])), #df$family.origin[df$n.cont == "2" & df$family.origin != ""]
             colour = "red", fill = "red") +
    plot_theme +
    theme(axis.text.x = element_text(hjust = 1, size = 26)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Continent of Family Origin",
                     labels = c("Australia" = "AU", "Africa" = "AF", 
                                "Eurasia" = "EA", "North.America" = "NA", 
                                "South.America" = "SA"))
#sample size
sum(as.numeric(df.origin.counts$n[df.origin.counts$family.origin != "" & df.origin.counts$n.cont == "3+"])) 
#6

ggsave(p.origin.3, 
       file = "./Figures/continent.origin.three.png", 
       width = 20, height = 10, units = "cm")

##### HOMIES (ON1) ORIGIN -----
homies <- df[df$n.cont == 1,]
homies.origin <- homies %>%
  group_by(family.origin) %>%
  dplyr::summarise(N = n(),
                   N.Africa = length(continent.Africa[continent.Africa == TRUE]),
                   N.Australia = length(continent.Australia[continent.Australia == TRUE]),
                   N.South.America = length(continent.South.America[continent.South.America == TRUE]),
                   N.North.America = length(continent.North.America[continent.North.America == TRUE]),
                   N.Eurasia = length(continent.Eurasia[continent.Eurasia == TRUE])) %>%
  as.data.frame() 
homies.origin <- homies.origin[homies.origin$family.origin != "",]

#get proportions
homies.origin$N.jump <- ""
homies.origin$prop.origin <- ""
homies.origin$prop.jump <- ""

homies.origin$N.jump[homies.origin$family.origin == "Africa"] <- as.numeric(homies.origin$N[homies.origin$family.origin == "Africa"] - homies.origin$N.Africa[homies.origin$family.origin == "Africa"])
homies.origin$prop.origin[homies.origin$family.origin == "Africa"] <- as.numeric(homies.origin$N.Africa[homies.origin$family.origin == "Africa"]/homies.origin$N[homies.origin$family.origin == "Africa"])
homies.origin$prop.jump[homies.origin$family.origin == "Africa"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "Africa"])/as.numeric(homies.origin$N[homies.origin$family.origin == "Africa"])
homies.origin$prop.stay[homies.origin$family.origin == "Africa"] <- as.numeric(homies.origin$N.Africa[homies.origin$family.origin == "Africa"])/as.numeric(sum(homies.origin$N))
homies.origin$prop.leave[homies.origin$family.origin == "Africa"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "Africa"])/as.numeric(sum(homies.origin$N))

homies.origin$N.jump[homies.origin$family.origin == "Australia"] <- as.numeric(homies.origin$N[homies.origin$family.origin == "Australia"] - homies.origin$N.Australia[homies.origin$family.origin == "Australia"])
homies.origin$prop.origin[homies.origin$family.origin == "Australia"] <- as.numeric(homies.origin$N.Australia[homies.origin$family.origin == "Australia"]/homies.origin$N[homies.origin$family.origin == "Australia"])
homies.origin$prop.jump[homies.origin$family.origin == "Australia"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "Australia"])/as.numeric(homies.origin$N[homies.origin$family.origin == "Australia"])
homies.origin$prop.stay[homies.origin$family.origin == "Australia"] <- as.numeric(homies.origin$N.Australia[homies.origin$family.origin == "Australia"])/as.numeric(sum(homies.origin$N))
homies.origin$prop.leave[homies.origin$family.origin == "Australia"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "Australia"])/as.numeric(sum(homies.origin$N))

homies.origin$N.jump[homies.origin$family.origin == "North.America"] <- as.numeric(homies.origin$N[homies.origin$family.origin == "North.America"] - homies.origin$N.North.America[homies.origin$family.origin == "North.America"])
homies.origin$prop.origin[homies.origin$family.origin == "North.America"] <- as.numeric(homies.origin$N.North.America[homies.origin$family.origin == "North.America"]/homies.origin$N[homies.origin$family.origin == "North.America"])
homies.origin$prop.jump[homies.origin$family.origin == "North.America"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "North.America"])/as.numeric(homies.origin$N[homies.origin$family.origin == "North.America"])
homies.origin$prop.stay[homies.origin$family.origin == "North.America"] <- as.numeric(homies.origin$N.North.America[homies.origin$family.origin == "North.America"])/as.numeric(sum(homies.origin$N))
homies.origin$prop.leave[homies.origin$family.origin == "North.America"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "North.America"])/as.numeric(sum(homies.origin$N))

homies.origin$N.jump[homies.origin$family.origin == "South.America"] <- as.numeric(homies.origin$N[homies.origin$family.origin == "South.America"] - homies.origin$N.South.America[homies.origin$family.origin == "South.America"])
homies.origin$prop.origin[homies.origin$family.origin == "South.America"] <- as.numeric(homies.origin$N.South.America[homies.origin$family.origin == "South.America"]/homies.origin$N[homies.origin$family.origin == "South.America"])
homies.origin$prop.jump[homies.origin$family.origin == "South.America"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "South.America"])/as.numeric(homies.origin$N[homies.origin$family.origin == "South.America"])
homies.origin$prop.stay[homies.origin$family.origin == "South.America"] <- as.numeric(homies.origin$N.South.America[homies.origin$family.origin == "South.America"])/as.numeric(sum(homies.origin$N))
homies.origin$prop.leave[homies.origin$family.origin == "South.America"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "South.America"])/as.numeric(sum(homies.origin$N))

homies.origin$N.jump[homies.origin$family.origin == "Eurasia"] <- as.numeric(homies.origin$N[homies.origin$family.origin == "Eurasia"] - homies.origin$N.Eurasia[homies.origin$family.origin == "Eurasia"])
homies.origin$prop.origin[homies.origin$family.origin == "Eurasia"] <- as.numeric(homies.origin$N.Eurasia[homies.origin$family.origin == "Eurasia"]/homies.origin$N[homies.origin$family.origin == "Eurasia"])
homies.origin$prop.jump[homies.origin$family.origin == "Eurasia"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "Eurasia"])/as.numeric(homies.origin$N[homies.origin$family.origin == "Eurasia"])
homies.origin$prop.stay[homies.origin$family.origin == "Eurasia"] <- as.numeric(homies.origin$N.Eurasia[homies.origin$family.origin == "Eurasia"])/as.numeric(sum(homies.origin$N))
homies.origin$prop.leave[homies.origin$family.origin == "Eurasia"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "Eurasia"])/as.numeric(sum(homies.origin$N))

write.csv(homies.origin, 
          "./Results/homies.family.origin.csv",
          row.names = FALSE)

unique(homies$order[homies$family.origin == "Africa"])

homies.origin$per.stay <- as.numeric(homies.origin$prop.stay)*100
homies.origin$per.leave <- as.numeric(homies.origin$prop.leave)*100

homies.origin.melt <- melt(homies.origin, 
                           id.vars = "family.origin",
                           measure.vars = c("per.stay",
                                            "per.leave"),
                           variable.name = "per")
homies.origin.melt$family.origin.per <- paste(homies.origin.melt$family.origin, 
                                              homies.origin.melt$per,
                                              sep = ".")

write.csv(homies.origin.melt,
          "./Results/homebodies.origin.results.csv",
          row.names = FALSE)

###### HOMIES (ON1) PIE CHART ------

##pie chart
## COLOR SCHEME
#South America = #E2C9F2; dark #9A8AA6
#North America = #B4D9C8; dark #748C81
#Africa = #C2D991; dark #7E8C5E
#Eurasia = #F2CDA0; dark #A68C6D
#Australia = #D9967E; dark #8C6151

p.homies.pie <- ggplot(homies.origin.melt, aes(x = "", y = value, fill = family.origin.per)) +
  geom_col(color = 'black',
           position = position_stack(reverse = TRUE),
           show.legend = TRUE) +
  #geom_bar(stat="identity", width=1) +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_manual(values = c("Africa.per.stay" = "#C2D991",
                               "Australia.per.stay" = "#D9967E",
                               "Eurasia.per.stay" = "#F2CDA0",
                               "North.America.per.stay" = "#B4D9C8",
                               "South.America.per.stay" = "#E2C9F2",
                               "Africa.per.leave" = "#7E8C5E",
                               "Australia.per.leave" = "#8C6151",
                               "Eurasia.per.leave" = "#A68C6D",
                               "North.America.per.leave" = "#748C81",
                               "South.America.per.leave" = "#9A8AA6"),
                    name = "Continent of Family Origin",
                    labels = c("Africa - jump",
                               "Africa - stay",
                               "Australia - jump",
                               "Australia - stay",
                               "Eurasia - jump",
                               "Eurasia - stay",
                               "North America - jump",
                               "North America - stay",
                               "South America - jump",
                               "South America - stay")) +
  coord_polar("y", start = 0) +
  theme_void()

#use light ones for homebodies and dark for wanderers?

##North America
homies.origin.na.melt <- melt(homies.origin[homies.origin$family.origin == "North.America",],
                              id.vars = "family.origin",
                              measure.vars = c("N.Africa", "N.Australia", "N.South.America",
                                               "N.North.America", "N.Eurasia"),
                              variable.name = "continent",
                              value.name = "N")
homies.origin.na.melt$per <- homies.origin.na.melt$N/homies.origin$N[homies.origin$family.origin == "North.America"]
homies.origin.na.melt <- as.data.frame(homies.origin.na.melt)

p.homies.pie.NA <- ggplot(homies.origin.na.melt, aes(x = "", y = per, fill = continent)) +
  #geom_col(color = 'black',
  #         position = position_stack(reverse = TRUE)) + #show.legend = TRUE
  #geom_bar(stat="identity", width=1) +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_manual(values = c("N.Africa" = "#c5dc93",
                               "N.Australia" = "#e4a182",
                               "N.Eurasia" = "#fee0a6",
                               "N.North.America" = "#bbe3d4",
                               "N.South.America" = "#f0d2ff")) +
  coord_polar("y", start = 0) +
  theme_void() 

ggsave(p.homies.pie.NA, 
       file = "./Figures/homies.pie.NA.png", 
       width = 20, height = 10, units = "cm")

## South America
homies.origin.sa.melt <- melt(homies.origin[homies.origin$family.origin == "South.America",],
                              id.vars = "family.origin",
                              measure.vars = c("N.Africa", "N.Australia", "N.South.America",
                                               "N.North.America", "N.Eurasia"),
                              variable.name = "continent",
                              value.name = "N")
homies.origin.sa.melt$per <- homies.origin.sa.melt$N/homies.origin$N[homies.origin$family.origin == "South.America"]
homies.origin.sa.melt <- as.data.frame(homies.origin.sa.melt)

p.homies.pie.SA <- ggplot(homies.origin.sa.melt, aes(x = "", y = per, fill = continent)) +
    #geom_col(color = 'black',
    #         position = position_stack(reverse = TRUE)) + #show.legend = TRUE
    #geom_bar(stat="identity", width=1) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(values = c("N.Africa" = "#c5dc93",
                                 "N.Australia" = "#e4a182",
                                 "N.Eurasia" = "#fee0a6",
                                 "N.North.America" = "#bbe3d4",
                                 "N.South.America" = "#f0d2ff")) +
    coord_polar("y", start = 0) +
    theme_void()

ggsave(p.homies.pie.SA, 
       file = "./Figures/homies.pie.SA.png", 
       width = 20, height = 10, units = "cm")

## Eurasia
homies.origin.ea.melt <- melt(homies.origin[homies.origin$family.origin == "Eurasia",],
                              id.vars = "family.origin",
                              measure.vars = c("N.Africa", "N.Australia", "N.South.America",
                                               "N.North.America", "N.Eurasia"),
                              variable.name = "continent",
                              value.name = "N")
homies.origin.ea.melt$per <- homies.origin.ea.melt$N/homies.origin$N[homies.origin$family.origin == "Eurasia"]
homies.origin.ea.melt <- as.data.frame(homies.origin.ea.melt)

p.homies.pie.EA <- ggplot(homies.origin.ea.melt, aes(x = "", y = per, fill = continent)) +
    #geom_col(color = 'black',
    #         position = position_stack(reverse = TRUE)) + #show.legend = TRUE
    #geom_bar(stat="identity", width=1) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(values = c("N.Africa" = "#c5dc93",
                                 "N.Australia" = "#e4a182",
                                 "N.Eurasia" = "#fee0a6",
                                 "N.North.America" = "#bbe3d4",
                                 "N.South.America" = "#f0d2ff")) +
    coord_polar("y", start = 0) +
    theme_void()

ggsave(p.homies.pie.EA, 
       file = "./Figures/homies.pie.EA.png", 
       width = 20, height = 10, units = "cm")

## Africa
homies.origin.af.melt <- melt(homies.origin[homies.origin$family.origin == "Africa",],
                              id.vars = "family.origin",
                              measure.vars = c("N.Africa", "N.Australia", "N.South.America",
                                               "N.North.America", "N.Eurasia"),
                              variable.name = "continent",
                              value.name = "N")
homies.origin.af.melt$per <- homies.origin.af.melt$N/homies.origin$N[homies.origin$family.origin == "Africa"]
homies.origin.af.melt <- as.data.frame(homies.origin.af.melt)

p.homies.pie.AU <- ggplot(homies.origin.af.melt, aes(x = "", y = per, fill = continent)) +
    #geom_col(color = 'black',
    #         position = position_stack(reverse = TRUE)) + #show.legend = TRUE
    #geom_bar(stat="identity", width=1) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(values = c("N.Africa" = "#c5dc93",
                                 "N.Australia" = "#e4a182",
                                 "N.Eurasia" = "#fee0a6",
                                 "N.North.America" = "#bbe3d4",
                                 "N.South.America" = "#f0d2ff")) +
    coord_polar("y", start = 0) +
    theme_void()

ggsave(p.homies.pie.AU, 
       file = "./Figures/homies.pie.AU.png", 
       width = 20, height = 10, units = "cm")

##### WIDE-RANGING (ON2) ORIGIN ----
rangers.origin <- on2 %>%
  group_by(family.origin) %>%
  dplyr::summarise(N = n(),
                   N.Africa = length(continent.Africa[continent.Africa == TRUE]),
                   N.Australia = length(continent.Australia[continent.Australia == TRUE]),
                   N.South.America = length(continent.South.America[continent.South.America == TRUE]),
                   N.North.America = length(continent.North.America[continent.North.America == TRUE]),
                   N.Eurasia = length(continent.Eurasia[continent.Eurasia == TRUE])) %>%
  as.data.frame() 
rangers.origin <- rangers.origin[rangers.origin$family.origin != "",]

#get proportions
rangers.origin$prop.spread <- ""
rangers.origin$prop.spread[rangers.origin$family.origin == "Africa"] <- rangers.origin$N.Africa[rangers.origin$family.origin == "Africa"]/rangers.origin$N[rangers.origin$family.origin == "Africa"]
rangers.origin$prop.spread[rangers.origin$family.origin == "Eurasia"] <- rangers.origin$N.Eurasia[rangers.origin$family.origin == "Eurasia"]/rangers.origin$N[rangers.origin$family.origin == "Eurasia"]
rangers.origin$prop.spread[rangers.origin$family.origin == "Australia"] <- rangers.origin$N.Australia[rangers.origin$family.origin == "Australia"]/rangers.origin$N[rangers.origin$family.origin == "Australia"]
rangers.origin$prop.spread[rangers.origin$family.origin == "South.America"] <- rangers.origin$N.South.America[rangers.origin$family.origin == "South.America"]/rangers.origin$N[rangers.origin$family.origin == "South.America"]
rangers.origin$prop.spread[rangers.origin$family.origin == "North.America"] <- rangers.origin$N.North.America[rangers.origin$family.origin == "North.America"]/rangers.origin$N[rangers.origin$family.origin == "North.America"]
#no jumpers in North.America
#no spreaders from Australia

rangers.origin$prop.jump <- 1- as.numeric(rangers.origin$prop.spread)

#want to know where the jumpers and spreaders went to
rangers.origin.shared <- on2 %>%
  group_by(family.origin) %>%
  dplyr::summarise(N = n(),
                   N.Africa.Eurasia = length(continent.Africa[continent.Africa == TRUE & continent.Eurasia == TRUE]),
                   N.Africa.Australia = length(continent.Africa[continent.Africa == TRUE & continent.Australia == TRUE]),
                   N.Africa.North.America = length(continent.Africa[continent.Africa == TRUE & continent.North.America == TRUE]),
                   N.Africa.South.America = length(continent.Africa[continent.Africa == TRUE & continent.South.America == TRUE]),
                   N.Australia.Eurasia = length(continent.Australia[continent.Australia == TRUE & continent.Eurasia == TRUE]),
                   N.Australia.South.America = length(continent.Australia[continent.Australia == TRUE & continent.South.America == TRUE]),
                   N.Australia.North.America = length(continent.Australia[continent.Australia == TRUE & continent.North.America == TRUE]),
                   N.Eurasia.North.America = length(continent.Eurasia[continent.Eurasia == TRUE & continent.North.America == TRUE]),
                   N.Eurasia.South.America = length(continent.Eurasia[continent.Eurasia == TRUE & continent.South.America == TRUE]),
                   N.South.America.North.America = length(continent.South.America[continent.South.America == TRUE & continent.North.America == TRUE])) %>%
  as.data.frame() 
rangers.origin.shared <- rangers.origin.shared[rangers.origin.shared$family.origin != "",]
#Australia not here...? No family now on two continents originated in Australia

rangers.origin.shared$prop.spread <- ""
rangers.origin.shared$prop.jump <- ""

rangers.origin.shared$prop.spread[rangers.origin.shared$family.origin == "Africa"] <- sum(rangers.origin.shared$N.Africa.Eurasia[rangers.origin.shared$family.origin == "Africa"] 
                                                                                          + rangers.origin.shared$N.Africa.Australia[rangers.origin.shared$family.origin == "Africa"] 
                                                                                          + rangers.origin.shared$N.Africa.North.America[rangers.origin.shared$family.origin == "Africa"] 
                                                                                          + rangers.origin.shared$N.Africa.South.America[rangers.origin.shared$family.origin == "Africa"])/rangers.origin.shared$N[rangers.origin.shared$family.origin == "Africa"]
rangers.origin.shared$prop.jump[rangers.origin.shared$family.origin == "Africa"] <- 1 - as.numeric(rangers.origin.shared$prop.spread[rangers.origin.shared$family.origin == "Africa"])

rangers.origin.shared$prop.stay[rangers.origin.shared$family.origin == "Africa"] <- sum(rangers.origin.shared$N.Africa.Eurasia[rangers.origin.shared$family.origin == "Africa"] 
                                                                                        + rangers.origin.shared$N.Africa.Australia[rangers.origin.shared$family.origin == "Africa"] 
                                                                                        + rangers.origin.shared$N.Africa.North.America[rangers.origin.shared$family.origin == "Africa"] 
                                                                                        + rangers.origin.shared$N.Africa.South.America[rangers.origin.shared$family.origin == "Africa"])/sum(rangers.origin.shared$N)
rangers.origin.shared$prop.leave[rangers.origin.shared$family.origin == "Africa"] <- (rangers.origin.shared$N[rangers.origin.shared$family.origin == "Africa"] - sum(rangers.origin.shared$N.Africa.Eurasia[rangers.origin.shared$family.origin == "Africa"] 
                                                                                                                                                                     + rangers.origin.shared$N.Africa.Australia[rangers.origin.shared$family.origin == "Africa"] 
                                                                                                                                                                     + rangers.origin.shared$N.Africa.North.America[rangers.origin.shared$family.origin == "Africa"] 
                                                                                                                                                                     + rangers.origin.shared$N.Africa.South.America[rangers.origin.shared$family.origin == "Africa"]))/as.numeric(sum(rangers.origin.shared$N))


rangers.origin.shared$prop.spread[rangers.origin.shared$family.origin == "Eurasia"] <- sum(rangers.origin.shared$N.Africa.Eurasia[rangers.origin.shared$family.origin == "Eurasia"] 
                                                                                           + rangers.origin.shared$N.Australia.Eurasia[rangers.origin.shared$family.origin == "Eurasia"] 
                                                                                           + rangers.origin.shared$N.Eurasia.North.America[rangers.origin.shared$family.origin == "Eurasia"] 
                                                                                           + rangers.origin.shared$N.Eurasia.South.America[rangers.origin.shared$family.origin == "Eurasia"])/rangers.origin.shared$N[rangers.origin.shared$family.origin == "Eurasia"]
rangers.origin.shared$prop.jump[rangers.origin.shared$family.origin == "Eurasia"] <- 1 - as.numeric(rangers.origin.shared$prop.spread[rangers.origin.shared$family.origin == "Eurasia"])


rangers.origin.shared$prop.stay[rangers.origin.shared$family.origin == "Eurasia"] <- sum(rangers.origin.shared$N.Africa.Eurasia[rangers.origin.shared$family.origin == "Eurasia"] 
                                                                                         + rangers.origin.shared$N.Australia.Eurasia[rangers.origin.shared$family.origin == "Eurasia"] 
                                                                                         + rangers.origin.shared$N.Eurasia.North.America[rangers.origin.shared$family.origin == "Eurasia"] 
                                                                                         + rangers.origin.shared$N.Eurasia.South.America[rangers.origin.shared$family.origin == "Eurasia"])/sum(rangers.origin.shared$N)
rangers.origin.shared$prop.leave[rangers.origin.shared$family.origin == "Eurasia"] <- (rangers.origin.shared$N[rangers.origin.shared$family.origin == "Eurasia"] - sum(rangers.origin.shared$N.Africa.Eurasia[rangers.origin.shared$family.origin == "Eurasia"] 
                                                                                                                                                                       + rangers.origin.shared$N.Australia.Eurasia[rangers.origin.shared$family.origin == "Eurasia"] 
                                                                                                                                                                       + rangers.origin.shared$N.Eurasia.North.America[rangers.origin.shared$family.origin == "Eurasia"] 
                                                                                                                                                                       + rangers.origin.shared$N.Eurasia.South.America[rangers.origin.shared$family.origin == "Eurasia"]))/as.numeric(sum(rangers.origin.shared$N))



rangers.origin.shared$prop.spread[rangers.origin.shared$family.origin == "North.America"] <- sum(rangers.origin.shared$N.Africa.North.America[rangers.origin.shared$family.origin == "North.America"] 
                                                                                                 + rangers.origin.shared$N.Australia.North.America[rangers.origin.shared$family.origin == "North.America"] 
                                                                                                 + rangers.origin.shared$N.Eurasia.North.America[rangers.origin.shared$family.origin == "North.America"] 
                                                                                                 + rangers.origin.shared$N.South.America.North.America[rangers.origin.shared$family.origin == "North.America"])/rangers.origin.shared$N[rangers.origin.shared$family.origin == "North.America"]
rangers.origin.shared$prop.jump[rangers.origin.shared$family.origin == "North.America"] <- 1 - as.numeric(rangers.origin.shared$prop.spread[rangers.origin.shared$family.origin == "North.America"])


rangers.origin.shared$prop.stay[rangers.origin.shared$family.origin == "North.America"] <- sum(rangers.origin.shared$N.Africa.North.America[rangers.origin.shared$family.origin == "North.America"] 
                                                                                               + rangers.origin.shared$N.Australia.North.America[rangers.origin.shared$family.origin == "North.America"] 
                                                                                               + rangers.origin.shared$N.Eurasia.North.America[rangers.origin.shared$family.origin == "North.America"] 
                                                                                               + rangers.origin.shared$N.South.America.North.America[rangers.origin.shared$family.origin == "North.America"])/sum(rangers.origin.shared$N)
rangers.origin.shared$prop.leave[rangers.origin.shared$family.origin == "North.America"] <- (rangers.origin.shared$N[rangers.origin.shared$family.origin == "North.America"] - sum(rangers.origin.shared$N.Africa.North.America[rangers.origin.shared$family.origin == "North.America"] 
                                                                                                                                                                                   + rangers.origin.shared$N.Australia.North.America[rangers.origin.shared$family.origin == "North.America"] 
                                                                                                                                                                                   + rangers.origin.shared$N.Eurasia.North.America[rangers.origin.shared$family.origin == "North.America"] 
                                                                                                                                                                                   + rangers.origin.shared$N.South.America.North.America[rangers.origin.shared$family.origin == "North.America"]))/as.numeric(sum(rangers.origin.shared$N))

rangers.origin.shared$prop.spread[rangers.origin.shared$family.origin == "South.America"] <- sum(rangers.origin.shared$N.Africa.South.America[rangers.origin.shared$family.origin == "South.America"] 
                                                                                                 + rangers.origin.shared$N.Australia.South.America[rangers.origin.shared$family.origin == "South.America"] 
                                                                                                 + rangers.origin.shared$N.Eurasia.South.America[rangers.origin.shared$family.origin == "South.America"] 
                                                                                                 + rangers.origin.shared$N.South.America.North.America[rangers.origin.shared$family.origin == "South.America"])/rangers.origin.shared$N[rangers.origin.shared$family.origin == "South.America"]
rangers.origin.shared$prop.jump[rangers.origin.shared$family.origin == "South.America"] <- 1 - as.numeric(rangers.origin.shared$prop.spread[rangers.origin.shared$family.origin == "South.America"])
#100% spread

rangers.origin.shared$prop.stay[rangers.origin.shared$family.origin == "South.America"] <- sum(rangers.origin.shared$N.Africa.South.America[rangers.origin.shared$family.origin == "South.America"] 
                                                                                               + rangers.origin.shared$N.Australia.South.America[rangers.origin.shared$family.origin == "South.America"] 
                                                                                               + rangers.origin.shared$N.Eurasia.South.America[rangers.origin.shared$family.origin == "South.America"] 
                                                                                               + rangers.origin.shared$N.South.America.North.America[rangers.origin.shared$family.origin == "South.America"])/sum(rangers.origin.shared$N)
rangers.origin.shared$prop.leave[rangers.origin.shared$family.origin == "South.America"] <- (rangers.origin.shared$N[rangers.origin.shared$family.origin == "South.America"] - sum(rangers.origin.shared$N.Africa.South.America[rangers.origin.shared$family.origin == "South.America"] 
                                                                                                                                                                                   + rangers.origin.shared$N.Australia.South.America[rangers.origin.shared$family.origin == "South.America"] 
                                                                                                                                                                                   + rangers.origin.shared$N.Eurasia.South.America[rangers.origin.shared$family.origin == "South.America"] 
                                                                                                                                                                                   + rangers.origin.shared$N.South.America.North.America[rangers.origin.shared$family.origin == "South.America"]))/as.numeric(sum(rangers.origin.shared$N))


sum(rangers.origin.shared$prop.leave)+sum(rangers.origin.shared$prop.stay)
write.csv(rangers.origin.shared, 
          "./Results/rangers.family.origin.csv",
          row.names = FALSE)

##FIGURE
rangers.origin.shared$per.stay <- as.numeric(rangers.origin.shared$prop.stay)*100
rangers.origin.shared$per.leave <- as.numeric(rangers.origin.shared$prop.leave)*100

rangers.origin.shared.melt <- melt(rangers.origin.shared, 
                          id.vars = "family.origin",
                          measure.vars = c("per.stay",
                                           "per.leave"),
                          variable.name = "per")
rangers.origin.shared.melt$family.origin.per <- paste(rangers.origin.shared.melt$family.origin, 
                                                     rangers.origin.shared.melt$per,
                                             sep = ".")
###### WIDE-RANGING (ON2) PIE CHART -----
##pie chart
## COLOR SCHEME
#South America = #E2C9F2; dark #9A8AA6
#North America = #B4D9C8; dark #748C81
#Africa = #C2D991; dark #7E8C5E
#Eurasia = #F2CDA0; dark #A68C6D
#Australia = #D9967E; dark #8C6151

p.ranger.pie <- ggplot(rangers.origin.shared.melt, aes(x = "", y = value, fill = family.origin.per)) +
  geom_col(color = 'black', 
           position = position_stack(reverse = TRUE), 
           show.legend = TRUE) +
  #geom_bar(stat="identity", width=1) +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_manual(values = c("Africa.per.stay" = "#C2D991",
                               "Australia.per.stay" = "#D9967E",
                               "Eurasia.per.stay" = "#F2CDA0",
                               "North.America.per.stay" = "#B4D9C8",
                               "South.America.per.stay" = "#E2C9F2",
                               "Africa.per.leave" = "#7E8C5E",
                               "Australia.per.leave" = "#8C6151",
                               "Eurasia.per.leave" = "#A68C6D",
                               "North.America.per.leave" = "#748C81",
                               "South.America.per.leave" = "#9A8AA6"),
                    name = "Continent of Family Origin",
                    labels = c("Africa - jump",
                               "Africa - stay",
                               "Australia - jump",
                               "Australia - stay",
                               "Eurasia - jump",
                               "Eurasia - stay",
                               "North America - jump",
                               "North America - stay",
                               "South America - jump",
                               "South America - stay")) +
  coord_polar("y", start = 0) +
  theme_void()

##by shared continents
rangers.shared <- on2[on2$family.origin != "",]

rangers.shared$shared <- ""
rangers.shared$shared[rangers.shared$continent.Africa == TRUE & rangers.shared$continent.Eurasia == TRUE] <- "EA.AF"
rangers.shared$shared[rangers.shared$continent.Eurasia == TRUE & rangers.shared$continent.North.America == TRUE] <- "EA.NA"
rangers.shared$shared[rangers.shared$continent.Eurasia == TRUE & rangers.shared$continent.Australia == TRUE] <- "EA.AU"
rangers.shared$shared[rangers.shared$continent.South.America == TRUE & rangers.shared$continent.North.America == TRUE] <- "NA.SA"

rangers.shared.stats <- rangers.shared %>%
    group_by(shared) %>%
    dplyr::summarise(N = n(),
                     N.Africa = length(family.origin[family.origin == "Africa"]),
                     N.Australia = length(family.origin[family.origin == "Australia"]),
                     N.South.America = length(family.origin[family.origin == "South.America"]),
                     N.North.America = length(family.origin[family.origin == "North.America"]),
                     N.Eurasia = length(family.origin[family.origin == "Eurasia"])) %>%
    as.data.frame() 

rangers.shared.stats$per.AF <- (rangers.shared.stats$N.Africa/rangers.shared.stats$N)*100
rangers.shared.stats$per.AU <- (rangers.shared.stats$N.Australia/rangers.shared.stats$N)*100
rangers.shared.stats$per.EA <- (rangers.shared.stats$N.Eurasia/rangers.shared.stats$N)*100
rangers.shared.stats$per.NA <- (rangers.shared.stats$N.North.America/rangers.shared.stats$N)*100
rangers.shared.stats$per.SA <- (rangers.shared.stats$N.South.America/rangers.shared.stats$N)*100

rangers.shared.melt <- melt(rangers.shared.stats, 
                            id.vars = "shared",
                            measure.vars = c("per.AF", "per.AU",
                                             "per.EA", "per.NA",
                                             "per.SA"),
                            variable.name = "per")
rangers.shared.melt$group <- paste(rangers.shared.melt$shared, 
                                   rangers.shared.melt$per,
                                   sep = ".")
rangers.shared.EA.AF <- rangers.shared.melt[rangers.shared.melt$shared == "EA.AF",]
rangers.shared.EA.AU <- rangers.shared.melt[rangers.shared.melt$shared == "EA.AU",]
rangers.shared.EA.NA <- rangers.shared.melt[rangers.shared.melt$shared == "EA.NA",]
rangers.shared.NA.SA <- rangers.shared.melt[rangers.shared.melt$shared == "NA.SA",]

## EA - AF
p.rangers.pie.EA.AF <- ggplot(rangers.shared.EA.AF, aes(x = "", y = value, fill = group)) +
    #geom_col(color = 'black',
    #         position = position_stack(reverse = TRUE)) + #show.legend = TRUE
    #geom_bar(stat="identity", width=1) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(values = c("EA.AF.per.AF" = "#c5dc93",
                                 "EA.AF.per.AU" = "#e4a182",
                                 "EA.AF.per.EA" = "#fee0a6",
                                 "EA.AF.per.NA" = "#bbe3d4",
                                 "EA.AF.per.SA" = "#f0d2ff"),
                      name = "Family Origin",
                      labels = c("EA.AF.per.AF" = paste0("Africa (", rangers.shared.EA.AF$value[rangers.shared.EA.AF$per == "per.AF"], ")"),
                                 "EA.AF.per.AU" = paste0("Australia (", rangers.shared.EA.AF$value[rangers.shared.EA.AF$per == "per.AU"], ")"),
                                 "EA.AF.per.EA" = paste0("Eurasia (", rangers.shared.EA.AF$value[rangers.shared.EA.AF$per == "per.EA"], ")"),
                                 "EA.AF.per.NA" = paste0("North America (", rangers.shared.EA.AF$value[rangers.shared.EA.AF$per == "per.NA"], ")"),
                                 "EA.AF.per.SA" = paste0("South America (", rangers.shared.EA.AF$value[rangers.shared.EA.AF$per == "per.SA"], ")"))) +
    coord_polar("y", start = 0) +
    theme_void()

ggsave(p.rangers.pie.EA.AF, 
       file = "./Figures/rangers.pie.EA.AF.png", 
       width = 20, height = 10, units = "cm")

p.rangers.pie.EA.AU <- ggplot(rangers.shared.EA.AU, aes(x = "", y = value, fill = group)) +
    #geom_col(color = 'black',
    #         position = position_stack(reverse = TRUE)) + #show.legend = TRUE
    #geom_bar(stat="identity", width=1) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(values = c("EA.AU.per.AF" = "#c5dc93",
                                 "EA.AU.per.AU" = "#e4a182",
                                 "EA.AU.per.EA" = "#fee0a6",
                                 "EA.AU.per.NA" = "#bbe3d4",
                                 "EA.AU.per.SA" = "#f0d2ff"),
                      name = "Family Origin",
                      labels = c("EA.AU.per.AF" = paste0("Africa (", rangers.shared.EA.AU$value[rangers.shared.EA.AU$per == "per.AF"], ")"),
                                 "EA.AU.per.AU" = paste0("Australia (", rangers.shared.EA.AU$value[rangers.shared.EA.AU$per == "per.AU"], ")"),
                                 "EA.AU.per.EA" = paste0("Eurasia (", rangers.shared.EA.AU$value[rangers.shared.EA.AU$per == "per.EA"], ")"),
                                 "EA.AU.per.NA" = paste0("North America (", rangers.shared.EA.AU$value[rangers.shared.EA.AU$per == "per.NA"], ")"),
                                 "EA.AU.per.SA" = paste0("South America (", rangers.shared.EA.AU$value[rangers.shared.EA.AU$per == "per.SA"], ")"))) +
    coord_polar("y", start = 0) +
    theme_void()

ggsave(p.rangers.pie.EA.AU, 
       file = "./Figures/rangers.pie.EA.AU.png", 
       width = 20, height = 10, units = "cm")

p.rangers.pie.EA.NA <- ggplot(rangers.shared.EA.NA, aes(x = "", y = value, fill = group)) +
    #geom_col(color = 'black',
    #         position = position_stack(reverse = TRUE)) + #show.legend = TRUE
    #geom_bar(stat="identity", width=1) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(values = c("EA.NA.per.AF" = "#c5dc93",
                                 "EA.NA.per.AU" = "#e4a182",
                                 "EA.NA.per.EA" = "#fee0a6",
                                 "EA.NA.per.NA" = "#bbe3d4",
                                 "EA.NA.per.SA" = "#f0d2ff"),
                      name = "Family Origin",
                      labels = c("EA.NA.per.AF" = paste0("Africa (", rangers.shared.EA.NA$value[rangers.shared.EA.NA$per == "per.AF"], ")"),
                                 "EA.NA.per.AU" = paste0("Australia (", rangers.shared.EA.NA$value[rangers.shared.EA.NA$per == "per.AU"], ")"),
                                 "EA.NA.per.EA" = paste0("Eurasia (", rangers.shared.EA.NA$value[rangers.shared.EA.NA$per == "per.EA"], ")"),
                                 "EA.NA.per.NA" = paste0("North America (", rangers.shared.EA.NA$value[rangers.shared.EA.NA$per == "per.NA"], ")"),
                                 "EA.NA.per.SA" = paste0("South America (", rangers.shared.EA.NA$value[rangers.shared.EA.NA$per == "per.SA"], ")"))) +
    coord_polar("y", start = 0) +
    theme_void()

ggsave(p.rangers.pie.EA.NA, 
       file = "./Figures/rangers.pie.EA.NA.png", 
       width = 20, height = 10, units = "cm")

p.rangers.pie.NA.SA <- ggplot(rangers.shared.NA.SA, aes(x = "", y = value, fill = group)) +
    #geom_col(color = 'black',
    #         position = position_stack(reverse = TRUE)) + #show.legend = TRUE
    #geom_bar(stat="identity", width=1) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(values = c("NA.SA.per.AF" = "#c5dc93",
                                 "NA.SA.per.AU" = "#e4a182",
                                 "NA.SA.per.EA" = "#fee0a6",
                                 "NA.SA.per.NA" = "#bbe3d4",
                                 "NA.SA.per.SA" = "#f0d2ff"),
                      name = "Family Origin",
                      labels = c("NA.SA.per.AF" = paste0("Africa (", rangers.shared.NA.SA$value[rangers.shared.NA.SA$per == "per.AF"], ")"),
                                 "NA.SA.per.AU" = paste0("Australia (", rangers.shared.NA.SA$value[rangers.shared.NA.SA$per == "per.AU"], ")"),
                                 "NA.SA.per.EA" = paste0("Eurasia (", rangers.shared.NA.SA$value[rangers.shared.NA.SA$per == "per.EA"], ")"),
                                 "NA.SA.per.NA" = paste0("North America (", rangers.shared.NA.SA$value[rangers.shared.NA.SA$per == "per.NA"], ")"),
                                 "NA.SA.per.SA" = paste0("South America (", rangers.shared.NA.SA$value[rangers.shared.NA.SA$per == "per.SA"], ")"))) +
    coord_polar("y", start = 0) +
    theme_void()

ggsave(p.rangers.pie.NA.SA, 
       file = "./Figures/rangers.pie.NA.SA.png", 
       width = 20, height = 10, units = "cm")

##### COSMOPOLITAN ORIGINS -----
cosmo <- df[df$n.cont == "3+",]
cosmo.origin <- cosmo %>%
  group_by(family.origin) %>%
  dplyr::summarise(N = n(),
                   N.Africa = length(continent.Africa[continent.Africa == TRUE]),
                   N.Australia = length(continent.Australia[continent.Australia == TRUE]),
                   N.South.America = length(continent.South.America[continent.South.America == TRUE]),
                   N.North.America = length(continent.North.America[continent.North.America == TRUE]),
                   N.Eurasia = length(continent.Eurasia[continent.Eurasia == TRUE])) %>%
  as.data.frame() 
cosmo.origin <- cosmo.origin[cosmo.origin$family.origin != "",]

#get proportions
cosmo.origin$prop.spread <- ""
cosmo.origin$prop.spread[cosmo.origin$family.origin == "Africa"] <- cosmo.origin$N.Africa[cosmo.origin$family.origin == "Africa"]/cosmo.origin$N[cosmo.origin$family.origin == "Africa"]
cosmo.origin$prop.spread[cosmo.origin$family.origin == "Eurasia"] <- cosmo.origin$N.Eurasia[cosmo.origin$family.origin == "Eurasia"]/cosmo.origin$N[cosmo.origin$family.origin == "Eurasia"]
cosmo.origin$prop.spread[cosmo.origin$family.origin == "Australia"] <- cosmo.origin$N.Australia[cosmo.origin$family.origin == "Australia"]/cosmo.origin$N[cosmo.origin$family.origin == "Australia"]
cosmo.origin$prop.spread[cosmo.origin$family.origin == "South.America"] <- cosmo.origin$N.South.America[cosmo.origin$family.origin == "South.America"]/cosmo.origin$N[cosmo.origin$family.origin == "South.America"]
cosmo.origin$prop.spread[cosmo.origin$family.origin == "North.America"] <- cosmo.origin$N.North.America[cosmo.origin$family.origin == "North.America"]/cosmo.origin$N[cosmo.origin$family.origin == "North.America"]
#no jumpers in North.America
#no spreaders from Australia

cosmo.origin$prop.jump <- 1- as.numeric(cosmo.origin$prop.spread)

#want to know where the jumpers and spreaders went to
cosmo.cont.shared <- cosmo %>%
  group_by(family.origin) %>%
  dplyr::summarise(N = n(),
                   N.Africa.Eurasia.North.America = length(continent.Africa[continent.Africa == TRUE & continent.Eurasia == TRUE & continent.North.America == TRUE]),
                   N.Eurasia.North.America.South.America = length(continent.Eurasia[continent.Eurasia == TRUE & continent.North.America == TRUE & continent.South.America == TRUE]),
                   N.Africa.Eurasia.Australia = length(continent.Africa[continent.Africa == TRUE & continent.Eurasia == TRUE & continent.Australia == TRUE])) %>%
  as.data.frame() 
cosmo.cont.shared <- cosmo.cont.shared[cosmo.cont.shared$family.origin != "",]
write.csv(cosmo.cont.shared, 
          "./Results/trotter.family.origin.csv",
          row.names = FALSE)

length(df$binomial[df$family.origin == "South.America"]) #658
length(df$binomial[df$family.origin == "South.America" &
                     df$n.cont == 2]) #83
length(df$binomial[df$family.origin == "North.America"]) #636
length(df$binomial[df$family.origin == "North.America" &
                     df$n.cont == 2]) #46
length(df$binomial[df$family.origin == "Eurasia"]) #1753
length(df$binomial[df$family.origin == "Eurasia" &
                     df$n.cont == 2]) #75

continent <- names(dplyr::select(df, starts_with("continent")))
homies.origin <- data.frame(continent  = continents)

family.origin <- df %>%
  group_by(n.cont, family) %>%
  dplyr::summarise(N = n()) %>% 
  dplyr::select(n.cont,
                family,
                starts_with("continent")) %>%
  as.data.frame()

homies.family <- family.origin[family.origin$n.cont == 1,]
colnames(homies.family)[colnames(homies.family) == "N"] <- "homies.N"
homies.family <- homies.family %>%
  dplyr::select(-n.cont)

rangers.family <- family.origin[family.origin$n.cont == 2,]
colnames(rangers.family)[colnames(rangers.family) == "N"] <- "rangers.N"
rangers.family <- rangers.family %>%
  dplyr::select(-n.cont)

cosmo.family <- family.origin[family.origin$n.cont == "3+",]
colnames(cosmo.family)[colnames(cosmo.family) == "N"] <- "cosmo.N"
cosmo.family <- cosmo.family %>%
  dplyr::select(-n.cont)

##### DISPERSAL -----

df.dispersal <- df.dispersal %>%
  drop_na(hmrg, avg.mass)

#calculate dispersal (from Sutherland et al. 2000) (distance in km)
#carnivore: Dc = 40.7M^0.81
#herb or omni = Dho = 3.31M^0.65
df.dispersal$dispersal.distance <- ""
df.dispersal$dispersal.distance[df.dispersal$carn == TRUE] = 40.7*(df.dispersal$avg.mass[df.dispersal$carn == TRUE]^0.81)
df.dispersal$dispersal.distance[df.dispersal$carn != TRUE] = 3.31*(df.dispersal$avg.mass[df.dispersal$carn != TRUE]^0.65)

df.dispersal$dispersal.distance <- as.numeric(df.dispersal$dispersal.distance)

#model: age of dispersal (delay), generation length, age of lineage (fossil age), and dispersal amount
df.dispersal$dispersal.foss =  ((df.dispersal$foss.avg.age * 365)/(df.dispersal$gen.length + df.dispersal$disp.age))*df.dispersal$dispersal.distance
df.dispersal$dispersal.phylo =  ((df.dispersal$age * 365)/(df.dispersal$gen.length + df.dispersal$disp.age))*df.dispersal$dispersal.distance

ggplot() +
  geom_density(aes(df.dispersal$dispersal.foss[!is.na(df.dispersal$dispersal.foss)]))

length(df.dispersal$dispersal.foss[!is.na(df.dispersal$dispersal.foss)]) #68

ggplot() +
  geom_density(aes(df.dispersal$dispersal.phylo[!is.na(df.dispersal$dispersal.phylo)]))
length(df.dispersal$dispersal.phylo[!is.na(df.dispersal$dispersal.phylo)]) #84

nrow(df.dispersal[is.na(df.dispersal$dispersal.foss),]) #543
nrow(df.dispersal[is.na(df.dispersal$dispersal.phylo),]) #527
length(unique(df.dispersal$family[is.na(df.dispersal$dispersal.foss)])) #85
length(unique(df.dispersal$family[is.na(df.dispersal$dispersal.phylo)])) #83

## which groups are we misssing?
table(df.dispersal$order[is.na(df.dispersal$dispersal.foss)])
table(df.dispersal$order[is.na(df.dispersal$dispersal.phylo)])
#primates and rodenets most represented

nrow(df.dispersal[!is.na(df.dispersal$dispersal.foss) &
                    df.dispersal$order == "Rodentia",]) #12; only have 12 rodents out of lots
nrow(df.dispersal[!is.na(df.dispersal$dispersal.foss) &
                    df.dispersal$order == "Primates",]) #0; i.e. missing ALL primates

table(df.dispersal$family.origin[is.na(df.dispersal$dispersal.foss) &
                                   df.dispersal$order == "Primates"])

#are we missing rodents because of body size, where the smallest rodents are missing?
ks.test(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss) &
                                df.dispersal$order == "Rodentia"], 
        df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss) &
                                df.dispersal$order == "Rodentia"])
# Exact two-sample Kolmogorov-Smirnov test
# 
# data:  df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss) & df.dispersal$order == "Rodentia"] and df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss) & df.dispersal$order == "Rodentia"]
# D = 0.68667, p-value = 8.802e-06
# alternative hypothesis: two-sided

ks.test(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss) &
                                df.dispersal$order == "Rodentia"], 
        df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss) &
                                df.dispersal$order == "Rodentia"],
        alternative = "less") #y lies below x;
# Exact two-sample Kolmogorov-Smirnov test
# 
# data:  df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss) & df.dispersal$order == "Rodentia"] and df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss) & df.dispersal$order == "Rodentia"]
# D^- = 0.68667, p-value = 4.401e-06
# alternative hypothesis: the CDF of x lies below that of y
#yes, have bigger rodenets when have this data

## how does body size look for those that we're aren't missing? are they all large?
hist(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]))
hist(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]))
#both pretty uniform

min(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]) #21.175
max(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]) #2949986

min(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]) #4.5
max(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]) #3940034

#is there diff in body size between those we do have fossil FAD data for and those we dont?
ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]))
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]) and log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)])
# D = 0.35218, p-value = 6.23e-07
# alternative hypothesis: two-sided

ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]), alternative = "greater")
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]) and log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)])
# D^+ = 0.0018416, p-value = 0.9996
# alternative hypothesis: the CDF of x lies above that of y

ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]), alternative = "less") #sig
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]) and log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)])
# D^- = 0.35463, p-value = 3.115e-07
# alternative hypothesis: the CDF of x lies below that of y
#yes, body size bigger for those we have fossil data for

min(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]) #same as above
max(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)])

min(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])
max(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])

#is there diff in body size between those we do have phylo data for and those we dont?
ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)]))
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]) and log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])
# D = 0.37078, p-value = <0.001
# alternative hypothesis: two-sided

ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)]), alternative = "greater")
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]) and log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])
# D^+ = 0.0019048, p-value = 0.9995
# alternative hypothesis: the CDF of x lies above that of y

ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)]), alternative = "less") #sig
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]) and log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])
# D^- = 0.37078, p-value = <0.001
# alternative hypothesis: the CDF of x lies below that of y
#yes, body size bigger for those we have phylo data for

table(df.dispersal$n.cont) #i.e., what has home range and mass
# 1     2       3+ 
# 544   62      5 
#only have data for 62/260 (24%) of species on 2 continents
#only have data for 544/4119 (13%) of speices on 1 continent

table(df.dispersal$n.cont[!is.na(df.dispersal$dispersal.foss)])
# 1     2   3+ 
# 44    20  4
#only have data for 20/260 (7.6%) of species on 2 continents for fossil data
#only have data for 44/4119 (1.1%) of speices on 1 continent for fossil data

table(df.dispersal$n.cont[!is.na(df.dispersal$dispersal.phylo)])
# 1     2   3+ 
# 59    21  4
#only have data for 21/260 (8%) of species on 2 continents for phylo data
#only have data for 59/4119 (1.4%) of speices on 1 continent for phylo data

###### CAN SPECIES MAKE IT? ------ 
##Eurasia 
#54750000 km2
#8403.60 #km from coast of Portugal to north-eastern most point of Russia 
#9906.94 #km from coast of Portugal to coast of China
#7969.22 #km from tip of India to north coast of Russia
##Africa 
#30380000 km2
#8022.72 #km from tip of South Africa to top of Tunisia 
#7325.79  #km from coast of Gambia to coast/tip of Somalia
##North.America 
#24700000 km2
#7611.82 #km from Panama to Northwest Territories, Canada 
#5948.90 #from coast of Alaska to coast of Newfoundland
##South.America 
#17830000 km2
#7428.91 #km from tip of Chile to top of Venezuela/Colombia 
#5054.76 #km from coast of Peru to coast of Brazil
##Australia
#7690000 km2
#2992.65 #km from Melbourne to Northern tip of Queensland 
#3624.46 #km from Perth to Brisbane

#round up
Eurasia.EW = 9907
Eurasia.NS = 7969
Africa.EW = 7326
Africa.NS = 8023
North.America.EW = 5949
North.America.NS = 7612
South.America.EW = 5055
South.America.NS = 7429
Australia.EW = 3624
Australia.NS = 2993

length(df.dispersal$binomial[!is.na(df.dispersal$dispersal.foss)]) #68

length(df.dispersal$binomial[df.dispersal$dispersal.foss >= Australia.EW & !is.na(df.dispersal$dispersal.foss)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.foss >= Australia.NS & !is.na(df.dispersal$dispersal.foss)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.foss >= Eurasia.EW & !is.na(df.dispersal$dispersal.foss)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.foss >= Eurasia.NS & !is.na(df.dispersal$dispersal.foss)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.foss >= Africa.EW & !is.na(df.dispersal$dispersal.foss)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.foss >= Africa.NS & !is.na(df.dispersal$dispersal.foss)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.foss >= North.America.EW & !is.na(df.dispersal$dispersal.foss)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.foss >= North.America.NS & !is.na(df.dispersal$dispersal.foss)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.foss >= South.America.EW & !is.na(df.dispersal$dispersal.foss)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.foss >= South.America.NS & !is.na(df.dispersal$dispersal.foss)]) 

length(df.dispersal$binomial[!is.na(df.dispersal$dispersal.phylo)]) #85

length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= Australia.EW & !is.na(df.dispersal$dispersal.phylo)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= Australia.NS & !is.na(df.dispersal$dispersal.phylo)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= Eurasia.EW & !is.na(df.dispersal$dispersal.phylo)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= Eurasia.NS & !is.na(df.dispersal$dispersal.phylo)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= Africa.EW & !is.na(df.dispersal$dispersal.phylo)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= Africa.NS & !is.na(df.dispersal$dispersal.phylo)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= North.America.EW & !is.na(df.dispersal$dispersal.phylo)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= North.America.NS & !is.na(df.dispersal$dispersal.phylo)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= South.America.EW & !is.na(df.dispersal$dispersal.phylo)]) 
length(df.dispersal$binomial[df.dispersal$dispersal.phylo >= South.America.NS & !is.na(df.dispersal$dispersal.phylo)]) 

#unimpeded animals can get across continents; clearly some filtering
#is the filtering clade or ecological type specific? answer than by looking at families or something
#problem with home range: already constricted by filtering of some sort

ggplot(df.dispersal) + #do histogram; .25 log 
    geom_histogram(aes(log10(dispersal.distance),
                       group = n.cont, 
                       fill = n.cont),
                   binwidth = .25) +
    plot_theme +
    #theme(legend.position = c(0.8, 0.6)) +
    scale_fill_manual(values = cont_bw, 
                      name="Number of Continents",
                      labels = c("1",
                                 "2",
                                 "3+")) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(dispersal~distance~(km)))
range(df.dispersal$dispersal.distance[df.dispersal$n.cont == "1"], na.rm = TRUE)
#11.56184 km to 676899.43949 km (10^1 to 10^5)
range(df.dispersal$dispersal.distance[df.dispersal$n.cont == "3+"])
#1749.828 km to 1103928.351 km (10^3 to 10^6)
#starts 2 orders are mag higher than everyone else?

ggplot(df.dispersal) + #do histogram; .25 log 
    geom_histogram(aes(log10(age),
                       group = n.cont, 
                       fill = n.cont),
                   binwidth = .25) +
    plot_theme +
    #theme(legend.position = c(0.8, 0.6)) +
    scale_fill_manual(values = cont_bw, 
                      name="Number of Continents",
                      labels = c("1",
                                 "2",
                                 "3+")) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(phylo~age~(mya)))
range(df.dispersal$age[df.dispersal$n.cont == "1"], na.rm = TRUE)
#28664.39 51999997.61
range(df.dispersal$age[df.dispersal$n.cont == "3+"])
#766080.9 2006204.8
#nothing spectacular?

###### DISPERSAL FIGURES ------
p.disp.1 <- ggplot() + 
    geom_histogram(aes(log10(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "1" & !is.na(df.dispersal$dispersal.phylo)])), 
                   colour = "black", fill = "black",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    theme(axis.text.x = element_text(size = 24)) +
    scale_x_continuous(name = "Dispersal distance (km)",
                       limits = c(log10(1), log10(10^12)),
                       breaks = c(log10(1), log10(10^2), log10(10^4),
                                  log10(10^6), log10(10^8), log10(10^10),
                                  log10(10^12)),
                       labels = c(expression(10^0), expression(10^2),
                                  expression(10^4), expression(10^6),
                                  expression(10^8), expression(10^10),
                                  expression(10^12)))
#sample size
length(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "1" & !is.na(df.dispersal$dispersal.phylo)]) 
#60

ggsave(p.disp.1, 
       file = "./Figures/dispersal.one.png", 
       width = 20, height = 10, units = "cm")

p.disp.2 <- ggplot() + 
    geom_histogram(aes(log10(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "2" & !is.na(df.dispersal$dispersal.phylo)])), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    theme(axis.text.x = element_text(size = 24)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Dispersal distance (km)",
                       limits = c(log10(1), log10(10^12)),
                       breaks = c(log10(1), log10(10^2), log10(10^4),
                                  log10(10^6), log10(10^8), log10(10^10),
                                  log10(10^12)),
                       labels = c(expression(10^0), expression(10^2),
                                  expression(10^4), expression(10^6),
                                  expression(10^8), expression(10^10),
                                  expression(10^12)))
#sample size
length(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "2" & !is.na(df.dispersal$dispersal.phylo)]) 
#21

ggsave(p.disp.2, 
       file = "./Figures/dispersal.two.png", 
       width = 20, height = 10, units = "cm")

p.disp.2.nonvol <- ggplot() + 
    geom_histogram(aes(log10(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "2" & !is.na(df.dispersal$dispersal.phylo) & df$habitat.mode != "volant"])), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    theme(axis.text.x = element_text(size = 24)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Nonvolant Dispersal distance (km)",
                       limits = c(log10(1), log10(10^12)),
                       breaks = c(log10(1), log10(10^2), log10(10^4),
                                  log10(10^6), log10(10^8), log10(10^10),
                                  log10(10^12)),
                       labels = c(expression(10^0), expression(10^2),
                                  expression(10^4), expression(10^6),
                                  expression(10^8), expression(10^10),
                                  expression(10^12)))
#sample size
length(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "2" & !is.na(df.dispersal$dispersal.phylo) & df$habitat.mode != "volant"]) 
#19

ggsave(p.disp.2.nonvol, 
       file = "./Figures/dispersal.two.nonvol.png", 
       width = 20, height = 10, units = "cm")


p.disp.2.vol <- ggplot() + 
    geom_histogram(aes(log10(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "2" & !is.na(df.dispersal$dispersal.phylo) & df$habitat.mode == "volant"])), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    theme(axis.text.x = element_text(size = 24)) +
    scale_y_continuous(name = "Count",
                       breaks = c(0, 1)) +
    scale_x_continuous(name = "Volant Dispersal distance (km)",
                       limits = c(log10(1), log10(10^12)),
                       breaks = c(log10(1), log10(10^2), log10(10^4),
                                  log10(10^6), log10(10^8), log10(10^10),
                                  log10(10^12)),
                       labels = c(expression(10^0), expression(10^2),
                                  expression(10^4), expression(10^6),
                                  expression(10^8), expression(10^10),
                                  expression(10^12)))
#sample size
length(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "2" & !is.na(df.dispersal$dispersal.phylo) & df$habitat.mode == "volant"]) 
#2

ggsave(p.disp.2.vol, 
       file = "./Figures/dispersal.two.vol.png", 
       width = 20, height = 10, units = "cm")

p.disp.3 <- ggplot() + 
    geom_histogram(aes(log10(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "3+" & !is.na(df.dispersal$dispersal.phylo)])), 
                   colour = "red", fill = "red",
                   binwidth = .25) +
    plot_theme +
    theme(axis.text.x = element_text(size = 24)) +
    scale_y_continuous(name = "Count",
                       breaks = c(0, 1)) +
    scale_x_continuous(name = "Dispersal distance (km)",
                       limits = c(log10(1), log10(10^12)),
                       breaks = c(log10(1), log10(10^2), log10(10^4),
                                  log10(10^6), log10(10^8), log10(10^10),
                                  log10(10^12)),
                       labels = c(expression(10^0), expression(10^2),
                                  expression(10^4), expression(10^6),
                                  expression(10^8), expression(10^10),
                                  expression(10^12)))
#sample size
length(df.dispersal$dispersal.phylo[df.dispersal$n.cont == "3+" & !is.na(df.dispersal$dispersal.phylo)])
#4

ggsave(p.disp.3, 
       file = "./Figures/dispersal.three.png", 
       width = 20, height = 10, units = "cm")

###### DISPERSAL DISTANCE BY AGE -----
#model: age of dispersal (delay), generation length, age of lineage (fossil age), and dispersal amount
#((df.dispersal$age * 365)/(df.dispersal$gen.length + df.dispersal$disp.age))*df.dispersal$dispersal.distance
df.dispersal$dispersal.10 =  ((10 * 365)/(df.dispersal$gen.length + 10))*df.dispersal$dispersal.distance
df.dispersal$dispersal.100 =  ((100 * 365)/(df.dispersal$gen.length + 100))*df.dispersal$dispersal.distance
df.dispersal$dispersal.1000 =  ((1000 * 365)/(df.dispersal$gen.length + 1000))*df.dispersal$dispersal.distance
df.dispersal$dispersal.10000 =  ((10000 * 365)/(df.dispersal$gen.length + 10000))*df.dispersal$dispersal.distance
df.dispersal$dispersal.100000 =  ((100000 * 365)/(df.dispersal$gen.length + 100000))*df.dispersal$dispersal.distance

#df.dispersal$dispersal.150000 =  ((150000 * 365)/(df.dispersal$gen.length + 150000))*df.dispersal$dispersal.distance
#df.dispersal$dispersal.500000 =  ((500000 * 365)/(df.dispersal$gen.length + 500000))*df.dispersal$dispersal.distance
#df.dispersal$dispersal.1000000 =  ((1000000 * 365)/(df.dispersal$gen.length + 1000000))*df.dispersal$dispersal.distance

#add in lines for continents

p.disp.age <- ggplot(df.dispersal[!is.na(df.dispersal$dispersal.phylo),]) +
    geom_point(aes(log10(age), log10(dispersal.phylo),
                   group = n.cont,
                   col = n.cont),
               size = 4) +
    plot_theme +
    #theme(legend.position = c(0.6, 0.82)) +
    theme(plot.background = element_rect(fill = 'transparent', color = NA)) +
    scale_y_continuous(name = "Dispersal Distance (km)",
                       limits = c(log10(10^0), log10(10^12)),
                       breaks = c(log10(10^0), log10(10^2), 
                                  log10(10^4), log10(10^6),
                                  log10(10^8), log10(10^10),
                                  log10(10^12)),
                       labels = c(expression(10^0), expression(10^2),
                                  expression(10^4), expression(10^6),
                                  expression(10^8), expression(10^10),
                                  expression(10^12))) +
    scale_x_continuous(name = "Time for Dispersal (y)",
                       limits = c(log10(10^0), log10(10^8)),
                       breaks = c(log10(10^0), log10(10^1),
                                  log10(10^2), log10(10^3),
                                  log10(10^4), log10(10^5), 
                                  log10(10^6), log10(10^7), 
                                  log10(10^8)),
                       labels = c(expression(10^0), expression(10^1),
                                  expression(10^2), expression(10^3),
                                  expression(10^4), expression(10^5),
                                  expression(10^6), expression(10^7),
                                  expression(10^8))) +
    scale_color_manual(values = cont_bw) +
    geom_hline(yintercept = (log10(Australia.NS)),
               col = "#afa298", size = 1) + 
    geom_hline(yintercept = (log10(Eurasia.EW + Africa.NS)),
               col = "#afa298", size = 1)

ggsave(p.disp.age, 
       file = "./Figures/dispersal.age.png", 
       width = 24, height = 15, units = "cm")


df.time <- df.dispersal[!is.na(df.dispersal$dispersal.phylo), c("n.cont", "binomial", 
                                                                "dispersal.distance", 
                                                                "dispersal.10", "dispersal.100", 
                                                                "dispersal.1000", "dispersal.10000", 
                                                                "dispersal.100000")]

#which species is the slowest?
which.min(df.time$dispersal.distance) #63 (all of them are the same below)
df.time[62,] #Peromyscus leucopus; still has a large geographic range!; in NA
which.min(df.time$dispersal.10)
which.min(df.time$dispersal.100)
which.min(df.time$dispersal.1000)
which.min(df.time$dispersal.10000)
which.min(df.time$dispersal.100000)
which.min(df.dispersal$dispersal.phylo) #492
df.dispersal[492,] #Sciurus carolinensis; also large geographic range and suprisingly similar to Peromyscus leucopus; in NA

which.max(df.time$dispersal.distance) #80
df.time[80,] #Ursus maritimus; arctic, but not one of our globe trotters; in EA and NA
which.max(df.time$dispersal.10) #58
df.time[58,] #Panthera leo; a globe trotter!
which.max(df.time$dispersal.100) #58
which.max(df.time$dispersal.1000) #58
which.max(df.time$dispersal.10000) #58
which.max(df.time$dispersal.100000) #80
which.max(df.dispersal$dispersal.phylo) #389
df.dispersal[389,] #Panthera tigris; large geogrpahic range, in EA, not a globe trotter

#need it so age a row and dispersal type and then dispersal value
disp.min <- c(min(df.time$dispersal.distance),
              min(df.time$dispersal.10),
              min(df.time$dispersal.100),
              min(df.time$dispersal.1000),
              min(df.time$dispersal.10000),
              min(df.time$dispersal.100000),
              1, 1, 1)
disp.max <- c(max(df.time$dispersal.distance),
              max(df.time$dispersal.10),
              max(df.time$dispersal.100),
              max(df.time$dispersal.1000),
              max(df.time$dispersal.10000),
              max(df.time$dispersal.100000),
              1, 1, 1)
disp.mean <- c(mean(df.time$dispersal.distance),
               mean(df.time$dispersal.10),
               mean(df.time$dispersal.100),
               mean(df.time$dispersal.1000),
               mean(df.time$dispersal.10000),
               mean(df.time$dispersal.100000),
               1, 1, 1)
disp.age <- c(1, 10, 100, 1000, 100000, 100000, 1000000, 10000000, 100000000)
disp.type <- c("gen", "decade", "century", "millenia", "10kya", "100kya",
               "1 mya", "10 mya", "100 mya")
df.time2 <- as.data.frame(cbind(disp.type, disp.min, disp.max, disp.mean, disp.age))
df.time2$disp.type <- as.factor(df.time2$disp.type)
df.time2$disp.age <- as.numeric(df.time2$disp.age)
df.time2$disp.min <- as.numeric(df.time2$disp.min)
df.time2$disp.max <- as.numeric(df.time2$disp.max)
df.time2$disp.mean <- as.numeric(df.time2$disp.mean)

df.time2$disp.type <- factor(df.time2$disp.type,
                             levels = c("gen", "decade", "century", "millenia", "10kya", "100kya",
                                        "1 mya", "10 mya", "100 mya"))

p.disp.age.time <-  ggplot(df.time2, 
                           aes(x = disp.type)) +
    geom_boxplot(aes(ymin = log10(disp.min),
                     lower = log10(disp.min),
                     middle = log10(disp.mean),
                     ymax = log10(disp.max),
                     upper = log10(disp.max)),
                 stat = "identity",
                 width=0.7) +
    # geom_point(aes(log10(100), log10(dispersal.100)),
    #            pch = 1, size = 2, col = "black") +
    # geom_point(aes(log10(1000), log10(dispersal.1000)),
    #            pch = 1, size = 2, col = "black") +
    # geom_point(aes(log10(10000), log10(dispersal.10000)),
    #            pch = 1, size = 2, col = "black") +
    # geom_point(aes(log10(100000), log10(dispersal.100000)),
    #            pch = 1, size = 2, col = "black") +
    plot_theme +
    #theme(legend.position = c(0.6, 0.82)) +
    scale_y_continuous(name = "Dispersal Distance (km)",
                       limits = c(log10(10^0), log10(10^12)),
                       breaks = c(log10(10^0), log10(10^2), 
                                  log10(10^4), log10(10^6),
                                  log10(10^8), log10(10^10),
                                  log10(10^12)),
                       labels = c(expression(10^0), expression(10^2),
                                  expression(10^4), expression(10^6),
                                  expression(10^8), expression(10^10),
                                  expression(10^12))) +
    scale_x_discrete(name = "Time for Dispersal (y)",
                     labels = c(expression(10^0), expression(10^1),
                                expression(10^2), expression(10^3),
                                expression(10^4), expression(10^5),
                                expression(10^6), expression(10^7),
                                expression(10^8)))

ggsave(p.disp.age.time, 
       file = "./Figures/dispersal.time.png", 
       width = 24, height = 15, units = "cm")

#size distribution of greatest dispersers
which.max(df.dispersal$dispersal.foss)
which.max(df.dispersal$dispersal.phylo)
df.dispersal$binomial[386]
df.dispersal$binomial[389]
df.dispersal$log.dist.foss <- log10(df.dispersal$dispersal.foss)
df.dispersal$log.dist.phylo <- log10(df.dispersal$dispersal.phylo)
range(df.dispersal$log.dist.foss, na.rm = TRUE)
range(df.dispersal$log.dist.phylo, na.rm = TRUE)

df.dispersal <- mutate(df.dispersal, qtr.bin.dist.foss = cut(log.dist.foss, breaks = seq(0, 12, .25)))
df.dispersal <- mutate(df.dispersal, qtr.bin.dist.phylo = cut(log.dist.phylo, breaks = seq(0, 12, .25)))

unique(df.dispersal$qtr.bin.dist.foss) #up to 7.25
unique(df.dispersal$qtr.bin.dist.phylo) #up to 7.25

df.dispersal$avg.mass[df.dispersal$qtr.bin.dist.foss == "(11.5,11.8]"]

which.max(df.dispersal$dispersal.distance)
df.dispersal$binomial[596]

df.dispersal$log.disp.dist <- log10(df.dispersal$dispersal.distance)
df.dispersal <- mutate(df.dispersal, qtr.bin.dist = cut(log.disp.dist, breaks = seq(0, 7, .25)))
sort(unique(df.dispersal$qtr.bin.dist)) #max  (6,6.25] 
df.dispersal[df.dispersal$qtr.bin.dist == "(6,6.25]",] #panthera leo and ursus maritimus
df.dispersal[df.dispersal$qtr.bin.dist == "(5.75,6]",] #panthera tigris
df.dispersal[df.dispersal$qtr.bin.dist == "(5.5,5.75]",] #Crocuta crocuta and Panthera onca

#Ursus martimus has greatest dispersal distance; Panthera tigris goes the farthest
#what are their geog ranges and home ranges and how does it compare?
range(df$home.range.km2, na.rm = TRUE)
mean(df$home.range.km2, na.rm = TRUE)
median(df$home.range.km2, na.rm = TRUE)
df$home.range.km2[df$binomial == "Ursus maritimus"] #largest
df$home.range.km2[df$binomial == "Panthera tigris"] #small
df$home.range.km2[df$binomial == "Panthera onca"] #large

range(df$gr.area.km2, na.rm = TRUE)
df$gr.area.km2[df$binomial == "Ursus maritimus"] #unknown, but in a single ecoregion
df$gr.area.km2[df$binomial == "Panthera tigris"] #large
df$gr.area.km2[df$binomial == "Crocuta crocuta"] #large
df$gr.area.km2[df$binomial == "Panthera onca"] #large

###### AGES FIGURE -----
p.age.1 <- ggplot() + #do histogram; .5 log 
    geom_histogram(aes(log10(df$age.median[df$n.cont == "1" & !is.na(df$age.median)])), 
                   colour = "black", fill = "black",
                   binwidth = .25) +
    plot_theme +
    theme(axis.text.x = element_text(size = 24)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Age (mya)",
                       limits = c(log10(10^-2.25), log10(10^2)),
                       labels = c(expression(10^-2), expression(10^-1),
                                  expression(10^0), expression(10^1),
                                  expression(10^2)))
#sample size
length(df$age.median[df$n.cont == "1" & !is.na(df$age.median)]) 
#3711

ggsave(p.age.1, 
       file = "./Figures/age.one.png", 
       width = 20, height = 10, units = "cm")


p.age.2 <- ggplot() + #do histogram; .5 log 
    geom_histogram(aes(log10(df$age.median[df$n.cont == "2" & !is.na(df$age.median)])), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    theme(axis.text.x = element_text(size = 24)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Age (mya)",
                       limits = c(log10(10^-2.25), log10(10^2)),
                       labels = c(expression(10^-2), expression(10^-1),
                                  expression(10^0), expression(10^1),
                                  expression(10^2)))
#sample size
length(df$age.median[df$n.cont == "2" & !is.na(df$age.median)]) 
#251

ggsave(p.age.2, 
       file = "./Figures/age.two.png", 
       width = 20, height = 10, units = "cm")

p.age.2.nonvol <- ggplot() + #do histogram; .5 log 
    geom_histogram(aes(log10(df$age.median[df$n.cont == "2" & !is.na(df$age.median) & df$habitat.mode != "volant"])), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    theme(axis.text.x = element_text(size = 24)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Nonvolant Age (mya)",
                       limits = c(log10(10^-2.25), log10(10^2)),
                       labels = c(expression(10^-2), expression(10^-1),
                                  expression(10^0), expression(10^1),
                                  expression(10^2)))
#sample size
length(df$age.median[df$n.cont == "2" & !is.na(df$age.median) & df$habitat.mode != "volant"]) 
#114

ggsave(p.age.2.nonvol, 
       file = "./Figures/age.two.nonvol.png", 
       width = 20, height = 10, units = "cm")

p.age.2.vol <- ggplot() + #do histogram; .5 log 
    geom_histogram(aes(log10(df$age.median[df$n.cont == "2" & !is.na(df$age.median) & df$habitat.mode == "volant"])), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    theme(axis.text.x = element_text(size = 24)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Volant Age (mya)",
                       limits = c(log10(10^-2.25), log10(10^2)),
                       labels = c(expression(10^-2), expression(10^-1),
                                  expression(10^0), expression(10^1),
                                  expression(10^2)))
#sample size
length(df$age.median[df$n.cont == "2" & !is.na(df$age.median) & df$habitat.mode == "volant"]) 
#137

ggsave(p.age.2.vol, 
       file = "./Figures/age.two.vol.png", 
       width = 20, height = 10, units = "cm")

p.age.3 <- ggplot() + #do histogram; .5 log 
    geom_histogram(aes(log10(df$age.median[df$n.cont == "3+" & !is.na(df$age.median)])), 
                   colour = "red", fill = "red",
                   binwidth = .25) +
    plot_theme +
    theme(axis.text.x = element_text(size = 24)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Age (mya)",
                       limits = c(log10(10^-2.25), log10(10^2)),
                       labels = c(expression(10^-2), expression(10^-1),
                                  expression(10^0), expression(10^1),
                                  expression(10^2)))
#sample size
length(df$age.median[df$n.cont == "3+" & !is.na(df$age.median)]) 
#6

ggsave(p.age.3, 
       file = "./Figures/age.three.png", 
       width = 20, height = 10, units = "cm")

#### H3: DIVERSITY OF CLADE ----
## TEST: wide-ranging species come from families with lots of species

null.family <- df %>%
  group_by(family) %>%
  dplyr::summarise(null.N = n()) %>%
  dplyr::select(family,
                null.N) %>%
  as.data.frame()

family <- df %>%
  group_by(n.cont, family) %>%
  dplyr::summarise(N = n()) %>% 
  as.data.frame()

homies.family <- family[family$n.cont == 1,]
colnames(homies.family)[colnames(homies.family) == "N"] <- "homies.N"
homies.family <- homies.family %>%
  dplyr::select(-n.cont)

rangers.family <- family[family$n.cont == 2,]
colnames(rangers.family)[colnames(rangers.family) == "N"] <- "rangers.N"
rangers.family <- rangers.family %>%
  dplyr::select(-n.cont)

family.nonvol <- df[df$habitat.mode != "volant",] %>%
    group_by(n.cont, family) %>%
    dplyr::summarise(N = n()) %>% 
    as.data.frame()

rangers.family.nonvol <- family.nonvol[family.nonvol$n.cont == 2,]
colnames(rangers.family.nonvol)[colnames(rangers.family.nonvol) == "N"] <- "rangers.N"
rangers.family.nonvol <- rangers.family.nonvol %>%
    dplyr::select(-n.cont)

family.vol <- df[df$habitat.mode == "volant",] %>%
    group_by(n.cont, family) %>%
    dplyr::summarise(N = n()) %>% 
    as.data.frame()

rangers.family.vol <- family.vol[family.vol$n.cont == 2,]
colnames(rangers.family.vol)[colnames(rangers.family.vol) == "N"] <- "rangers.N"
rangers.family.vol <- rangers.family.vol %>%
    dplyr::select(-n.cont)

cosmo.family <- family[family$n.cont == "3+",]
colnames(cosmo.family)[colnames(cosmo.family) == "N"] <- "cosmo.N"
cosmo.family <- cosmo.family %>%
  dplyr::select(-n.cont)

#create full dataset
fam.null.cosmo <- merge(null.family, cosmo.family, by = "family", all.x = TRUE, all.y = TRUE)
fam.null.cosmo.rangers <- merge(fam.null.cosmo, rangers.family, by = "family", all.x = TRUE, all.y = TRUE)
fam.null.cosmo.rangers.homies <- merge(fam.null.cosmo.rangers, homies.family, by = "family", all.x = TRUE, all.y = TRUE)

df.family <- fam.null.cosmo.rangers.homies
df.family[is.na(df.family)] <- 0

df.family$prop.null <- df.family$null.N/nrow(df)

df.family$prop.homies <- df.family$homies.N/nrow(df[df$n.cont == "1",])
df.family$prop.rangers <- df.family$rangers.N/nrow(df[df$n.cont == "2",])
df.family$prop.cosmo <- df.family$cosmo.N/nrow(df[df$n.cont == "3+",])

#binomial test
for(i in 1:nrow(df.family)){
  test <- binom.test(df.family$homies.N[i], nrow(df[df$n.cont == "1",]), p = df.family$prop.null[i], alternative = "two.sided")
  df.family$p.homies[i] <- test$p.value
}

for(i in 1:nrow(df.family)){
  test <- binom.test(df.family$rangers.N[i], nrow(df[df$n.cont == "2",]), p = df.family$prop.null[i], alternative = "two.sided")
  df.family$p.rangers[i] <- test$p.value
}

for(i in 1:nrow(df.family)){
  test <- binom.test(df.family$cosmo.N[i], nrow(df[df$n.cont == "3+",]), p = df.family$prop.null[i], alternative = "two.sided")
  df.family$p.cosmo[i] <- test$p.value
}

#add sidak correction
df.family <- arrange(df.family, p.homies) %>%
  dplyr::mutate(signif.homies = p.homies < 0.05,
                signif.bonferoni.homies = p.homies < 0.05/n(),
                signif.holm.homies = !0.05/(n() + 1 - 1:n()) < p.homies,
                signif.sidak.homies = p.homies < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.homies = !(1 - (1 - 0.05)^(1/n())) < p.homies)

df.family <- arrange(df.family, p.rangers) %>%
  dplyr::mutate(signif.rangers = p.rangers < 0.05,
                signif.bonferoni.rangers = p.rangers < 0.05/n(),
                signif.holm.rangers = !0.05/(n() + 1 - 1:n()) < p.rangers,
                signif.sidak.rangers = p.rangers < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.rangers = !(1 - (1 - 0.05)^(1/n())) < p.rangers)

df.family <- arrange(df.family, p.cosmo) %>%
  dplyr::mutate(signif.cosmo = p.cosmo < 0.05,
                signif.bonferoni.cosmo = p.cosmo < 0.05/n(),
                signif.holm.cosmo = !0.05/(n() + 1 - 1:n()) < p.cosmo,
                signif.sidak.cosmo = p.cosmo < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.cosmo = !(1 - (1 - 0.05)^(1/n())) < p.cosmo)

write.csv(df.family, 
          "./Results/family.results.csv",
          row.names = FALSE)

##maybe do at order level; maybe don't care about continent of origin for order because too far in past, and families maybe sp w/in families more ecol similar and so behave similarly

#test for body size bias
##limited dispersers
rangers.true <- subset(df.family, df.family$signif.sidak.rangers == TRUE, select = c(family,
                                                                                     null.N,
                                                                                     rangers.N,
                                                                                     prop.null,
                                                                                     prop.rangers,
                                                                                     signif.sidak.rangers))

rangers.false <- subset(df.family, df.family$signif.sidak.rangers == FALSE, select = c(family,
                                                                                       null.N,
                                                                                       rangers.N,
                                                                                       prop.null,
                                                                                       prop.rangers,
                                                                                       signif.sidak.rangers))
        
family.rangers.true <- df[df$family %in% rangers.true$family,]
hist(log10(family.rangers.true$avg.mass))

family.rangers.false <- df[df$family %in% rangers.false$family,]
hist(log10(family.rangers.false$avg.mass))

family.rangers.true.na <- family.rangers.true %>%
  drop_na(avg.mass)

family.rangers.false.na <- family.rangers.false %>%
  drop_na(avg.mass)

ks.test(family.rangers.true.na$avg.mass, 
        family.rangers.false.na$avg.mass) #sig diff
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  family.lim.true.na$avg.mass and family.lim.false.na$avg.mass
# D = 0.36586, p-value < 2.2e-16
# alternative hypothesis: two-sided

ks.test(family.rangers.true$avg.mass, 
        family.rangers.false$avg.mass, alternative = "greater") ##sig; wide ranging speices are SMALLER than non
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  family.lim.true$avg.mass and family.lim.false$avg.mass
# D^+ = 0.36586, p-value < 2.2e-16
# alternative hypothesis: the CDF of x lies above that of y

ks.test(family.rangers.true$avg.mass, family.rangers.false$avg.mass, alternative = "less") #not sig
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  family.lim.true$avg.mass and family.lim.false$avg.mass
# D^- = -3.8164e-17, p-value = 1
# alternative hypothesis: the CDF of x lies below that of y

##cosmopolitan
cosmo.true <- subset(df.family, df.family$signif.sidak.cosmo == TRUE, select = c(family,
                                                                                 null.N,
                                                                                 cosmo.N,
                                                                                 prop.null,
                                                                                 prop.cosmo,
                                                                                 signif.sidak.cosmo))
#THERE AREN'T ANY

#is it drivine by insectivores? 
insect.family <- c("Erinaceidae", "Soricidae", "Talpidae", "Solenodontidae",
                   "Chrysochloridae", "Tenrecidae", "Potamogalidae", "Macroscelididae",
                   "Tupaiidae", "Ptilocercidae", "Cynocephalidae")
insect.df <- df[df$family %in% insect.family,]
table(insect.df$n.cont) #most on 1 continent (416); 5 on 2 continents
table(df$n.cont[df$diet.invertivore.tot == TRUE]) #most on 1 continent (1824); 140 on 2 continents; 3 on 3
insect.stat <- df %>%
  group_by(order, n.cont) %>%
  filter(diet.invertivore.tot == TRUE) %>%
  dplyr::summarise(count = n())
sum(insect.stat$count) #1973 total
sum(insect.stat$count[insect.stat$n.cont == 2]) #141
#106 out of 141 are chiroptera

bats <- df[df$order == "Chiroptera",]
bats.2 <- bats[bats$n.cont == 2,]
length(bats.2$binomial[bats.2$diet.invertivore.tot == TRUE]) #106 of
length(bats.2$binomial) #142
table(bats.2$diet.breadth)
# 1    2    3 
# 117  23   2 
bats.2.2 <- bats.2[bats.2$diet.breadth == 2,] #3 eat meat, 2 eat fish, 18 eat fruit; all eat invert


#Number of species per clade and number of species on 2+ continents
df.wr <- df %>%
  group_by(order) %>%
  summarise(n.ord.sp = length(unique(binomial)),
            n.cont.sp = length(unique(binomial[n.cont !=1]))) %>%
  as.data.frame()

sp.clade.lm <- ggplot() +
  geom_point(aes(x = df.wr$n.ord.sp, 
                 y = df.wr$n.cont.sp)) +
  geom_smooth(aes(x = df.wr$n.ord.sp, 
                 y = df.wr$n.cont.sp)) +
  plot_theme + 
  scale_x_continuous(name = "Species per Order") +
  scale_y_continuous(name = "Number Species on mult continents")

summary(lm(df.wr$n.cont.sp ~ df.wr$n.ord.sp))
# Call:
#   lm(formula = df.wr$n.cont.sp ~ df.wr$n.ord.sp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -43.610  -3.479  -3.070  -2.546 108.038 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)     2.98864    4.74761    0.63  0.53431   
# df.wr$n.ord.sp  0.04089    0.01206    3.39  0.00216 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 23.6 on 27 degrees of freedom
# Multiple R-squared:  0.2986,	Adjusted R-squared:  0.2726 
# F-statistic: 11.49 on 1 and 27 DF,  p-value: 0.002163

###### DIVERSITY BAR GRAPH -----
df <- merge(df, null.family,
             by = "family")
colnames(df)[colnames(df) == 'null.N'] <- 'fam.richness'

p.div.1 <- ggplot(df[df$n.cont == "1",]) + #do histogram; .25 log 
    geom_histogram(aes(log10(fam.richness)), 
                   colour = "black", fill = "black",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(Family~Diversity),
                       limits = c(log10(.1), log10(10^3)),
                       labels = c(expression(10^-1), expression(10^0), expression(10^1), 
                                  expression(10^2), expression(10^3)))
#sample size
length(homies.family$homies.N) #132 fam
sum(homies.family$homies.N) #4120 sp

ggsave(p.div.1, 
       file = "./Figures/fam.div_one.cont.png", 
       width = 14, height = 10, units = "cm")

p.div.2 <- ggplot(df[df$n.cont == "2",]) + #do histogram; .25 log 
    geom_histogram(aes(log10(fam.richness)), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(Family~Diversity),
                       limits = c(log10(.1), log10(10^3)),
                       labels = c(expression(10^-1), expression(10^0), expression(10^1), 
                                  expression(10^2), expression(10^3)))
#sample size
length(rangers.family$rangers.N) #54 fam
sum(rangers.family$rangers.N) #260 sp

ggsave(p.div.2, 
       file = "./Figures/fam.div_two.cont.png", 
       width = 14, height = 10, units = "cm")

p.div.2.nonvol <- ggplot(df[df$n.cont == "2" & df$habitat.mode != "volant",]) + #do histogram; .25 log 
    geom_histogram(aes(log10(fam.richness)), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(Family~Diversity),
                       limits = c(log10(.1), log10(10^3)),
                       labels = c(expression(10^-1), expression(10^0), expression(10^1), 
                                  expression(10^2), expression(10^3)))
#sample size
length(rangers.family.nonvol$rangers.N) #41 fam
sum(rangers.family.nonvol$rangers.N) #118 sp

ggsave(p.div.2.nonvol, 
       file = "./Figures/fam.div.nonvol_two.cont.png", 
       width = 14, height = 10, units = "cm")

p.div.2.vol <- ggplot(df[df$n.cont == "2" & df$habitat.mode == "volant",]) + #do histogram; .25 log 
    geom_histogram(aes(log10(fam.richness)), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(Family~Diversity),
                       limits = c(log10(.1), log10(10^3)),
                       labels = c(expression(10^-1), expression(10^0), expression(10^1), 
                                  expression(10^2), expression(10^3)))
#sample size
length(rangers.family.vol$rangers.N) #13 fam
sum(rangers.family.vol$rangers.N) #142 sp

ggsave(p.div.2.vol, 
       file = "./Figures/fam.div.vol_two.cont.png", 
       width = 14, height = 10, units = "cm")

p.div.3 <- ggplot(df[df$n.cont == "3+",]) + #do histogram; .25 log 
    geom_histogram(aes(log10(fam.richness)), 
                   colour = "red", fill = "red",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression("Family's Diversity"),
                       limits = c(log10(.1), log10(10^3)),
                       labels = c(expression(10^-1), expression(10^0), expression(10^1), 
                                  expression(10^2), expression(10^3)))
#sample size
length(cosmo.family$cosmo.N) #6 fam
sum(cosmo.family$cosmo.N) #6 sp

ggsave(p.div.3, 
       file = "./Figures/fam.div_three.cont.png", 
       width = 14, height = 10, units = "cm")

#### H4: BODY SIZE ----

global.mass <- df$log.mass[!is.na(df$log.mass)] #3316 records
median(global.mass) #1.95424
median(df$avg.mass[!is.na(df$avg.mass)]) #89.9995

##### binomial tests of qtr bings -----

null.bs <- df %>%
    group_by(qtr.bin) %>%
    dplyr::summarise(null.N = n()) %>%
    dplyr::select(qtr.bin,
                  null.N) %>%
    as.data.frame()

bs.cont <- df %>%
    group_by(n.cont, qtr.bin) %>%
    dplyr::summarise(N = n()) %>% 
    as.data.frame()

homies.bs <- bs.cont[bs.cont$n.cont == 1,]
colnames(homies.bs)[colnames(homies.bs) == "N"] <- "homies.N"
homies.bs <- homies.bs %>%
    dplyr::select(-n.cont)

rangers.bs <- bs.cont[bs.cont$n.cont == 2,]
colnames(rangers.bs)[colnames(rangers.bs) == "N"] <- "rangers.N"
rangers.bs <- rangers.bs %>%
    dplyr::select(-n.cont)

bs.nonvol <- df[df$habitat.mode != "volant",] %>%
    group_by(n.cont, qtr.bin) %>%
    dplyr::summarise(N = n()) %>% 
    as.data.frame()

rangers.bs.nonvol <- bs.nonvol[bs.nonvol$n.cont == 2,]
colnames(rangers.bs.nonvol)[colnames(rangers.bs.nonvol) == "N"] <- "nonvol.rangers.N"
rangers.bs.nonvol <- rangers.bs.nonvol %>%
    dplyr::select(-n.cont)

bs.vol <- df[df$habitat.mode == "volant",] %>%
    group_by(n.cont, qtr.bin) %>%
    dplyr::summarise(N = n()) %>% 
    as.data.frame()

rangers.bs.vol <- bs.vol[bs.vol$n.cont == 2,]
colnames(rangers.bs.vol)[colnames(rangers.bs.vol) == "N"] <- "vol.rangers.N"
rangers.bs.vol <- rangers.bs.vol %>%
    dplyr::select(-n.cont)

cosmo.bs <- bs.cont[bs.cont$n.cont == "3+",]
colnames(cosmo.bs)[colnames(cosmo.bs) == "N"] <- "cosmo.N"
cosmo.bs <- cosmo.bs %>%
    dplyr::select(-n.cont)

#create full dataset
bs.null.cosmo <- merge(null.bs, cosmo.bs, 
                       by = "qtr.bin", 
                       all.x = TRUE, all.y = TRUE)
bs.null.cosmo.rangers <- merge(bs.null.cosmo, rangers.bs, 
                               by = "qtr.bin", 
                               all.x = TRUE, all.y = TRUE)
bs.null.cosmo.rangers.homies <- merge(bs.null.cosmo.rangers, homies.bs, 
                                      by = "qtr.bin", 
                                      all.x = TRUE, all.y = TRUE)

df.bs <- bs.null.cosmo.rangers.homies
df.bs <- df.bs[!is.na(df.bs$qtr.bin),]
df.bs[is.na(df.bs)] <- 0

df.bs$prop.null <- df.bs$null.N/nrow(df)

df.bs$prop.homies <- df.bs$homies.N/nrow(df[df$n.cont == "1",])
df.bs$prop.rangers <- df.bs$rangers.N/nrow(df[df$n.cont == "2",])
df.bs$prop.cosmo <- df.bs$cosmo.N/nrow(df[df$n.cont == "3+",])

#binomial test
for(i in 1:nrow(df.bs)){
    test <- binom.test(df.bs$homies.N[i], nrow(df[df$n.cont == "1",]), 
                       p = df.bs$prop.null[i], alternative = "two.sided")
    df.bs$p.homies[i] <- test$p.value
}

for(i in 1:nrow(df.bs)){
    test <- binom.test(df.bs$rangers.N[i], nrow(df[df$n.cont == "2",]), 
                       p = df.bs$prop.null[i], alternative = "two.sided")
    df.bs$p.rangers[i] <- test$p.value
}

for(i in 1:nrow(df.bs)){
    test <- binom.test(df.bs$cosmo.N[i], nrow(df[df$n.cont == "3+",]), 
                       p = df.bs$prop.null[i], alternative = "two.sided")
    df.bs$p.cosmo[i] <- test$p.value
}

#add sidak correction
df.bs <- arrange(df.bs, p.homies) %>%
    dplyr::mutate(signif.homies = p.homies < 0.05,
                  signif.bonferoni.homies = p.homies < 0.05/n(),
                  signif.holm.homies = !0.05/(n() + 1 - 1:n()) < p.homies,
                  signif.sidak.homies = p.homies < 1 - (1 - 0.05)^(1/n()),
                  signif.holm.sidak.homies = !(1 - (1 - 0.05)^(1/n())) < p.homies)

df.bs <- arrange(df.bs, p.rangers) %>%
    dplyr::mutate(signif.rangers = p.rangers < 0.05,
                  signif.bonferoni.rangers = p.rangers < 0.05/n(),
                  signif.holm.rangers = !0.05/(n() + 1 - 1:n()) < p.rangers,
                  signif.sidak.rangers = p.rangers < 1 - (1 - 0.05)^(1/n()),
                  signif.holm.sidak.rangers = !(1 - (1 - 0.05)^(1/n())) < p.rangers)

df.bs <- arrange(df.bs, p.cosmo) %>%
    dplyr::mutate(signif.cosmo = p.cosmo < 0.05,
                  signif.bonferoni.cosmo = p.cosmo < 0.05/n(),
                  signif.holm.cosmo = !0.05/(n() + 1 - 1:n()) < p.cosmo,
                  signif.sidak.cosmo = p.cosmo < 1 - (1 - 0.05)^(1/n()),
                  signif.holm.sidak.cosmo = !(1 - (1 - 0.05)^(1/n())) < p.cosmo)

write.csv(df.bs, 
          "./Results/bs.qtr.bin.results.csv",
          row.names = FALSE)

##who are the significant ones?
#rangers: sig for 0.75, 1 and 1, 1.25 (these sizes found more than expected)
table(df$order[df$qtr.bin == "(0.75,1]" & df$n.cont == "2"]) #chiroptera
table(df$order[df$qtr.bin == "(1,1.25]" & df$n.cont == "2"]) #chiroptera

#cosmo: sig for 5.25, 5.5 (these sizes found more than expected)
table(df$order[df$qtr.bin == "(5.25,5.5]" & df$n.cont == "3+"]) #have 2 carnivorans and an artio
df$binomial[df$qtr.bin == "(5.25,5.5]" & df$n.cont == "3+"]
#"Cervus elaphus" "Panthera leo"   "Ursus arctos" 

##### 1 compared to global -----
hb.mass <- df$log.mass[df$n.cont == "1" & !is.na(df$log.mass)] #3068 records
median(hb.mass) #1.97
median(df$avg.mass[df$n.cont == "1" & !is.na(df$avg.mass)]) #93.91
ks.test(hb.mass, global.mass)
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  hb.mass and global.mass
# D = 0.015662, p-value = 0.8292
# alternative hypothesis: two-sided
#NO DIFF
 
##### 2 compared to global -----
ld.mass <- df$log.mass[df$n.cont == "2" & !is.na(df$log.mass)] #243
median(ld.mass) #1.5
median(df$avg.mass[df$n.cont == "2" & !is.na(df$avg.mass)]) #31.6585
ks.test(ld.mass, global.mass)
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  ld.mass and global.mass
# D = 0.20185, p-value = 1.944e-08
# alternative hypothesis: two-sided
ks.test(ld.mass, global.mass, alternative = "greater") #on2 smaller than global
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  ld.mass and global.mass
# D^+ = 0.20185, p-value = 9.719e-09
# alternative hypothesis: the CDF of x lies above that of y
ks.test(ld.mass, global.mass, alternative = "less")
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  ld.mass and global.mass
# D^- = 0.035066, p-value = 0.573
# alternative hypothesis: the CDF of x lies below that of y

##### 3+ compared to global -----
gt.mass <- df$log.mass[df$n.cont == "3+" & !is.na(df$log.mass)] #6
median(gt.mass) #4.49
median(df$avg.mass[df$n.cont == "3+" & !is.na(df$avg.mass)]) #92753.02
ks.test(gt.mass, global.mass)
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  gt.mass and global.mass
# D = 0.49332, p-value = 0.1084
# alternative hypothesis: two-sided

##### DELVING DEEPER -----

min(df$avg.mass[df$order == "Carnivora"], na.rm = TRUE) #103.883
max(df$avg.mass[df$order == "Carnivora"], na.rm = TRUE) #720000

min(df$avg.mass[df$order == "Carnivora" &
                  df$n.cont == "3+"], na.rm = TRUE) #103.883
max(df$avg.mass[df$order == "Carnivora" &
                  df$n.cont == "3+"], na.rm = TRUE) #297349.5

### what about frugivores on 2 continents? (Pineda-Munoz et al. 2016, Paleobiology)
# frugivore size 1–30 kg ==> 1000g to 30000g ==> 10^3 to 10^4
# would expect to see few species in this size range
table(df$log.size.bin[df$n.cont == "2" & df$habitat.mode != "volant"])
#few in 1, 5, 6; a lot in 3, 
#it's hard to be on 2 continents if you're really small; could also be there aren't that many small
table(df$log.size.bin[!is.na(df$log.mass)])
#0      1       2       3       4       5       6       7 
#385    1315    720     436     252     160     46      2 
#there's actually a fair number of small species, so it add to the story that it is hard if you're small

#where is the gap? I think we need it at .25 log bin
bin.table <- df[df$n.cont == "2" & df$habitat.mode != "volant",] %>%
    dplyr::group_by(qtr.bin) %>%
    dplyr::summarise(n = n()) %>%
    as.data.frame()
View(qtr.bins)
#reduction at (2.5,2.75]
#these are:
unique(df$order[df$qtr.bin == "(2.5,2.75]"]) 
#these are lots of things 
table(df$order[df$qtr.bin == "(2.5,2.75]"]) #most are rodents, primates, and carnivorans
table(df$habitat.mode[df$qtr.bin == "(2.5,2.75]"]) 
#half are terrestrial, 1/3 are arboreal, 20% are fossorial

#what size range are our arboreal species?
table(df$qtr.bin[df$habitat.mode == "arboreal"])
#they are from 2.25 to 4 (aligns with Silvia, although maybe a bit smaller)

df[df$n.cont == "2" & df$habitat.mode == "arboreal",] %>%
    dplyr::group_by(qtr.bin) %>%
    dplyr::summarise(n = n()) %>%
    as.data.frame() #there aren't many.....but none are larger arboreal
#where are the large arboreal?
View(df[df$qtr.bin == "(4,4.25]" & df$habitat.mode == "arboreal",])
#mostly primates

#what does the qtr bin look like for all sp?
df[df$n.cont == "1" & df$habitat.mode != "volant",] %>%
    dplyr::group_by(qtr.bin) %>%
    dplyr::summarise(n = n()) %>%
    as.data.frame()

df[df$n.cont == "1" & df$habitat.mode == "arboreal",] %>%
    dplyr::group_by(qtr.bin) %>%
    dplyr::summarise(n = n()) %>%
    as.data.frame() #most are at bs bins 2.25 to 3, so this aligns

nrow(df[df$n.cont == "2" & df$diet.frugivore.tot == TRUE & df$habitat.mode != "volant",]) #44 out of 260 (~17%)
#who are they?
table(df$order[df$n.cont == "2" & df$diet.frugivore.tot == TRUE & df$habitat.mode != "volant"])
#mostly rodents
#how big are they?
table(df$log.size.bin[df$n.cont == "2" & df$diet.frugivore.tot == TRUE & df$habitat.mode != "volant"])
# 0  1   2  3   4 
# 1  20  9  10  4 
#how big are the rodents?
table(df$log.size.bin[df$n.cont == "2" & df$diet.frugivore.tot == TRUE & df$habitat.mode != "volant" & df$order == "Rodentia"])
#mostly 10g
#0  1   2  3  4 
#1  19  7  2  1 

##understand if nonvolant animals on 2 conntinents aren't small
#quarter size bins
nonvol.2.qtr.bins <- df[df$n.cont == "2" & df$habitat.mode != "volant",] %>%
    dplyr::group_by(qtr.bin) %>%
    dplyr::summarise(n = n()) %>%
    as.data.frame()

nonvol.qtr.bins <- df[df$n.cont == "1" & df$habitat.mode != "volant",] %>%
    dplyr::group_by(qtr.bin) %>%
    dplyr::summarise(n = n()) %>%
    as.data.frame()
#there are a lot of species that are in 0.75 to 3 size bins that aren't on 2

## how do globetrotters body size compare to their family or order?
#p. leo; kinda big for it's family
df %>%
    filter(family == "Felidae") %>%
ggplot() +
    geom_histogram(aes(log.mass),
                   binwidth = .25) +
    geom_vline(xintercept = df$log.mass[df$binomial == "Panthera leo"],
               col = "red") +
    plot_theme

#m. nivalis; kinda small for it's family
df %>%
    filter(family == "Mustelidae") %>%
    ggplot() +
    geom_histogram(aes(log.mass),
                   binwidth = .25) +
    geom_vline(xintercept = df$log.mass[df$binomial == "Mustela nivalis"],
               col = "red") +
    plot_theme

#ursus arctos; kinda big for it's family
df %>%
    filter(family == "Ursidae") %>%
    ggplot() +
    geom_histogram(aes(log.mass),
                   binwidth = .25) +
    geom_vline(xintercept = df$log.mass[df$binomial == "Ursus arctos"],
               col = "red") +
    plot_theme

#bat; avg. size for it's family
df %>%
    filter(family == "Vespertilionidae") %>%
    ggplot() +
    geom_histogram(aes(log.mass),
                   binwidth = .25) +
    geom_vline(xintercept = df$log.mass[df$binomial == "Miniopterus schreibersii"],
               col = "red") +
    plot_theme

#c. elephas; avg. size?
df %>%
    filter(family == "Cervidae") %>%
    ggplot() +
    geom_histogram(aes(log.mass),
                   binwidth = .25) +
    geom_vline(xintercept = df$log.mass[df$binomial == "Cervus elaphus"],
               col = "red") +
    plot_theme

#fox; avg. size
df %>%
    filter(family == "Canidae") %>%
    ggplot() +
    geom_histogram(aes(log.mass),
                   binwidth = .25) +
    geom_vline(xintercept = df$log.mass[df$binomial == "Vulpes vulpes"],
               col = "red") +
    plot_theme

#carnivora; two large, one small, one avg
df %>%
    filter(order == "Carnivora") %>%
    ggplot() +
    geom_histogram(aes(log.mass),
                   binwidth = .25) +
    geom_vline(xintercept = df$log.mass[df$binomial == "Vulpes vulpes"],
               col = "red") +
    geom_vline(xintercept = df$log.mass[df$binomial == "Panthera leo"],
               col = "red") +
    geom_vline(xintercept = df$log.mass[df$binomial == "Mustela nivalis"],
               col = "red") +
    geom_vline(xintercept = df$log.mass[df$binomial == "Ursus arctos"],
               col = "red") +
    plot_theme

#artiodactyla; about avg
df %>%
    filter(order == "Artiodactyla") %>%
    ggplot() +
    geom_histogram(aes(log.mass),
                   binwidth = .25) +
    geom_vline(xintercept = df$log.mass[df$binomial == "Cervus elaphus"],
               col = "red") +
    plot_theme
df$log.mass[df$binomial == "Cervus elaphus"] #5.255725

#chiroptera; about avg
df %>%
    filter(order == "Chiroptera") %>%
    ggplot() +
    geom_histogram(aes(log.mass),
                   binwidth = .25) +
    geom_vline(xintercept = df$log.mass[df$binomial == "Miniopterus schreibersii"],
               col = "red") +
    plot_theme

##### BODY SIZE FIGURE ----

df.na <- df %>%
    drop_na(log.mass) %>%
    as.data.frame()

p.bs.legend <- ggplot(df.na) + #do histogram; .25 log 
    geom_histogram(aes(log.mass,
                       group = n.cont, 
                       fill = n.cont),
                   binwidth = .25) +
    plot_theme +
    theme(legend.position = c(0.8, 0.6)) +
    theme(plot.background = element_rect(fill='transparent', color=NA)) +
    scale_fill_manual(values = cont_bw, 
                      name="Number of Continents",
                      labels = c("1",
                                 "2",
                                 "3+")) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)))

ggsave(p.bs.legend, 
       file = "./Figures/body_size_bw-red_legend.png", 
       width = 20, height = 10, units = "cm")

p.bs.no.legend <- ggplot(df.na) + #do histogram; .25 log 
    geom_histogram(aes(log.mass,
                       group = n.cont, 
                       fill = n.cont),
                   binwidth = .25) +
    plot_theme +
    scale_fill_manual(values = cont_bw, 
                      name="Number of Continents",
                      labels = c("1",
                                 "2",
                                 "3+")) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                       limits = c(log10(1), log10(10^6)),
                       labels = c(expression(10^0), expression(10^2), 
                                  expression(10^4), expression(10^6)))

ggsave(p.bs.no.legend, 
       file = "./Figures/body_size_no.legend_bw-red.png", 
       width = 20, height = 10, units = "cm")

p.bs.2.1 <- ggplot() + #do histogram; .25 log 
    geom_histogram(aes(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass)]), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    geom_histogram(aes(df$log.mass[df$n.cont == "3+" & !is.na(df$log.mass)]), 
                   colour = "red", fill = "red",
                   binwidth = .25) +
    plot_theme +
    scale_fill_manual(values = cont_bw, 
                      name="Number of Continents",
                      labels = c("1",
                                 "2",
                                 "3+")) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                       limits = c(log10(1), log10(10^6)),
                       labels = c(expression(10^0), expression(10^2), 
                                  expression(10^4), expression(10^6)))

ggsave(p.bs.2.1, 
       file = "./Figures/body_size_zoom_bw-red.png", 
       width = 14, height = 10, units = "cm")

p.bs.2 <- ggplot() + #do histogram; .25 log 
    geom_histogram(aes(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass)]), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                       limits = c(log10(1), log10(10^6)),
                       labels = c(expression(10^0), expression(10^2), 
                                  expression(10^4), expression(10^6)))
#sample size
length(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass)]) #243

ggsave(p.bs.2, 
       file = "./Figures/body_size_two.cont.png", 
       width = 14, height = 10, units = "cm")

p.bs.2.vol <- ggplot() + #do histogram; .25 log 
    geom_histogram(aes(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass) & df$habitat.mode == "volant"]), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                       limits = c(log10(1), log10(10^6)),
                       labels = c(expression(10^0), expression(10^2), 
                                  expression(10^4), expression(10^6)))
#sample size
length(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass) & df$habitat.mode == "volant"]) #129

ggsave(p.bs.2.vol, 
       file = "./Figures/body_size.vol_two.cont.png", 
       width = 14, height = 10, units = "cm")

p.bs.2.nonvol <- ggplot() + #do histogram; .25 log 
    geom_histogram(aes(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass) & df$habitat.mode != "volant"]), 
                   colour = "gray47", fill = "gray47",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                       limits = c(log10(1), log10(10^6)),
                       labels = c(expression(10^0), expression(10^2), 
                                  expression(10^4), expression(10^6)))
#sample size
length(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass) & df$habitat.mode != "volant"]) #114

ggsave(p.bs.2.nonvol, 
       file = "./Figures/body_size.nonvol_two.cont.png", 
       width = 14, height = 10, units = "cm")

p.bs.1 <- ggplot() + #do histogram; .25 log 
    geom_histogram(aes(df$log.mass[df$n.cont == "1" & !is.na(df$log.mass)]), 
                   colour = "black", fill = "black",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                       limits = c(log10(1), log10(10^6)),
                       labels = c(expression(10^0), expression(10^2), 
                                  expression(10^4), expression(10^6)))
#sample size
length(df$log.mass[df$n.cont == "1" & !is.na(df$log.mass)]) #3067

ggsave(p.bs.1, 
       file = "./Figures/body_size_one.cont.png", 
       width = 14, height = 10, units = "cm")

p.bs.3 <- ggplot() + #do histogram; .25 log 
    geom_histogram(aes(df$log.mass[df$n.cont == "3+" & !is.na(df$log.mass)]), 
                   colour = "red", fill = "red",
                   binwidth = .25) +
    plot_theme +
    scale_y_continuous(name = "Count",
                       breaks = c(0, 1, 2)) +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)),
                       limits = c(log10(1), log10(10^6)),
                       labels = c(expression(10^0), expression(10^2), 
                                  expression(10^4), expression(10^6)))
#sample size
length(df$log.mass[df$n.cont == "3+" & !is.na(df$log.mass)]) #3067
ggsave(p.bs.3, 
       file = "./Figures/body_size_three.cont.png", 
       width = 14, height = 10, units = "cm")

##### 1 v 2+ -----

length(df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass)]) #3068
length(df$avg.mass[df$n.cont == 2 | df$n.cont == "3+" & !is.na(df$avg.mass)]) #266
median(df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass)]) #93.91
median(df$avg.mass[df$n.cont != 1 & !is.na(df$avg.mass)]) #33.3
ks.test(df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass) ], 
        df$avg.mass[df$n.cont != 1 & !is.na(df$avg.mass)]) #p-value = 3.922e-09
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass)] and df$avg.mass[df$n.cont != 1 & !is.na(df$avg.mass)]
# D = 0.20863, p-value = 3.922e-09
# alternative hypothesis: two-sided

ks.test(df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass)], 
        df$avg.mass[df$n.cont != 1 & !is.na(df$avg.mass)],
        alternative = "greater") #p-value = 0.2475
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass)] and df$avg.mass[df$n.cont != 1 & !is.na(df$avg.mass)]
# D^+ = 0.048633, p-value = 0.3364
# alternative hypothesis: the CDF of x lies above that of y

ks.test(df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass)], 
        df$avg.mass[df$n.cont != 1 & !is.na(df$avg.mass)],
        alternative = "less") #p-value = 1.961e-09 (i.e., 1 cont is not "less" than 2+ cont)
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass)] and df$avg.mass[df$n.cont != 1 & !is.na(df$avg.mass)]
# D^- = 0.20863, p-value = 1.961e-09
# alternative hypothesis: the CDF of x lies below that of y

ks.test(df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass)], 
        df$avg.mass[df$n.cont == 2 & !is.na(df$avg.mass)],
        alternative = "less") #p-value = 1.961e-09 (i.e., 1 cont is not "less" than 2+ cont)
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  df$avg.mass[df$n.cont == 1 & !is.na(df$avg.mass)] and df$avg.mass[df$n.cont != 1 & !is.na(df$avg.mass)]
# D^- = 0.20863, p-value = 1.961e-09
# alternative hypothesis: the CDF of x lies below that of y

##### 1+2 v 3+ -----
length(df$avg.mass[df$n.cont != "3+" & !is.na(df$avg.mass)]) #3311
length(df$avg.mass[df$n.cont == "3+" & !is.na(df$avg.mass)]) #6
median(df$avg.mass[df$n.cont != "3+" & !is.na(df$avg.mass)]) #90.00
median(df$avg.mass[df$n.cont == "3+" & !is.na(df$avg.mass)]) #92753.02
ks.test(df$avg.mass[df$n.cont != "3+" & !is.na(df$avg.mass)], 
        df$avg.mass[df$n.cont == "3+" & !is.na(df$avg.mass)]) 
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  df$avg.mass[df$n.cont != "3+" & !is.na(df$avg.mass)] and df$avg.mass[df$n.cont == "3+" & !is.na(df$avg.mass)]
# D = 0.49421, p-value = 0.1072
# alternative hypothesis: two-sided

ks.test(df$avg.mass[df$n.cont != "3+" & !is.na(df$avg.mass)], 
        df$avg.mass[df$n.cont == "3+" & !is.na(df$avg.mass)],
        alternative = "greater") #p-value = 0.05277 (i.e., 1 or 2 cont is not "greater" than 3+)
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  df$avg.mass[df$n.cont != "3+" & !is.na(df$avg.mass)] and df$avg.mass[df$n.cont == "3+" & !is.na(df$avg.mass)]
# D^+ = 0.49421, p-value = 0.05363
# alternative hypothesis: the CDF of x lies above that of y

#### H5: DIET ----
##### DIET BREADTH -----
#are there diet breadth differences between homiess, limited dispersers, and globe trotters?

null.breadth <- df %>%
  group_by(diet.breadth) %>%
  drop_na(diet.breadth) %>%
  dplyr::summarise(null.N = n()) %>%
  dplyr::select(diet.breadth,
                null.N) %>%
  as.data.frame()

breadth <- df %>%
  group_by(n.cont, diet.breadth) %>%
  drop_na(diet.breadth) %>%
  dplyr::summarise(N = n()) %>% 
  as.data.frame()

homies.breadth <- breadth[breadth$n.cont == 1,]
colnames(homies.breadth)[colnames(homies.breadth) == "N"] <- "homies.N"
homies.breadth <- homies.breadth %>%
  dplyr::select(-n.cont)

rangers.breadth <- breadth[breadth$n.cont == 2,]
colnames(rangers.breadth)[colnames(rangers.breadth) == "N"] <- "rangers.N"
rangers.breadth <- rangers.breadth %>%
  dplyr::select(-n.cont)

cosmo.breadth <- breadth[breadth$n.cont == "3+",]
colnames(cosmo.breadth)[colnames(cosmo.breadth) == "N"] <- "cosmo.N"
cosmo.breadth <- cosmo.breadth %>%
  dplyr::select(-n.cont)

#create full dataset
breadth.null.cosmo <- merge(null.breadth, cosmo.breadth, by = "diet.breadth", all.x = TRUE, all.y = TRUE)
breadth.null.cosmo.rangers <- merge(breadth.null.cosmo, rangers.breadth, by = "diet.breadth", all.x = TRUE, all.y = TRUE)
breadth.null.trol.rangers.homies <- merge(breadth.null.cosmo.rangers, homies.breadth, by = "diet.breadth", all.x = TRUE, all.y = TRUE)

df.breadth <- breadth.null.trol.rangers.homies
df.breadth[is.na(df.breadth)] <- 0

df.breadth$prop.null <- df.breadth$null.N/nrow(df)

df.breadth$prop.homies <- df.breadth$homies.N/nrow(df[df$n.cont == 1,])
df.breadth$prop.rangers <- df.breadth$rangers.N/nrow(df[df$n.cont == 2,])
df.breadth$prop.cosmo <- df.breadth$cosmo.N/nrow(df[df$n.cont == "3+",])

#binomial test

for(i in 1:nrow(df.breadth)){
  test <- binom.test(df.breadth$homies.N[i], nrow(df[df$n.cont == 1,]), p = df.breadth$prop.null[i], alternative = "two.sided")
  df.breadth$p.homies[i] <- test$p.value
}

for(i in 1:nrow(df.breadth)){
  test <- binom.test(df.breadth$rangers.N[i], nrow(df[df$n.cont == 2,]), p = df.breadth$prop.null[i], alternative = "two.sided")
  df.breadth$p.rangers[i] <- test$p.value
}

for(i in 1:nrow(df.breadth)){
  test <- binom.test(df.breadth$cosmo.N[i], nrow(df[df$n.cont == "3+",]), p = df.breadth$prop.null[i], alternative = "two.sided")
  df.breadth$p.cosmo[i] <- test$p.value
}

#add sidak correction
df.breadth <- arrange(df.breadth, p.homies) %>%
  dplyr::mutate(signif.homies = p.homies < 0.05,
                signif.bonferoni.homies = p.homies < 0.05/n(),
                signif.holm.homies = !0.05/(n() + 1 - 1:n()) < p.homies,
                signif.sidak.homies = p.homies < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.homies = !(1 - (1 - 0.05)^(1/n())) < p.homies)

df.breadth <- arrange(df.breadth, p.rangers) %>%
  dplyr::mutate(signif.rangers = p.rangers < 0.05,
                signif.bonferoni.rangers = p.rangers < 0.05/n(),
                signif.holm.rangers = !0.05/(n() + 1 - 1:n()) < p.rangers,
                signif.sidak.rangers = p.rangers < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.rangers = !(1 - (1 - 0.05)^(1/n())) < p.rangers)

df.breadth <- arrange(df.breadth, p.cosmo) %>%
  dplyr::mutate(signif.cosmo = p.cosmo < 0.05,
                signif.bonferoni.cosmo = p.cosmo < 0.05/n(),
                signif.holm.cosmo = !0.05/(n() + 1 - 1:n()) < p.cosmo,
                signif.sidak.cosmo = p.cosmo < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.cosmo = !(1 - (1 - 0.05)^(1/n())) < p.cosmo)

write.csv(df.breadth, 
          "./Results/diet.breadth.results.csv",
         row.names = FALSE)

#include trot.N, null prop, expected, observed-expected, (O-E)^2, (O-E)^2/exp, X crit, p

##### LOOK AT COMBOS -----
nrow(df[df$diet.breadth == 1 & df$diet.browser.tot == TRUE,]) #423
nrow(df[df$diet.breadth == 1 & df$diet.grazer.tot == TRUE,]) #233
nrow(df[df$diet.breadth == 1 & df$diet.frugivore.tot == TRUE,]) #487
nrow(df[df$diet.breadth == 1 & df$diet.carnivore.tot == TRUE,]) #130
nrow(df[df$diet.breadth == 1 & df$diet.invertivore.tot == TRUE,]) #1182
nrow(df[df$diet.breadth == 1 & df$diet.piscivore.tot == TRUE,]) #7

df %>%
    summarise(n.two = nrow(df[df$diet.breadth == 2,]),
              carn.pisc = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.piscivore.tot == TRUE & df$diet.breadth == 2]),
              carn.invt = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$diet.breadth == 2]),
              pisc.invt = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$diet.breadth == 2]),
              carn.brow = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.browser.tot == TRUE & df$diet.breadth == 2]),
              carn.graz = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$diet.breadth == 2]),
              carn.frug = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$diet.breadth == 2]),
              pisc.brow = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.browser.tot == TRUE & df$diet.breadth == 2]),
              pisc.graz = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$diet.breadth == 2]),
              pisc.frug = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$diet.breadth == 2]),
              invt.brow = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.browser.tot == TRUE & df$diet.breadth == 2]),
              invt.graz = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$diet.breadth == 2]),
              invt.frug = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$diet.breadth == 2]),
              brow.graz = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.grazer.tot == TRUE & df$diet.breadth == 2]),
              brow.frug = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.frugivore.tot == TRUE & df$diet.breadth == 2]),
              graz.frug = length(df$binomial[df$diet.grazer.tot == TRUE & df$diet.frugivore.tot == TRUE & df$diet.breadth == 2])) %>%
    as.data.frame()

df %>%
  summarise(n.two = nrow(df[df$n.cont == "2" & df$diet.breadth == 2,]),
            carn.pisc = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.piscivore.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            carn.invt = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            pisc.invt = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            carn.brow = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            carn.graz = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            carn.frug = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            pisc.brow = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            pisc.graz = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            pisc.frug = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            invt.brow = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            invt.graz = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            invt.frug = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            brow.graz = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            brow.frug = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2]),
            graz.frug = length(df$binomial[df$diet.grazer.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2" & df$diet.breadth == 2])) %>%
  as.data.frame()

df %>%
    summarise(n.one = nrow(df[df$n.cont == "1" & df$diet.breadth == 2,]),
              carn.pisc = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.piscivore.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              carn.invt = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              pisc.invt = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              carn.brow = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              carn.graz = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              carn.frug = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              pisc.brow = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              pisc.graz = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              pisc.frug = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              invt.brow = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              invt.graz = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              invt.frug = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              brow.graz = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              brow.frug = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2]),
              graz.frug = length(df$binomial[df$diet.grazer.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "1" & df$diet.breadth == 2])) %>%
    as.data.frame()

df %>%
    summarise(n.three = nrow(df[df$n.cont == "3+" & df$diet.breadth == 2,]),
              carn.pisc = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.piscivore.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              carn.invt = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              pisc.invt = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              carn.brow = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              carn.graz = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              carn.frug = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              pisc.brow = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              pisc.graz = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              pisc.frug = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              invt.brow = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              invt.graz = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              invt.frug = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              brow.graz = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              brow.frug = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2]),
              graz.frug = length(df$binomial[df$diet.grazer.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "3+" & df$diet.breadth == 2])) %>%
    as.data.frame()

##deeper look into dietary breadth of 3
df.3 <- df[df$diet.breadth ==3,]

table(df.3$n.cont[df.3$diet.browser.tot == TRUE &
                     df.3$diet.invertivore.tot == TRUE &
                     df.3$diet.frugivore.tot == TRUE]) #152

table(df.3$n.cont[df.3$diet.browser.tot == TRUE &
                       df.3$diet.carnivore.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #2

table(df.3$n.cont[df.3$diet.invertivore.tot == TRUE &
                       df.3$diet.carnivore.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #18

table(df.3$n.cont[df.3$diet.browser.tot == TRUE &
                       df.3$diet.grazer.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #12

table(df.3$n.cont[df.3$diet.browser.tot == TRUE &
                      df.3$diet.carnivore.tot == TRUE &
                      df.3$diet.invertivore.tot == TRUE]) #1

table(df.3$n.cont[df.3$diet.piscivore.tot == TRUE &
                      df.3$diet.carnivore.tot == TRUE &
                      df.3$diet.invertivore.tot == TRUE]) #1

table(df.3$n.cont) #180 on 1 continent, 6 on 2 continents

##deeper look into those with dietary breadth of 2
#only for diet.breadth == 2
## total
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2]) #308
## a lot are browser & grazer combo
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.carnivore.tot == TRUE & 
                   df$diet.breadth == 2]) #0
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2]) #702
##a lot are browser & frugivore comnbo
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2]) #20
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2]) #0
length(df$binomial[df$diet.grazer.tot == TRUE & 
                   df$diet.carnivore.tot == TRUE & 
                   df$diet.breadth == 2]) #1
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2]) #2
length(df$binomial[df$diet.frugivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2]) #58
length(df$binomial[df$diet.piscivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2]) #0
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2]) #127
##a lot are carnivore & invertivore combo
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2]) #46
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2]) #4
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2]) #452
#a lot are invertivore and frugivore combo
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2]) #17
length(df$binomial[df$diet.piscivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2]) #0

##n.cont = 1
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #297
#a lot are browser & grazer combo
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #682
#a lot are browser & frugivore combo
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #20
length(df$binomial[df$diet.grazer.tot == TRUE & 
                   df$diet.carnivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #0
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #2
length(df$binomial[df$diet.frugivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #54
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #113
#a lot are carnivore and invertivore combo
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #44
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #4
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #424
#a lot are invertivore and frugivore combo
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1]) #13

##n.cont = 2
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2]) #11
#not as many browsers and grazers; not a lot of either are on 2 continents
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2]) #20
#do still have this
length(df$binomial[df$diet.grazer.tot == TRUE & 
                   df$diet.carnivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2]) #1
length(df$binomial[df$diet.frugivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2]) #4
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2]) #12
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2]) #1
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2]) #28
#and this one
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2]) #4

##n.cont = 3+
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == "3+"]) #2
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == "3+"]) #1

#role of bats in driving these patterns
length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2]) #142
length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2 &
                   df$diet.frugivore.tot == TRUE]) #52 of them are frugivores
length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2 &
                   df$diet.invertivore.tot == TRUE]) #106 of them are insectivores
length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2 &
                   df$diet.carnivore.tot == TRUE]) #9 are carnivores
length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2 &
                   df$diet.piscivore.tot== TRUE]) #2! are piscivores

table(df$n.cont[df$diet.breadth == 3]) #186 total; 180 on 1 cont
#152 total species have a diet breadth of 3 and eat i, b, f; 152/186 = 81.7%
#1188 eat b+g, b+f, c+i, c+p, or i+p with a diet breadth of 2, out of 1737 = 68.4

##### DIET TYPE -----
#are there diet types differences between homiess, limited dispersers, and globe trotters?
diets <- names(dplyr::select(df, starts_with("diet"), -diet.breadth))
df.diet <- data.frame(diet = diets)

df.diet["null.N"] <- colSums(df[diets])
df.diet["homies.N"] <- colSums(df[df$n.cont == 1, diets])
df.diet["rangers.N"] <- colSums(df[df$n.cont == 2, diets])
df.diet["cosmo.N"] <- colSums(df[df$n.cont == "3+", diets])

df.diet$prop.null <- df.diet$null.N/nrow(df)

df.diet$prop.homies <- df.diet$homies.N/nrow(df[df$n.cont == 1,])
df.diet$prop.rangers <- df.diet$rangers.N/nrow(df[df$n.cont == 2,])
df.diet$prop.cosmo <- df.diet$cosmo.N/nrow(df[df$n.cont == "3+",])

#binomial test

for(i in 1:nrow(df.diet)){
  test <- binom.test(df.diet$homies.N[i], nrow(df[df$n.cont == 1,]), p = df.diet$prop.null[i], alternative = "two.sided")
  df.diet$p.homies[i] <- test$p.value
}

for(i in 1:nrow(df.diet)){
  test <- binom.test(df.diet$rangers.N[i], nrow(df[df$n.cont == 2,]), p = df.diet$prop.null[i], alternative = "two.sided")
  df.diet$p.rangers[i] <- test$p.value
}

for(i in 1:nrow(df.diet)){
  test <- binom.test(df.diet$cosmo.N[i], nrow(df[df$n.cont == "3+",]), p = df.diet$prop.null[i], alternative = "two.sided")
  df.diet$p.cosmo[i] <- test$p.value
}

#add sidak correction
df.diet <- arrange(df.diet, p.homies) %>%
  dplyr::mutate(signif.homies = p.homies < 0.05,
                signif.bonferoni.homies = p.homies < 0.05/n(),
                signif.holm.homies = !0.05/(n() + 1 - 1:n()) < p.homies,
                signif.sidak.homies = p.homies < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.homies = !(1 - (1 - 0.05)^(1/n())) < p.homies)

df.diet <- arrange(df.diet, p.rangers) %>%
  dplyr::mutate(signif.rangers = p.rangers < 0.05,
                signif.bonferoni.rangers = p.rangers < 0.05/n(),
                signif.holm.rangers = !0.05/(n() + 1 - 1:n()) < p.rangers,
                signif.sidak.rangers = p.rangers < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.rangers = !(1 - (1 - 0.05)^(1/n())) < p.rangers)
#nothing special about frugivores

df.diet <- arrange(df.diet, p.cosmo) %>%
  dplyr::mutate(signif.cosmo = p.cosmo < 0.05,
                signif.bonferoni.cosmo = p.cosmo < 0.05/n(),
                signif.holm.cosmo = !0.05/(n() + 1 - 1:n()) < p.cosmo,
                signif.sidak.cosmo = p.cosmo < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.cosmo = !(1 - (1 - 0.05)^(1/n())) < p.cosmo)

write.csv(df.diet, 
          "./Results/diet.results.csv",
          row.names = FALSE)

##mostly losing browsers, grazers, and piscivores when go from 1 to 2
##gaining carnivores
#slight decrease in frugivores and slight increase in invertivores


#test if carnivorans of n=2 and n=3+ are larger than expected
ks.test(log10(df$avg.mass[df$n.cont == 2 & df$diet.carnivore.tot == TRUE]), log10(df$avg.mass[df$n.cont == 2]))
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df$avg.mass[df$n.cont == 2 & df$diet.carnivore.tot == TRUE]) and log10(df$avg.mass[df$n.cont == 2])
# D = 0.47582, p-value = 2.622e-08
# alternative hypothesis: two-sided

ks.test(log10(df$avg.mass[df$n.cont == 2 & df$diet.carnivore.tot == TRUE]), log10(df$avg.mass[df$n.cont == 2]), alternative = "greater")
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df$avg.mass[df$n.cont == 2 & df$diet.carnivore.tot == TRUE]) and log10(df$avg.mass[df$n.cont == 2])
# D^+ = 0.024691, p-value = 0.9523
# alternative hypothesis: the CDF of x lies above that of y

ks.test(log10(df$avg.mass[df$n.cont == 2 & df$diet.carnivore.tot == TRUE]), log10(df$avg.mass[df$n.cont == 2]), alternative = "less") #sig; y less than x
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df$avg.mass[df$n.cont == 2 & df$diet.carnivore.tot == TRUE]) and log10(df$avg.mass[df$n.cont == 2])
# D^- = 0.47582, p-value = 1.311e-08
# alternative hypothesis: the CDF of x lies below that of y
#yes, larger

ks.test(log10(df$avg.mass[df$n.cont == "3+" & df$diet.carnivore.tot == TRUE]), log10(df$avg.mass[df$n.cont == "3+"])) #not sig
# Exact two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df$avg.mass[df$n.cont == "3+" & df$diet.carnivore.tot == TRUE]) and log10(df$avg.mass[df$n.cont == "3+"])
# D = 0.2, p-value = 1
# alternative hypothesis: two-sided

###### DELVING DEEPER ------

## do bats drive trends in n.cont == 2 for diet; look at size range to understand where bats are
df.2 <- df[df$n.cont == 2,]
range(df.2$avg.mass[df.2$order == "Chiroptera"], na.rm = TRUE)
#most in between 10^0 and 10^2
range(df$log.size.bin[df$order == "Chiroptera"], na.rm = TRUE)
#lost 10^3 when just 2 continents
#want to know what we're losing in mid ranges and what their diets are
bins.diff <- df[df$log.size.bin >= 2 & df$log.size.bin <=4, ] %>%
    dplyr::group_by(log.size.bin, n.cont) %>%
    dplyr::summarise(n.pisc = sum(diet.piscivore.tot == TRUE),
                     n.carn = sum(diet.carnivore.tot == TRUE),
                     n.invert = sum(diet.invertivore.tot == TRUE),
                     n.brows = sum(diet.browser.tot = TRUE),
                     n.graz = sum(diet.grazer.tot == TRUE),
                     n.frug = sum(diet.frugivore.tot == TRUE)) %>%
    as.data.frame()
View(bins.diff)
#bin 2: lose graz and invert as go from 1 to 2 cont
#bin 3: about same, but decrease frug and carn as go from 1 to 2 cont
#bin 4: also about the same
#dramatic decrease in everything except piscivores
#log bin 2: most of 1 continent are frugivores; most are still frugivores with a dramatic decrease in invert and graz
#log bin 3: most of 1 continent are frugivores or carnivores; most are carn (but significant less) and a dramatic decrease in frug
#log bin 4: most of 1 continent are grazers; most are now carnivores and dramatic decrease in grazers
#take away: find that the composition is significantly different; bin 2 switches from dominant in 3 types on 1 cont to just frug in 2 cont
# bin 3 switches from dominant in 2 types on 1 continent to just carn on 2 continents (i.e., lost carn in bin 2 and gained in bin 3; lost frug in bin 3 and kept in bin 2)
# bin 4 switches from grazers to carnivores

## where is the turnover happening?
ord.diff <- df[df$log.size.bin >= 2 & df$log.size.bin <= 4,] %>%
    dplyr::group_by(log.size.bin, n.cont, order) %>%
    dplyr::summarise(n = n()) %>%
    as.data.frame()
View(ord.diff)
View(ord.diff[ord.diff$n.cont == 1 & ord.diff$log.size.bin == 2,])
View(ord.diff[ord.diff$n.cont == 2 & ord.diff$log.size.bin == 2,])
View(ord.diff[ord.diff$n.cont == 1 & ord.diff$log.size.bin == 3,])
View(ord.diff[ord.diff$n.cont == 2 & ord.diff$log.size.bin == 3,])
View(ord.diff[ord.diff$n.cont == 1 & ord.diff$log.size.bin == 4,])
View(ord.diff[ord.diff$n.cont == 2 & ord.diff$log.size.bin == 4,])
#lots of artodactyla, carnivorans, primates, and rodents

table(df.2$order[df.2$diet.frugivore.tot == TRUE]) 
#96 total; 52 are Chiroptera, 30 are Rodents

table(df.2$order[df.2$diet.invertivore.tot == TRUE])
#141 total; 106 are Chiroptera

df.2[df.2$order == "Chiroptera",] %>%
    dplyr::summarise(n.pisc = sum(diet.piscivore.tot == TRUE),
                     n.carn = sum(diet.carnivore.tot == TRUE),
                     n.invert = sum(diet.invertivore.tot == TRUE),
                     n.brows = sum(diet.browser.tot = TRUE),
                     n.graz = sum(diet.grazer.tot == TRUE),
                     n.frug = sum(diet.frugivore.tot == TRUE))
#most are invert (106), then frug (52)
df.2 %>%
    dplyr::summarise(n.pisc = sum(diet.piscivore.tot == TRUE),
                     n.carn = sum(diet.carnivore.tot == TRUE),
                     n.invert = sum(diet.invertivore.tot == TRUE),
                     n.brows = sum(diet.browser.tot = TRUE),
                     n.graz = sum(diet.grazer.tot == TRUE),
                     n.frug = sum(diet.frugivore.tot == TRUE))
#compared to overall dist of diets; 106/141 of invert are bats; 52/96 of frug are bats

range(df$avg.mass[df$diet.frugivore.tot == TRUE], na.rm = TRUE)
#3.92 300000  #missing 10^2 to 10^4; here the range is 10^1 to 10^5
hist(df$log.mass[df$diet.frugivore.tot == TRUE])
range(df$avg.mass[df$diet.invertivore.tot == TRUE], na.rm = TRUE)
#same range here

## diet breadth 1, homebodies
nrow(df[df$diet.breadth == 1 & df$diet.browser.tot == TRUE & df$n.cont == 1,])
nrow(df[df$diet.breadth == 1 & df$diet.grazer.tot == TRUE & df$n.cont == 1,])
nrow(df[df$diet.breadth == 1 & df$diet.frugivore.tot == TRUE & df$n.cont == 1,])
nrow(df[df$diet.breadth == 1 & df$diet.carnivore.tot == TRUE & df$n.cont == 1,])
nrow(df[df$diet.breadth == 1 & df$diet.invertivore.tot == TRUE & df$n.cont == 1,])
nrow(df[df$diet.breadth == 1 & df$diet.piscivore.tot == TRUE & df$n.cont == 1,])

## diet breadth 1, wanderers
nrow(df[df$diet.breadth == 1 & df$diet.browser.tot == TRUE & df$n.cont == 2,])
nrow(df[df$diet.breadth == 1 & df$diet.grazer.tot == TRUE & df$n.cont == 2,])
nrow(df[df$diet.breadth == 1 & df$diet.frugivore.tot == TRUE & df$n.cont == 2,])
nrow(df[df$diet.breadth == 1 & df$diet.carnivore.tot == TRUE & df$n.cont == 2,])
nrow(df[df$diet.breadth == 1 & df$diet.invertivore.tot == TRUE & df$n.cont == 2,])
nrow(df[df$diet.breadth == 1 & df$diet.piscivore.tot == TRUE & df$n.cont == 2,])

## diet breadth 1, globetrotters
nrow(df[df$diet.breadth == 1 & df$diet.browser.tot == TRUE & df$n.cont == "3+",])
nrow(df[df$diet.breadth == 1 & df$diet.grazer.tot == TRUE & df$n.cont == "3+",])
nrow(df[df$diet.breadth == 1 & df$diet.frugivore.tot == TRUE & df$n.cont == "3+",])
nrow(df[df$diet.breadth == 1 & df$diet.carnivore.tot == TRUE & df$n.cont == "3+",])
nrow(df[df$diet.breadth == 1 & df$diet.invertivore.tot == TRUE & df$n.cont == "3+",])
nrow(df[df$diet.breadth == 1 & df$diet.piscivore.tot == TRUE & df$n.cont == "3+",])

## diet breadth 3
table(df$n.cont[df$diet.breadth == 3 & df$diet.browser.tot == TRUE & df$diet.grazer.tot == TRUE & df$diet.frugivore.tot == TRUE])
table(df$n.cont[df$diet.breadth == 3 & df$diet.browser.tot == TRUE & df$diet.carnivore.tot == TRUE & df$diet.frugivore.tot == TRUE])
table(df$n.cont[df$diet.breadth == 3 & df$diet.invertivore.tot == TRUE & df$diet.browser.tot == TRUE & df$diet.frugivore.tot == TRUE])
table(df$n.cont[df$diet.breadth == 3 & df$diet.carnivore.tot == TRUE & df$diet.browser.tot == TRUE & df$diet.invertivore.tot == TRUE])
table(df$n.cont[df$diet.breadth == 3 & df$diet.carnivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$diet.frugivore.tot == TRUE])
table(df$n.cont[df$diet.breadth == 3 & df$diet.carnivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$diet.piscivore.tot == TRUE])

##### DIET BREADTH BAR GRAPH -----
dietbreadth_bargraph <- plyr::ddply(df, c("n.cont", "diet.breadth"), function(x){
  nrow(x)
})

dietbreadth_bargraph_full <- tidyr::complete(dietbreadth_bargraph, n.cont, diet.breadth)

dietbreadth_bargraph_full$tots <- NA
dietbreadth_bargraph_full$tots[dietbreadth_bargraph_full$n.cont == "1"] <- sum(dietbreadth_bargraph_full$V1[dietbreadth_bargraph_full$n.cont == "1"], na.rm = TRUE) #4148
dietbreadth_bargraph_full$tots[dietbreadth_bargraph_full$n.cont == "2"] <- sum(dietbreadth_bargraph_full$V1[dietbreadth_bargraph_full$n.cont == "2"], na.rm = TRUE) #272
dietbreadth_bargraph_full$tots[dietbreadth_bargraph_full$n.cont == "3+"] <-sum(dietbreadth_bargraph_full$V1[dietbreadth_bargraph_full$n.cont == "3+"], na.rm = TRUE) #6 #phew, they match!

dietbreadth_bargraph_full$prop <- dietbreadth_bargraph_full$V1 / dietbreadth_bargraph_full$tots

dietbreadth_bargraph_full$n.cont <- as.factor(dietbreadth_bargraph_full$n.cont)
dietbreadth_bargraph_full$n.cont <- factor(dietbreadth_bargraph_full$n.cont,                                    # Change ordering manually
                                           levels = c("1", "2", "3+"))


dietbreadth_bargraph.vol <- plyr::ddply(df[df$habitat.mode == "volant",], c("n.cont", "diet.breadth"), function(x){
    nrow(x)
})

dietbreadth_bargraph_full.vol <- tidyr::complete(dietbreadth_bargraph.vol, n.cont, diet.breadth)

dietbreadth_bargraph_full.vol$tots <- NA
dietbreadth_bargraph_full.vol$tots[dietbreadth_bargraph_full.vol$n.cont == "1"] <- sum(dietbreadth_bargraph_full.vol$V1[dietbreadth_bargraph_full.vol$n.cont == "1"], na.rm = TRUE) #4148
dietbreadth_bargraph_full.vol$tots[dietbreadth_bargraph_full.vol$n.cont == "2"] <- sum(dietbreadth_bargraph_full.vol$V1[dietbreadth_bargraph_full.vol$n.cont == "2"], na.rm = TRUE) #272
dietbreadth_bargraph_full.vol$tots[dietbreadth_bargraph_full.vol$n.cont == "3+"] <-sum(dietbreadth_bargraph_full.vol$V1[dietbreadth_bargraph_full.vol$n.cont == "3+"], na.rm = TRUE) #6 #phew, they match!

dietbreadth_bargraph_full.vol$prop <- dietbreadth_bargraph_full.vol$V1 / dietbreadth_bargraph_full.vol$tots

dietbreadth_bargraph_full.vol$n.cont <- as.factor(dietbreadth_bargraph_full.vol$n.cont)
dietbreadth_bargraph_full.vol$n.cont <- factor(dietbreadth_bargraph_full.vol$n.cont,                                    # Change ordering manually
                                           levels = c("1", "2", "3+"))



dietbreadth_bargraph.nonvol <- plyr::ddply(df[df$habitat.mode != "volant",], c("n.cont", "diet.breadth"), function(x){
    nrow(x)
})

dietbreadth_bargraph_full.nonvol <- tidyr::complete(dietbreadth_bargraph.nonvol, n.cont, diet.breadth)

dietbreadth_bargraph_full.nonvol$tots <- NA
dietbreadth_bargraph_full.nonvol$tots[dietbreadth_bargraph_full.nonvol$n.cont == "1"] <- sum(dietbreadth_bargraph_full.nonvol$V1[dietbreadth_bargraph_full.nonvol$n.cont == "1"], na.rm = TRUE) #4148
dietbreadth_bargraph_full.nonvol$tots[dietbreadth_bargraph_full.nonvol$n.cont == "2"] <- sum(dietbreadth_bargraph_full.nonvol$V1[dietbreadth_bargraph_full.nonvol$n.cont == "2"], na.rm = TRUE) #272
dietbreadth_bargraph_full.nonvol$tots[dietbreadth_bargraph_full.nonvol$n.cont == "3+"] <-sum(dietbreadth_bargraph_full.nonvol$V1[dietbreadth_bargraph_full.nonvol$n.cont == "3+"], na.rm = TRUE) #6 #phew, they match!

dietbreadth_bargraph_full.nonvol$prop <- dietbreadth_bargraph_full.nonvol$V1 / dietbreadth_bargraph_full.nonvol$tots

dietbreadth_bargraph_full.nonvol$n.cont <- as.factor(dietbreadth_bargraph_full.nonvol$n.cont)
dietbreadth_bargraph_full.nonvol$n.cont <- factor(dietbreadth_bargraph_full.nonvol$n.cont,                                    # Change ordering manually
                                           levels = c("1", "2", "3+"))

#show as proportions
ggplot(dietbreadth_bargraph_full, aes(x = diet.breadth, 
                                      y = prop, 
                                      fill = n.cont)) + 
  scale_fill_manual(values = cont_bw,
                    name = "Number of Continents",
                    labels = c("1",
                               "2",
                               "3+")) +
  geom_bar(stat = "identity") +
  xlab("Dietary Breadth") + 
  ylab("Proportion") + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) + 
  plot_theme + theme(panel.border = element_rect(fill = NA),
                    strip.background = element_rect(fill = NA),
                    legend.position = c(0.82, 0.8)) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))

p.diet.brd.1 <-  ggplot() +  
    geom_bar(aes(df$diet.breadth[df$n.cont == "1"]),
             colour = "black", fill = "black") +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -1),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Dietary breadth")
##sample size
length(df$diet.breadth[df$n.cont == "1"]) #4119

ggsave(p.diet.brd.1, 
       file = "./Figures/diet.breadth.one.png", 
       width = 20, height = 10, units = "cm")

p.diet.brd.2 <-  ggplot() +  
    geom_bar(aes(df$diet.breadth[df$n.cont == "2"]),
             colour = "gray47", fill = "gray47") +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -1),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Dietary breadth")
##sample size
length(df$diet.breadth[df$n.cont == "2"]) #260

ggsave(p.diet.brd.2, 
       file = "./Figures/diet.breadth.two.png", 
       width = 20, height = 10, units = "cm")

p.diet.brd.2.nonvol <- ggplot() +  
    geom_bar(aes(df$diet.breadth[df$n.cont == "2" & df$habitat.mode != "volant"]),
             colour = "gray47", fill = "gray47") +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -1),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Dietary breadth Nonvolant")
##sample size
length(df$diet.breadth[df$n.cont == "2" & df$habitat.mode != "volant"]) #118

ggsave(p.diet.brd.2.nonvol, 
       file = "./Figures/diet.breadth.nonvol.two.png", 
       width = 20, height = 10, units = "cm")

p.diet.brd.2.vol <- ggplot() +  
    geom_bar(aes(df$diet.breadth[df$n.cont == "2" & df$habitat.mode == "volant"]),
             colour = "gray47", fill = "gray47") +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -1),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Dietary breadth Volant")
##sample size
length(df$diet.breadth[df$n.cont == "2" & df$habitat.mode == "volant"]) #142

ggsave(p.diet.brd.2.vol, 
       file = "./Figures/diet.breadth.vol.two.png", 
       width = 20, height = 10, units = "cm")

p.diet.brd.3 <-  ggplot() +  
    geom_bar(aes(df$diet.breadth[df$n.cont == "3+"]),
             colour = "red", fill = "red") +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -.5),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Dietary Breadth",
                       limits = c(.5, 3.5),
                       breaks = c(1, 2, 3),
                       labels = c(1, 2, 3))
##sample size
length(df$diet.breadth[df$n.cont == "3+"]) #6

ggsave(p.diet.brd.3, 
       file = "./Figures/diet.breadth.three.png", 
       width = 20, height = 10, units = "cm")

##### DIET TYPE ------
diet.melt <- melt(df, id.vars = c("order", "binomial", "n.cont", "diet.breadth", "habitat.mode"), 
                  measure.vars = c("diet.carnivore.tot", 
                                   "diet.browser.tot", 
                                   "diet.grazer.tot", 
                                   "diet.invertivore.tot", 
                                   "diet.piscivore.tot", 
                                   "diet.frugivore.tot"),
                  variable.name = "diet.type")

diet.melt <- diet.melt %>%
    filter(diet.melt$value == TRUE)

table(diet.melt$diet.type[diet.melt$diet.breadth == 3])

#group by binomial so don't recount
unique.diet.melt <- diet.melt %>%
    group_by(binomial) %>%
    dplyr:: summarise(diettype = diet.type[1], numconts = n.cont[1]) %>%
    mutate_at("diettype", as.factor) %>%
    as.data.frame()

unique.diet.melt.nonvol <- diet.melt[diet.melt$habitat.mode != "volant",] %>%
    group_by(binomial) %>%
    dplyr:: summarise(diettype = diet.type[1], numconts = n.cont[1]) %>%
    mutate_at("diettype", as.factor) %>%
    as.data.frame()

unique.diet.melt.vol <- diet.melt[diet.melt$habitat.mode == "volant",] %>%
    group_by(binomial) %>%
    dplyr:: summarise(diettype = diet.type[1], numconts = n.cont[1]) %>%
    mutate_at("diettype", as.factor) %>%
    as.data.frame()

diettype_bargraph <- plyr::ddply(unique.diet.melt, c("numconts", "diettype"), function(x){
    nrow(x)
})
diettype_bargraph_full <- complete(diettype_bargraph, numconts, diettype)

diettype_bargraph_full$tots <- NA
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "1"] <- sum(diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "1"], na.rm = TRUE) #4120
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "2"] <- sum(diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "2"], na.rm = TRUE) #260
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "3+"] <- sum(diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "3+"], na.rm = TRUE) #6

diettype_bargraph_full$prop <- diettype_bargraph_full$V1 / diettype_bargraph_full$tots

diettype_bargraph.vol <- plyr::ddply(unique.diet.melt.vol, c("numconts", "diettype"), function(x){
    nrow(x)
})
diettype_bargraph_full.vol <- complete(diettype_bargraph.vol , numconts, diettype)

diettype_bargraph_full.vol$tots <- NA
diettype_bargraph_full.vol$tots[diettype_bargraph_full.vol$numconts == "1"] <- sum(diettype_bargraph_full.vol$V1[diettype_bargraph_full.vol$numconts == "1"], na.rm = TRUE) #4120
diettype_bargraph_full.vol$tots[diettype_bargraph_full.vol$numconts == "2"] <- sum(diettype_bargraph_full.vol$V1[diettype_bargraph_full.vol$numconts == "2"], na.rm = TRUE) #260
diettype_bargraph_full.vol$tots[diettype_bargraph_full.vol$numconts == "3+"] <- sum(diettype_bargraph_full.vol$V1[diettype_bargraph_full.vol$numconts == "3+"], na.rm = TRUE) #6

diettype_bargraph_full.vol$prop <- diettype_bargraph_full.vol$V1 / diettype_bargraph_full.vol$tots

diettype_bargraph.nonvol <- plyr::ddply(unique.diet.melt.nonvol, c("numconts", "diettype"), function(x){
    nrow(x)
})
diettype_bargraph_full.nonvol <- complete(diettype_bargraph.nonvol, numconts, diettype)

diettype_bargraph_full.nonvol$tots <- NA
diettype_bargraph_full.nonvol$tots[diettype_bargraph_full.nonvol$numconts == "1"] <- sum(diettype_bargraph_full.nonvol$V1[diettype_bargraph_full.nonvol$numconts == "1"], na.rm = TRUE) #4120
diettype_bargraph_full.nonvol$tots[diettype_bargraph_full.nonvol$numconts == "2"] <- sum(diettype_bargraph_full.nonvol$V1[diettype_bargraph_full.nonvol$numconts == "2"], na.rm = TRUE) #260
diettype_bargraph_full.nonvol$tots[diettype_bargraph_full.nonvol$numconts == "3+"] <- sum(diettype_bargraph_full.nonvol$V1[diettype_bargraph_full.nonvol$numconts == "3+"], na.rm = TRUE) #6

diettype_bargraph_full.nonvol$prop <- diettype_bargraph_full.nonvol$V1 / diettype_bargraph_full.nonvol$tots

#show as proportions
diettype_bargraph_full$diettype <- factor(diettype_bargraph_full$diettype, 
                                          levels = c("diet.browser.tot", "diet.grazer.tot", "diet.frugivore.tot",
                                                     "diet.carnivore.tot", "diet.piscivore.tot", "diet.invertivore.tot"))

diettype_bargraph_full$numconts <- factor(diettype_bargraph_full$numconts,
                                          levels = c("1", "2", "3+"))

diettype_bargraph_full.nonvol$diettype <- factor(diettype_bargraph_full.nonvol$diettype, 
                                                 levels = c("diet.browser.tot", "diet.grazer.tot", "diet.frugivore.tot",
                                                            "diet.carnivore.tot", "diet.piscivore.tot", "diet.invertivore.tot"))
diettype_bargraph_full.nonvol$numconts <- factor(diettype_bargraph_full.nonvol$numconts,
                                                 levels = c("1", "2", "3+"))

diettype_bargraph_full.vol$diettype <- factor(diettype_bargraph_full.vol$diettype, 
                                              levels = c("diet.browser.tot", "diet.grazer.tot", "diet.frugivore.tot",
                                                         "diet.carnivore.tot", "diet.piscivore.tot", "diet.invertivore.tot"))
diettype_bargraph_full.vol$numconts <- factor(diettype_bargraph_full.vol$numconts,
                                              levels = c("1", "2", "3+"))

ggplot(diettype_bargraph_full, 
       aes(x = diettype, y = prop, 
           fill = numconts)) + 
    geom_bar(stat = "identity") +
    xlab("Diet Type") + 
    ylab("Proportion") + 
    scale_x_discrete(labels=c("diet.carnivore.tot" = "Carnivore", "diet.piscivore.tot" = "Piscivore", 
                              "diet.invertivore.tot" = "Invertivore", "diet.browser.tot" = "Browser", 
                              "diet.grazer.tot" = "Grazer", "diet.frugivore.tot" = "Frugivore")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) + 
    scale_fill_manual(values = c("1" = "black",
                                 "2" = "gray47",
                                 "3+" = "red"),
                      name = "Number of Continents",
                      labels = c("1",
                                 "2",
                                 "3+")) +
    geom_col(position = position_stack(reverse = TRUE)) +
    plot_theme +
    theme(panel.border = element_rect(fill = NA),
          strip.background = element_rect(fill = NA),
          legend.position = c(1.25, 0.5)) +
    theme(axis.title.y = element_text(margin = margin(r = 5)))

p.diet.1 <- ggplot() + 
    geom_bar(aes(x = diettype_bargraph_full$diettype[diettype_bargraph_full$numconts == "1"], 
                 y = diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "1"]),
             colour = "black", fill = "black",
             stat = "identity") +
    xlab("Diet Type") + 
    ylab("Count") + 
    scale_x_discrete(labels=c("diet.carnivore.tot" = "Ca.", 
                              "diet.piscivore.tot" = "Pi.", 
                              "diet.invertivore.tot" = "In.", 
                              "diet.browser.tot" = "Br.", 
                              "diet.grazer.tot" = "Gr.", 
                              "diet.frugivore.tot" = "Fr.")) + 
    geom_col(position = position_stack(reverse = TRUE)) +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -1),
          axis.title.x = element_text(vjust = -1))
##sample size
sum(diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "1"]) #4119

ggsave(p.diet.1, 
       file = "./Figures/diet.type.one.png", 
       width = 20, height = 10, units = "cm")

p.diet.2 <- ggplot() + 
    geom_bar(aes(x = diettype_bargraph_full$diettype[diettype_bargraph_full$numconts == "2"], 
                 y = diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "2"]),
             colour = "gray47", fill = "gray47",
             stat = "identity") +
    xlab("Diet Type") + 
    ylab("Count") + 
    scale_x_discrete(labels=c("diet.carnivore.tot" = "Ca.", 
                              "diet.piscivore.tot" = "Pi.", 
                              "diet.invertivore.tot" = "In.", 
                              "diet.browser.tot" = "Br.", 
                              "diet.grazer.tot" = "Gr.", 
                              "diet.frugivore.tot" = "Fr.")) + 
    geom_col(position = position_stack(reverse = TRUE)) +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -1),
          axis.title.x = element_text(vjust = -1))
##sample size
sum(diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "2"], na.rm = TRUE) #260

ggsave(p.diet.2, 
       file = "./Figures/diet.type.two.png", 
       width = 20, height = 10, units = "cm")

p.diet.2.nonvol <- ggplot() + 
    geom_bar(aes(x = diettype_bargraph_full.nonvol$diettype[diettype_bargraph_full.nonvol$numconts == "2"], 
                 y = diettype_bargraph_full.nonvol$V1[diettype_bargraph_full.nonvol$numconts == "2"]),
             colour = "gray47", fill = "gray47",
             stat = "identity") +
    xlab("Diet Type Non-Volant") + 
    ylab("Count") + 
    scale_x_discrete(labels=c("diet.carnivore.tot" = "Ca.", 
                              "diet.piscivore.tot" = "Pi.", 
                              "diet.invertivore.tot" = "In.", 
                              "diet.browser.tot" = "Br.", 
                              "diet.grazer.tot" = "Gr.", 
                              "diet.frugivore.tot" = "Fr.")) + 
    geom_col(position = position_stack(reverse = TRUE)) +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -1),
          axis.title.x = element_text(vjust = -1))
##sample size
sum(diettype_bargraph_full.nonvol$V1[diettype_bargraph_full.nonvol$numconts == "2"], na.rm = TRUE) #118

ggsave(p.diet.2.nonvol, 
       file = "./Figures/diet.type.nonvol.two.png", 
       width = 20, height = 10, units = "cm")

p.diet.2.vol <- ggplot() + 
    geom_bar(aes(x = diettype_bargraph_full.vol$diettype[diettype_bargraph_full.vol$numconts == "2"], 
                 y = diettype_bargraph_full.vol$V1[diettype_bargraph_full.vol$numconts == "2"]),
             colour = "gray47", fill = "gray47",
             stat = "identity") +
    xlab("Diet Type Volant") + 
    ylab("Count") + 
    scale_x_discrete(labels=c("diet.carnivore.tot" = "Ca.", 
                              "diet.piscivore.tot" = "Pi.", 
                              "diet.invertivore.tot" = "In.", 
                              "diet.browser.tot" = "Br.", 
                              "diet.grazer.tot" = "Gr.", 
                              "diet.frugivore.tot" = "Fr.")) + 
    geom_col(position = position_stack(reverse = TRUE)) +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -1),
          axis.title.x = element_text(vjust = -1))
##sample size
sum(diettype_bargraph_full.vol$V1[diettype_bargraph_full.vol$numconts == "2"], na.rm = TRUE) #142

ggsave(p.diet.2.vol, 
       file = "./Figures/diet.type.vol.two.png", 
       width = 20, height = 10, units = "cm")

p.diet.3 <- ggplot() + 
    geom_bar(aes(x = diettype_bargraph_full$diettype[diettype_bargraph_full$numconts == "3+"], 
                 y = diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "3+"]),
             colour = "red", fill = "red",
             stat = "identity") +
    xlab("Diet Type") + 
    ylab("Count") + 
    scale_x_discrete(labels=c("diet.carnivore.tot" = "Ca.", 
                              "diet.piscivore.tot" = "Pi.", 
                              "diet.invertivore.tot" = "In.", 
                              "diet.browser.tot" = "Br.", 
                              "diet.grazer.tot" = "Gr.", 
                              "diet.frugivore.tot" = "Fr.")) + 
    geom_col(position = position_stack(reverse = TRUE)) +
    plot_theme +
    theme(axis.text.x = element_text(vjust = -.5),
          axis.title.x = element_text(vjust = -1))
##sample size
sum(diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "3+"], na.rm = TRUE) #6

ggsave(p.diet.3, 
       file = "./Figures/diet.type.three.png", 
       width = 20, height = 10, units = "cm")

##habitats of frugivores and browsers for wanderers
unique(df$habitat.mode[df$n.cont == 2 & df$diet.frugivore.tot == TRUE])
unique(df$habitat.mode[df$n.cont == 2 & df$diet.browser.tot == TRUE])

#are all fossorial and arboreal species either frugivores or browsers?
df[df$n.cont == 2 & df$habitat.mode == "arboreal",] #all but two are frug, the two that aren't are also browse
df[df$n.cont == 2 & df$habitat.mode == "terr- fossorial",] #all but two are frug, the two that aren't are also browse
#these are carnivores or invertivores

#### H6: HABITAT MODE ----
## TEST: animals that are widespread have a specific habitat modes (or aren't specific habitat modes)
null.habitat.mode <- df[df$habitat.mode != "",] %>%
    group_by(habitat.mode) %>%
    dplyr::summarise(null.N = n()) %>%
    dplyr::select(habitat.mode,
                  null.N) %>%
    as.data.frame()

habitat.mode <- df[df$habitat.mode != "",] %>%
    group_by(n.cont, habitat.mode) %>%
    dplyr::summarise(N = n()) %>% 
    as.data.frame()

homies.habitat.mode <- habitat.mode[habitat.mode$n.cont == 1,]
colnames(homies.habitat.mode)[colnames(homies.habitat.mode) == "N"] <- "homies.N"
homies.habitat.mode <- homies.habitat.mode %>%
    dplyr::select(-n.cont)

rangers.habitat.mode <- habitat.mode[habitat.mode$n.cont == 2,]
colnames(rangers.habitat.mode)[colnames(rangers.habitat.mode) == "N"] <- "rangers.N"
rangers.habitat.mode <- rangers.habitat.mode %>%
    dplyr::select(-n.cont)

cosmo.habitat.mode <- habitat.mode[habitat.mode$n.cont == "3+",]
colnames(cosmo.habitat.mode)[colnames(cosmo.habitat.mode) == "N"] <- "cosmo.N"
cosmo.habitat.mode <- cosmo.habitat.mode %>%
    dplyr::select(-n.cont)

#create full dataset
hab.null.cosmo <- merge(null.habitat.mode, cosmo.habitat.mode, by = "habitat.mode", all.x = TRUE, all.y = TRUE)
hab.null.cosmo.rangers <- merge(hab.null.cosmo, rangers.habitat.mode, by = "habitat.mode", all.x = TRUE, all.y = TRUE)
hab.null.cosmo.rangers.homies <- merge(hab.null.cosmo.rangers, homies.habitat.mode, by = "habitat.mode", all.x = TRUE, all.y = TRUE)

df.hab <- hab.null.cosmo.rangers.homies
df.hab[is.na(df.hab)] <- 0

df.hab$prop.null <- df.hab$null.N/nrow(df)

df.hab$prop.homies <- df.hab$homies.N/nrow(df[df$n.cont == "1",])
df.hab$prop.rangers <- df.hab$rangers.N/nrow(df[df$n.cont == "2",])
df.hab$prop.cosmo <- df.hab$cosmo.N/nrow(df[df$n.cont == "3+",])

#binomial test
for(i in 1:nrow(df.hab)){
    test <- binom.test(df.hab$homies.N[i], nrow(df[df$n.cont == "1",]), 
                       p = df.hab$prop.null[i], alternative = "two.sided")
    df.hab$p.homies[i] <- test$p.value
}

for(i in 1:nrow(df.hab)){
    test <- binom.test(df.hab$rangers.N[i], nrow(df[df$n.cont == "2",]), 
                       p = df.hab$prop.null[i], alternative = "two.sided")
    df.hab$p.rangers[i] <- test$p.value
}

for(i in 1:nrow(df.hab)){
    test <- binom.test(df.hab$cosmo.N[i], nrow(df[df$n.cont == "3+",]), 
                       p = df.hab$prop.null[i], alternative = "two.sided")
    df.hab$p.cosmo[i] <- test$p.value
}

#add sidak correction
df.hab <- arrange(df.hab, p.homies) %>%
    dplyr::mutate(signif.homies = p.homies < 0.05,
                  signif.bonferoni.homies = p.homies < 0.05/n(),
                  signif.holm.homies = !0.05/(n() + 1 - 1:n()) < p.homies,
                  signif.sidak.homies = p.homies < 1 - (1 - 0.05)^(1/n()),
                  signif.holm.sidak.homies = !(1 - (1 - 0.05)^(1/n())) < p.homies)

df.hab <- arrange(df.hab, p.rangers) %>%
    dplyr::mutate(signif.rangers = p.rangers < 0.05,
                  signif.bonferoni.rangers = p.rangers < 0.05/n(),
                  signif.holm.rangers = !0.05/(n() + 1 - 1:n()) < p.rangers,
                  signif.sidak.rangers = p.rangers < 1 - (1 - 0.05)^(1/n()),
                  signif.holm.sidak.rangers = !(1 - (1 - 0.05)^(1/n())) < p.rangers)

df.hab <- arrange(df.hab, p.cosmo) %>%
    dplyr::mutate(signif.cosmo = p.cosmo < 0.05,
                  signif.bonferoni.cosmo = p.cosmo < 0.05/n(),
                  signif.holm.cosmo = !0.05/(n() + 1 - 1:n()) < p.cosmo,
                  signif.sidak.cosmo = p.cosmo < 1 - (1 - 0.05)^(1/n()),
                  signif.holm.sidak.cosmo = !(1 - (1 - 0.05)^(1/n())) < p.cosmo)

write.csv(df.hab, 
          "./Results/habitat.results.csv",
          row.names = FALSE)
#volant sig for homies, fewer are volant
#terr, volant, arboreal, fossorial sig for lim
##terr dec; vol incr; arboreal dec; foss dec
#non for trotters

##### HABITAT MODE BAR GRAPH -----
df.hab.trim <- df[!is.na(df$habitat.mode),]
df.hab.trim <- df[df$habitat.mode != "",]
unique(df.hab.trim$habitat.mode)
df.hab.trim$habitat.mode <- factor(df.hab.trim$habitat.mode,
                                   levels = c("terr",
                                              "terr- fossorial",
                                              "terr _ aquatic",
                                              "arboreal",
                                              "volant"))

df.habitat.counts <- df.hab.trim %>%
    group_by(n.cont, habitat.mode) %>%
    summarize(n = n()) %>%
    as.data.frame()
three.add.fossorial <- c("3+", "terr- fossorial", 0)
three.add.arbor <- c("3+", "arboreal", 0)
three.add.aqua <- c("3+", "terr _ aquatic", 0)
df.habitat.counts <- rbind(df.habitat.counts, three.add.fossorial, three.add.arbor,
                           three.add.aqua)

p.hab.1 <-  ggplot() +  
    geom_col(aes(df.habitat.counts$habitat.mode[df.habitat.counts$n.cont == "1"],
                 as.numeric(df.habitat.counts$n[df.habitat.counts$n.cont == "1"])), 
             colour = "black", fill = "black") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), #vjust = -1),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Habitat Mode",
                     labels = c("terr" = "terrestrial",
                                "terr- fossorial" = "fossorial",
                                "terr _ aquatic" = "aquatic",
                                "arboreal" = "arboreal",
                                "volant" = "volant"))
##sample size
length(df.hab.trim$habitat.mode[df.hab.trim$n.cont == "1"]) #3516

ggsave(p.hab.1, 
       file = "./Figures/habit.one.png", 
       width = 20, height = 10, units = "cm")

p.hab.2 <-  ggplot() +  
    geom_col(aes(df.habitat.counts$habitat.mode[df.habitat.counts$n.cont == "2"],
                 as.numeric(df.habitat.counts$n[df.habitat.counts$n.cont == "2"])), 
             colour = "gray47", fill = "gray47") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), #vjust = -1),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Habitat Mode",
                     labels = c("terr" = "terrestrial",
                                "terr- fossorial" = "fossorial",
                                "terr _ aquatic" = "aquatic",
                                "arboreal" = "arboreal",
                                "volant" = "volant"))
##sample size
length(df.hab.trim$habitat.mode[df.hab.trim$n.cont == "2"]) #258

ggsave(p.hab.2, 
       file = "./Figures/habit.two.png", 
       width = 20, height = 10, units = "cm")

df.habitat.counts.nonvol <- df.hab.trim[df.hab.trim$habitat.mode != "volant" & df.hab.trim$n.cont == "2",] %>%
    group_by(n.cont, habitat.mode) %>%
    summarize(n = n()) %>%
    as.data.frame()
two.add.vol <- c("2", "volant", 0)
df.habitat.counts.nonvol <- rbind(df.habitat.counts.nonvol, 
                                  two.add.vol)

p.hab.2.nonvol <-  ggplot() +  
    geom_col(aes(df.habitat.counts.nonvol$habitat.mode,
                 as.numeric(df.habitat.counts.nonvol$n)), 
             colour = "gray47", fill = "gray47") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), #vjust = -1),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Habitat Mode",
                     labels = c("terr" = "terrestrial",
                                "terr- fossorial" = "fossorial",
                                "terr _ aquatic" = "aquatic",
                                "arboreal" = "arboreal",
                                "volant" = "volant"))
##sample size
length(df.hab.trim$habitat.mode[df.hab.trim$n.cont == "2" & df.hab.trim$habitat.mode != "volant"]) #116

ggsave(p.hab.2.nonvol, 
       file = "./Figures/habit.two.nonvol.png", 
       width = 20, height = 10, units = "cm")

df.habitat.counts.vol <- df.hab.trim[df.hab.trim$habitat.mode == "volant" & df.hab.trim$n.cont == "2",] %>%
    group_by(n.cont, habitat.mode) %>%
    summarize(n = n()) %>%
    as.data.frame()
two.add.fossorial <- c("2", "terr- fossorial", 0)
two.add.arbor <- c("2", "arboreal", 0)
two.add.aqua <- c("2", "terr _ aquatic", 0)
two.add.terr <- c("2", "terr", 0)
df.habitat.counts.vol <- rbind(df.habitat.counts.vol, 
                               two.add.fossorial, two.add.arbor,
                               two.add.aqua, two.add.terr)

p.hab.2.vol <-  ggplot() +  
    geom_col(aes(df.habitat.counts.vol$habitat.mode,
                 as.numeric(df.habitat.counts.vol$n)), 
             colour = "gray47", fill = "gray47") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), #vjust = -1),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Habitat Mode",
                     labels = c("terr" = "terrestrial",
                                "terr- fossorial" = "fossorial",
                                "terr _ aquatic" = "aquatic",
                                "arboreal" = "arboreal",
                                "volant" = "volant"))

##sample size
length(df.hab.trim$habitat.mode[df.hab.trim$n.cont == "2" & df.hab.trim$habitat.mode == "volant"]) #142

ggsave(p.hab.2.vol, 
       file = "./Figures/habit.two.vol.png", 
       width = 20, height = 10, units = "cm")

p.hab.3 <-  ggplot() +  
    geom_col(aes(df.habitat.counts$habitat.mode[df.habitat.counts$n.cont == "3+"],
                 as.numeric(df.habitat.counts$n[df.habitat.counts$n.cont == "3+"])), 
             colour = "red", fill = "red") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), #vjust = -1),
          axis.title.x = element_text(vjust = -1)) +
    scale_y_continuous(name = "Count") +
    scale_x_discrete(name = "Habitat Mode",
                     labels = c("terr" = "terrestrial",
                                "terr- fossorial" = "fossorial",
                                "terr _ aquatic" = "aquatic",
                                "arboreal" = "arboreal",
                                "volant" = "volant"))
##sample size
length(df.hab.trim$habitat.mode[df.hab.trim$n.cont == "3+"]) #6

ggsave(p.hab.3, 
       file = "./Figures/habit.three.png", 
       width = 20, height = 10, units = "cm")

#### DECISION TREE ----
#df.tree <- left_join(df, df.dispersal,
#                     by = "binomial")
#not enough dispersal data

df.tree <- df

##make categories for tree
# volant or not
df.tree$locomotion <- "nonvolant"
df.tree$locomotion[df.tree$habitat.mode == "volant"] <- "volant"

# herbivore or carnivore
df.tree$diet.type <- "herbivore"
df.tree$diet.type[df.tree$diet.carnivore.tot == TRUE | df.tree$diet.invertivore.tot == TRUE | df.tree$diet.piscivore.tot == TRUE] <- "carnivore"

# n connectivity of family origin
df.tree$fam.connectivity <- ""
df.tree$fam.connectivity[df.tree$family.origin == "North.America" | df.tree$family.origin == "Eurasia"] <- 2
df.tree$fam.connectivity[df.tree$family.origin == "South.America" | df.tree$family.origin == "Africa"] <- 1
df.tree$fam.connectivity[df.tree$family.origin == "Australia"] <- 0

# species richness by family
fam.richness <- df.tree %>%
    group_by(family) %>%
    summarise(fam.richness = n()) %>%
    as.data.frame()

df.tree <- left_join(df.tree, fam.richness, by = "family")

df.tree$wide.ranging <- df.tree$n.cont
df.tree$wide.ranging[df.tree$wide.ranging == "3+"] <- "2"
df.tree$wide.ranging <- as.factor(df.tree$wide.ranging)

df.tree$diet.breadth <- as.factor(df.tree$diet.breadth)

df.tree$fam.connectivity <- as.integer(df.tree$fam.connectivity)

#select specific rows
df.tree.trim <- dplyr::select(df.tree, "locomotion", 
                              "fam.connectivity", "avg.mass",
                              "age.median", 
                              "diet.type", 
                              "fam.richness", "diet.breadth", 
                              "wide.ranging")

df.tree.trim <- na.omit(df.tree.trim)
nrow(df.tree.trim)
str(df.tree.trim)

##### CLASSIFICATION TREE -----
set.seed(2000)

# Split the data into training (75%) and test (25%) sets
train_index <- sample(1:nrow(df.tree.trim), nrow(df.tree.trim)*0.8)

# train dataset formation
train_set <- df.tree.trim[train_index, ]
str(train_set)

# test dataset formation
test_set <- df.tree.trim[-train_index, ]
str(test_set)

gm_tree <- rpart(wide.ranging ~ ., data = train_set, method = "class")

rpart.plot(gm_tree, type = 2, main = "Decision Tree for Global Mammals")

# Random Forest
ind <- sample(2, nrow(df.tree.trim), 
              replace = TRUE, prob = c(0.75, 0.25))
train <- df.tree.trim[ind == 1,]
test <- df.tree.trim[ind == 2,]

rf <- randomForest(wide.ranging ~ ., data = df.tree.trim, 
                   importance = TRUE, proximity = TRUE)

print(rf)

p1 <- predict(rf, train)
confusionMatrix(p1, train$wide.ranging)

p2 <- predict(rf, test)
confusionMatrix(p2, test$wide.ranging)

plot(rf)

varImpPlot(rf,
           sort = T,
           n.var = 6,
           main = "Variable Importance for Global Mammals")
importance(rf)

##### NO BATS -----
df.tree.nobats <- df.tree[df.tree$order != "Chiroptera",]

#select specific rows
df.tree.trim.nobats <- select(df.tree.nobats, "avg.mass", "age.median", 
                              "diet.breadth", "diet.type", 
                              "locomotion", "fam.richness", "fam.connectivity", 
                              "wide.ranging")

df.tree.trim.nobats <- na.omit(df.tree.trim.nobats)

str(df.tree.trim.nobats)

##### CLASSIFICATION TREE -----
set.seed(1000)

# Split the data into training (75%) and test (25%) sets
train_index.nobats <- sample(1:nrow(df.tree.trim.nobats), nrow(df.tree.trim.nobats)*0.75)

# train dataset formation
train_set.nobats <- df.tree.trim.nobats[train_index, ]
str(train_set.nobats)

# test dataset formation
test_set.nobats <- df.tree.trim.nobats[-train_index.nobats, ]
str(test_set.nobats)

gm_tree.nobats <- rpart(n.cont ~ ., data = train_set.nobats, method = "class")

rpart.plot(gm_tree.nobats, 
           type = 2, box.palette = "blue", 
           main = "Decision Tree for Global Mammals No Bats")

# Random Forest
ind.nobats <- sample(2, nrow(df.tree.trim.nobats), 
                     replace = TRUE, prob = c(0.75, 0.25))
train.nobats <- df.tree.trim.nobats[ind.nobats == 1,]
test.nobats <- df.tree.trim.nobats[ind.nobats == 2,]

rf.nobats <- randomForest(n.cont ~ ., data = df.tree.trim.nobats, 
                          importance = TRUE, proximity = TRUE)

print(rf.nobats)

p1.nobats <- predict(rf.nobats, train)
confusionMatrix(p1.nobats, train$n.cont)

p2.nobats <- predict(rf.nobats, test)
confusionMatrix(p2.nobats, test$n.cont)

plot(rf.nobats)

varImpPlot(rf.nobats,
           sort = T,
           n.var = 6,
           main = "Variable Importance for Global Mammals No Bats")
importance(rf.nobats)

##### DELVING DEEPER -----
#all the bats
table(df.tree$family[df.tree$locomotion == "volant"])

#which bats are at either side of the 11 g break?
unique(df.tree$family[df.tree$locomotion == "volant" & df.tree$avg.mass >= 11])
table(df.tree$family[df.tree$locomotion == "volant" & df.tree$avg.mass >= 11])
#all of the Megadermatidae, most of the Molossidae and Phyllostomidae; all of the Pteropodidae

df.tree[df.tree$locomotion == "volant",] %>%
    group_by(n.cont) %>%
    filter(avg.mass < 11) %>%
    summarise(n = n())

df.tree[df.tree$locomotion == "volant",] %>%
    group_by(n.cont) %>%
    filter(avg.mass >= 11) %>%
    summarise(n = n())

#which bats are on either side of the 1.3 mya?
table(df.tree$family[df.tree$vol == "volant" & df.tree$age.median >= 1.3])
#most of the bats are in this category

table(df.tree$family[df.tree$locomotion == "volant" & df.tree$age.median < 1.3])

df.tree[df.tree$locomotion == "volant",] %>%
    group_by(n.cont) %>%
    filter(age.median < 1.3) %>%
    summarise(n = n())

df.tree[df.tree$locomotion == "volant",] %>%
    group_by(n.cont) %>%
    filter(age.median >= 1.3) %>%
    summarise(n = n()) #a lot of older ones are still on 1 continent

#### LOG ODDS ----
## use habitat mode volant as reference
df.tree.trim$log.mass <- log10(df.tree.trim$avg.mass)
model <- glm(wide.ranging ~ locomotion + log.mass + diet.type + fam.connectivity + fam.richness + age.median, 
             family = binomial(link = "logit"), data = df.tree.trim)
summary(model)

#get odds ratio
cbind(Estimate = round(coef(model), 4),
      OR = round(exp(coef(model)), 4))

#same thing, different way
logitor(wide.ranging ~ locomotion + log.mass + diet.type + fam.connectivity + fam.richness + age.median, 
        data = df.tree.trim)

#look at outputs
stargazer(model, type = "text", out = "logit.htm")
    
#look at outputs of odds ratio
logit.or = exp(coef(model))
logit.or

stargazer(model, type = "text", 
          coef = list(logit.or), p.auto = FALSE, out = "logitor.htm")

##### PLOT ODDS RATIO -----

vars <- c("locomotionvolant", "log.mass", "diet.typeherbivore",
          "fam.connectivity", "fam.richness", "age.median")
mod.coef <- c(2.5225, 0.4897, -0.9578, 
              -0.1511, -0.0022, -0.0034)
mod.stdErr <- c(0.227, 0.082, 0.192, 
                0.136, 0.001, 0.014)
oddsRatio <- c(12.4603, 1.6319, 0.3837,
               0.8597, 0.9978, 0.9966)
p.vals.sig <- c("***", "***", "***", 
                "", "***", "", "")

or.table <- as.data.frame(cbind(vars, mod.coef, mod.stdErr, oddsRatio, p.vals.sig))
or.table$mod.coef <- as.numeric(or.table$mod.coef)
or.table$mod.stdErr <- as.numeric(or.table$mod.stdErr)
or.table$oddsRatio <- as.numeric(or.table$oddsRatio)
or.table$vars <- factor(or.table$vars,
                        levels = c("fam.connectivity",
                                   "age.median",
                                   "fam.richness",
                                   "log.mass",
                                   "diet.typeherbivore",
                                   "locomotionvolant"))

p.or <- ggplot(or.table, aes(x = vars, y = mod.coef)) +
    geom_hline(yintercept = 0,
               col = "#afa298", alpha = 0.5,
               size = 1, lty = 2) + 
    geom_point(shape = 19, size = 3) +
    geom_errorbar(width =.2, aes(ymin = mod.coef - mod.stdErr, 
                                 ymax = mod.coef + mod.stdErr), 
                  colour = "black") +
    plot_theme + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
    scale_x_discrete(name = "Variables",
                     labels = c("age.median" = "Age (mya)", 
                                "diet.typeherbivore" = "Herbivore", 
                                "fam.connectivity" = "Connectivity", 
                                "fam.richness" = "Family Richness", 
                                "locomotionvolant" = "Volant", 
                                "log.mass" = expression(log[10]~Mass))) +
    scale_y_continuous(name = "Odds Ratio")


ggsave(p.or, 
       file = paste0("./Figures/odds.ratio",".png"), 
       width = 14, height = 10, units = "cm")

    
#### H7: GEOGRAPHIC RANGE SIZE ----
## TEST: animals that are widespread have a larger geographic range than predicted for body size

for(i in 1:length(df$binomial)){
  if(df$continent.Eurasia[i] == TRUE){
    df$tot.area[i] <- (54.75*10^6)
  }else if(df$continent.Africa[i] == TRUE){
    df$tot.area[i] <- (30.38*10^6)
  }else if(df$continent.North.America[i] == TRUE){
    df$tot.area[i] <- (24.70*10^6)
  }else if(df$continent.South.America[i] == TRUE){
    df$tot.area[i] <- (17.83*10^6)
  }else{
    df$tot.area[i] <- (7.69*10^6)
  }
}

#want to combine to unique spp
df.cont <- df %>%
  dplyr::select(binomial = binomial,
                family = family,
                cont.tot.area = tot.area, 
                   pan.gr.area = gr.area.km2, 
                   hmrg = home.range.km2,
                   faurby.nat.range = present.natural.range.km2, 
                   faurby.current.range = current.range.km2,
                   num.cont = n.cont, 
                   size = avg.mass,
                logSize = log.mass) %>%
  as.data.frame()

#get cleanest dataset
df.pan <- subset(df.cont, !is.na(df.cont$pan.gr.area) & !is.na(df.cont$size))

df.faurby <- subset(df.cont, !is.na(df.cont$faurby.nat.range) & !is.na(df.cont$size) & df.cont$faurby.nat.range != 0) 

length(unique(df.pan$binomial)) #2946
length(unique(df.pan$binomial[df.pan$num.cont == 1])) #2709
length(unique(df.pan$binomial[df.pan$num.cont == 2])) #231
length(unique(df.pan$binomial[df.pan$num.cont == "3+"])) #6

length(unique(df.faurby$binomial)) # 2475
length(unique(df.faurby$binomial[df.faurby$num.cont == 1])) #2362
length(unique(df.faurby$binomial[df.faurby$num.cont == 2])) #108
length(unique(df.faurby$binomial[df.faurby$num.cont == "3+"])) #5

df.pan$ratio <- df.pan$pan.gr.area/df.pan$cont.tot.area

# ranges
p.geog.range.w.legend <- ggplot(data = df.pan, aes(x = logSize, y = ratio)) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = cont_bw) +
  #scale_fill_manual(values = cont_bw) +
  labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Geographic~Range/Continent~Size), color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "top")

ggsave(p.geog.range.w.legend, 
       file = "./Figures/geog.range.w.legend.png", 
       width = 20, height = 10, units = "cm")

p.geog.range <- ggplot(data = df.pan, aes(x = logSize, y = ratio)) +
    geom_point(alpha = 0.7, aes(col = num.cont)) +
    geom_smooth(aes(color = num.cont), method = "lm") +
    scale_color_manual(values = cont_bw) +
    #scale_fill_manual(values = cont_bw) +
    labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Geographic~Range/Continent~Size), color = "Number of Continents") +
    plot_theme + 
    theme(legend.position = "")

ggsave(p.geog.range, 
       file = "./Figures/geog.range.png", 
       width = 14, height = 10, units = "cm")

#do cosmo species have a larger home ranges for the geographical range?
summary(lm(log10(df.pan$ratio) ~ log10(df.pan$logSize) + as.factor(df.pan$num.cont))) #r2 = 0.67; sig

ggplot(data = df.pan, aes(x = logSize, y = ratio)) +
    geom_point(alpha = 0.7, aes(col = num.cont)) +
    geom_smooth(method = "lm", color = "#76c476") +
    scale_color_manual(values = cont_bw) +
    labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Geographic~Range/Continent~Size), color = "Number of Continents") +
    plot_theme + 
    theme(legend.position = "none")

ggplot(data = df.pan, aes(x = log10(hmrg), y = log10(ratio))) +
    geom_point(alpha = 0.7, aes(col = num.cont)) +
    geom_smooth(method = "lm", color = "#76c476") +
    scale_color_manual(values = cont_bw) +
    labs(x = expression(log[10]~Home~Range), y = expression(log[10]~Geographic~Range/Continent~Size), color = "Number of Continents") +
    plot_theme + 
    theme(legend.position = "none")

df$log.gr <- log10(df$gr.area.km2)
range(df$log.gr, na.rm = TRUE)
df <- mutate(df, qtr.bin.gr = cut(log.gr, breaks = seq(-4, 8, .25)))
sort(unique(df$qtr.bin.gr))
df[df$qtr.bin.gr == "(7.75,8]",] %>% drop_na(binomial)
df[df$qtr.bin.gr == "(7.5,7.75]",] %>% drop_na(binomial)
#groups with the largest geogr ranges are msutelids, canids, felids, and bears
df[df$qtr.bin.gr == "(7.25,7.5]",] %>% drop_na(binomial)
df[df$qtr.bin.gr == "(7.25,7.5]" & df$n.cont == 1,] %>% drop_na(binomial)
df[df$qtr.bin.gr == "(7.25,7.5]" & df$n.cont == 1 & df$log.size.bin < 3,] %>% drop_na(binomial)

df.dispersal[df.dispersal$binomial == "Sorex caecutiens",]
df.dispersal[df.dispersal$binomial == "Sciurus vulgaris",]

df.pan$log.ratio <- log10(df.pan$ratio)
df.pan <- mutate(df.pan, qtr.bin.ratio = cut(log.ratio, breaks = seq(-11, 1, .25)))
sort(unique(df.pan$qtr.bin.ratio))
df.pan[df.pan$qtr.bin.ratio == "(0,0.25]",] %>% drop_na(binomial) #Tachyglossus aculeatus

## look at relationships between hmr and gr for the families of globetrotters
glb.fam <- c("Ursidae", "Mustelidae", "Canidae", "Felidae", "Cervidae", "Vespertilionidae")
glb <- df.pan[df.pan$family %in% glb.fam,]

ggplot(data = glb, aes(x = log10(hmrg), y = log10(ratio))) +
    geom_point(alpha = 0.7, aes(col = family)) +
    geom_smooth(method = "lm", color = "#76c476") +
    #scale_color_manual(values = cont_bw) +
    labs(x = expression(log[10]~Home~Range), y = expression(log[10]~Geographic~Range/Continent~Size), color = "Number of Continents") +
    plot_theme + 
    theme(legend.position = "none")
summary(lm(log10(glb$ratio) ~ log10(glb$hmrg))) #barely sig

glb.carn <- glb[glb$family != "Cervidae" & glb$family != "Vespertilionidae",]
ggplot(data = glb.carn, aes(x = log10(hmrg), y = log10(ratio))) +
    geom_point(alpha = 0.7, aes(col = family)) +
    geom_smooth(method = "lm", color = "#76c476") +
    #scale_color_manual(values = cont_bw) +
    labs(x = expression(log[10]~Home~Range), y = expression(log[10]~Geographic~Range/Continent~Size), color = "Number of Continents") +
    plot_theme #+ 
    #theme(legend.position = "none")
summary(lm(log10(glb.carn$ratio) ~ log10(glb.carn$hmrg))) #non sig

df.faurby$ratio.nat <- df.faurby$hmrg/df.faurby$faurby.nat.range
df.faurby$ratio.cur <- df.faurby$hmrg/df.faurby$faurby.current.range
df.faurby$ratio.cur[df.faurby$faurby.current.range == 0] <- NA

#home range
ggplot(data = df.faurby, aes(x = logSize, y = ratio.cur)) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = cont_bw) +
  labs(x = expression(log[10]~Body~Mass), 
       y = expression(log[10]~Home~Range/Current~Geographic~Range), 
       color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "top")

#do cosmo species have a larger geographical based on which continents they are on?
summary(lm(log10(df.faurby$ratio.cur) ~ log10(df.faurby$logSize) + as.factor(df.faurby$num.cont))) #r2 = 0.67; sig
#no

ggplot(data = df.faurby, aes(x = logSize, y = ratio.nat)) +
    geom_point(alpha = 0.7, aes(col = num.cont)) +
    geom_smooth(aes(color = num.cont), method = "lm") +
    scale_color_manual(values = cont_bw) +
    labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Home~Range/Natural~Geographic~Range), color = "Number of Continents") +
    plot_theme + 
    theme(legend.position = "top")

#do cosmo species have a larger geographical based on which continents they are on?
summary(lm(log10(df.faurby$ratio.nat) ~ log10(df.faurby$logSize) + as.factor(df.faurby$num.cont))) #r2 = 0.67; sig
#nope

#homerange to geographic range
plot(log10(df.pan$pan.gr.area) ~ log10(df.pan$hmrg))
summary(lm(df.pan$pan.gr.area ~ df.pan$hmrg)) #significant, but r2 = 0.04
#home range does not predict geographic range

#do bigger animals have a larger hmrg?
summary(lm(log10(df.pan$hmrg) ~ log10(df.pan$size) + as.factor(df.pan$num.cont))) #r2 = 0.67; sig
#size affects 2 and 3
summary(lm(log10(df.pan$hmrg) ~ log10(df.pan$size))) #r2 = 0.66; sig

p.hr <- ggplot(data = df.pan, 
               aes(x = log10(df.pan$size), y = log10(df.pan$hmrg))) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = cont_bw) +
  labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Home~Range), color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "")

ggsave(p.hr, 
       file = "./Figures/home.range.png", 
       width = 14, height = 10, units = "cm")

## TEST: Place of origin dictates how far you can travel----
#depends on how far can go
#depends on how connected your country of origin is (that is, SA and AUS won't be able to disperse; EURA and NA should have the most)


## TEST: older clades have dispersed farther----

#how big will the datasets be?
length(unique(df$binomial[df$foss.age > 0])) #695
length(unique(df$binomial[df$age.median > 0])) #4041

# range of dates
max(df$foss.age, na.rm = TRUE) #22.185
min(df$foss.age, na.rm = TRUE) #0.00585

#H1 spp that are on mult cont are older

#foss.age
data.foss.age <- df %>%
  dplyr::select(binomial, foss.age, n.cont, continent)

data.foss.age <- data.foss.age %>%
  na.omit()

length(unique(data.foss.age$binomial)) #694
length(unique(data.foss.age$binomial[data.foss.age$n.cont == 1])) #595
length(unique(data.foss.age$binomial[data.foss.age$n.cont == 2])) #94
length(unique(data.foss.age$binomial[data.foss.age$n.cont == "3+"])) #5

#1 v 3
ks.test(data.foss.age$foss.age[data.foss.age$n.cont == 1], 
        data.foss.age$foss.age[data.foss.age$n.cont == 2 | data.foss.age$n.cont == "3+"]) 
#sig

#1+2 v 3+
ks.test(data.foss.age$foss.age[data.foss.age$n.cont == 2 | data.foss.age$n.cont == 1], 
        data.foss.age$foss.age[data.foss.age$n.cont == "3+"]) 
#sig

## continents by age  of family
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
data.faurby <- df %>%
  dplyr::select(binomial, age.median, n.cont, continent)

data.faurby <- data.faurby %>%
  na.omit()

length(unique(data.faurby$binomial)) #4040
length(unique(data.faurby$binomial[data.faurby$n.cont == 1])) #3762
length(unique(data.faurby$binomial[data.faurby$n.cont == 2])) #272
length(unique(data.faurby$binomial[data.faurby$n.cont == "3+"])) #6

#1 v 2+
ks.test(data.faurby$age.median[data.faurby$n.cont == 1], 
        data.faurby$age.median[data.faurby$n.cont == 2 | data.faurby$n.cont == "3+"]) 
#sig

#1+2 v 3+
ks.test(data.faurby$age.median[data.faurby$n.cont == 1 | data.faurby$n.cont == 2], 
        data.faurby$age.median[data.faurby$n.cont == "3+"]) 
#sig

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

## what about continent connectivity and age of family to disperse?
#for continent pairs
#NA and SA
N_S_America <- subset(data.foss.age, data.foss.age$continent == "North.America" | data.foss.age$continent == "South.America")
N_S_America1 <- subset(N_S_America, N_S_America$n.cont == 1)

two.cont_NS <- subset(N_S_America, N_S_America$n.cont == 2)

length(two.cont_NS$binomial) #111
length(N_S_America1$binomial) #324

ks.test(N_S_America1$foss.age, two.cont_NS$foss.age, alternative = "two.sided") #sig
ks.test(N_S_America1$foss.age, two.cont_NS$foss.age, alternative = "less") #sig x is statically younger than y
ks.test(N_S_America1$foss.age, two.cont_NS$foss.age, alternative = "greater") #not sig

#NA and EA
N_E <- subset(data.foss.age, data.foss.age$continent == "North.America" | data.foss.age$continent == "Eurasia")
N_E1 <- subset(N_E, N_E$n.cont == 1)

two.cont_NE <- subset(N_E, N_E$n.cont == 2)

length(two.cont_NE$binomial) #115
length(N_E1$binomial) #332

ks.test(N_E1$foss.age, two.cont_NE$foss.age, alternative = "two.sided") #barely sig
ks.test(N_E1$foss.age, two.cont_NE$foss.age, alternative = "less") #sig x is statically younger than y
ks.test(N_E1$foss.age, two.cont_NE$foss.age, alternative = "greater") #non-sig


#EA and AF
E_A <- subset(data.foss.age, data.foss.age$continent == "Eurasia" | data.foss.age$continent == "Africa")
E_A1 <- subset(E_A, E_A$n.cont == 1)

two.cont_EA <- subset(E_A, E_A$n.cont == 2)

length(two.cont_EA$binomial) #77
length(E_A$binomial) #276

ks.test(E_A1$foss.age, two.cont_EA$foss.age, alternative = "two.sided") #sig
ks.test(E_A1$foss.age, two.cont_EA$foss.age, alternative = "less") #non-sig
ks.test(E_A1$foss.age, two.cont_EA$foss.age, alternative = "greater") #sig; x is statistically older than y

## TESTING ALL THE THINGS
df$n.cont <- as.character(df$n.cont)

mamm.tree <- read.tree("https://de.cyverse.org/dl/d/DD53DD75-07A0-4609-A321-F3819E72AE5D/Mammal2.tre")

#filter species that don't match
mamm.tree$tip.label #give indices for each mamm
#use tip labels to filter the results
df$binomial <- gsub(" ", "_", df$binomial)
sp.results_sub <- subset(df, binomial %in% mamm.tree$tip.label) #214

glm(df$n.cont ~ df$mass + df$age.median + df$diet.breadth + df$X22.1_HomeRange_km2)

#Blomberg's K
phylosig(mamm.tree, slope, method="lambda", test = TRUE, nsim = 1000, se = NULL, start = NULL, control = list()) #39.2275 
phylosig(mamm.tree, slope, method="K", test = TRUE, nsim = 1000, se = NULL, start = NULL, control = list()) #0.0883418

## AVERAGE LIFESPAN OF A SP CORR FOR BS----

hist(df$foss.age)
plot(df$foss.age ~ log10(df$mass))
summary(lm(df$foss.age ~ log10(df$mass))) #sig buy r2 = 0.03



# x <- df.origin.gen[duplicated(df.origin.gen$index),]
# y.1 <- df.origin.gen[df.origin.gen$index == x$index[1],]
# pacifici.trim[pacifici.trim$binomial == y.1$binomial,] #no information for GenLength == 3206.296, remove; 1st index
# y.2 <- df.origin.gen[df.origin.gen$index == x$index[2],]
# pacifici.trim[pacifici.trim$binomial == y.2$binomial,] #no information for GenLength == 6095.5, remove; 2nd index
# a <- df.origin.gen
# a.1 <- a[!(a$binomial == y.1$binomial[1] & a$GenerationLength_d == y.1$GenerationLength_d[1]),]
# a.2 <- a.1[!(a.1$binomial == y.2$binomial[1] & a.1$GenerationLength_d == y.2$GenerationLength_d[2]),]

