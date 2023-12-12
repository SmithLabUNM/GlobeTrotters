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

#### PLOT THEME ----

## COLOR SCHEME
#South America = #E2C9F2; dark #9A8AA6
#North America = #B4D9C8; dark #748C81
#Africa = #C2D991; dark #7E8C5E
#Eurasia = #F2CDA0; dark #A68C6D
#Australia = #D9967E; dark #8C6151

cont_col <- c("#2ca25f", "#99d8c9", "#e5f5f9")
cont_bw <- c("gray72", "gray47", "black")

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

#### LOAD DATA ----

options(stringsAsFactors = FALSE)

## MOM database
mm.df <- read.csv("./Data/MOMv11.csv", 
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

#### DATA WRANGLE ----

##### FIX DIET -----

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

mm.df$diet.breadth <- select(mm.df, diet.invertivore:diet.piscivore) %>% rowSums()
table(mm.df$diet.breadth[!duplicated(mm.df$binomial)])
# 0    1    2    3 
# 2195 2002 1390  149 

###### MAKE GENERIC & FAMILY AVERAGES -----
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

mm.df$diet.breadth <- select(mm.df, diet.invertivore:diet.piscivore) %>% rowSums()
table(mm.df$diet.breadth[!duplicated(mm.df$binomial)])
# 0    1    2    3 
# 214 3266 1995  261 

length(unique(mm.df$binomial[mm.df$diet.breadth == 0]))

setdiff(unique(mm.df$binomial[mm.df$diet.breadth == 0]), 
        unique(mm.df$binomial[is.na(mm.df$diet.src)]))
View(mm.df[mm.df$diet.breadth == 0 &!(is.na(mm.df$diet.src)),]) #all diets are FALSE

##### ABOUT FULL DATASET -----

###### TOTAL RECORDS -------

nrow(mm.df) #6695

length(unique(mm.df$order)) #31
length(unique(mm.df$family)) #170
length(unique(mm.df$genus)) #1360
length(unique(mm.df$binomial)) #5736

###### INVASIVES & DOMESTICATES ------

invasive <- length(unique(mm.df$binomial[mm.df$extant.status == "introduction"])) #49
domesticated <- length(unique(mm.df$binomial[mm.df$extant.status == "domesticated"])) #3

total <- length(unique(mm.df$binomial)) #5736

(invasive/total)*100 #0.85%

(domesticated/total)*100 #0.05%

intro <- mm.df[mm.df$extant.status == "introduction" |
                   mm.df$extant.status == "domesticated",]
intro <- intro[intro$continent != "Insular",]

intro <- intro %>% 
    mutate(Africa = continent == "Africa",
           North.America = continent == "North.America",
           South.America = continent == "South.America",
           Eurasia = continent == "Eurasia",
           Australia = continent == "Australia")

intro.sums <- intro %>%
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

intro.continent <- intro %>%
    group_by(binomial) %>%
    dplyr::summarise(continent.Africa = as.logical(sum(Africa)),
                     continent.North.America = as.logical(sum(North.America)),
                     continent.South.America = as.logical(sum(South.America)),
                     continent.Eurasia = as.logical(sum(Eurasia)),
                     continent.Australia = as.logical(sum(Australia)))

intro.taxa <- intro[!duplicated(intro$binomial),] %>%
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
# 1  2  3 
# 18 9  2 

intro.contTaxaSums$n.cont <- intro.contTaxaSums %>%
    dplyr::select(starts_with("continent.")) %>%
    rowSums()
table(intro.contTaxaSums$n.cont)
# 1    2    3    5 
# 18   3    7    1 
intro.contTaxaSums$binomial[intro.contTaxaSums$n.cont == 5] #Felis catus

length(unique(intro$binomial)) #29
nrow(intro.contTaxaSums) #29

write.csv(intro.contTaxaSums,
          "./Results/invasive.species.csv",
          row.names = FALSE)

unique(intro.contTaxaSums$binomial)
range(intro.contTaxaSums$avg.mass, na.rm = TRUE)
#16.5 900000.0
median(intro.contTaxaSums$avg.mass, na.rm = TRUE) #48500 (log10: 4.69)
mean(intro.contTaxaSums$avg.mass, na.rm = TRUE) #179554.4 (log10: 5.25)
ggplot() + #do histogram; .25 log 
    geom_histogram(aes(log10(intro.contTaxaSums$avg.mass)), 
                   colour = "gray", fill = "gray",
                   binwidth = .25) +
    plot_theme +
    theme(legend.position = c(0.8, 0.6)) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)))
#sampling from most bins, more in bins 4.25 to 6; most in 4.75

table(intro.contTaxaSums$diet.breadth)
# 1  2  3 
# 18 9  2 

intro.contTaxaSums %>%
    dplyr::group_by(n.cont, diet.breadth) %>%
    dplyr::summarise(n = n())
#no real difference in dietary breadth if you're on 1 or 2 continents
#sp on 3 continents more often have dietary breadth of 1 (5 total) than 2 (2 total)
#the one sp on 5 continents have dietary breadth of 1

intro.contTaxaSums %>%
    dplyr::group_by(n.cont) %>%
    dplyr::summarise(n.carn = sum(diet.carnivore.tot == TRUE),
                     n.invert = sum(diet.invertivore.tot == TRUE),
                     n.frug = sum(diet.frugivore.tot == TRUE),
                     n.graz = sum(diet.grazer.tot == TRUE),
                     n.brows = sum(diet.browser.tot == TRUE),
                     n.pisc = sum(diet.piscivore.tot == TRUE))
#1 continent: highest is brows (11), followed by invert (5)
#2 continent: even (2) for both brows and graz
#3 continent: (4) for graz, (3) for brows, and (2) frug
#5 continent: 1 carn (because domestic cat)

intro.contTaxaSums %>%
    dplyr::group_by(n.cont, family) %>%
    dplyr::summarise(n = n())

intro.contTaxaSums %>%
    dplyr::group_by(n.cont, order) %>%
    dplyr::summarise(n = n())

table(intro.contTaxaSums$order)
# Artiodactyla    Carnivora      Lagomorpha     Perissodactyla    Rodentia   Soricomorpha 
# 14              5              2              2                 5          1 

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

###### BY DIET ------

length(unique(mm.df$binomial[mm.df$diet.browser == TRUE])) #1933
length(unique(mm.df$binomial[mm.df$diet.grazer == TRUE])) #658
length(unique(mm.df$binomial[mm.df$diet.frugivore == TRUE])) #2529
length(unique(mm.df$binomial[mm.df$diet.carnivore == TRUE])) #357
length(unique(mm.df$binomial[mm.df$diet.piscivore == TRUE])) #114
length(unique(mm.df$binomial[mm.df$diet.invertivore == TRUE])) #2448

table(mm.df$diet.breadth[!duplicated(mm.df$binomial)])
# 0    1    2    3 
# 214 3266 1995  261 

###### N INVALID MASS ------

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

###### N DATA DEFICIENT (IUCN) ------

nrow(mm.df[mm.df$iucn.status == "DD",]) #684 records

length(unique(mm.df$order[mm.df$iucn.status == "DD"])) #17
length(unique(mm.df$family[mm.df$iucn.status == "DD"])) #56
length(unique(mm.df$genus[mm.df$iucn.status == "DD"])) #267
length(unique(mm.df$binomial[mm.df$iucn.status == "DD"])) #616

unique(mm.df$continent[mm.df$iucn.status == "DD"]) #all continents

## SIZE
table(mm.df$size.bin[mm.df$iucn.status == "DD"])
# 0  1   2    3    4   5   6    7 
# 1  95  127  42   9   2   11   8 

##### TRIM DATA -----

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

#write.csv(mm.df, "data.trimmed.csv")

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
                binomial)

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

length(unique(mm.df$binomial)) #4386
nrow(df.contTaxaSums) #4386

#write.csv(df.contTaxaSums, "data.long.csv")

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

##### WRITE DATA FOR ANALYSES -----
#write.csv(df, 
#          "./Data/data.for.analyses.csv",
#          row.names = FALSE)

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
length(unique(df$binomial[!is.na(df$GenerationLength_d)])) #3851

## fossil ages
# pbdb
length(unique(df$genus[!is.na(df$foss.age)])) #390
length(unique(df$binomial[df$foss.age > 0])) #662
# faurby
length(unique(df$genus[!is.na(df$age.median)])) #1027
length(unique(df$binomial[!is.na(df$age.median)])) #3968

#### BIASES IN DATA FOR ANALYSES ----

##### TOTAL RECODS ----
nrow(df) #4386
length(unique(df$order)) #29
length(unique(df$family)) #135
length(unique(df$genus)) #1072
length(unique(df$binomial)) #4386

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

#ggsave(bs.cont, file = paste0("./Figures/bodyMassDensityByContinent",".png"), 
#       width = 14, height = 10, units = "cm")

#qualitatively similar

## COUNT OF BODY MASS
nrow(df[!is.na(df$log.mass),]) #3317
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

#write.csv(df.clade.mass,
#          "./Results/clade.missing.mass.csv",
#          row.names = FALSE)

## BY CONTINENT
## are certain continents more affected by this missing data?
nrow(df[df$continent.Africa == TRUE & is.na(df$log.mass),]) #357 (out of 1150; 31%)
nrow(df[df$continent.North.America == TRUE & is.na(df$log.mass),]) #92 (out of 808; 11.3%)
nrow(df[df$continent.South.America == TRUE & is.na(df$log.mass),]) #277 (out of 1200; 14.8%)
nrow(df[df$continent.Eurasia == TRUE & is.na(df$log.mass),]) #325 (out of 1165; 28%
nrow(df[df$continent.Australia == TRUE & is.na(df$log.mass),]) #35 (out of 336; 10%)

##### FOSSIL AGES ----
## PBDB COUNTS
length(unique(df$order[df$foss.age >= 0]))  #21
length(unique(df$family[df$foss.age >= 0])) #93
length(unique(df$genus[df$foss.age >= 0])) #391
length(unique(df$binomial[df$foss.age >= 0])) #662
nrow(df[is.na(df$foss.age),]) #3725

length(unique(df$binomial[df$foss.age >= 0 &
                          df$n.cont == 1])) #574
length(unique(df$binomial[df$foss.age >= 0 &
                            df$n.cont == 2])) #84
length(unique(df$binomial[df$foss.age >= 0 &
                            df$n.cont == "3+"])) #6

## FAURBY COUNTS
length(unique(df$order[df$age.median >= 0]))  #30
length(unique(df$family[df$age.median >= 0])) #136
length(unique(df$genus[df$age.median >= 0])) #1028
length(unique(df$binomial[df$age.median >= 0])) #3969
nrow(df[is.na(df$age.median),]) #418

length(unique(df$binomial[df$age.median >= 0 &
                            df$n.cont == 1])) #3712
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

#ggsave(foss.na, file = paste0("./Figures/PBDBfossilAgeMissing",".png"), 
#       width = 14, height = 10, units = "cm")

# FAURBY
phyl.na <- ggplot() +
  geom_histogram(aes(x = df$age.median[!is.na(df$age.median) & is.na(df$log.mass)])) + 
  plot_theme +
  scale_x_continuous(name = "Phylogenetic Age (Ma)") +
  scale_y_continuous(name = "Number of Missing Mass Values")
#missing a lot of younger fossils, actually

#ggsave(phyl.na, file = paste0("./Figures/PhyloAgeMissing",".png"), 
#       width = 14, height = 10, units = "cm")

## WHICH SIZES DO WE HAVE FOSSIL AGES FOR?
# PBDB
size.fossil.na <- ggplot() +
  geom_histogram(aes(x = df$log.mass[is.na(df$foss.age) & !is.na(df$log.mass)])) + 
  plot_theme +
  scale_x_continuous(name = "Log Body Size (g)") +
  scale_y_continuous(name = "Number of Missing Age Values")
#missing a lot of smaller bodied organisms, of course

#ggsave(size.fossil.na, file = paste0("./Figures/FossilSizeMissing",".png"), 
#       width = 14, height = 10, units = "cm")

# FAURBY
size.phylo.na <- ggplot() +
  geom_histogram(aes(x = df$log.mass[is.na(df$age.median) & !is.na(df$log.mass)])) + 
  plot_theme +
  scale_x_continuous(name = "Log Body Size (g)") +
  scale_y_continuous(name = "Number of Missing Age Values")
#missing a size classes from all over...looks weird

#ggsave(size.phylo.na, file = paste0("./Figures/PhyloSizeMissing",".png"), 
#       width = 14, height = 10, units = "cm")

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
         data = df)) #sig

#write.csv(df.mass.age,
#          "./Results/age.missing.mass.csv",
#          row.names = FALSE)

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

#ggsave(foss.age.lm , file = paste0("./Figures/lm.fossil.age",".png"), 
#       width = 14, height = 10, units = "cm")

summary(lm(df$foss.age ~ df$avg.mass)) #p-value: 3.173e-07; Adjusted R-squared: 0.03849

phyl.age.lm <- ggplot() +
  geom_point(aes(x = df$log.mass[!is.na(df$log.mass) & !is.na(df$age.median)], 
                 y = df$age.median[!is.na(df$log.mass) & !is.na(df$age.median)])) +
  geom_smooth(aes(x = df$log.mass[!is.na(df$log.mass) & !is.na(df$age.median)], 
                  y = df$age.median[!is.na(df$log.mass) & !is.na(df$age.median)])) +
  plot_theme + 
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_y_continuous(name = "Median Phylogenetic Age (Genus)")

#ggsave(phyl.age.lm , file = paste0("./Figures/lm.phylo.age",".png"), 
#       width = 14, height = 10, units = "cm")

summary(lm(df$age.median ~ df$avg.mass)) #p-value: 0.001473; Adjusted R-squared: 0.002964

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

#write.csv(df.clade.foss,
#          "./Results/foss.missing.clade.csv",
#          row.names = FALSE)

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

#write.csv(df.cont.foss,
#          "./Results/DataExploration/foss.missing.cont.csv",
#          row.names = FALSE)

##### DISPERSAL ------
nrow(df[!is.na(df$dispersal.age.d),])

table(df$log.size.bin[!is.na(df$dispersal.age.d)])
# 1  2  3  4  5  6 
# 3 24 44 24 12  2 
table(df$log.size.bin[is.na(df$dispersal.age.d)])
# 0    1    2    3    4    5    6    7 
# 385 1312  696  392  229  148   44    2 

nrow(df[!is.na(df$dispersal.age.d),]) #109
nrow(df[!is.na(df$GenerationLength_d),]) #3851

##### CONTINENT OF ORIGIN ------
## FAMILY ORIGIN

length(unique(df$family[df$family.origin != ""])) #128
nrow(df[df$family.origin != "",]) #3572

length(unique(df$family[df$family.origin == ""])) #7
length(df$binomial[df$family.origin == ""]) #814
table(df$order[df$family.origin == ""])
# "Chiroptera"  "Rodentia" "Paucituberculata" "Carnivora"  "Artiodactyla"  
#  87            713        6                  1            7

length(unique(df$family[df$family.origin == "" & df$order == "Chiroptera"])) #2
unique(df$family[df$family.origin == "" & df$order == "Chiroptera"]) 
#Molossidae and Nycteridae
nrow(df[df$family.origin == "" &
          df$family == "Molossidae",])

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
nrow(df[df$family.origin != "",]) #3572
df %>%
  filter(family.origin != "") %>%
  group_by(family.origin) %>%
  summarise(N = n())
# family.origin     N
# Africa          319 (9%)
# Australia       205 (5.7%)
# Eurasia        1754 (49%)
# North.America   636 (17.8%)
# South.America   658 (18.4%)

## SIZE
nrow(df[!is.na(df$family.origin) & is.na(df$log.mass),]) #1069

##### CONTINENT -----

## counts
length(unique(df$binomial[df$continent.North.America == TRUE])) #808
length(unique(df$binomial[df$continent.South.America == TRUE])) #1200
length(unique(df$binomial[df$continent.Eurasia == TRUE])) #1165
length(unique(df$binomial[df$continent.Africa == TRUE])) #1150
length(unique(df$binomial[df$continent.Australia == TRUE])) #336

#want to check for taxonomic and geographic coverage
cont.counts <- df %>%
  dplyr::select(starts_with("continent.")) %>%
  dplyr::summarise_all(sum)
#  Africa   North.America   South.America   Eurasia   Australia
#  1150     808             1200            1165      336

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

#write.csv(df.cont.mass,
#          "./Results/cont.missing.mass.csv",
#          row.names = FALSE)

##### DIET TYPE ----

## missing diet for?
diet.counts <- df %>%
  dplyr::select(starts_with("diet.")) %>%
  dplyr::summarise_all(sum)

unique(df$diet.breadth)
table(df$diet.breadth)

## BY SIZE
# which diet types are missing the most mass data?
nrow(df[is.na(df$log.mass),])


df$mass.TF <- !is.na(df$avg.mass) #TRUE = mass, FALSE = no mass

df.melt <- melt(df, measure.vars = c(6:11),
                variable.name = "diet.type",
                value.name = "has.diet")
df.diet <- df.melt[df.melt$has.diet == TRUE,]

df.diet %>%
  group_by(diet.type) %>%
  summarise(n.mass = sum(mass.TF == TRUE),
            n.na = sum(mass.TF == FALSE))
#diet.type              n.mass  n.na
# diet.invertivore.tot   1419   553
# diet.carnivore.tot      281    49
# diet.browser.tot       1239   381
# diet.grazer.tot         493   121
# diet.piscivore.tot       28     1
# diet.frugivore.tot     1460   469

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

#write.csv(df.diet.mass,
#          "./Results/diet.missing.mass.csv",
#          row.names = FALSE)

##### DIET BREADTH ----

table(df$diet.breadth)
#1    2     3 
#2463 1737  186 

##### N DATA DEFICIENT (IUCN) -----

dd <- df[df$iucn == "DD",]
#[2 rows are NA]

nrow(dd) #434 records

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
#Afrosoricida    Artiodactyla       Carnivora      Chiroptera 
#3              10               3             103 
#Cingulata  Dasyuromorphia Didelphimorphia  Erinaceomorpha 
#5               2              10               1 
#Lagomorpha   Macroscelidea        Primates        Rodentia 
#3               3               4             222 
#Soricomorpha 
#63 
#most Chiroptera, Rodents
#may be why we don't have information for ranges?

##what is overlap between DD and other missing information?

#missing size
View(dd[is.na(dd$avg.mass),]) #249 (out of 434; 57%); 2 rows NA

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
  select(-foss.avg.age) %>%
  drop_na() %>%
  summarise(n = n()) #80 for phylo age


#### Q1: NUM SP PER CONTINENT ----
#Tally number of species on 1, 2, or 3+ continents

tot <- length(unique(df$binomial)) #4386
n.one <- length(unique(df$binomial[df$n.cont == 1])) #4120
per.one <- n.one/tot #93.94%
n.two <- length(unique(df$binomial[df$n.cont == 2])) #260
per.two <- n.two/tot #5.93%
n.more <- length(unique(df$binomial[df$n.cont == "3+"])) #6
per.more <- n.more/tot #0.14%

unique(df[which(df$n.cont == "3+"), c("order", "family", "binomial")])
# "Miniopterus schreibersii" "Mustela nivalis"          "Vulpes vulpes"           
# "Ursus arctos"             "Cervus elaphus"           "Panthera leo" 

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
#8 species similar to fox (including fox)
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
          df$diet.breadth == 2,])
#2 (including Ursus arctos)
#other one is cave bear and Ursus americanus; both on 1

xx <- df[df$family.origin == "North.America" & 
           df$log.size.bin == 5 & 
           df$diet.breadth >= 2,]
xx[xx$diet.carnivore.tot == TRUE | 
     xx$diet.frugivore.tot == TRUE |
     xx$diet.piscivore.tot == TRUE,] #if allow for fish too
#JUST BEARS

## Mustela nivalis
nrow(df[df$family.origin == "Eurasia" & 
          df$log.size.bin == 2 & 
          df$diet.carnivore.tot == TRUE & 
          df$diet.invertivore.tot == TRUE &
          df$diet.breadth == 2,])
# 13 species (including Mustela are similar)
# 1 on two; the rest on 1

must.sp <- df$binomial[df$family.origin == "Eurasia" & 
                df$log.size.bin == 2 & 
                df$diet.carnivore.tot == TRUE & 
                df$diet.invertivore.tot == TRUE &
                df$diet.breadth == 2]
View(mm.df[mm.df$binomial %in% must.sp,])

## Cervus elaphus
nrow(df[df$family.origin == "Eurasia" & 
          df$log.size.bin == 5 & 
          df$diet.browser.tot == TRUE & 
          df$diet.breadth == 1,])
# 16 things similar (including Cervus elephas)
cerv.sp <- df$binomial[df$family.origin == "Eurasia" & 
                df$log.size.bin == 5 & 
                df$diet.browser.tot == TRUE & 
                df$diet.breadth == 1]
View(mm.df[mm.df$binomial %in% cerv.sp,])

## Panthera leo
df[df$family.origin == "Eurasia" & 
   df$log.size.bin == 5 & 
   df$diet.carnivore.tot == TRUE & 
   df$diet.breadth == 1,]
#6 things including P. leo similar; all Felids

fel.sp <- df$binomial[df$family.origin == "Eurasia" & 
               df$log.size.bin == 5 & 
               df$diet.carnivore.tot == TRUE & 
               df$diet.breadth == 1]
View(mm.df[mm.df$binomial %in% fel.sp,])

## Miniopterus schreibersii
yy <- df[df$family.origin == "North.America" & 
         df$log.size.bin == 1 & 
         df$diet.invertivore.tot == TRUE & 
         df$diet.breadth == 1,]
yy <- yy %>% drop_na(binomial)
nrow(yy) #68 incl. bat
table(yy$n.cont)
# 1  2   3+ 
# 58 11  1 
nrow(yy[yy$continent.Africa == TRUE,]) #18
nrow(yy[yy$continent.North.America == TRUE,]) #16
nrow(yy[yy$continent.South.America == TRUE,]) #9
nrow(yy[yy$continent.Eurasia == TRUE,]) #32
nrow(yy[yy$continent.Australia == TRUE,]) #8
bat.sp <- yy$binomial
View(mm.df[mm.df$binomial %in% bat.sp,])

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
#write.csv(indeces, 
#          "./Results/sorensen.index.csv",
#          row.names = FALSE)

##which two continents are limited dispersers on?
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
lim.disp <- df[df$n.cont == 2,]
lim.cont <- lim.disp %>%
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
lim.cont <- lim.disp %>%
  group_by(order, n.cont) %>%
  dplyr::summarise(count = n()) %>%
  as.data.frame()
marsup <- c("Didelphimorphia", "Paucituberculata", "Microbiotheria",
            "Dasyuromorphia", "Peramelemorphia", "Notoryctemorphia",
            "Diprotodontia")
lim.cont[lim.cont$order %in% marsup,] #only didelmorphia
length(unique(df$binomial[df$order == "Didelphimorphia"])) #87 total
length(unique(lim.disp$binomial[lim.disp$order == "Didelphimorphia"])) #5 (5.755 of total Didelmorphia diversity (5/87))
length(unique(df$binomial[df$order == "Didelphimorphia" &
                            df$continent.South.America == TRUE])) #83 (represent 6%; 83/1200)
length(unique(df$binomial[df$continent.South.America == TRUE])) #1200
length(unique(df$binomial[df$continent.South.America == TRUE &
                            df$n.cont == 2])) #162

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
length(df$family.origin[df$family.origin != ""]) #3572

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
#2. looking at family level
nrow(df[df$family.origin == "Eurasia",])
df %>% 
  group_by(n.cont) %>%
  summarise(n.sp.af = sum(family.origin == "Africa"), #316 have Africa as origin
            n.sp.na = sum(family.origin == "North.America"), #636 have as origin
            n.sp.aus = sum(family.origin == "Australia"), #205 have as origin
            n.sp.sa = sum(family.origin == "South.America"), #658 have as origin
            n.sp.ea = sum(family.origin == "Eurasia")) #1754

unique(df$family[df$family.origin == ""]) #only 7 families; most small groups but really diverse
# Molossidae, Muridae, and Sciuridae are super diverse
length(df$binomial[df$family == "Muridae"]) #486
length(df$binomial[df$family == "Sciuridae"]) #227

nrow(df[df$n.cont == 2 & df$family == "Molossidae",]) #19
#3 Molossidae on Africa and Eurasia, the rest are on N & S america
nrow(df[df$n.cont == 2 & df$family == "Sciuridae",]) #2
nrow(df[df$n.cont == 2 & df$family == "Muridae",]) #14
#all Muridae on Africa and Eurasia


homies <- df[df$n.cont == 1,]
limited <- df[df$n.cont == 2,]
trotter <- df[df$n.cont == "3+",]

#homies
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

#write.csv(homies.origin, 
#          "./Results/homies.family.origin.csv",
#          row.names = FALSE)

unique(homies$order[homies$family.origin == "Africa"])

##FIGURE
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

#write.csv(homies.origin.melt,
#          "./Results/homebodies.origin.results.csv",
#          row.names = FALSE)

##pie chart
## COLOR SCHEME
#South America = #E2C9F2; dark #9A8AA6
#North America = #B4D9C8; dark #748C81
#Africa = #C2D991; dark #7E8C5E
#Eurasia = #F2CDA0; dark #A68C6D
#Australia = #D9967E; dark #8C6151

p <- ggplot(homies.origin.melt, aes(x = "", y = value, fill = family.origin.per)) +
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

##limited dispersers
limited.origin <- limited %>%
  group_by(family.origin) %>%
  dplyr::summarise(N = n(),
                   N.Africa = length(continent.Africa[continent.Africa == TRUE]),
                   N.Australia = length(continent.Australia[continent.Australia == TRUE]),
                   N.South.America = length(continent.South.America[continent.South.America == TRUE]),
                   N.North.America = length(continent.North.America[continent.North.America == TRUE]),
                   N.Eurasia = length(continent.Eurasia[continent.Eurasia == TRUE])) %>%
  as.data.frame() 
limited.origin <- limited.origin[limited.origin$family.origin != "",]

#get proportions
limited.origin$prop.spread <- ""
limited.origin$prop.spread[limited.origin$family.origin == "Africa"] <- limited.origin$N.Africa[limited.origin$family.origin == "Africa"]/limited.origin$N[limited.origin$family.origin == "Africa"]
limited.origin$prop.spread[limited.origin$family.origin == "Eurasia"] <- limited.origin$N.Eurasia[limited.origin$family.origin == "Eurasia"]/limited.origin$N[limited.origin$family.origin == "Eurasia"]
limited.origin$prop.spread[limited.origin$family.origin == "Australia"] <- limited.origin$N.Australia[limited.origin$family.origin == "Australia"]/limited.origin$N[limited.origin$family.origin == "Australia"]
limited.origin$prop.spread[limited.origin$family.origin == "South.America"] <- limited.origin$N.South.America[limited.origin$family.origin == "South.America"]/limited.origin$N[limited.origin$family.origin == "South.America"]
limited.origin$prop.spread[limited.origin$family.origin == "North.America"] <- limited.origin$N.North.America[limited.origin$family.origin == "North.America"]/limited.origin$N[limited.origin$family.origin == "North.America"]
#no jumpers in North.America
#no spreaders from Australia

limited.origin$prop.jump <- 1- as.numeric(limited.origin$prop.spread)

#want to know where the jumpers and spreaders went to
limited.cont <- limited %>%
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
limited.cont <- limited.cont[limited.cont$family.origin != "",]
#Australia not here...? No family now on two continents originated in Australia

limited.cont$prop.spread <- ""
limited.cont$prop.jump <- ""

limited.cont$prop.spread[limited.cont$family.origin == "Africa"] <- sum(limited.cont$N.Africa.Eurasia[limited.cont$family.origin == "Africa"] + 
                                                                          limited.cont$N.Africa.Australia[limited.cont$family.origin == "Africa"] + 
                                                                          limited.cont$N.Africa.North.America[limited.cont$family.origin == "Africa"] +
                                                                          limited.cont$N.Africa.South.America[limited.cont$family.origin == "Africa"])/limited.cont$N[limited.cont$family.origin == "Africa"]
limited.cont$prop.jump[limited.cont$family.origin == "Africa"] <- 1 - as.numeric(limited.cont$prop.spread[limited.cont$family.origin == "Africa"])

limited.cont$prop.stay[limited.cont$family.origin == "Africa"] <- sum(limited.cont$N.Africa.Eurasia[limited.cont$family.origin == "Africa"] + 
                                                                        limited.cont$N.Africa.Australia[limited.cont$family.origin == "Africa"] + 
                                                                        limited.cont$N.Africa.North.America[limited.cont$family.origin == "Africa"] +
                                                                        limited.cont$N.Africa.South.America[limited.cont$family.origin == "Africa"])/sum(limited.cont$N)
limited.cont$prop.leave[limited.cont$family.origin == "Africa"] <- (limited.cont$N[limited.cont$family.origin == "Africa"] - sum(limited.cont$N.Africa.Eurasia[limited.cont$family.origin == "Africa"] + 
                                                                                                                                   limited.cont$N.Africa.Australia[limited.cont$family.origin == "Africa"] + 
                                                                                                                                   limited.cont$N.Africa.North.America[limited.cont$family.origin == "Africa"] +
                                                                                                                                   limited.cont$N.Africa.South.America[limited.cont$family.origin == "Africa"]))/as.numeric(sum(limited.cont$N))


limited.cont$prop.spread[limited.cont$family.origin == "Eurasia"] <- sum(limited.cont$N.Africa.Eurasia[limited.cont$family.origin == "Eurasia"] + 
                                                                           limited.cont$N.Australia.Eurasia[limited.cont$family.origin == "Eurasia"] + 
                                                                           limited.cont$N.Eurasia.North.America[limited.cont$family.origin == "Eurasia"] + 
                                                                           limited.cont$N.Eurasia.South.America[limited.cont$family.origin == "Eurasia"])/limited.cont$N[limited.cont$family.origin == "Eurasia"]
limited.cont$prop.jump[limited.cont$family.origin == "Eurasia"] <- 1 - as.numeric(limited.cont$prop.spread[limited.cont$family.origin == "Eurasia"])


limited.cont$prop.stay[limited.cont$family.origin == "Eurasia"] <- sum(limited.cont$N.Africa.Eurasia[limited.cont$family.origin == "Eurasia"] + 
                                                                         limited.cont$N.Australia.Eurasia[limited.cont$family.origin == "Eurasia"] + 
                                                                         limited.cont$N.Eurasia.North.America[limited.cont$family.origin == "Eurasia"] + 
                                                                         limited.cont$N.Eurasia.South.America[limited.cont$family.origin == "Eurasia"])/sum(limited.cont$N)
limited.cont$prop.leave[limited.cont$family.origin == "Eurasia"] <- (limited.cont$N[limited.cont$family.origin == "Eurasia"] - sum(limited.cont$N.Africa.Eurasia[limited.cont$family.origin == "Eurasia"] + 
                                                                                                                                   limited.cont$N.Australia.Eurasia[limited.cont$family.origin == "Eurasia"] + 
                                                                                                                                   limited.cont$N.Eurasia.North.America[limited.cont$family.origin == "Eurasia"] + 
                                                                                                                                   limited.cont$N.Eurasia.South.America[limited.cont$family.origin == "Eurasia"]))/as.numeric(sum(limited.cont$N))



limited.cont$prop.spread[limited.cont$family.origin == "North.America"] <- sum(limited.cont$N.Africa.North.America[limited.cont$family.origin == "North.America"] + 
                                                                                 limited.cont$N.Australia.North.America[limited.cont$family.origin == "North.America"] + 
                                                                                 limited.cont$N.Eurasia.North.America[limited.cont$family.origin == "North.America"] + 
                                                                                 limited.cont$N.South.America.North.America[limited.cont$family.origin == "North.America"])/limited.cont$N[limited.cont$family.origin == "North.America"]
limited.cont$prop.jump[limited.cont$family.origin == "North.America"] <- 1 - as.numeric(limited.cont$prop.spread[limited.cont$family.origin == "North.America"])


limited.cont$prop.stay[limited.cont$family.origin == "North.America"] <- sum(limited.cont$N.Africa.North.America[limited.cont$family.origin == "North.America"] + 
                                                                               limited.cont$N.Australia.North.America[limited.cont$family.origin == "North.America"] + 
                                                                               limited.cont$N.Eurasia.North.America[limited.cont$family.origin == "North.America"] + 
                                                                               limited.cont$N.South.America.North.America[limited.cont$family.origin == "North.America"])/sum(limited.cont$N)
limited.cont$prop.leave[limited.cont$family.origin == "North.America"] <- (limited.cont$N[limited.cont$family.origin == "North.America"] - sum(limited.cont$N.Africa.North.America[limited.cont$family.origin == "North.America"] + 
                                                                                                                                                 limited.cont$N.Australia.North.America[limited.cont$family.origin == "North.America"] + 
                                                                                                                                                 limited.cont$N.Eurasia.North.America[limited.cont$family.origin == "North.America"] + 
                                                                                                                                                 limited.cont$N.South.America.North.America[limited.cont$family.origin == "North.America"]))/as.numeric(sum(limited.cont$N))

limited.cont$prop.spread[limited.cont$family.origin == "South.America"] <- sum(limited.cont$N.Africa.South.America[limited.cont$family.origin == "South.America"] + 
                                                                                 limited.cont$N.Australia.South.America[limited.cont$family.origin == "South.America"] + 
                                                                                 limited.cont$N.Eurasia.South.America[limited.cont$family.origin == "South.America"] + 
                                                                                 limited.cont$N.South.America.North.America[limited.cont$family.origin == "South.America"])/limited.cont$N[limited.cont$family.origin == "South.America"]
limited.cont$prop.jump[limited.cont$family.origin == "South.America"] <- 1 - as.numeric(limited.cont$prop.spread[limited.cont$family.origin == "South.America"])
#100% spread

limited.cont$prop.stay[limited.cont$family.origin == "South.America"] <- sum(limited.cont$N.Africa.South.America[limited.cont$family.origin == "South.America"] + 
                                                                               limited.cont$N.Australia.South.America[limited.cont$family.origin == "South.America"] + 
                                                                               limited.cont$N.Eurasia.South.America[limited.cont$family.origin == "South.America"] + 
                                                                               limited.cont$N.South.America.North.America[limited.cont$family.origin == "South.America"])/sum(limited.cont$N)
limited.cont$prop.leave[limited.cont$family.origin == "South.America"] <- (limited.cont$N[limited.cont$family.origin == "South.America"] - sum(limited.cont$N.Africa.South.America[limited.cont$family.origin == "South.America"] + 
                                                                                                                                                 limited.cont$N.Australia.South.America[limited.cont$family.origin == "South.America"] + 
                                                                                                                                                 limited.cont$N.Eurasia.South.America[limited.cont$family.origin == "South.America"] + 
                                                                                                                                                 limited.cont$N.South.America.North.America[limited.cont$family.origin == "South.America"]))/as.numeric(sum(limited.cont$N))


sum(limited.cont$prop.leave)+sum(limited.cont$prop.stay)
#write.csv(limited.cont, 
#          "./Results/limited.family.origin.csv",
#          row.names = FALSE)

##FIGURE
limited.cont$per.stay <- as.numeric(limited.cont$prop.stay)*100
limited.cont$per.leave <- as.numeric(limited.cont$prop.leave)*100

limited.cont.melt <- melt(limited.cont, 
                          id.vars = "family.origin",
                          measure.vars = c("per.stay",
                                           "per.leave"),
                          variable.name = "per")
limited.cont.melt$family.origin.per <- paste(limited.cont.melt$family.origin, 
                                             limited.cont.melt$per,
                                             sep = ".")

##pie chart
## COLOR SCHEME
#South America = #E2C9F2; dark #9A8AA6
#North America = #B4D9C8; dark #748C81
#Africa = #C2D991; dark #7E8C5E
#Eurasia = #F2CDA0; dark #A68C6D
#Australia = #D9967E; dark #8C6151

q <- ggplot(limited.cont.melt, aes(x = "", y = value, fill = family.origin.per)) +
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

##trotter
trotter.origin <- trotter %>%
  group_by(family.origin) %>%
  dplyr::summarise(N = n(),
                   N.Africa = length(continent.Africa[continent.Africa == TRUE]),
                   N.Australia = length(continent.Australia[continent.Australia == TRUE]),
                   N.South.America = length(continent.South.America[continent.South.America == TRUE]),
                   N.North.America = length(continent.North.America[continent.North.America == TRUE]),
                   N.Eurasia = length(continent.Eurasia[continent.Eurasia == TRUE])) %>%
  as.data.frame() 
trotter.origin <- trotter.origin[trotter.origin$family.origin != "",]

#get proportions
trotter.origin$prop.spread <- ""
trotter.origin$prop.spread[trotter.origin$family.origin == "Africa"] <- trotter.origin$N.Africa[trotter.origin$family.origin == "Africa"]/trotter.origin$N[trotter.origin$family.origin == "Africa"]
trotter.origin$prop.spread[trotter.origin$family.origin == "Eurasia"] <- trotter.origin$N.Eurasia[trotter.origin$family.origin == "Eurasia"]/trotter.origin$N[trotter.origin$family.origin == "Eurasia"]
trotter.origin$prop.spread[trotter.origin$family.origin == "Australia"] <- trotter.origin$N.Australia[trotter.origin$family.origin == "Australia"]/trotter.origin$N[trotter.origin$family.origin == "Australia"]
trotter.origin$prop.spread[trotter.origin$family.origin == "South.America"] <- trotter.origin$N.South.America[trotter.origin$family.origin == "South.America"]/trotter.origin$N[trotter.origin$family.origin == "South.America"]
trotter.origin$prop.spread[trotter.origin$family.origin == "North.America"] <- trotter.origin$N.North.America[trotter.origin$family.origin == "North.America"]/trotter.origin$N[trotter.origin$family.origin == "North.America"]
#no jumpers in North.America
#no spreaders from Australia

trotter.origin$prop.jump <- 1- as.numeric(trotter.origin$prop.spread)

#want to know where the jumpers and spreaders went to
trotter.cont <- trotter %>%
  group_by(family.origin) %>%
  dplyr::summarise(N = n(),
                   N.Africa.Eurasia.North.America = length(continent.Africa[continent.Africa == TRUE & continent.Eurasia == TRUE & continent.North.America == TRUE]),
                   N.Eurasia.North.America.South.America = length(continent.Eurasia[continent.Eurasia == TRUE & continent.North.America == TRUE & continent.South.America == TRUE]),
                   N.Africa.Eurasia.Australia = length(continent.Africa[continent.Africa == TRUE & continent.Eurasia == TRUE & continent.Australia == TRUE])) %>%
  as.data.frame() 
trotter.cont <- trotter.cont[trotter.cont$family.origin != "",]
#write.csv(trotter.cont, 
#          "./Results/trotter.family.origin.csv",
#          row.names = FALSE)

length(df$binomial[df$family.origin == "South.America"]) #658
length(df$binomial[df$family.origin == "South.America" &
                     df$n.cont == 2]) #83
length(df$binomial[df$family.origin == "North.America"]) #636
length(df$binomial[df$family.origin == "North.America" &
                     df$n.cont == 2]) #46
length(df$binomial[df$family.origin == "Eurasia"]) #1754
length(df$binomial[df$family.origin == "Eurasia" &
                     df$n.cont == 2]) #75

homies.origin$N.jump[homies.origin$family.origin == "Africa"] <- as.numeric(homies.origin$total[homies.origin$family.origin == "Africa"] - homies.origin$N.Africa[homies.origin$family.origin == "Africa"])
homies.origin$prop.origin[homies.origin$family.origin == "Africa"] <- as.numeric(homies.origin$N.Africa[homies.origin$family.origin == "Africa"]/homies.origin$total[homies.origin$family.origin == "Africa"])
homies.origin$prop.jump[homies.origin$family.origin == "Africa"] <- as.numeric(homies.origin$N.jump[homies.origin$family.origin == "Africa"])/as.numeric(homies.origin$total[homies.origin$family.origin == "Africa"])

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

limited.family <- family.origin[family.origin$n.cont == 2,]
colnames(limited.family)[colnames(limited.family) == "N"] <- "limited.N"
limited.family <- limited.family %>%
  dplyr::select(-n.cont)

trotter.family <- family.origin[family.origin$n.cont == "3+",]
colnames(trotter.family)[colnames(trotter.family) == "N"] <- "trotter.N"
trotter.family <- trotter.family %>%
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

length(df.dispersal$dispersal.foss[!is.na(df.dispersal$dispersal.foss)]) #69

ggplot() +
  geom_density(aes(df.dispersal$dispersal.phylo[!is.na(df.dispersal$dispersal.phylo)]))
length(df.dispersal$dispersal.phylo[!is.na(df.dispersal$dispersal.phylo)]) #85

nrow(df.dispersal[is.na(df.dispersal$dispersal.foss),]) #541
nrow(df.dispersal[is.na(df.dispersal$dispersal.phylo),]) #526
length(unique(df.dispersal$family[is.na(df.dispersal$dispersal.foss)])) #85
length(unique(df.dispersal$family[is.na(df.dispersal$dispersal.phylo)])) #83

table(df.dispersal$order[is.na(df.dispersal$dispersal.foss)])
table(df.dispersal$order[is.na(df.dispersal$dispersal.phylo)])

nrow(df.dispersal[!is.na(df.dispersal$dispersal.foss) &
                    df.dispersal$order == "Rodentia",]) #12
nrow(df.dispersal[!is.na(df.dispersal$dispersal.foss) &
                    df.dispersal$order == "Primates",]) #0

table(df.dispersal$family.origin[is.na(df.dispersal$dispersal.foss) &
                                   df.dispersal$order == "Primates"])

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

length(df.dispersal$binomial[!is.na(df.dispersal$dispersal.foss)]) #69

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

hist(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]))
hist(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]))

min(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]) #21.2
max(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]) #2949986

min(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]) #4.5
max(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]) #3940034

ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]))
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]) and log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)])
# D = 0.35218, p-value = 6.212e-07
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
# D^- = 0.35463, p-value = 2.053e-07
# alternative hypothesis: the CDF of x lies below that of y

min(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]) #same as above
max(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)])

min(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])
max(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])

ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)]))
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]) and log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])
# D = 0.37078, p-value = 3.638e-09
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
# D^- = 0.37078, p-value = 1.819e-09
# alternative hypothesis: the CDF of x lies below that of y

table(df.dispersal$n.cont)
# 1   2  3+ 
# 545  60   4 

table(df.dispersal$n.cont[!is.na(df.dispersal$dispersal.foss)])
# 1  2 3+ 
# 45 20  4

table(df.dispersal$n.cont[!is.na(df.dispersal$dispersal.phylo)])
# 1  2 3+ 
# 60 21  4

#unimpeded animals can get across continents; clearly some filtering
#is the filtering clade or ecological type specific? answer than by looking at families or something
#problem with home range: already constricted by filtering of some sort

#### H3: DIVERSITY OF CLADE ----

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

limited.family <- family[family$n.cont == 2,]
colnames(limited.family)[colnames(limited.family) == "N"] <- "limited.N"
limited.family <- limited.family %>%
  dplyr::select(-n.cont)

trotter.family <- family[family$n.cont == "3+",]
colnames(trotter.family)[colnames(trotter.family) == "N"] <- "trotter.N"
trotter.family <- trotter.family %>%
  dplyr::select(-n.cont)

#create full dataset
fam.null.trot <- merge(null.family, trotter.family, by = "family", all.x = TRUE, all.y = TRUE)
fam.null.trot.lim <- merge(fam.null.trot, limited.family, by = "family", all.x = TRUE, all.y = TRUE)
fam.null.trol.lim.homies <- merge(fam.null.trot.lim, homies.family, by = "family", all.x = TRUE, all.y = TRUE)

df.family <- fam.null.trol.lim.homies
df.family[is.na(df.family)] <- 0

df.family$prop.null <- df.family$null.N/nrow(df)

df.family$prop.homies <- df.family$homies.N/nrow(df[df$n.cont == "1",])
df.family$prop.lim <- df.family$limited.N/nrow(df[df$n.cont == "2",])
df.family$prop.trot <- df.family$trotter.N/nrow(df[df$n.cont == "3+",])

#binomial test
for(i in 1:nrow(df.family)){
  test <- binom.test(df.family$homies.N[i], nrow(df[df$n.cont == "1",]), p = df.family$prop.null[i], alternative = "two.sided")
  df.family$p.homies[i] <- test$p.value
}

for(i in 1:nrow(df.family)){
  test <- binom.test(df.family$limited.N[i], nrow(df[df$n.cont == "2",]), p = df.family$prop.null[i], alternative = "two.sided")
  df.family$p.lim[i] <- test$p.value
}

for(i in 1:nrow(df.family)){
  test <- binom.test(df.family$trotter.N[i], nrow(df[df$n.cont == "3+",]), p = df.family$prop.null[i], alternative = "two.sided")
  df.family$p.trot[i] <- test$p.value
}

#add sidak correction
df.family <- arrange(df.family, p.homies) %>%
  dplyr::mutate(signif.homies = p.homies < 0.05,
                signif.bonferoni.homies = p.homies < 0.05/n(),
                signif.holm.homies = !0.05/(n() + 1 - 1:n()) < p.homies,
                signif.sidak.homies = p.homies < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.homies = !(1 - (1 - 0.05)^(1/n())) < p.homies)

df.family <- arrange(df.family, p.lim) %>%
  dplyr::mutate(signif.lim = p.lim < 0.05,
                signif.bonferoni.lim = p.lim < 0.05/n(),
                signif.holm.lim = !0.05/(n() + 1 - 1:n()) < p.lim,
                signif.sidak.lim = p.lim < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.lim = !(1 - (1 - 0.05)^(1/n())) < p.lim)

df.family <- arrange(df.family, p.trot) %>%
  dplyr::mutate(signif.trot = p.trot < 0.05,
                signif.bonferoni.trot = p.trot < 0.05/n(),
                signif.holm.trot = !0.05/(n() + 1 - 1:n()) < p.trot,
                signif.sidak.trot = p.trot < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.trot = !(1 - (1 - 0.05)^(1/n())) < p.trot)

#write.csv(df.family, 
#          "./Results/family.results.csv",
#          row.names = FALSE)

##maybe do at order level; maybe don't care about continent of origin for order because too far in past, and families maybe sp w/in families more ecol similar and so behave similarly

#test for body size bias
##limited dispersers
lim.true <- subset(df.family, df.family$signif.sidak.lim == TRUE, select = c(family,
                                                                             null.N,
                                                                             limited.N,
                                                                             prop.null,
                                                                             prop.lim,
                                                                             signif.sidak.lim))

lim.false <- subset(df.family, df.family$signif.sidak.lim == FALSE, select = c(family,
                                                                               null.N,
                                                                               limited.N,
                                                                               prop.null,
                                                                               prop.lim,
                                                                               signif.sidak.lim))

family.lim.true <- df[df$family %in% lim.true$family,]
hist(log10(family.lim.true$avg.mass))

family.lim.false <- df[df$family %in% lim.false$family,]
hist(log10(family.lim.false$avg.mass))

family.lim.true.na <- family.lim.true %>%
  drop_na(avg.mass)

family.lim.false.na <- family.lim.false %>%
  drop_na(avg.mass)

ks.test(family.lim.true.na$avg.mass, 
        family.lim.false.na$avg.mass) #sig diff
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  family.lim.true.na$avg.mass and family.lim.false.na$avg.mass
# D = 0.36586, p-value < 2.2e-16
# alternative hypothesis: two-sided

ks.test(family.lim.true$avg.mass, family.lim.false$avg.mass, alternative = "greater") ##sig
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  family.lim.true$avg.mass and family.lim.false$avg.mass
# D^+ = 0.36586, p-value < 2.2e-16
# alternative hypothesis: the CDF of x lies above that of y

ks.test(family.lim.true$avg.mass, family.lim.false$avg.mass, alternative = "less") #not sig
# Asymptotic two-sample Kolmogorov-Smirnov test
# 
# data:  family.lim.true$avg.mass and family.lim.false$avg.mass
# D^- = -3.8164e-17, p-value = 1
# alternative hypothesis: the CDF of x lies below that of y

##globe trotters
trot.true <- subset(df.family, df.family$signif.sidak.trot == TRUE, select = c(family,
                                                                               null.N,
                                                                               trotter.N,
                                                                               prop.null,
                                                                               prop.trot,
                                                                               signif.sidak.trot))
#THERE AREN'T ANY

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

#### H4: BODY SIZE ----

global.mass <- df$log.mass[!is.na(df$log.mass)] #3310 records
median(global.mass) #1.95424
median(df$avg.mass[!is.na(df$avg.mass)]) #89.9995

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
ks.test(ld.mass, global.mass, alternative = "greater") #ld smaller than global
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


##### FIGURE ----

cb_viridis <- c("#FDE725FF", #3+
                "#1F9E89FF", #2 
                "#482878FF") #1 

ggplot() + #do histogram; .25 log 
  geom_histogram(aes(df$log.mass[df$n.cont == "1" & !is.na(df$log.mass)]), 
               colour = "#482878FF", fill = "#482878FF",
               binwidth = .25) +
  geom_histogram(aes(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass)]), 
               colour = "#1F9E89FF", fill = "#1F9E89FF",
               binwidth = .25) +
  geom_histogram(aes(df$log.mass[df$n.cont == "3+" & !is.na(df$log.mass)]), 
               colour = "#FDE725FF", fill = "#FDE725FF",
               binwidth = .25) +
  plot_theme +
  theme(legend.position = c(0.8, 0.6)) +
  scale_fill_manual(values = cont_col, 
                    name="Number of Continents",
                    labels = c("1",
                               "2",
                               "3+")) +
  scale_y_continuous(name = "Count") +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)))

ggplot() + #do histogram; .25 log 
    geom_histogram(aes(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass)]), 
                   colour = "#1F9E89FF", fill = "#1F9E89FF",
                   binwidth = .25) +
    geom_histogram(aes(df$log.mass[df$n.cont == "3+" & !is.na(df$log.mass)]), 
                   colour = "#FDE725FF", fill = "#FDE725FF",
                   binwidth = .25) +
    plot_theme +
    theme(legend.position = c(0.6, 0.82)) +
    scale_fill_manual(values = cont_col, 
                      name="Number of Continents",
                      labels = c("1",
                                 "2",
                                 "3+")) +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = expression(log[10]~Body~Mass~(g)))

ggplot(df) + #do histogram; .25 log 
  geom_histogram(aes(log.mass, 
                 group = n.cont,
                 colour = n.cont, fill = n.cont),
                 binwidth = .25) +
  #geom_histogram(aes(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass)]), 
  #               colour = "#99d8c9", fill = "#99d8c9",
  #               binwidth = .25) +
  #geom_histogram(aes(df$log.mass[df$n.cont == "3+" & !is.na(df$log.mass)]), 
  #               colour = "#e5f5f9", fill = "#e5f5f9",
  #               binwidth = .25) +
  plot_theme +
  theme(legend.position = c(0.8, 0.6)) +
  scale_fill_manual(values = cont_col, 
                    name="Number of Continents",
                    labels = c("1",
                               "2",
                               "3+")) +
  scale_color_manual(values = cont_col, 
                    name="Number of Continents",
                    labels = c("1",
                               "2",
                               "3+")) +
  scale_y_continuous(name = "Count") +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)))

ggplot(df) + #do histogram; .25 log 
  geom_histogram(aes(log.mass, 
                     group = n.cont,
                     colour = n.cont, fill = n.cont),
                 binwidth = .25) +
  #geom_histogram(aes(df$log.mass[df$n.cont == "2" & !is.na(df$log.mass)]), 
  #               colour = "#99d8c9", fill = "#99d8c9",
  #               binwidth = .25) +
  #geom_histogram(aes(df$log.mass[df$n.cont == "3+" & !is.na(df$log.mass)]), 
  #               colour = "#e5f5f9", fill = "#e5f5f9",
  #               binwidth = .25) +
  plot_theme +
  theme(legend.position = c(0.8, 0.6)) +
  scale_fill_manual(values = cont_bw, 
                    name="Number of Continents",
                    labels = c("1",
                               "2",
                               "3+")) +
  scale_color_manual(values = cont_bw, 
                     name="Number of Continents",
                     labels = c("1",
                                "2",
                                "3+")) +
  scale_y_continuous(name = "Count") +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)))

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

limited.breadth <- breadth[breadth$n.cont == 2,]
colnames(limited.breadth)[colnames(limited.breadth) == "N"] <- "limited.N"
limited.breadth <- limited.breadth %>%
  dplyr::select(-n.cont)

trotter.breadth <- breadth[breadth$n.cont == "3+",]
colnames(trotter.breadth)[colnames(trotter.breadth) == "N"] <- "trotter.N"
trotter.breadth <- trotter.breadth %>%
  dplyr::select(-n.cont)

#create full dataset
breadth.null.trot <- merge(null.breadth, trotter.breadth, by = "diet.breadth", all.x = TRUE, all.y = TRUE)
breadth.null.trot.lim <- merge(breadth.null.trot, limited.breadth, by = "diet.breadth", all.x = TRUE, all.y = TRUE)
breadth.null.trol.lim.homies <- merge(breadth.null.trot.lim, homies.breadth, by = "diet.breadth", all.x = TRUE, all.y = TRUE)

df.breadth <- breadth.null.trol.lim.homies
df.breadth[is.na(df.breadth)] <- 0

df.breadth$prop.null <- df.breadth$null.N/nrow(df)

df.breadth$prop.homies <- df.breadth$homies.N/nrow(df[df$n.cont == 1,])
df.breadth$prop.lim <- df.breadth$limited.N/nrow(df[df$n.cont == 2,])
df.breadth$prop.trot <- df.breadth$trotter.N/nrow(df[df$n.cont == "3+",])

#binomial test

for(i in 1:nrow(df.breadth)){
  test <- binom.test(df.breadth$homies.N[i], nrow(df[df$n.cont == 1,]), p = df.breadth$prop.null[i], alternative = "two.sided")
  df.breadth$p.homies[i] <- test$p.value
}

for(i in 1:nrow(df.breadth)){
  test <- binom.test(df.breadth$limited.N[i], nrow(df[df$n.cont == 2,]), p = df.breadth$prop.null[i], alternative = "two.sided")
  df.breadth$p.lim[i] <- test$p.value
}

for(i in 1:nrow(df.breadth)){
  test <- binom.test(df.breadth$trotter.N[i], nrow(df[df$n.cont == "3+",]), p = df.breadth$prop.null[i], alternative = "two.sided")
  df.breadth$p.trot[i] <- test$p.value
}

#add sidak correction
df.breadth <- arrange(df.breadth, p.homies) %>%
  dplyr::mutate(signif.homies = p.homies < 0.05,
                signif.bonferoni.homies = p.homies < 0.05/n(),
                signif.holm.homies = !0.05/(n() + 1 - 1:n()) < p.homies,
                signif.sidak.homies = p.homies < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.homies = !(1 - (1 - 0.05)^(1/n())) < p.homies)

df.breadth <- arrange(df.breadth, p.lim) %>%
  dplyr::mutate(signif.lim = p.lim < 0.05,
                signif.bonferoni.lim = p.lim < 0.05/n(),
                signif.holm.lim = !0.05/(n() + 1 - 1:n()) < p.lim,
                signif.sidak.lim = p.lim < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.lim = !(1 - (1 - 0.05)^(1/n())) < p.lim)

df.breadth <- arrange(df.breadth, p.trot) %>%
  dplyr::mutate(signif.trot = p.trot < 0.05,
                signif.bonferoni.trot = p.trot < 0.05/n(),
                signif.holm.trot = !0.05/(n() + 1 - 1:n()) < p.trot,
                signif.sidak.trot = p.trot < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.trot = !(1 - (1 - 0.05)^(1/n())) < p.trot)

#write.csv(df.breadth, 
#          "./Results/diet.breadth.results.csv",
#         row.names = FALSE)

#include trot.N, null prop, expected, observed-expected, (O-E)^2, (O-E)^2/exp, X crit, p

df.diet.stats <- df %>%
  summarise(n.two = nrow(df[df$n.cont == "2",]),
            carn.pisc = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.piscivore.tot == TRUE & df$n.cont == "2"]),
            carn.invt = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$n.cont == "2"]),
            pisc.invt = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.invertivore.tot == TRUE & df$n.cont == "2"]),
            carn.brow = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "2"]),
            carn.graz = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "2"]),
            carn.frug = length(df$binomial[df$diet.carnivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2"]),
            pisc.brow = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "2"]),
            pisc.graz = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "2"]),
            pisc.frug = length(df$binomial[df$diet.piscivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2"]),
            invt.brow = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.browser.tot == TRUE & df$n.cont == "2"]),
            invt.graz = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "2"]),
            invt.frug = length(df$binomial[df$diet.invertivore.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2"]),
            brow.graz = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.grazer.tot == TRUE & df$n.cont == "2"]),
            brow.frug = length(df$binomial[df$diet.browser.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2"]),
            graz.frug = length(df$binomial[df$diet.grazer.tot == TRUE & df$diet.frugivore.tot == TRUE & df$n.cont == "2"])) %>%
  as.data.frame()

#write.csv(df.diet.stats,
#          "./Results/diet.summary.csv",
#          row.names = FALSE)

##deeper look into dietary breadth of 3
df.3 <- df[df$diet.breadth ==3,]

length(df.3$binomial[df.3$diet.browser.tot == TRUE &
                     df.3$diet.invertivore.tot == TRUE &
                     df.3$diet.frugivore.tot == TRUE]) #152

length(df.3$binomial[df.3$diet.browser.tot == TRUE &
                       df.3$diet.carnivore.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #2

length(df.3$binomial[df.3$diet.invertivore.tot == TRUE &
                       df.3$diet.carnivore.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #16

length(df.3$binomial[df.3$diet.browser.tot == TRUE &
                       df.3$diet.grazer.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #12

table(df.3$n.cont) #180 on 1 continent, 6 on 2 continents

##deeper look into those with dietary breadth of 2
#only for diet.breadth == 2
## total
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2]) #308
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.carnivore.tot == TRUE & 
                   df$diet.breadth == 2]) #0
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2]) #702
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2]) #20
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2]) #0
length(df$binomial[df$diet.grazer.tot == TRUE & 
                   df$diet.carnivore.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.frugivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.piscivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.piscivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2])

##n.cont = 1
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.grazer.tot == TRUE & 
                   df$diet.carnivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.frugivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 1])

##n.cont = 2
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2])
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2])
length(df$binomial[df$diet.grazer.tot == TRUE & 
                   df$diet.carnivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2])
length(df$binomial[df$diet.frugivore.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2])
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2])
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2])
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2])
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == 2])

##n.cont = 3+
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == "3+"])
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2 &
                   df$n.cont == "3+"])


length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2])
length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2 &
                   df$diet.frugivore.tot == TRUE])
length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2 &
                   df$diet.invertivore.tot == TRUE])
length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2 &
                   df$diet.carnivore.tot == TRUE])
length(df$binomial[df$order == "Chiroptera" &
                   df$n.cont == 2 &
                   df$diet.piscivore.tot== TRUE])

length(df$binomial[df$family.origin == "South.America"])
length(df$binomial[df$family.origin == "South.America" &
                   df$n.cont == 2])
length(df$binomial[df$family.origin == "North.America"])
length(df$binomial[df$family.origin == "North.America" &
                   df$n.cont == 2])
length(df$binomial[df$family.origin == "Eurasia"]) 
length(df$binomial[df$family.origin == "Eurasia" &
                   df$n.cont == 2]) 

##### DIET TYPE -----
#are there diet types differences between homiess, limited dispersers, and globe trotters?
diets <- names(dplyr::select(df, starts_with("diet"), -diet.breadth))
df.diet <- data.frame(diet = diets)

df.diet["null.N"] <- colSums(df[diets])
df.diet["homies.N"] <- colSums(df[df$n.cont == 1, diets])
df.diet["limited.N"] <- colSums(df[df$n.cont == 2, diets])
df.diet["trotter.N"] <- colSums(df[df$n.cont == "3+", diets])

df.diet$prop.null <- df.diet$null.N/nrow(df)

df.diet$prop.homies <- df.diet$homies.N/nrow(df[df$n.cont == 1,])
df.diet$prop.lim <- df.diet$limited.N/nrow(df[df$n.cont == 2,])
df.diet$prop.trot <- df.diet$trotter.N/nrow(df[df$n.cont == "3+",])

#binomial test

for(i in 1:nrow(df.diet)){
  test <- binom.test(df.diet$homies.N[i], nrow(df[df$n.cont == 1,]), p = df.diet$prop.null[i], alternative = "two.sided")
  df.diet$p.homies[i] <- test$p.value
}

for(i in 1:nrow(df.diet)){
  test <- binom.test(df.diet$limited.N[i], nrow(df[df$n.cont == 2,]), p = df.diet$prop.null[i], alternative = "two.sided")
  df.diet$p.lim[i] <- test$p.value
}

for(i in 1:nrow(df.diet)){
  test <- binom.test(df.diet$trotter.N[i], nrow(df[df$n.cont == "3+",]), p = df.diet$prop.null[i], alternative = "two.sided")
  df.diet$p.trot[i] <- test$p.value
}

#add sidak correction
df.diet <- arrange(df.diet, p.homies) %>%
  dplyr::mutate(signif.homies = p.homies < 0.05,
                signif.bonferoni.homies = p.homies < 0.05/n(),
                signif.holm.homies = !0.05/(n() + 1 - 1:n()) < p.homies,
                signif.sidak.homies = p.homies < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.homies = !(1 - (1 - 0.05)^(1/n())) < p.homies)

df.diet <- arrange(df.diet, p.lim) %>%
  dplyr::mutate(signif.lim = p.lim < 0.05,
                signif.bonferoni.lim = p.lim < 0.05/n(),
                signif.holm.lim = !0.05/(n() + 1 - 1:n()) < p.lim,
                signif.sidak.lim = p.lim < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.lim = !(1 - (1 - 0.05)^(1/n())) < p.lim)

df.diet <- arrange(df.diet, p.trot) %>%
  dplyr::mutate(signif.trot = p.trot < 0.05,
                signif.bonferoni.trot = p.trot < 0.05/n(),
                signif.holm.trot = !0.05/(n() + 1 - 1:n()) < p.trot,
                signif.sidak.trot = p.trot < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.trot = !(1 - (1 - 0.05)^(1/n())) < p.trot)

#write.csv(df.diet, 
#          "./Results/diet.results.csv",
#          row.names = FALSE)

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

ks.test(log10(df$avg.mass[df$n.cont == "3+" & df$diet.carnivore.tot == TRUE]), log10(df$avg.mass[df$n.cont == "3+"])) #not sig
# Exact two-sample Kolmogorov-Smirnov test
# 
# data:  log10(df$avg.mass[df$n.cont == "3+" & df$diet.carnivore.tot == TRUE]) and log10(df$avg.mass[df$n.cont == "3+"])
# D = 0.2, p-value = 1
# alternative hypothesis: two-sided

###### DELVING DEEPER ------
##deeper look into dietary breadth of 3
df.3 <- df[df$diet.breadth ==3,]

length(df.3$binomial[df.3$diet.browser.tot == TRUE &
                       df.3$diet.invertivore.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #152

length(df.3$binomial[df.3$diet.browser.tot == TRUE &
                       df.3$diet.carnivore.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #2

length(df.3$binomial[df.3$diet.invertivore.tot == TRUE &
                       df.3$diet.carnivore.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #18

length(df.3$binomial[df.3$diet.browser.tot == TRUE &
                       df.3$diet.grazer.tot == TRUE &
                       df.3$diet.frugivore.tot == TRUE]) #12

table(df.3$n.cont) #180 on 1 continent, 6 on 2 continents

## do bats drive trends in n.cont == 2 for diet
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

##deeper look into those with dietary breadth of 2
#only for diet.breadth == 2
## total
length(df$binomial[df$diet.browser.tot == TRUE & 
                     df$diet.grazer.tot == TRUE & 
                     df$diet.breadth == 2]) #308
length(df$binomial[df$diet.browser.tot == TRUE & 
                     df$diet.carnivore.tot == TRUE & 
                     df$diet.breadth == 2]) #0
length(df$binomial[df$diet.browser.tot == TRUE & 
                     df$diet.frugivore.tot == TRUE & 
                     df$diet.breadth == 2]) #702
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
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                     df$diet.frugivore.tot == TRUE & 
                     df$diet.breadth == 2]) #46
length(df$binomial[df$diet.carnivore.tot == TRUE & 
                     df$diet.piscivore.tot == TRUE & 
                     df$diet.breadth == 2]) #4
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                     df$diet.frugivore.tot == TRUE & 
                     df$diet.breadth == 2]) #452
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
length(df$binomial[df$diet.browser.tot == TRUE & 
                     df$diet.frugivore.tot == TRUE & 
                     df$diet.breadth == 2 &
                     df$n.cont == 1]) #682
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
length(df$binomial[df$diet.invertivore.tot == TRUE & 
                     df$diet.piscivore.tot == TRUE & 
                     df$diet.breadth == 2 &
                     df$n.cont == 1]) #13

##n.cont = 2
length(df$binomial[df$diet.browser.tot == TRUE & 
                     df$diet.grazer.tot == TRUE & 
                     df$diet.breadth == 2 &
                     df$n.cont == 2]) #11
length(df$binomial[df$diet.browser.tot == TRUE & 
                     df$diet.frugivore.tot == TRUE & 
                     df$diet.breadth == 2 &
                     df$n.cont == 2]) #20
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


length(df$binomial[df$order == "Chiroptera" &
                     df$n.cont == 2]) #142
length(df$binomial[df$order == "Chiroptera" &
                     df$n.cont == 2 &
                     df$diet.frugivore.tot == TRUE]) #52
length(df$binomial[df$order == "Chiroptera" &
                     df$n.cont == 2 &
                     df$diet.invertivore.tot == TRUE]) #106
length(df$binomial[df$order == "Chiroptera" &
                     df$n.cont == 2 &
                     df$diet.carnivore.tot == TRUE]) #9
length(df$binomial[df$order == "Chiroptera" &
                     df$n.cont == 2 &
                     df$diet.piscivore.tot== TRUE]) #2

###### DIET FIGURES -----
#UNstacked bar graph
cb_viridis <- c("#482878FF", #1
                "#1F9E89FF", #2 
                "#FDE725FF") #3+
#Continent as x axis for continent and y for diet

##DIET BREADTH
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

ggplot(dietbreadth_bargraph_full, 
       aes(x = diet.breadth, y = prop, 
           fill = as.factor(n.cont))) + 
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_fill_manual(name = "Number of Continents", 
                    labels = c("1",
                               "2",
                               "3+"),
                    values = cb_viridis) +
  xlab("\n\nDietary Breadth") + 
  ylab("Proportion") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) + 
  plot_theme + theme(panel.border = element_rect(fill = NA),
                     strip.background = element_rect(fill = NA),
                     legend.position = c(.9, 0.8)) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))

## x axis = continent, y axis = diet

ggplot(dietbreadth_bargraph_full, 
       aes(x = diet.breadth, y = prop, 
           fill = as.factor(n.cont))) + 
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_fill_manual("Continents", 
                    values = cb_viridis) +
  xlab("\n\nDietary Breadth") + 
  ylab("Proportion") + 
  theme(legend.position = "none") + 
  plot_theme

##DIET TYPE
diet.melt <- melt(df, id.vars = c("binomial", "n.cont", "diet.breadth"), 
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

diettype_bargraph <- plyr::ddply(unique.diet.melt, c("numconts", "diettype"), function(x){
  nrow(x)
})
diettype_bargraph_full <- complete(diettype_bargraph, numconts, diettype)

diettype_bargraph_full$tots <- NA
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "1"] <- sum(diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "1"], na.rm = TRUE) #4120
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "2"] <- sum(diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "2"], na.rm = TRUE) #260
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "3+"] <- sum(diettype_bargraph_full$V1[diettype_bargraph_full$numconts == "3+"], na.rm = TRUE) #6

diettype_bargraph_full$prop <- diettype_bargraph_full$V1 / diettype_bargraph_full$tots

#show as proportions
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
                                  "3+" = "gray72"),
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

#show as proportions

diettype_bargraph_full$diettype <- factor(diettype_bargraph_full$diettype, 
                                          levels=c("diet.browser.tot", "diet.grazer.tot", "diet.frugivore.tot",
                                                   "diet.carnivore.tot", "diet.piscivore.tot", "diet.invertivore.tot"))

ggplot(diettype_bargraph_full, 
       aes(x = diettype, y = prop, 
           fill = as.factor(numconts))) + 
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_fill_manual(name = "Number of Continents",
                    labels = c("1",
                               "2",
                               "3+"),
                    values = cb_viridis) +
  xlab("Diet Type") + 
  ylab("Proportion") + 
  scale_x_discrete(labels=c("diet.browser.tot" = "Browser", 
                            "diet.grazer.tot" = "Grazer", 
                            "diet.frugivore.tot" = "Frugivore",
                            "diet.carnivore.tot" = "Carnivore", 
                            "diet.piscivore.tot" = "Piscivore", 
                            "diet.invertivore.tot" = "Invertivore")) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=14)) + 
  plot_theme + 
  theme(panel.border = element_rect(fill=NA),
                    strip.background = element_rect(fill=NA),
                    legend.position = c(.3, 0.8)) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))

#### H6: GEOGRAPHIC RANGE SIZE ----
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
  dplyr::group_by(binomial) %>%
  dplyr::summarise(cont.tot.area = sum(tot.area), 
                   pan.gr.area = gr.area.km2[1], 
                   hmrg = home.range.km2[1],
                   faurby.nat.range = present.natural.range.km2[1], 
                   faurby.current.range = current.range.km2[1],
                   num.cont = n.cont[1], 
                   size = mean(avg.mass)) %>%
  as.data.frame()

#get cleanest dataset
df.pan <- subset(df.cont, !is.na(df.cont$pan.gr.area) & !is.na(df.cont$size))

df.faurby <- subset(df.cont, !is.na(df.cont$faurby.nat.range) & !is.na(df.cont$size) & df.cont$faurby.nat.range != 0) 

length(unique(df.pan$binomial)) #2972
length(unique(df.pan$binomial[df.pan$num.cont == 1])) #2730
length(unique(df.pan$binomial[df.pan$num.cont == 2])) #237
length(unique(df.pan$binomial[df.pan$num.cont == "3+"])) #5

length(unique(df.faurby$binomial)) # 2515
length(unique(df.faurby$binomial[df.faurby$num.cont == 1])) #2396
length(unique(df.faurby$binomial[df.faurby$num.cont == 2])) #115
length(unique(df.faurby$binomial[df.faurby$num.cont == "3+"])) #4

# ranges
ggplot(data = df.pan, aes(x = logSize, y = ratio)) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = col) +
  labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Geographic~Range/Continent~Size), color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "top")

ggplot(data = df.faurby, aes(x = logSize, y = ratio)) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = col) +
  labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Home~Range/Geographic~Range), color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "top")

#homerange to geographic range
plot(log10(df.pan$pan.gr.area) ~ log10(df.pan$hmrg))
summary(lm(df.pan$pan.gr.area ~ df.pan$hmrg)) #significant, but r2 = 0.04
#home range does not predict geographic range

#do bigger animals have a larger hmrg?
summary(lm(log10(df.pan$hmrg) ~ log10(df.pan$size) + as.factor(df.pan$num.cont))) #r2 = 0.67; sig
summary(lm(log10(df.pan$hmrg) ~ log10(df.pan$size))) #r2 = 0.66; sig

ggplot(data = df.pan, aes(x = log10(df.pan$size), y = log10(df.pan$hmrg))) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = col) +
  labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Home~Range), color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "top")

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

#
