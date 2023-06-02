#Data exploration of dataset from Init.R
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

## FIX CONTINENT NUMBER
data$n.cont[data$n.cont == 4] <- "3+"
data$n.cont[data$n.cont == 3] <- "3+"
data$n.cont <- as.factor(data$n.cont)

## MAKE FACTORS FOR LATER

data$iucn.status <- as.factor(data$iucn.status)
data$family <- as.factor(data$family)
data$continent <- as.factor(data$continent)

data$mass.TF <- !is.na(data$mass) #TRUE = mass, FALSE = no mass

## CREATE SIZE BINS

data$log.mass <- log10(data$mass)
data$size.bin <- round(data$log.mass, digits = 0)

#### TABLES ----

##### TOTAL RECODS ----
nrow(data) #4715
length(unique(data$order)) #29
length(unique(data$family)) #135
length(unique(data$genus)) #1074
length(unique(data$binomial)) #4426

data %>%
  group_by(order) %>%
  summarise(n = length(unique(binomial))) %>%
  as.data.frame()

##### SIZE -----

ggplot(data = data) +
  geom_density(aes(x = log10(mass), color = continent))
#qualitatively similar

###### BIASES ------

# CLADE

#from which groups are we missing things from?

df.clade.mass <- data %>% 
  group_by(family) %>% 
  summarise(order = order[1],
            n.mass = sum(!is.na(mass)),
            n.na = sum(is.na(mass)),
            percent = n.na/(n.na + n.mass),
            avg.size = mean(mass, na.rm = TRUE)) %>% 
  as.data.frame()

write.csv(df.clade.mass,
          "../Results/DataExploration/clade.missing.mass.csv",
          row.names = FALSE)

# CONTINENT

df.cont.mass <- data %>% 
  group_by(continent) %>% 
  summarise(n.mass = sum(!is.na(mass)),
            n.na = sum(is.na(mass)),
            percent = n.na/(n.na + n.mass),
            avg.size = mean(mass, na.rm = TRUE)) %>% 
  as.data.frame()

write.csv(df.cont.mass,
          "../Results/DataExploration/cont.missing.mass.csv",
          row.names = FALSE)

##### FOSSIL AGES ----
## PBDB
length(unique(data$order[data$foss.age >= 0]))  #21
length(unique(data$family[data$foss.age >= 0])) #94
length(unique(data$genus[data$foss.age >= 0])) #396
length(unique(data$binomial[data$foss.age >= 0])) #60

## FAURBY
length(unique(data$order[data$age.median >= 0]))  #30
length(unique(data$family[data$age.median >= 0])) #136
length(unique(data$genus[data$age.median >= 0])) #1031
length(unique(data$binomial[data$age.median >= 0])) #4007

###### BIASES ------

## SIZE

ggplot(data[is.na(data$mass),]) +
  geom_histogram(aes(x = foss.age))
#missing a lot of younger fossils, actually

ggplot(data[is.na(data$mass),]) +
  geom_histogram(aes(x = age.median))
#missing a lot of younger fossils, actually


ggplot(data[is.na(data$foss.age),]) +
  geom_histogram(aes(x = log10(mass)))
#missing a lot of smaller bodied organisms, of course

ggplot(data[is.na(data$age.median),]) +
  geom_histogram(aes(x = log10(mass)))
#missing a size classes from all over...looks weird

df.mass.foss <- data %>% 
  group_by(size.bin) %>% 
  summarise(n.foss.age = sum(!is.na(foss.age)),
            n.foss.na = sum(is.na(foss.age)),
            foss.per = n.foss.na/(n.foss.na + n.foss.age),
            n.phyl.age = sum(!is.na(age.median)),
            n.phyl.na = sum(is.na(age.median)),
            phyl.per = n.phyl.na/(n.phyl.na + n.phyl.age)) %>% 
  as.data.frame()

write.csv(df.mass.foss,
          "../Results/DataExploration/foss.missing.mass.csv",
          row.names = FALSE)

# CLADE

#from which groups are we missing things from?

df.clade.foss <- data %>% 
  group_by(family) %>% 
  summarise(order = order[1],
            n.foss.fam = sum(!is.na(foss.age)),
            n.foss.na = sum(is.na(foss.age)),
            foss.per = n.foss.na/(n.foss.na + n.foss.fam),
            n.phyl.fam = sum(!is.na(age.median)),
            n.phyl.na = sum(is.na(age.median)),
            phyl.per = n.phyl.fam/(n.phyl.fam + n.phyl.na),
            avg.size = mean(mass, na.rm = TRUE)) %>% 
  as.data.frame()

write.csv(df.clade.foss,
          "../Results/DataExploration/foss.missing.clade.csv",
          row.names = FALSE)

# CONTINENT

df.cont.foss <- data %>% 
  group_by(continent) %>% 
  summarise(n.foss.fam = sum(!is.na(foss.age)),
            n.foss.na = sum(is.na(foss.age)),
            foss.per = n.foss.na/(n.foss.na + n.foss.fam),
            n.phyl.fam = sum(!is.na(age.median)),
            n.phyl.na = sum(is.na(age.median)),
            phyl.per = n.phyl.fam/(n.phyl.fam + n.phyl.na)) %>% 
  as.data.frame()

write.csv(df.cont.foss,
          "../Results/DataExploration/foss.missing.cont.csv",
          row.names = FALSE)

##### CONTINENTS ----

length(unique(data$binomial[data$continent == "North.America"])) #822
length(unique(data$binomial[data$continent == "South.America"])) #1214
length(unique(data$binomial[data$continent == "Eurasia"])) #1179
length(unique(data$binomial[data$continent == "Africa"])) #1159
length(unique(data$binomial[data$continent == "Australia"])) #341

###### BIASES ------

## SIZE
# which continents are missing the most mass data?

ggplot(data = data) + 
  geom_bar(aes(fill = mass.TF, x = continent),
           position = 'stack', stat = 'count') 

df.cont.mass <- data %>% 
  group_by(continent) %>% 
  summarise(n.mass = sum(!is.na(mass)),
            n.na = sum(is.na(mass)),
            cont.per = n.na/(n.na + n.mass)) %>% 
  as.data.frame()

write.csv(df.cont.mass,
          "../Results/DataExploration/cont.missing.mass.csv",
          row.names = FALSE)

##### DIET ----

length(unique(data$binomial[data$diet.browser == TRUE])) #1631
length(unique(data$binomial[data$diet.grazer == TRUE])) #618
length(unique(data$binomial[data$diet.frugivore == TRUE])) #1941
length(unique(data$binomial[data$diet.carnivore == TRUE])) #333
length(unique(data$binomial[data$diet.piscivore == TRUE])) #30
length(unique(data$binomial[data$diet.invertivore == TRUE])) #1998

###### BIASES ------

## SIZE
# which diet types are missing the most mass data?

df.melt <- melt(data, measure.vars = c(26:31),
                variable.name = "diet.type",
                value.name = "has.diet")
df.melt.true <- df.melt[df.melt$has.diet == TRUE,]

ggplot(data = df.melt.true) + 
  geom_bar(aes(fill = mass.TF, x = diet.type),
           position = 'stack', stat = 'count') 

df.diet.mass <- df.melt.true %>%
  group_by(diet.type) %>%
  summarise(n.mass = sum(!is.na(mass)),
            n.na = sum(is.na(mass)),
            per = n.na/(n.na+n.mass)) %>% 
  as.data.frame()

write.csv(df.diet.mass,
          "../Results/DataExploration/diet.missing.mass.csv",
          row.names = FALSE)

## CLADES
# which clades are missing the most diet data?

df.diet.fam <- data %>%
  group_by(family) %>%
  summarise(order = order[1],
            n.diet = sum(diet.src == "species.diet"),
            n.na = sum(diet.src != "species.diet"),
            per = n.na/(n.na+n.diet)) %>% 
  as.data.frame()

write.csv(df.diet.fam,
          "../Results/DataExploration/fam.missing.mass.csv",
          row.names = FALSE)

##### DIET BREADTH ----

table(data$diet.breadth[!duplicated(data$binomial)])
#1    2     3 
#2490 1747  189 

##### WITHOUT INVALID MASS ESTIMATES -----

nrow(mm.df[mm.df$mass.status == "invalid",]) #73 records

length(unique(mm.df$order[mm.df$mass.status == "invalid"])) #6
length(unique(mm.df$family[mm.df$mass.status == "invalid"])) #15
length(unique(mm.df$genus[mm.df$mass.status == "invalid"])) #35
length(unique(mm.df$binomial[mm.df$mass.status == "invalid"])) #44

unique(mm.df$continent[mm.df$mass.status == "invalid"]) #all continents

###### BIASES ------

### CONTINENT

### FAMILY

### SIZE

ggplot(data = data[data$mass.status == "invalid",]) +
  geom_density(aes(x = log10(mass)))

data$mass[data$mass.status == "invalid"]
#6 under 10 g
#35 between 10-99g
#4 between 100-999g
#5 1000g or above

##### N DATA DEFICIENT (IUCN) -----

nrow(mm.df[mm.df$iucn.status == "DD",]) #684 records

length(unique(mm.df$order[mm.df$iucn.status == "DD"])) #17
length(unique(mm.df$family[mm.df$iucn.status == "DD"])) #56
length(unique(mm.df$genus[mm.df$iucn.status == "DD"])) #267
length(unique(mm.df$binomial[mm.df$iucn.status == "DD"])) #616

unique(mm.df$continent[mm.df$iucn.status == "DD"]) #all continents

###### BIASES ------

### CONTINENT

### FAMILY

### SIZE

ggplot(data = data[data$iucn.status == "DD",]) + 
  geom_density(aes(x = log10(mass)))

sort(data$mass[data$iucn.status == "DD"])
#28 below 10g
#86 between 10-99g
#60 between 100-999g
#15 between 1000-9999g
#3 between 10000-99999g

#### WITH INTRODUCED SPECIES ----

## rerunning Init.R without removing introduced or domesticated species
nrow(df.intro.dom[df.intro.dom$extant.status == "introduction",]) #44
nrow(df.intro.dom[df.intro.dom$extant.status == "domesticated",]) #6

df.intro.dom[df.intro.dom$extant.status == "introduction", c("binomial", "n.cont", "continent")]
# binomial              n.cont  introduced to   n.intro  native on
# Suncus murinus          2     Africa              1    Eurasia
# Rattus rattus           2     Australia           1    Eurasia
# Rattus norvegicus       2     Australia           1    Eurasia
# Vulpes vulpes           4     Australia           1    EA, NA, AF
# Canis lupus             3     Australia           1    EA, NA
# Capra hircus            3     Australia           1    Eurasia
# Axis porcinus           2     Australia           1    Eurasia
# Rusa timorensis         1     Australia           1    Insular
# Axis axis               2     Australia           1    Eurasia
# Sus scrofa              2     Australia           1    Eurasia
# Cervus elaphus          4     Australia           1    EA, NA, AF
# Equus asinus            2     Australia           1    Africa
# Bos javanicus           2     Australia           1    Eurasia
# Camelus dromedarius     2     Australia           1    Eurasia
# Bubalus bubalis         4     Australia           1    Eurasia
# Bos taurus              2     AF, EA              2    <NA>
# Ovis aries              4     NA, SA, AF          3    Eurasia
# Oryctolagus cuniculus   3     Australia           1    EA, AF
# Lepus europaeus         4     NA, SA, AU          3    Eurasia
# Mus musculus            5     NA, SA, AU          3    EA, AF
# Odocoileus virginianus  3     Eurasia             1    NA, SA
# Sciurus carolinensis    4     EA, AF, AU          3    North.America
# Neovison vison          2     Eurasia             1    North.America
# Rusa unicolor           2     Australia           1    Eurasia
# Felis catus             5     NA, SA, AU, AF, EA  5    <NA>
# Castor canadensis       3     EA, SA              2    North.America
# Dama dama               4     SA, AU, AF          3    Eurasia
# Procyon lotor           2     Eurasia             1    North.America

df.intro.dom[df.intro.dom$extant.status == "domesticated", c("binomial", "n.cont", "continent")]

# binomial        n.cont    domesticated on  n.dom  native to
# Bubalus bubalis   4       AF, SA            2     Eurasia
# Capra hircus      3       North.America     1     Eurasia
# Equus caballus    4       SA, AU, EA        3     North.America


## BIASES IN DATA ----

#want to check that we get same skewed & bimodal distributions as in MOM
ggplot() +
  geom_density(aes(log10(df$avg.mass)), colour = "black") +
  geom_density(aes(log10(df$avg.mass[df$continent.North.America == TRUE])), colour = "#B4D9C8") +
  geom_density(aes(log10(df$avg.mass[df$continent.South.America == TRUE])), colour = "#E2C9F2") +
  geom_density(aes(log10(df$avg.mass[df$continent.Eurasia == TRUE])), colour = "#F2CDA0") +
  geom_density(aes(log10(df$avg.mass[df$continent.Australia == TRUE])), colour = "#D9967E") +
  geom_density(aes(log10(df$avg.mass[df$continent.Africa == TRUE])), colour = "#C2D991") +
  plot_theme +
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_y_continuous(name = "Density")

#want to check for taxonomic and geographic coverage
cont.counts <- df %>%
  dplyr::select(starts_with("continent.")) %>%
  dplyr::summarise_all(sum)

table(df$family)

#want to look for coverage in species (genus) age
#species (genus) age by body size
ggplot() +
  geom_point(aes(x = log10(df$avg.mass), y = df$foss.age)) +
  geom_smooth(aes(x = log10(df$avg.mass), y = df$foss.age)) +
  plot_theme + 
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_y_continuous(name = "Fossil Age (Genus)")

summary(lm(df$foss.age ~ df$avg.mass))

ggplot() +
  geom_point(aes(x = log10(df$avg.mass), y = df$age.median)) +
  geom_smooth(aes(x = log10(df$avg.mass), y = df$age.median)) +
  plot_theme + 
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g))) +
  scale_y_continuous(name = "Median Phylogenetic Age (Genus)")

summary(lm(df$age.median ~ df$avg.mass))

#species (genus) age by taxnomic coverage
tax.foss.count <- df %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(n.foss = length(!is.na(foss.age)),
                   n.age = length(!is.na(age.median)))

#species (genus) age by geographic coverage
df.short <- df %>%
  pivot_longer(cols = starts_with("continent."),
               names_to = "continent",
               values_to = "TorF") %>%
  filter(TorF == TRUE)

df.cont.short <- df.short %>%
  group_by(continent) %>%
  dplyr::summarise(N.foss = length(!is.na(foss.age)),
                   N.age = length(!is.na(age.median)))

##biases in family of origin data
ggplot(df[!is.na(df$family.origin),]) +
  geom_density(aes(log10(avg.mass))) + 
  plot_theme + 
  scale_x_continuous(name = expression(log[10]~Body~Mass~(g)))

length(unique(df$family[df$family.origin != ""])) #128
length(unique(df$family[df$family.origin == ""])) #7
length(df$binomial[df$family.origin == ""]) #814
unique(df$order[df$family.origin == ""])

length(df$binomial[df$family.origin == "" & df$order == "Chiroptera"]) #87 
length(unique(df$family[df$family.origin == "" & df$order == "Chiroptera"])) #2

length(df$binomial[df$family.origin == "" & df$order == "Rodentia"]) #713
length(unique(df$family[df$family.origin == "" & df$order == "Rodentia"])) #2

#geographic bias
df %>%
  filter(family.origin != "") %>%
  group_by(family.origin) %>%
  summarise(N = n())



