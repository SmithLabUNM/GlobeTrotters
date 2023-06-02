# Globe Trotters
# Meghan A. Balk
# meghan.balk@gmail.com
#

#### LOAD PACKAGES ----
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)
require(stringr)

## LOAD DATA ----
options(stringsAsFactors = FALSE)

mom <- read.csv("../Data/MOMv11.csv", header = TRUE)
pacifici <- read.csv("../Data/Generation Length for Mammals.csv", header = TRUE)
origin <- read.csv("../Data/familyOrigin.csv", header = TRUE)
pbdb <- read.csv("../Data/pbdb.data.csv", as.is = T)
faurby.ages <- read.csv("../Data/species.age_Faurby.csv", header = TRUE, row.names = 1)
ranges <- read.csv("../Data/ranges.csv", header = TRUE)
pantheria <- read.csv("../Data/pantheria.csv", header = TRUE)




##ABOUT DATA ----
invasive <- length(unique(mom$binomial[mom$extant.status == "introduction" |
                                       mom$extant.status == "domesticated"]))

total <- length(unique(mom$binomial))

(invasive/total)*100

## TRIM DATA ----

#remove humans
mom <- mom[mom$binomial != "Homo sapiens",]

#remove invalid mass estimates
mom <- mom[mom$mass.status != "invalid",]

##remove marine species
mom <- mom %>%
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
table(mom$continent, useNA = "always")

# Remove NA's, Insulars, and Marine
mom <- filter(mom, !is.na(continent) &
               continent != "Insular" &
               continent != "Marine")

# Remove introduced and domesticated species
table(mom$extant.status)
mom <- filter(mom, extant.status != "introduction",
                   extant.status != "domesticated")

# Checking for accidents
stopifnot(!any(str_trim(mom$genus) != mom$genus))
stopifnot(!any(str_trim(mom$species) != mom$species))

##remove continental duplicates
mom[(duplicated(mom[c("binomial", "continent")])),]

data <- mom
write.csv(data, "data.trimmed.csv")

## FIX DIET ----

mom$trophic[which(mom$trophic == "")] <- NA
sort(table(mom$trophic))

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


troph.diet <- which(mom$trophic %in% c(invertivore, carnivore, browser, grazer, frugivore, piscivore))

mom <- mom %>% 
  mutate(diet.invertivore = trophic %in% invertivore,
         diet.carnivore = trophic %in% carnivore,
         diet.browser = trophic %in% browser,
         diet.grazer = trophic %in% grazer,
         diet.frugivore = trophic %in% frugivore,
         diet.piscivore = trophic %in% piscivore)

# Find NAs and replace them with genereic averages
mom %>% 
  dplyr::select(starts_with("diet")) %>% 
  colSums()

mom$diet.src <- NA
mom$diet.src[troph.diet] <- "troph.diet"

diet <- mom %>%
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

for(i in 1:nrow(mom)) {
  if(mom$binomial[i] %in% species.diet$binomial) {
    k <- which(species.diet$binomial == mom$binomial[i])
    mom[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      species.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else if(mom$genus[i] %in% genus.diet$genus) {
    k <- which(genus.diet$genus == mom$genus[i])
    mom[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      genus.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else if(mom$family[i] %in% family.diet$family) {
    k <- which(family.diet$family == mom$family[i])
    mom[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      family.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else {
    stop("Cannot find diet")
  }
}

table(mom$diet.src, useNA = "always")

## CREATE LONG CONTINENT VERSION

mom <- mom %>% 
  mutate(Africa = continent == "Africa",
         North.America = continent == "North.America",
         South.America = continent == "South.America",
         Eurasia = continent == "Eurasia",
         Australia = continent == "Australia")

## GROUP_BY SPECIES ----

#diets differ by continent, taking the widest diet possible
df.sums <- mom %>%
  group_by(binomial) %>%
  dplyr::summarise(diet.invertivore.tot = isTRUE(sum(diet.invertivore) > 0),
                   diet.carnivore.tot = isTRUE(sum(diet.carnivore) > 0),
                   diet.browser.tot = isTRUE(sum(diet.browser) > 0), 
                   diet.grazer.tot = isTRUE(sum(diet.grazer) > 0),
                   diet.piscivore.tot = isTRUE(sum(diet.piscivore) > 0),
                   diet.frugivore.tot = isTRUE(sum(diet.frugivore) > 0),
                   avg.mass = mean(mass),
                   n.cont = length(unique(continent)))

df.continent <- mom %>%
  group_by(binomial) %>%
  dplyr::summarise(continent.Africa = as.logical(sum(Africa)),
                   continent.North.America = as.logical(sum(North.America)),
                   continent.South.America = as.logical(sum(South.America)),
                   continent.Eurasia = as.logical(sum(Eurasia)),
                   continent.Australia = as.logical(sum(Australia)))

df.taxa <- mom[!duplicated(mom$binomial),] %>%
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

df.contTaxaSums$n.cont <- df.contTaxaSums %>%
  dplyr::select(starts_with("continent.")) %>%
  rowSums()
table(df.contTaxaSums$n.cont)

length(unique(mom$binomial))
nrow(df.contTaxaSums)

write.csv(df.contTaxaSums, "data.long.csv")

## AGES----

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


## COMBINE DATA ---

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
                indiv.home.range.km2 = X22.2_HomeRange_Indiv_km2,
                dispersal.age.d = X7.1_DispersalAge_d)

##Combine data
  
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

write.csv(df, "global.mammal.data.csv")

##plot themes----
## COLOR SCHEME
#South America = #E2C9F2; dark #9A8AA6
#North America = #B4D9C8; dark #748C81
#Africa = #C2D991; dark #7E8C5E
#Eurasia = #F2CDA0; dark #A68C6D
#Australia = #D9967E; dark #8C6151

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

##data for analyses ----
#df <- read.csv("global.mammal.data.csv", header = TRUE)

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

## NUM SP PER CONTINENT ----
length(unique(df$binomial))
length(unique(df$binomial[df$n.cont == 1]))
length(unique(df$binomial[df$n.cont == 2])) 
length(unique(df$binomial[df$n.cont == "3+"]))

unique(df[which(df$n.cont == "3+"), "binomial"])

##limited disperses
ld <- df[df$n.cont == 2,]
length(unique(ld$order))
length(unique(ld$family))
length(unique(ld$genus))

sort(table(ld$order, useNA = "always"))

##bats
length(unique(df$binomial[df$order == "Chiroptera"]))
##rodents
length(unique(df$binomial[df$order == "Rodentia"]))
##artiodactyla
length(unique(df$binomial[df$order == "Artiodactyla"]))
##carnivora
length(unique(df$binomial[df$order == "Carnivora"]))

## DIET BREADTH ----
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

write.csv(df.breadth, "diet.breadth.results.csv")

## create a chi squared
#include trot.N, null prop, expected, observed-expected, (O-E)^2, (O-E)^2/exp, X crit, p

## DIET TYPE ----
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

write.csv(df.diet, "diet.results.csv")

#test if carnivorans of n=2 and n=3+ are larger than expected
ks.test(log10(df$avg.mass[df$n.cont == 2 & df$diet.carnivore.tot == TRUE]), log10(df$avg.mass[df$n.cont == 2]))
ks.test(log10(df$avg.mass[df$n.cont == 2 & df$diet.carnivore.tot == TRUE]), log10(df$avg.mass[df$n.cont == 2]), alternative = "greater")
ks.test(log10(df$avg.mass[df$n.cont == 2 & df$diet.carnivore.tot == TRUE]), log10(df$avg.mass[df$n.cont == 2]), alternative = "less") #sig; y less than x

ks.test(log10(df$avg.mass[df$n.cont == "3+" & df$diet.carnivore.tot == TRUE]), log10(df$avg.mass[df$n.cont == "3+"])) #not sig

## DIET FIGURES ----
#stacked bar graph

col <- c("gray72", "gray47", "black")

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
  scale_fill_manual(values = c("1" = "black",
                               "2" = "gray47",
                               "3+" = "gray72"),
                    name = "Number of Continents",
                    labels = c("Homebodies",
                               "Limited dispersers",
                               "Globetrotters")) +
  geom_bar(stat = "identity") +
  xlab("Dietary Breadth") + 
  ylab("Proportion") + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) + 
  plot_theme +theme(panel.border = element_rect(fill = NA),
                    strip.background = element_rect(fill = NA),
                    legend.position = c(1.15, 0.5)) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))


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
ggplot(diettype_bargraph_full, aes(x = diettype, y = prop, fill = numconts)) + 
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
                       labels = c("Homebodies",
                                  "Limited dispersers",
                                  "Globetrotters")) +
  geom_col(position = position_stack(reverse = TRUE)) +
  plot_theme +
  theme(panel.border = element_rect(fill = NA),
        strip.background = element_rect(fill = NA),
        legend.position = c(1.25, 0.5)) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))
##DISPERSAL----
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

ggplot(data = df.dispersal) +
  geom_density(aes(dispersal.foss))
length(df.dispersal$dispersal.foss[!is.na(df.dispersal$dispersal.foss)]) #68

ggplot(data = df.dispersal) +
  geom_density(aes(dispersal.phylo))
length(df.dispersal$dispersal.phylo[!is.na(df.dispersal$dispersal.phylo)]) #84

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
table(df.dispersal$family.origin[is.na(df.dispersal$dispersal.phylo) &
                                   df.dispersal$order == "Primates"])

ks.test(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss) &
                       df.dispersal$order == "Rodentia"], 
        df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss) &
                       df.dispersal$order == "Rodentia"])
ks.test(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss) &
                                df.dispersal$order == "Rodentia"], 
        df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss) &
                                df.dispersal$order == "Rodentia"],
        alternative = "less") #y lies below x;

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

length(df.dispersal$binomial[!is.na(df.dispersal$dispersal.phylo)]) #84

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
ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]), alternative = "greater")
ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.foss)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.foss)]), alternative = "less") #sig

min(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]) #same as above
max(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)])

min(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])
max(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)])

ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)]))
ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)]), alternative = "greater")
ks.test(log10(df.dispersal$avg.mass[!is.na(df.dispersal$dispersal.phylo)]), 
        log10(df.dispersal$avg.mass[is.na(df.dispersal$dispersal.phylo)]), alternative = "less") #sig

table(df.dispersal$n.cont)
table(df.dispersal$n.cont[!is.na(df.dispersal$dispersal.foss)])
table(df.dispersal$n.cont[!is.na(df.dispersal$dispersal.phylo)])

## FAMILY AND ORDER ----

##FAMILY
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

write.csv(df.family, "family.results.csv")

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

ks.test(family.lim.true$avg.mass, family.lim.false$avg.mass) #sig diff
ks.test(family.lim.true$avg.mass, family.lim.false$avg.mass, alternative = "greater") ##sig
ks.test(family.lim.true$avg.mass, family.lim.false$avg.mass, alternative = "less") #not sig

##globe trotters
trot.true <- subset(df.family, df.family$signif.sidak.trot == TRUE, select = c(family,
                                                                               null.N,
                                                                               trotter.N,
                                                                               prop.null,
                                                                               prop.trot,
                                                                               signif.sidak.trot))
#THERE AREN'T ANY

##ORDER
null.order <- df %>%
  group_by(order) %>%
  dplyr::summarise(null.N = n()) %>%
  dplyr::select(order,
                null.N) %>%
  as.data.frame()

order <- df %>%
  group_by(n.cont, order) %>%
  dplyr::summarise(N = n()) %>% 
  as.data.frame()

homies.order <- order[order$n.cont == 1,]
colnames(homies.order)[colnames(homies.order) == "N"] <- "homies.N"
homies.order <- homies.order %>%
  dplyr::select(-n.cont)

limited.order <- order[order$n.cont == 2,]
colnames(limited.order)[colnames(limited.order) == "N"] <- "limited.N"
limited.order <- limited.order %>%
  dplyr::select(-n.cont)

trotter.order <- order[order$n.cont == "3+",]
colnames(trotter.order)[colnames(trotter.order) == "N"] <- "trotter.N"
trotter.order <- trotter.order %>%
  dplyr::select(-n.cont)

#create full dataset
ord.null.trot <- merge(null.order, trotter.order, by = "order", all.x = TRUE, all.y = TRUE)
ord.null.trot.lim <- merge(ord.null.trot, limited.order, by = "order", all.x = TRUE, all.y = TRUE)
ord.null.trol.lim.homies <- merge(ord.null.trot.lim, homies.order, by = "order", all.x = TRUE, all.y = TRUE)

df.order <- ord.null.trol.lim.homies
df.order[is.na(df.order)] <- 0

df.order$prop.null <- df.order$null.N/nrow(df)

df.order$prop.homies <- df.order$homies.N/nrow(df[df$n.cont == "1",])
df.order$prop.lim <- df.order$limited.N/nrow(df[df$n.cont == "2",])
df.order$prop.trot <- df.order$trotter.N/nrow(df[df$n.cont == "3+",])

#binomial test
for(i in 1:nrow(df.order)){
  test <- binom.test(df.order$homies.N[i], nrow(df[df$n.cont == "1",]), p = df.order$prop.null[i], alternative = "two.sided")
  df.order$p.homies[i] <- test$p.value
}

for(i in 1:nrow(df.order)){
  test <- binom.test(df.order$limited.N[i], nrow(df[df$n.cont == "2",]), p = df.order$prop.null[i], alternative = "two.sided")
  df.order$p.lim[i] <- test$p.value
}

for(i in 1:nrow(df.order)){
  test <- binom.test(df.order$trotter.N[i], nrow(df[df$n.cont == "3+",]), p = df.order$prop.null[i], alternative = "two.sided")
  df.order$p.trot[i] <- test$p.value
}

#add sidak correction
df.order <- arrange(df.order, p.homies) %>%
  dplyr::mutate(signif.homies = p.homies < 0.05,
                signif.bonferoni.homies = p.homies < 0.05/n(),
                signif.holm.homies = !0.05/(n() + 1 - 1:n()) < p.homies,
                signif.sidak.homies = p.homies < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.homies = !(1 - (1 - 0.05)^(1/n())) < p.homies)

df.order <- arrange(df.order, p.lim) %>%
  dplyr::mutate(signif.lim = p.lim < 0.05,
                signif.bonferoni.lim = p.lim < 0.05/n(),
                signif.holm.lim = !0.05/(n() + 1 - 1:n()) < p.lim,
                signif.sidak.lim = p.lim < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.lim = !(1 - (1 - 0.05)^(1/n())) < p.lim)

df.order <- arrange(df.order, p.trot) %>%
  dplyr::mutate(signif.trot = p.trot < 0.05,
                signif.bonferoni.trot = p.trot < 0.05/n(),
                signif.holm.trot = !0.05/(n() + 1 - 1:n()) < p.trot,
                signif.sidak.trot = p.trot < 1 - (1 - 0.05)^(1/n()),
                signif.holm.sidak.trot = !(1 - (1 - 0.05)^(1/n())) < p.trot)

write.csv(df.order, "order.results.csv")

#test if body size dependent
##limited dispersers
lim.true <- subset(df.order, df.order$signif.sidak.lim == TRUE, select = c(order,
                                                                           null.N,
                                                                           limited.N,
                                                                           prop.null,
                                                                           prop.lim,
                                                                           signif.sidak.lim))

lim.false <- subset(df.order, df.order$signif.sidak.lim == FALSE, select = c(order,
                                                                             null.N,
                                                                             limited.N,
                                                                             prop.null,
                                                                             prop.lim,
                                                                             signif.sidak.lim))

order.lim.true <- df[df$order %in% lim.true$order,]
hist(log10(order.lim.true$avg.mass))

order.lim.false <- df[df$order %in% lim.false$order,]
hist(log10(order.lim.false$avg.mass))

ks.test(order.lim.true$avg.mass, order.lim.false$avg.mass) #sig diff
ks.test(order.lim.true$avg.mass, order.lim.false$avg.mass, alternative = "greater") ##sig
ks.test(order.lim.true$avg.mass, order.lim.false$avg.mass, alternative = "less") #not sig

##globe trotters
trot.true <- subset(df.order, df.order$signif.sidak.trot == TRUE, select = c(order,
                                                                             null.N,
                                                                             trotter.N,
                                                                             prop.null,
                                                                             prop.trot,
                                                                             signif.sidak.trot))
##THERE AREN'T ANY (means something else is at play to be truly wide ranging)

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
sum(insect.stat$count) #1967 total
sum(insect.stat$count[insect.stat$n.cont == 2]) #140
#105 out of 140 are chiroptera

bats <- df[df$order == "Chiroptera",]
bats.2 <- bats[bats$n.cont == 2,]
length(bats.2$binomial[bats.2$diet.invertivore.tot == TRUE]) #105 of
length(bats.2$binomial) #142
table(bats.2$diet.breadth)
bats.2.2 <- bats.2[bats.2$diet.breadth == 2,] #3 eat meat, 2 eat fish, 18 eat fruit

## ORIGIN OF FAMILY ----
#jumpers (no longer living where family originated) and spreaders (living where family originated and other continents too)

#gather data

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

write.csv(homies.origin, "homies.family.origin.csv")

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
                    labels = c("Africa",
                               "Australia",
                               "Eurasia",
                               "North America",
                               "South America")) +
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
write.csv(limited.cont, "limited.family.origin.csv")

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
                    labels = c("Africa",
                               "Australia",
                               "Eurasia",
                               "North America",
                               "South America")) +
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
write.csv(trotter.cont, "trotter.family.origin.csv")

## CONNECTIVITY ----
#calculate sørensen index
sorensen <- function(x,y) {
  index = (2*(length(intersect(x, y))))/(length(x) + length(y))
  return(index)
}

continents <- names(dplyr::select(df, starts_with("continent.")))
continent <- gsub("continent.", "", continents)
indeces <- matrix(nrow = 5, ncol = 5, dimnames = list(continent, continent))

indeces["North.America", "South.America"] <- sorensen(x = df$binomial[df$continent.North.America == TRUE], 
                                                      y = df$binomial[df$continent.South.America == TRUE])
indeces["North.America", "Eurasia"] <- sorensen(x = df$binomial[df$continent.North.America == TRUE], 
                                                y = df$binomial[df$continent.Eurasia == TRUE])
indeces["North.America", "Africa"] <- sorensen(x = df$binomial[df$continent.North.America == TRUE], 
                                               y = df$binomial[df$continent.Africa == TRUE])
indeces["North.America", "Australia"] <- sorensen(x = df$binomial[df$continent.North.America == TRUE], 
                                                  y = df$binomial[df$continent.Australia == TRUE])
indeces["South.America", "Eurasia"] <- sorensen(x = df$binomial[df$continent.South.America == TRUE], 
                                                y = df$binomial[df$continent.Eurasia == TRUE])
indeces["South.America", "Africa"] <- sorensen(x = df$binomial[df$continent.South.America == TRUE], 
                                               y = df$binomial[df$continent.Africa == TRUE])
indeces["South.America", "Australia"] <- sorensen(x = df$binomial[df$continent.South.America == TRUE], 
                                                  y = df$binomial[df$continent.Australia == TRUE])
indeces["Eurasia", "Africa"] <- sorensen(x = df$binomial[df$continent.Eurasia == TRUE], 
                                         y = df$binomial[df$continent.Africa == TRUE])
indeces["Eurasia", "Australia"] <- sorensen(x = df$binomial[df$continent.Eurasia == TRUE], 
                                            y = df$binomial[df$continent.Australia == TRUE])
indeces["Africa", "Australia"] <- sorensen(x = df$binomial[df$continent.Africa == TRUE], 
                                           y = df$binomial[df$continent.Australia == TRUE])
write.csv(indeces, "sorensen.index.csv")

####DELVING DEEPER----

min(df$avg.mass[df$order == "Carnivora"], na.rm = TRUE)
max(df$avg.mass[df$order == "Carnivora"], na.rm = TRUE)

min(df$avg.mass[df$order == "Carnivora" &
                df$n.cont == "3+"], na.rm = TRUE)
max(df$avg.mass[df$order == "Carnivora" &
                df$n.cont == "3+"], na.rm = TRUE)

##which two continents are limited dispersers on?
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
limited.cont <- limited.cont[limited.cont$family.origin != "",]

#eutherians v marsupial limited dispersals
lim.cont <- lim.disp %>%
  group_by(order, n.cont) %>%
  dplyr::summarise(count = n()) %>%
  as.data.frame()
marsup <- c("Didelphimorphia", "Paucituberculata", "Microbiotheria",
            "Dasyuromorphia", "Peramelemorphia", "Notoryctemorphia",
            "Diprotodontia")
lim.cont[lim.cont$order %in% marsup,]
length(unique(df$binomial[df$order == "Didelphimorphia"])) #5 (only 3%; 5.162)
length(unique(df$binomial[df$order == "Didelphimorphia" &
                          df$continent.South.America == TRUE])) #83 (represent 6%; 83/1200)
length(unique(df$binomial[df$continent.South.America == TRUE])) #1200
length(unique(df$binomial[df$continent.South.America == TRUE &
                          df$n.cont == 2])) #162

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

table(df.3$n.cont) #179 on 1 continent, 5 on 2 continents

##deeper look into those with dietary breadth of 2
#only for diet.breadth == 2
## total
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.grazer.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.carnivore.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.frugivore.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.invertivore.tot == TRUE & 
                   df$diet.breadth == 2])
length(df$binomial[df$diet.browser.tot == TRUE & 
                   df$diet.piscivore.tot == TRUE & 
                   df$diet.breadth == 2])
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

#gather proportions




# what does that look like for those on lots of continents?
dist$n.cont[dist$n.cont == 4] <- "3+"
dist$n.cont[dist$n.cont == 3] <- "3+"
dist$n.cont <- as.factor(dist$n.cont)

dist_stats <- dist %>%
  dplyr::group_by(n.cont) %>%
  dplyr::summarise(sample.size = length(unique(binomial)),
                   min.dist.hmrg = min(tot.disp.hmrg, na.rm = TRUE),
                   max.dist.hmrg = max(tot.disp.hmrg, na.rm = TRUE),
                   mean.dist.hmrg = mean(tot.disp.hmrg, na.rm = TRUE),
                   min.dist.disp = min(disp.dist, na.rm = TRUE),
                   max.dist.disp = max(disp.dist, na.rm = TRUE),
                   mean.dist.disp = mean(disp.dist, na.rm = TRUE)) %>%
  as.data.frame()

#unimpeded animals can get across continents; clearly some filtering
#is the filtering clade or ecological type specific? answer than by looking at families or something
#problem with home range: already constricted by filtering of some sort

ggplot(data = dist) + 
  geom_histogram(aes(log10(disp.dist), fill = n.cont))

## TEST: Connectivity and shared species----
#calculate sørensen index
sorensen <- function(x,y) {
  index = (2*(length(intersect(x, y))))/(length(x) + length(y))
  return(index)
}

continents <- c("North.America", "South.America", "Eurasia", "Africa", "Australia")
indeces <- matrix(nrow = 5, ncol = 5, dimnames = list(continents, continents))

indeces[1,2] <- sorensen(x = df$binomial[df$continent == "North.America"], 
                         y = df$binomial[df$continent == "South.America"])
indeces[1,3] <- sorensen(x = df$binomial[df$continent == "North.America"], 
                         y = df$binomial[df$continent == "Eurasia"])
indeces[1,4] <- sorensen(x = df$binomial[df$continent == "North.America"], 
                         y = df$binomial[df$continent == "Africa"])
indeces[1,5] <- sorensen(x = df$binomial[df$continent == "North.America"], 
                         y = df$binomial[df$continent == "Australia"])
indeces[2, 3] <- sorensen(x = df$binomial[df$continent == "South.America"], 
                          y = df$binomial[df$continent == "Eurasia"])
indeces[2,4] <- sorensen(x = df$binomial[df$continent == "South.America"], 
                         y = df$binomial[df$continent == "Africa"])
indeces[2,5] <- sorensen(x = df$binomial[df$continent == "South.America"], 
                         y = df$binomial[df$continent == "Australia"])
indeces[3,4] <- sorensen(x = df$binomial[df$continent == "Eurasia"], 
                         y = df$binomial[df$continent == "Africa"])
indeces[3,5] <- sorensen(x = df$binomial[df$continent == "Eurasia"], 
                         y = df$binomial[df$continent == "Australia"])
indeces[4,5] <- sorensen(x = df$binomial[df$continent == "Africa"], 
                         y = df$binomial[df$continent == "Australia"])
write.csv(indeces, "sorensen.index.csv")

#place of origin: where do most globe trotters originate from? chi.square test
#locomotion
#figure: stacked bar graph

##phylogenetic test---
#test for lambda


## TEST: Proportion of mammal species on 1, 2, 3+ continents----
#n per order
n.df <- df %>%
  group_by(order) %>%
  summarise(n.order = length(unique(binomial)),
            n.cont.order = length(unique(n.cont)),
            percent.order = (length(unique(binomial))/length(unique(df$binomial)))*100) %>%
  as.data.frame()


length(unique(df$binomial)) #4463
length(unique(df$binomial[df$n.cont == 1])) #4176
length(unique(df$binomial[df$n.cont == 2])) #281 
length(unique(df$binomial[df$n.cont == "3+"])) #6

unique(df[which(df$n.cont == "3+"), "binomial"])


sorensen <- function(x,y) {
  index = (2*(length(intersect(x, y))))/(length(x) + length(y))
  return(index)
}

continents <- c("North.America", "South.America", "Eurasia", "Africa", "Australia")
indeces <- matrix(nrow = 5, ncol = 5, dimnames = list(continents, continents))

indeces[1,2] <- sorensen(x = df$binomial[df$continent == "North.America"], 
                         y = df$binomial[df$continent == "South.America"])
indeces[1,3] <- sorensen(x = df$binomial[df$continent == "North.America"], 
                         y = df$binomial[df$continent == "Eurasia"])
indeces[1,4] <- sorensen(x = df$binomial[df$continent == "North.America"], 
                         y = df$binomial[df$continent == "Africa"])
indeces[1,5] <- sorensen(x = df$binomial[df$continent == "North.America"], 
                         y = df$binomial[df$continent == "Australia"])
indeces[2, 3] <- sorensen(x = df$binomial[df$continent == "South.America"], 
                          y = df$binomial[df$continent == "Eurasia"])
indeces[2,4] <- sorensen(x = df$binomial[df$continent == "South.America"], 
                         y = df$binomial[df$continent == "Africa"])
indeces[2,5] <- sorensen(x = df$binomial[df$continent == "South.America"], 
                         y = df$binomial[df$continent == "Australia"])
indeces[3,4] <- sorensen(x = df$binomial[df$continent == "Eurasia"], 
                         y = df$binomial[df$continent == "Africa"])
indeces[3,5] <- sorensen(x = df$binomial[df$continent == "Eurasia"], 
                         y = df$binomial[df$continent == "Australia"])
indeces[4,5] <- sorensen(x = df$binomial[df$continent == "Africa"], 
                         y = df$binomial[df$continent == "Australia"])
write.csv(indeces, "sorensen.index.csv")


## TEST: animals that are widespread have a larger geographic range than predicted for body size----

for(i in 1:length(df$binomial)){
  if(df$continent[i] == "Eurasia"){
    df$tot.area[i] <- (54.75*10^6)
  }else if(df$continent[i] == "Africa"){
    df$tot.area[i] <- (30.38*10^6)
  }else if(df$continent[i] == "North.America"){
    df$tot.area[i] <- (24.70*10^6)
  }else if(df$continent[i] == "South.America"){
    df$tot.area[i] <- (17.83*10^6)
  }else{
    df$tot.area[i] <- (7.69*10^6)
  }
}

#want to combine to unique spp
df.cont <- df %>%
  dplyr::group_by(binomial) %>%
  dplyr::summarise(cont.tot.area = sum(tot.area), 
                   pan.gr.area = X26.1_GR_Area_km2[1], 
                   hmrg = X22.1_HomeRange_km2[1],
                   faurby.nat.range = present.natural.range.km2[1], 
                   faurby.current.range = current.range.km2[1],
                   num.cont = n.cont[1], 
                   size = mean(mass)) %>%
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
