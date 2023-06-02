#Meghan A. Balk
#meghan.balk@gmail.com
#Rasmus Ø. Pedersen

#Setting up data set to:
#1. remove marine species
#2. remove introduced and domesticated species
#3. fix continental names
#4. categorize diet
#5. add diet breadth
#6. add geographic ranges via Pantheria and Faurby present natural ranges
#7. add fossil ages via Faurby phylogeny and PBDB data
#8. remove duplicates
#9. write out a dataset for subsequent analyses

###### SETUP >>>>>>
# Set working directory

# Loading required packages
require(tidyverse)
require(stringr)
options(stringsAsFactors = FALSE)

#### LOAD DATA ----
df <- read.csv("../Data/MOMv11.csv",
               header = TRUE)

pan <- read.csv("../Data/pantheria.csv",
                header = TRUE)

ranges <- read.csv("../Data/ranges.csv",
                   header = TRUE)

pacifici <- read.csv("../Data/Generation Length for Mammals.csv", 
                     header = TRUE)

origin <- read.csv("../Data/familyOrigin.csv", 
                   header = TRUE)

#### REMOVE MARINE SPECIES ----

df <- df %>%
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

#### REMOVE INTRODUCED AND DOMESTICATED SPECIES ----

df <- df %>%
  filter(extant.status != "introduction") %>%
  filter(extant.status != "domesticated")

#### FIX CONTINENTS ----
# Search for continental mistakes
table(df$continent, useNA = "always")

# Remove NA's, Insulars, and Marine
df <- filter(df, !is.na(continent) &
                 continent != "Insular" &
                 continent != "Marine")

#### FIX DIET ----
df$trophic[which(df$trophic == "")] <- NA
sort(table(df$trophic))


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


troph.diet <- which(df$trophic %in% c(invertivore, carnivore, browser, grazer, frugivore, piscivore))

df <- df %>% mutate(diet.invertivore = trophic %in% invertivore,
                    diet.carnivore = trophic %in% carnivore,
                    diet.browser = trophic %in% browser,
                    diet.grazer = trophic %in% grazer,
                    diet.frugivore = trophic %in% frugivore,
                    diet.piscivore = trophic %in% piscivore)

# Find NAs and replace them with genereic averages
select(df, starts_with("diet")) %>% colSums()

df$diet.src <- NA
df$diet.src[troph.diet] <- "troph.diet"

diet <- select(df, order, family, genus, binomial, starts_with("diet"))

species.diet <- filter(diet, !is.na(diet.src)) %>% group_by(binomial) %>%
  summarise(family = family[1],
            genus = genus[1],
            diet.invertivore = sum(diet.invertivore, na.rm = TRUE) >= 1,
            diet.carnivore = sum(diet.carnivore, na.rm = TRUE) >= 1,
            diet.browser = sum(diet.browser, na.rm = TRUE) >= 1,
            diet.grazer = sum(diet.grazer, na.rm = TRUE) >= 1,
            diet.frugivore = sum(diet.frugivore, na.rm = TRUE) >= 1,
            diet.piscivore = sum(diet.piscivore, na.rm = TRUE) >= 1,
            diet.src = "species.diet")

genus.diet <- group_by(species.diet, genus) %>%
  summarise(family = family[1],
            diet.invertivore = mean(diet.invertivore, na.rm = TRUE) > 0.5,
            diet.carnivore = mean(diet.carnivore, na.rm = TRUE) > 0.5,
            diet.browser = mean(diet.browser, na.rm = TRUE) > 0.5,
            diet.grazer = mean(diet.grazer, na.rm = TRUE) > 0.5,
            diet.frugivore = mean(diet.frugivore, na.rm = TRUE) > 0.5,
            diet.piscivore = mean(diet.piscivore, na.rm = TRUE) > 0.5,
            diet.src = "mean.genus")

family.diet <- group_by(genus.diet, family) %>%
  summarise(diet.invertivore = mean(diet.invertivore, na.rm = TRUE) > 0.5,
            diet.carnivore = mean(diet.carnivore, na.rm = TRUE) > 0.5,
            diet.browser = mean(diet.browser, na.rm = TRUE) > 0.5,
            diet.grazer = mean(diet.grazer, na.rm = TRUE) > 0.5,
            diet.frugivore = mean(diet.frugivore, na.rm = TRUE) > 0.5,
            diet.piscivore = mean(diet.piscivore, na.rm = TRUE) > 0.5,
            diet.src = "mean.family")

for(i in 1:nrow(df)) {
  if(df$binomial[i] %in% species.diet$binomial) {
    k <- which(species.diet$binomial == df$binomial[i])
    df[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      species.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else if(df$genus[i] %in% genus.diet$genus) {
    k <- which(genus.diet$genus == df$genus[i])
    df[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      genus.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else if(df$family[i] %in% family.diet$family) {
    k <- which(family.diet$family == df$family[i])
    df[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      family.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else {
    stop("Cannot find diet")
  }
}

table(df$diet.src, useNA = "always")

#### ADD DIET BREADTH ----

df$diet.breadth <- select(df, diet.invertivore:diet.piscivore) %>% rowSums()
table(df$diet.breadth)

df[which(df$diet.breadth == 4),]

#### ADD RANGES ----
binomials <- df$binomial

pan <- pan %>%
  dplyr::rename(binomial = MSW05_Binomial,
                home.range.km2 = X22.1_HomeRange_km2,
                gr.area.km2 = X26.1_GR_Area_km2,
                indiv.home.range.km2 = X22.2_HomeRange_Indiv_km2,
                dispersal.age.d = X7.1_DispersalAge_d) %>%
  dplyr::select(binomial, gr.area.km2,
                home.range.km2, indiv.home.range.km2,
                dispersal.age.d)
pan <- pan[(pan$binomial %in% binomials),]

ranges <- ranges %>%
  rename(binomial = Binomial.1.2) %>%
  dplyr::select(binomial, current.range.km2, present.natural.range.km2)

ranges$binomial <- gsub("_", " ", ranges$binomial)
ranges <- ranges[(ranges$binomial %in% binomials),]

df <- left_join(df, pan, "binomial")
df <- left_join(df, ranges, "binomial")

#### ADD AGES ----

##### Fossil age -----
#age data fossil = PBDB min & max occurence estiamtes. 
#This provides different fossil data at different resolutions. 
#The ages extracted here are based only on species level identifications of fossils. 
#All fossils are provided with a maximum and minimum estimated age. 
#To get the most likely age of species origin we found the oldest minimum species age, and the oldest maximum species age for each species. 
#The midpoint of this range was used as species age. Because of species name mismatches and missing species the following analysis includes 693 species out of 4443 possible.

pbdb <- read.csv("../Data/pbdb.data.csv",
                 as.is = T)
foss.age <-
  pbdb %>%
  mutate(binomial = accepted_name) %>%
  group_by(binomial) %>%
  summarise(lw.range = max(min_ma),
            hi.range = max(max_ma),
            foss.age = (hi.range+lw.range)/2)


df <- left_join(df, foss.age, "binomial")

##### Faurby ages -----
#age data phyl = from Faurby tree estimates. 
#This source provides 1000 equally likely trees. 
#The species ages were extracted as branch length to the parent node of each species for all trees. 
#The estimate used for species ages were the median ages found by this method. Because of species name mismatches the following analysis includes 4019 species out of 4443 possible.

age <- read.csv("../Data/species.age_Faurby.csv", #from Faurby
                header = TRUE,
                row.names = 1)
species.age.summary <- function(x) {
  c(age.mean = mean(x),
    age.median = median(x),
    age.lower.range = range(x)[1],
    age.upper.range = range(x)[2],
    age.q95 = quantile(x, .95),
    age.q05 = quantile(x, .05),
    age.sd = sd(x))
}
species.age <- apply(age, 1, species.age.summary)
str(species.age)

species.age[1:7,1:5]

faurby.ages <- as.data.frame(t(species.age))
faurby.ages$binomial <- rownames(faurby.ages)
faurby.ages$binomial <- gsub("_", " ", faurby.ages$binomial)

faurby.ages <- faurby.ages %>%
  dplyr::select(binomial, age.median) 

df <- left_join(df, faurby.ages, "binomial")

# genus level
# age <- read.csv("age.csv", header = TRUE) #match on genus
# age <- age %>%
#   dplyr::select(-cont)
# genera <- unique(mom$genus)
# age <- age[(age$genus %in% genera),]

#### CONTINENTAL DUPLICATES ----
# Checking for accidents
stopifnot(!any(str_trim(df$genus) != df$genus))
stopifnot(!any(str_trim(df$species) != df$species))

df$binomial[duplicated(df$binomial)]

df[(duplicated(df[c("binomial", "continent")])),]

cont <- group_by(df, binomial) %>% summarise(n.cont = n())
df <- left_join(df, cont, by = "binomial")

#### DATA WRANGEL ----

df$n.cont[df$n.cont == 4] <- "3+"
df$n.cont[df$n.cont == 3] <- "3+"
df$n.cont <- as.factor(df$n.cont)

#### ADD OTHER DATA ----

pacifici.trim <- pacifici %>%
  dplyr::select(binomial = Scientific_name, 
                Max_longevity_d, 
                Rspan_d, 
                AFR_d, 
                Calculated_GL_d, 
                GenerationLength_d)

df <- left_join(df, pacifici.trim,
                  by = "binomial")

origin.trim <- origin %>%
  dplyr::select(family,
                continent.family = final.continent)

df <- left_join(df, origin.trim,
                by = "family")

#### WRITE OUT DATA ----

write.csv(df, "../Data/MOM.global.mammals.csv", row.names = FALSE)
