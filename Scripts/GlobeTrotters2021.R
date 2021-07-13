# Globe Trotters
# Meghan A. Balk
# balkm@email.arizona.edu

##load packages----
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)
require(stringr)
require(ape)
require(caper)
require(phytools)

##load data----
options(stringsAsFactors = FALSE)

mom <- read.csv("MOMv11.csv", header = TRUE)
pacifici <- read.csv("Generation Lenght for Mammals.csv", header = TRUE)
origin <- read.csv("familyOrigin.csv", header = TRUE)
pbdb <- read.csv("pbdb.data.csv", as.is = T)
faurby.ages <- read.csv("species.age.csv", header = TRUE, row.names = 1)
ranges <- read.csv("ranges.csv", header = TRUE)
pantheria <- read.csv("pantheria.csv", header = TRUE)

## AGES----

sp <- unique(mom$binomial)

### Fossil age
#age data fossil = PBDB min & max occurence estiamtes. 
#This provides different fossil data at different resolutions. 
#The ages extracted here are based only on species level identifications of fossils. 
#All fossils are provided with a maximum and minimum estimated age. 
#To get the most likely age of species origin we found the oldest minimum species age, and the oldest maximum species age for each species. 
#The midpoint of this range was used as species age. Because of species name mismatches and missing species the following analysis includes 693 species out of 4443 possible.

foss.ages <- pbdb %>%
  mutate(binomial = accepted_name) %>%
  group_by(binomial) %>%
  summarise(lw.range = max(min_ma),
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

##manipulate data----
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
                continent.family = continent)

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
mom.origin <- left_join(mom, origin.trim,
                        by = "family")

mom.origin.age <- left_join(mom.origin, age.trim, 
                            by = "genus")

mom.origin.age.gen <- left_join(mom.origin.age, pacifici.trim,
                                by = "binomial") #why is it adding 2 rows?
x <- mom.origin.age.gen[duplicated(mom.origin.age.gen$index),]
y.1 <- mom.origin.age.gen[mom.origin.age.gen$index == x$index[1],]
pacifici.trim[pacifici.trim$binomial == y.1$binomial,] #no information for GenLength == 3206.296, remove; 1st index
y.2 <- mom.origin.age.gen[mom.origin.age.gen$index == x$index[2],]
pacifici.trim[pacifici.trim$binomial == y.2$binomial,] #no information for GenLength == 6095.5, remove; 2nd index
a <- mom.origin.age.gen
a.1 <- a[!(a$binomial == y.1$binomial[1] & a$GenerationLength_d == y.1$GenerationLength_d[1]),]
a.2 <- a.1[!(a.1$binomial == y.2$binomial[1] & a.1$GenerationLength_d == y.2$GenerationLength_d[2]),]

mom.origin.age.gen.foss <- left_join(a.2, foss.ages,
                                     by = "binomial")

mom.origin.age.gen.foss.phyl <- left_join(mom.origin.age.gen.foss, phyl.ages,
                                          by = "binomial")

mom.origin.age.gen.foss.phyl.ranges <- left_join(mom.origin.age.gen.foss.phyl, ranges.trim,
                                                 by = "binomial")

mom.origin.age.gen.foss.phyl.ranges.pan <- left_join(mom.origin.age.gen.foss.phyl.ranges, pantheria.trim,
                                                     by = "binomial")

df <- mom.origin.age.gen.foss.phyl.ranges.pan

##remove marine species
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

##fix continents
# Search for continental mistakes
table(df$continent, useNA = "always")

# Remove NA's, Insulars, and Marine
df <- filter(df, !is.na(continent) &
             continent != "Insular" &
             continent != "Marine")

##fix diet
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
df %>% 
  dplyr::select(starts_with("diet")) %>% 
  colSums()

df$diet.src <- NA
df$diet.src[troph.diet] <- "troph.diet"

diet <- df %>%
  dplyr::select(order, family, genus, binomial, starts_with("diet"))

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

df$diet.breadth <- df %>%
  dplyr::select(diet.invertivore:diet.piscivore) %>% 
         rowSums()
table(df$diet.breadth)

##remove continental duplicates
# Checking for accidents
stopifnot(!any(str_trim(df$genus) != df$genus))
stopifnot(!any(str_trim(df$species) != df$species))

df$binomial[duplicated(df$binomial)]

df[(duplicated(df[c("binomial", "continent")])),]

# Remove introduced species
table(df$extant.status)
df <- df[-which(df$extant.status == "introduction"), ]

# Remove domesticated species
df <- df[-which(df$extant.status == "domesticated"), ]

#create ncont
cont <- group_by(df, binomial) %>% summarise(n.cont = n())
df <- left_join(df, cont, 
                by = "binomial")

##data used for analyses----
#write.csv(df, "data.csv")

##plot themes----
col <- c("#2ca25f", "#99d8c9", "#e5f5f9")
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

##data for analyses----
#df <- read.csv("data.csv", header = TRUE)

## TEST: How many sp are on each continent?----
length(unique(df$binomial))
length(unique(df$binomial[df$n.cont == 1]))
length(unique(df$binomial[df$n.cont == 2])) 
length(unique(df$binomial[df$n.cont >= 3]))

unique(df[which(df$n.cont >= 3), "binomial"])

##DIET----


#figure: stacked bar graph

##DISPERSAL----
## TEST: How far can an animal go?----
df$AFR_d <- as.numeric(df$AFR_d)

dist <- df %>%
  dplyr::group_by(binomial) %>%
  dplyr::summarise(n = n(),
                   foss.avg.age = fossil.age[1]*1000000,
                   age = age.median[1]*1000000,
                   avg.mass = mean(mass, na.rm = TRUE),
                   hmrg = home.range.km2[1],
                   disp.age = dispersal.age.d[1],
                   gen.length = GenerationLength_d[1],
                   repro.age = AFR_d[1], 
                   n.cont = n.cont[1],
                   carn = isTRUE(sum(diet.piscivore + diet.invertivore + diet.carnivore) >= 1 & sum(diet.browser + diet.grazer + diet.frugivore) == 0)) %>%
  as.data.frame()

dist <- dist %>%
  drop_na(hmrg, avg.mass)
  

#model: include lineage age, generation length, dispersal distance, age of reproduction
dist$tot.disp.hmrg = ((dist$age*365)/(dist$gen.length+dist$disp.age))*(dist$hmrg/2) #go radius of homerange
dist$dist.hmrg.per.bs = dist$tot.disp.hmrg/dist$avg.mass

ggplot(data = dist) +
  geom_density(aes(dist.hmrg.per.bs))

#calculate dispersal (From Smith et al. 2016)
#carnivore: Dc = 40.7M^0.81
#herb or omni = Dho = D3.31M^0.65

#model: include lineage age divided by number of generations (generation length + age of first reproduction) multiply by dispersal from above equation

dist$disp.dist[dist$carn == TRUE] = (dist$age[dist$carn == TRUE]*365/(dist$gen.length[dist$carn == TRUE]+dist$disp.age[dist$carn == TRUE]))*(40.7*(dist$avg.mass[dist$carn == TRUE]^0.81))
dist$disp.dist[dist$carn != TRUE] = (dist$age[dist$carn != TRUE]*365/(dist$gen.length[dist$carn != TRUE]+dist$disp.age[dist$carn != TRUE]))*(3.31*(dist$avg.mass[dist$carn != TRUE]^0.65))

Eurasia = 54750000
Africa = 30380000
North.America = 24700000
South.America = 17830000
Australia = 7690000

length(dist$binomial[!is.na(dist$disp.dist)])
length(dist$binomial[!is.na(dist$tot.disp.hmrg)]) 
length(dist$binomial[!is.na(dist$dist.hmrg.per.bs)]) 

length(dist$binomial[dist$disp.dist >= Australia & !is.na(dist$disp.dist)]) 

length(dist$binomial[dist$disp.dist >= South.America & !is.na(dist$tot.disp.hmrg)]) 

length(dist$binomial[dist$disp.dist >= North.America & !is.na(dist$tot.disp.hmrg)]) 

length(dist$binomial[dist$disp.dist >= Africa & !is.na(dist$tot.disp.hmrg)]) 

length(dist$binomial[dist$disp.dist >= Eurasia & !is.na(dist$tot.disp.hmrg)]) 


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

