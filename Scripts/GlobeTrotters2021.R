#load libraries
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)
require(stringr)

## LOAD DATA----
options(stringsAsFactors = FALSE)

mom <- read.csv("MOMv11.csv", header = TRUE)

## TRIM DATA----
#reduce data to major continents, terrestrial things, and non-introduced or domesticated spp
clean.mom <- as.data.frame(subset(mom, mom$continent == "Africa" | 
                                       mom$continent == "North.America" | 
                                       mom$continent == "South.America" | 
                                       mom$continent == "Eurasia" |
                                       mom$continent == "Australia"))
clean.mom <- as.data.frame(subset(clean.mom, clean.mom$extant.status != "introduction" &
                                             clean.mom$extant.status != "domesticated"))

## DIET----
diet.mom <- clean.mom
diet.mom$trophic[which(diet.mom$trophic == "")] <- NA
sort(table(diet.mom$trophic))


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


troph.diet <- which(diet.mom$trophic %in% c(invertivore, carnivore, browser, grazer, frugivore, piscivore))

diet.mom <- diet.mom %>% mutate(diet.invertivore = trophic %in% invertivore,
                    diet.carnivore = trophic %in% carnivore,
                    diet.browser = trophic %in% browser,
                    diet.grazer = trophic %in% grazer,
                    diet.frugivore = trophic %in% frugivore,
                    diet.piscivore = trophic %in% piscivore)

# Find NAs and replace them with genereic averages
select(diet.mom, starts_with("diet")) %>% colSums()

diet.mom$diet.src <- NA
diet.mom$diet.src[troph.diet] <- "troph.diet"

diet <- select(diet.mom, order, family, genus, binomial, starts_with("diet"))

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

for(i in 1:nrow(diet.mom)) {
  if(diet.mom$binomial[i] %in% species.diet$binomial) {
    k <- which(species.diet$binomial == diet.mom$binomial[i])
    diet.mom[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      species.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else if(diet.mom$genus[i] %in% genus.diet$genus) {
    k <- which(genus.diet$genus == diet.mom$genus[i])
    diet.mom[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      genus.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else if(diet.mom$family[i] %in% family.diet$family) {
    k <- which(family.diet$family == diet.mom$family[i])
    diet.mom[i, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")] <-
      family.diet[k, c("diet.invertivore", "diet.carnivore", "diet.browser", "diet.grazer", "diet.frugivore", "diet.piscivore", "diet.src")]
  } else {
    stop("Cannot find diet")
  }
}

table(diet.mom$diet.src, useNA = "always")

diet.mom$diet.breadth <- select(diet.mom, diet.invertivore:diet.piscivore) %>% rowSums()
table(diet.mom$diet.breadth)

## RANGES----
range.mom <- diet.mom
binomials <- range.mom$binomial

pan <- read.csv("pantheria.csv", header = TRUE)
pan <- pan %>%
  dplyr::rename(binomial = MSW05_Binomial) %>%
  dplyr::select(binomial, X26.1_GR_Area_km2, X22.1_HomeRange_km2, 
                X22.2_HomeRange_Indiv_km2, X7.1_DispersalAge_d)
pan <- pan[(pan$binomial %in% binomials),]

ranges <- read.csv("ranges.csv", header = TRUE)
ranges <- ranges %>%
  rename(binomial = Binomial.1.2) %>%
  dplyr::select(binomial, current.range.km2, present.natural.range.km2)

ranges$binomial <- gsub("_", " ", ranges$binomial)
ranges <- ranges[(ranges$binomial %in% binomials),]

range.mom <- left_join(range.mom , pan, "binomial")
range.mom <- left_join(range.mom , ranges, "binomial")

## AGES----

### Fossil age
#age data fossil = PBDB min & max occurence estiamtes. 
#This provides different fossil data at different resolutions. 
#The ages extracted here are based only on species level identifications of fossils. 
#All fossils are provided with a maximum and minimum estimated age. 
#To get the most likely age of species origin we found the oldest minimum species age, and the oldest maximum species age for each species. 
#The midpoint of this range was used as species age. Because of species name mismatches and missing species the following analysis includes 693 species out of 4443 possible.

age.mom <- range.mom

pbdb <- read.csv("pbdb.data.csv", as.is = T)
foss.age <-
  pbdb %>%
  mutate(binomial = accepted_name) %>%
  group_by(binomial) %>%
  summarise(lw.range = max(min_ma),
            hi.range = max(max_ma),
            foss.age = (hi.range+lw.range)/2)


age.mom <- left_join(age.mom, foss.age, "binomial")

### Faurby ages
#age data phyl = from Faurby tree estimates. 
#This source provides 1000 equally likely trees. 
#The species ages were extracted as branch length to the parent node of each species for all trees. 
#The estimate used for species ages were the median ages found by this method. Because of species name mismatches the following analysis includes 4019 species out of 4443 possible.

age <- read.csv("species.age.csv", header = TRUE, row.names = 1)
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

age.mom <- left_join(age.mom, faurby.ages, "binomial")

# genus level
# age <- read.csv("age.csv", header = TRUE) #match on genus
# age <- age %>%
#   dplyr::select(-cont)
# genera <- unique(mom$genus)
# age <- age[(age$genus %in% genera),]

## CONTINENTAL----
# Checking for accidents
cont.mom <- age.mom

stopifnot(!any(str_trim(df$genus) != cont.mom$genus))
stopifnot(!any(str_trim(df$species) != cont.mom$species))

cont.mom$binomial[duplicated(cont.mom$binomial)]

cont.mom[(duplicated(cont.mom[c("binomial", "continent")])),]

cont <- group_by(cont.mom, binomial) %>% summarise(n.cont = n())
cont.mom <- left_join(cont.mom, cont, by = "binomial")

## GENERATION LENGTH
pacifici <- read.csv("Generation Lenght for Mammals.csv", header = TRUE)

pacifici <- pacifici %>%
  dplyr::rename(binomial = Scientific_name) %>%
  dplyr::select(binomial, GenerationLength_d, Calculated_GL_d, AFR_d, Rspan_d)
pacifici <- pacifici[(pan$binomial %in% binomials),]

genL.mom <- left_join(cont.mom, pacifici, "binomial")

## WRITE DATA----
df <- genL.mom
write.csv(df, "MOM.global.mammals.csv", row.names = FALSE)

## TEST: Proportion of mammal species on 1, 2, 3+ continents----
#n per order
n.df <- df %>%
  group_by(order) %>%
  summarise(n.order = length(unique(binomial)),
            n.cont.order = length(unique(n.cont)),
            percent.order = (length(unique(binomial))/length(unique(df$binomial)))*100) %>%
  as.data.frame()

## TEST: How many spp are on ea continent?----
length(unique(df$binomial)) #4426
length(unique(df$binomial[df$n.cont == 1])) #4176
length(unique(df$binomial[df$n.cont == 2])) #281 (6.73%)
length(unique(df$binomial[df$n.cont == 3 | df$n.cont == 4])) #6 (0.14%)

unique(df[which(df$n.cont == 3 | df$n.cont == 4), "binomial"])

## TEST: Connectivity and shared species----
## calculate sørensen index
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

## TEST: Species from more diverse orders are more places----

#make matrix of stats

#baseline for order
n.df <- df %>%
  group_by(order) %>%
  summarise(n.tot = length(unique(binomial)),
            n.cont.tot = length(unique(n.cont)),
            per.tot = length(unique(binomial))/length(unique(df$binomial))) %>% 
  as.data.frame()

n.df.per <- df %>%
  group_by(order, n.cont) %>%
  summarise(n = length(unique(binomial))) %>%
  as.data.frame()

df.orders <- merge(n.df, n.df.per, by = "order", all.y = TRUE)

df.orders$per.1[df.orders$n.cont == 1] <- (df.orders$n[df.orders$n.cont == 1]/sum(df.orders$n[df.orders$n.cont == 1])) * 100
df.orders$per.2[df.orders$n.cont == 2] <- (df.orders$n[df.orders$n.cont == 2]/sum(df.orders$n[df.orders$n.cont == 2])) * 100
df.orders$per.3.4[df.orders$n.cont >= 3] <- (df.orders$n[df.orders$n.cont >= 3]/sum(df.orders$n[df.orders$n.cont >= 3])) * 100

df.orders$comp.2[df.orders$n.cont == 2]  <- df.orders$per.2[df.orders$n.cont == 2] - df.orders$per.tot[df.orders$n.cont == 2]
df.orders$comp.3.4[df.orders$n.cont >= 3] <- df.orders$per.3.4[df.orders$n.cont >= 3] - df.orders$per.tot[df.orders$n.cont >= 3]

View(df.orders)

#which orders are over or under represented on 2 or 3+ continents?
sort(df.orders$comp.2)
df.orders$order[df.orders$comp.2 > 10] #Carnivora, Chiroptera, Rodentia

min(df.orders$comp.2, na.rm = TRUE) #0.35%; likely not sig

max(df.orders$comp.2, na.rm = TRUE) #53.55%
which.max(df.orders$comp.2)
df.orders[10,] #Chiroptera

sort(df.orders$comp.3.4) #all high
df.orders$order[df.orders$comp.3.4 > 1]
#Artiodactyla, Carnivora, Chiroptera

min(df.orders$comp.3.4, na.rm = TRUE) #16.49%
which.min(df.orders$comp.3.4)
df.orders[11,] #Chiroptera
max(df.orders$comp.3.4, na.rm = TRUE) #49.94%
which.max(df.orders$comp.3.4)
df.orders[7,] #Carnivora

df$n.cont[df$n.cont == 4] <- "3+"
df$n.cont[df$n.cont == 3] <- "3+"
df$n.cont <- as.factor(df$n.cont)

####1 v 2 cont####

df$global <- df$n.cont == 2

# Create a data frame with all species
res <- data.frame(order = unique(df$order))

# Create a dataframe with famlies and count how many species are in each family
res0 <- df %>% 
  group_by(order) %>% 
  dplyr::summarise(count = n())

# Create a dataframe with global orders and count how many species are global
res1 <- filter(df, global) %>% 
  group_by(order) %>% 
  dplyr::summarise(onMult = n())

# Merge the data
res <- left_join(res, res0, by = "order") %>% 
  left_join(res1, by = "order")

# Change NA's to zero
res[is.na(res)] <- 0

# Count how large a proportion we expect of a given family
res <- mutate(res, glob.proportion = count/sum(count))

# Find the total species number on 2+ continents
onMult.tot <- sum(res$onMult)
res <- mutate(res, onMult.proportion = onMult/onMult.tot)
# Run binomial test and add that to the result
# The test compares each familys presence globally to their overall precense in mammalia
res <- purrrlyr::invoke_rows(.d = res,
                             .f = function(onMult, glob.proportion, ...) {
                               test <- binom.test(onMult, onMult.tot, p = glob.proportion, alternative="greater")
                               c(p.val = as.numeric(test$p.value))
                             },
                             .collate = "cols", .to = c("p.value"))

# Find which ones are signifcicant
# And which ones are signifcant bonferoni-corrected since we run many tests
# And 3 other stronger types of corrections which give the same results
res <- arrange(res, p.value) %>% mutate(signif = p.value < 0.05,
                                        signif.bonferoni = p.value < 0.05/n(),
                                        signif.holm = !0.05/(n() + 1 - 1:n()) < p.value,
                                        signif.sidak = p.value < 1 - (1 - 0.05)^(1/n()),
                                        signif.holm.sidak = !(1 - (1 - 0.05)^(1/n())) < p.value)

# Look at the significants
filter(res, signif)

write.csv(res, "onecontvtwo.orders.csv", row.names = FALSE)

####1+2 v 3+####
df$global <- df$n.cont == "3+"

# Create a data frame with all species
res <- data.frame(order = unique(df$order))

# Create a dataframe with famlies and count how many species are in each family
res0 <- df %>% 
  group_by(order) %>% 
  dplyr::summarise(count = n())

# Create a dataframe with global famlies and count how many species are global
res1 <- filter(df, global) %>% 
  group_by(order) %>% 
  dplyr::summarise(on3 = n())

# Merge the data
res <- left_join(res, res0, by = "order") %>% 
  left_join(res1, by = "order")

# Change NA's to zero
res[is.na(res)] <- 0

# Count how large a proportion we expect of a given family
res <- mutate(res, glob.proportion = count/sum(count))

# Find the total species number on 2+ continents
on3.tot <- sum(res$on3)
res <- mutate(res, on3.proportion = on3/on3.tot)
# Run binomial test and add that to the result
# The test compares each familys presence globally to their overall precense in mammalia
res <- purrrlyr::invoke_rows(.d = res,
                             .f = function(on3, glob.proportion, ...) {
                               test <- binom.test(on3, on3.tot, p = glob.proportion, alternative="greater")
                               c(p.val = as.numeric(test$p.value))
                             },
                             .collate = "cols", .to = c("p.value"))

# Find which ones are signifcicant
# And which ones are signifcant bonferoni-corrected since we run many tests
# And 3 other stronger types of corrections which give the same results
res <- arrange(res, p.value) %>% mutate(signif = p.value < 0.05,
                                        signif.bonferoni = p.value < 0.05/n(),
                                        signif.holm = !0.05/(n() + 1 - 1:n()) < p.value,
                                        signif.sidak = p.value < 1 - (1 - 0.05)^(1/n()),
                                        signif.holm.sidak = !(1 - (1 - 0.05)^(1/n())) < p.value)

# Look at the significants
filter(res, signif)

write.csv(res, "onetwocontvthree.orders.csv", row.names = FALSE)

## TEST: How far can an animal go?----
df$AFR_d <- as.numeric(df$AFR_d)

# small mammal (10-1000g)
sm.mamm <- df[df$mass >=10 & df$mass < 1000,]

#get averages for disperal factors
sm.avg <- sm.mamm %>%
  summarise(n = n(),
            avg.age = mean(age.median, na.rm = TRUE)*1000000,
            avg.mass = mean(mass, na.rm = TRUE),
            avg.hmrg = mean(X22.2_HomeRange_Indiv_km2, na.rm = TRUE),
            disp.age = mean(X7.1_DispersalAge_d, na.rm = TRUE),
            gen.length = mean(GenerationLength_d, na.rm = TRUE),
            repro.age = mean(AFR_d, na.rm = TRUE)) %>%
  as.data.frame()
            
# model
#age of lineage in yr*365day/gen.length*hmrg
sm.tot.dist <- ((sm.avg$avg.age*365)/sm.avg$gen.length)*sm.avg$avg.hmrg #164,038.7 km2

#per body size
sm.dist <- sm.tot.dist/sm.avg$avg.mass #1,184.505 km2

# medium mammal (1000-100000g)
med.mamm <- df[df$mass >= 1000 & df$mass < 100000,]

med.avg <- med.mamm %>%
  summarise(n = n(),
            avg.age = mean(age.median, na.rm = TRUE)*1000000,
            avg.mass = mean(mass, na.rm = TRUE),
            avg.hmrg = mean(X22.2_HomeRange_Indiv_km2, na.rm = TRUE),
            disp.age = mean(X7.1_DispersalAge_d, na.rm = TRUE),
            gen.length = mean(GenerationLength_d, na.rm = TRUE),
            repro.age = mean(AFR_d, na.rm = TRUE)) %>%
  as.data.frame()

# model
#age of lineage in yr*365day/gen.length*hmrg
med.tot.dist <- ((med.avg$avg.age*365)/med.avg$gen.length)*med.avg$avg.hmrg #15,586,722 km2

#per body size
med.dist <- med.tot.dist/med.avg$avg.mass #960.8239 km2

# large mammal (>100000g)
lrg.mamm <- df[df$mass >= 100000,]

lrg.avg <- lrg.mamm %>%
  summarise(n = n(),
            avg.age = mean(age.median, na.rm = TRUE)*1000000,
            avg.mass = mean(mass, na.rm = TRUE),
            avg.hmrg = mean(X22.2_HomeRange_Indiv_km2, na.rm = TRUE),
            disp.age = mean(X7.1_DispersalAge_d, na.rm = TRUE),
            gen.length = mean(GenerationLength_d, na.rm = TRUE),
            repro.age = mean(AFR_d, na.rm = TRUE)) %>%
  as.data.frame()

# model
#age of lineage in yr*365day/gen.length*hmrg
lrg.tot.dist <- ((lrg.avg$avg.age*365)/lrg.avg$gen.length)*lrg.avg$avg.hmrg #1,695,920,562 km2

#per body size
lrg.dist <- lrg.tot.dist/lrg.avg$avg.mass #1,864.738 km2

#for all
dist <- df %>%
  group_by(binomial) %>%
  summarise(n = n(),
            avg.age = age.median[1]*1000000,
            avg.mass = mean(mass, na.rm = TRUE),
            avg.hmrg = mean(X22.2_HomeRange_Indiv_km2, na.rm = TRUE),
            disp.age = mean(X7.1_DispersalAge_d, na.rm = TRUE),
            gen.length = mean(GenerationLength_d, na.rm = TRUE),
            repro.age = mean(AFR_d, na.rm = TRUE),
            n.cont = n.cont[1]) %>%
  as.data.frame()

dist$dist.tot.dist = ((dist$avg.age*365)/dist$gen.length)*dist$avg.hmrg
dist$dist.per.bs = dist$dist.tot.dist/dist$avg.mass

# what does that look like for those on lots of continents?
x <- dist[dist$n.cont >= 3,]
x$dist.tot.dist #ranges from 499.66 to 29,139,370
mean(x$dist.tot.dist, na.rm = TRUE) #4,926,767
mean(dist$dist.tot.dist, na.rm = TRUE) #21,242,857

#animals that are widespread have a larger home range than predicted for body size


## TEST: animals that are widespread have a larger geographic range than predicted for body size

for(i in 1:length(data$binomial)){
  if(data$continent[i] == "Eurasia"){
    data$tot.area[i] <- (54.75*10^6)
  }else if(data$continent[i] == "Africa"){
    data$tot.area[i] <- (30.38*10^6)
  }else if(data$continent[i] == "North.America"){
    data$tot.area[i] <- (24.70*10^6)
  }else if(data$continent[i] == "South.America"){
    data$tot.area[i] <- (17.83*10^6)
  }else{
    data$tot.area[i] <- (7.69*10^6)
  }
}

#want to combine to unique spp
df <- data %>%
  dplyr::group_by(binomial) %>%
  dplyr::summarise(cont.tot.area = sum(tot.area), pan.gr.area = X26.1_GR_Area_km2[1], 
                   faurby.nat.range = present.natural.range.km2[1], faurby.current.range = current.range.km2[1],
                   num.cont = n.cont[1], size = mean(mass)) %>%
  as.data.frame()

#get cleanest dataset
df.pan <- subset(df, !is.na(df$pan.gr.area) & !is.na(df$size)) #3084 spp (out of 3497 w/ bs est)

df.faurby <- subset(df, !is.na(df$faurby.nat.range) & !is.na(df$size) & df$faurby.nat.range != 0) #2629 spp (out of 3497 w/ bs est)

length(unique(df.pan$binomial)) #3084
length(unique(df.pan$binomial[df.pan$num.cont == 1])) #2841
length(unique(df.pan$binomial[df.pan$num.cont == 2])) #238
length(unique(df.pan$binomial[df.pan$num.cont == "3+"])) #5

length(unique(df.faurby$binomial)) # 2629
length(unique(df.faurby$binomial[df.faurby$num.cont == 1])) #2517
length(unique(df.faurby$binomial[df.faurby$num.cont == 2])) #108
length(unique(df.faurby$binomial[df.faurby$num.cont == "3+"])) #4

#get ratio of geog range out of total area
df.pan$ratio <- df.pan$pan.gr.area/df.pan$cont.tot.area
df.faurby$ratio <- df.faurby$faurby.nat.range/df.faurby$cont.tot.area

#change to log units
df.pan$logSize <- log10(df.pan$size)
df.pan$logRatio <- log10(df.pan$ratio)

df.faurby$logSize <- log10(df.faurby$size)
df.faurby$logRatio <- log10(df.faurby$ratio)

#Do species on multiple continents occupy a larger area than available when taking body size into account? Yes!
summary(glm(lm(log10(df.pan$ratio) ~ log10(df.pan$size) + as.factor(df.pan$num.cont))))
summary(lm(log10(df.pan$ratio[df.pan$num.cont == "1"]) ~ log10(df.pan$size[df.pan$num.cont == "1"])))
summary(lm(log10(df.pan$ratio[df.pan$num.cont == "2"]) ~ log10(df.pan$size[df.pan$num.cont == "2"])))
summary(lm(log10(df.pan$ratio[df.pan$num.cont == "3+"]) ~ log10(df.pan$size[df.pan$num.cont == "3+"])))

summary(glm(lm(log10(df.faurby$ratio) ~ log10(df.faurby$size) + as.factor(df.faurby$num.cont))))
summary(lm(log10(df.faurby$ratio[df.faurby$num.cont == "1"]) ~ log10(df.faurby$size[df.faurby$num.cont == "1"])))
summary(lm(log10(df.faurby$ratio[df.faurby$num.cont == "2"]) ~ log10(df.faurby$size[df.faurby$num.cont == "2"])))
summary(lm(log10(df.faurby$ratio[df.faurby$num.cont == "3+"]) ~ log10(df.faurby$size[df.faurby$num.cont == "3+"])))

#Do species have a lager geo-range for their body size? Yes!
summary(glm(lm(log10(df.pan$pan.gr.area) ~ log10(df.pan$size) + as.factor(df.pan$num.cont))))
summary(glm(lm(log10(df.faurby$faurby.nat.range) ~ log10(df.faurby$size) + as.factor(df.faurby$num.cont))))

# ranges
ggplot(data = df.pan, aes(x = logSize, y = ratio)) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = col) +
  labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Home~Range/Geographic~Range), color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "top")

ggplot(data = df.faurby, aes(x = logSize, y = ratio)) +
  geom_point(alpha = 0.7, aes(col = num.cont)) +
  geom_smooth(aes(color = num.cont), method = "lm") +
  scale_color_manual(values = col) +
  labs(x = expression(log[10]~Body~Mass), y = expression(log[10]~Home~Range/Geographic~Range), color = "Number of Continents") +
  plot_theme + 
  theme(legend.position = "top")
## TEST: Place of origin dictates how far you can travel----
#depends on how far can go
#depends on how connected your country of origin is (that is, SA and AUS won't be able to disperse; EURA and NA should have the most)


## TEST: older clades have dispersed farther----

data$n.cont[data$n.cont == 4] <- "3+"
data$n.cont[data$n.cont == 3] <- "3+"
data$n.cont <- as.factor(data$n.cont)

#how big will the datasets be?
length(unique(data$binomial[data$foss.age > 0]))
length(unique(data$binomial[data$age.median > 0]))

# range of dates
max(data$foss.age, na.rm = TRUE) #22.185
min(data$foss.age, na.rm = TRUE) #0.00585

#H1 spp that are on mult cont are older

#foss.age
data.foss.age <- data %>%
  dplyr::select(binomial, foss.age, n.cont, continent)

data.foss.age <- data.foss.age %>%
  na.omit()

length(unique(data.foss.age$binomial)) #669
length(unique(data.foss.age$binomial[data.foss.age$n.cont == 1])) #578
length(unique(data.foss.age$binomial[data.foss.age$n.cont == 2])) #86
length(unique(data.foss.age$binomial[data.foss.age$n.cont == "3+"])) #5

#1 v 3
ks.test(data.foss.age$foss.age[data.foss.age$n.cont == 1], 
        data.foss.age$foss.age[data.foss.age$n.cont == 2 | data.foss.age$n.cont == "3+"]) 

#1+2 v 3+
ks.test(data.foss.age$foss.age[data.foss.age$n.cont == 2 | data.foss.age$n.cont == 1], 
        data.foss.age$foss.age[data.foss.age$n.cont == "3+"]) 

## continents by age  of family
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
data.faurby <- data %>%
  dplyr::select(binomial, age.median, n.cont, continent)

data.faurby <- data.faurby %>%
  na.omit()

length(unique(data.faurby$binomial)) #4005
length(unique(data.faurby$binomial[data.faurby$n.cont == 1])) #3736
length(unique(data.faurby$binomial[data.faurby$n.cont == 2])) #263
length(unique(data.faurby$binomial[data.faurby$n.cont == "3+"])) #6

#1 v 2+
ks.test(data.faurby$age.median[data.faurby$n.cont == 1], 
        data.faurby$age.median[data.faurby$n.cont == 2 | data.faurby$n.cont == "3+"]) 

#1+2 v 3+
ks.test(data.faurby$age.median[data.faurby$n.cont == 1 | data.faurby$n.cont == 2], 
        data.faurby$age.median[data.faurby$n.cont == "3+"]) 

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

## what about continent connectivity and aage of family to disperse?
#for continent pairs
#NA and SA
N_S_America <- subset(data.foss.age, data.foss.age$continent == "North.America" | data.foss.age$continent == "South.America")
N_S_America1 <- subset(N_S_America, N_S_America$n.cont == 1)

two.cont_NS <- subset(N_S_America, N_S_America$n.cont == 2)

length(two.cont_NS$binomial) #104
length(N_S_America1$binomial) #317

ks.test(N_S_America1$foss.age, two.cont_NS$foss.age, alternative = "two.sided") #p = 0.0008218
ks.test(N_S_America1$foss.age, two.cont_NS$foss.age, alternative = "less") #p = 0.0004109 x is statically younger than y
ks.test(N_S_America1$foss.age, two.cont_NS$foss.age, alternative = "greater") #p = 0.4979

#NA and EA
N_E <- subset(data.foss.age, data.foss.age$continent == "North.America" | data.foss.age$continent == "Eurasia")
N_E1 <- subset(N_E, N_E$n.cont == 1)

two.cont_NE <- subset(N_E, N_E$n.cont == 2)

length(two.cont_NE$binomial) #100
length(N_E1$binomial) #324

ks.test(N_E1$foss.age, two.cont_NE$foss.age, alternative = "two.sided") #p = 0.03584
ks.test(N_E1$foss.age, two.cont_NE$foss.age, alternative = "less") #p = 0.01792 x is statically younger than y
ks.test(N_E1$foss.age, two.cont_NE$foss.age, alternative = "greater") #p = 0.04871


#EA and AF
E_A <- subset(data.foss.age, data.foss.age$continent == "Eurasia" | data.foss.age$continent == "Africa")
E_A1 <- subset(E_A, E_A$n.cont == 1)

two.cont_EA <- subset(E_A, E_A$n.cont == 2)

length(two.cont_EA$binomial) #68
length(E_A$binomial) #265

ks.test(E_A1$foss.age, two.cont_EA$foss.age, alternative = "two.sided") #p = 0.0005699
ks.test(E_A1$foss.age, two.cont_EA$foss.age, alternative = "less") #p = 0.9747 
ks.test(E_A1$foss.age, two.cont_EA$foss.age, alternative = "greater") #p = 0.0002849 x is statically older than y



