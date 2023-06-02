#How many species can traverse entire continents?
#How does place of family origin influence which groups are on 1, 2, or 3+ continents?
#meghan.balk@gmail.com

#### LOAD DATA ----
options(stringsAsFactors = FALSE)

## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data <- read.table("../Data/MOM.global.mammals.csv", 
                   header = TRUE, sep = ",")

#### DATA WRANGLE ----

data$AFR_d <- as.numeric(data$AFR_d)
data$foss.age <- as.numeric(data$foss.age)
data$age.median <- as.numeric(data$age.median)

#### DISPERSAL ----
## TEST: How far can an animal go?----

dist <- data %>%
  drop_na(home.range.km2, mass, 
          dispersal.age.d, GenerationLength_d) #118 records

dist <- dist %>%
  dplyr::group_by(binomial) %>%
  dplyr::summarise(n = n(),
                   order = order[1],
                   family = family[1],
                   foss.avg.age = foss.age[1]*1000000,
                   age = age.median[1]*1000000,
                   avg.mass = mean(mass, na.rm = TRUE),
                   hmrg = home.range.km2[1],
                   disp.age = dispersal.age.d[1],
                   gen.length = GenerationLength_d[1],
                   repro.age = AFR_d[1], 
                   n.cont = n.cont[1],
                   carn = isTRUE(sum(diet.piscivore + diet.invertivore + diet.carnivore) >= 1 & sum(diet.browser + diet.grazer + diet.frugivore) == 0)) %>%
  as.data.frame()
nrow(dist) #88

#model: include lineage age, generation length, dispersal distance, age of reproduction
dist$tot.disp.hmrg.foss = ((dist$foss.avg.age*365)/(dist$gen.length+dist$disp.age))*(dist$hmrg/2) #go radius of homerange
dist$dist.hmrg.per.bs.foss = dist$tot.disp.hmrg.foss/dist$avg.mass
nrow(dist[!is.na(dist$foss.avg.age),]) #71

dist$tot.disp.hmrg.phyl = ((dist$age*365)/(dist$gen.length+dist$disp.age))*(dist$hmrg/2) #go radius of homerange
dist$dist.hmrg.per.bs.phyl = dist$tot.disp.hmrg.foss/dist$avg.mass
nrow(dist[!is.na(dist$age),]) #87

ggplot(data = dist) +
  geom_density(aes(dist.hmrg.per.bs.foss))

ggplot(data = dist) +
  geom_density(aes(dist.hmrg.per.bs.phyl))

#calculate dispersal (From Smith et al. 2016)
#carnivore: Dc = 40.7M^0.81
#herb or omni = Dho = D3.31M^0.65

#model: include lineage age divided by number of generations (generation length + age of first reproduction) multiply by dispersal from above equation

dist$disp.dist.foss[dist$carn == TRUE] = (dist$foss.avg.age[dist$carn == TRUE]*365/(dist$gen.length[dist$carn == TRUE]+dist$disp.age[dist$carn == TRUE]))*(40.7*(dist$avg.mass[dist$carn == TRUE]^0.81))
dist$disp.dist.foss[dist$carn != TRUE] = (dist$foss.avg.age[dist$carn != TRUE]*365/(dist$gen.length[dist$carn != TRUE]+dist$disp.age[dist$carn != TRUE]))*(3.31*(dist$avg.mass[dist$carn != TRUE]^0.65))

dist$disp.dist.phyl[dist$carn == TRUE] = (dist$age[dist$carn == TRUE]*365/(dist$gen.length[dist$carn == TRUE]+dist$disp.age[dist$carn == TRUE]))*(40.7*(dist$avg.mass[dist$carn == TRUE]^0.81))
dist$disp.dist.phyl[dist$carn != TRUE] = (dist$age[dist$carn != TRUE]*365/(dist$gen.length[dist$carn != TRUE]+dist$disp.age[dist$carn != TRUE]))*(3.31*(dist$avg.mass[dist$carn != TRUE]^0.65))


Eurasia = 54750000
Africa = 30380000
North.America = 24700000
South.America = 17830000
Australia = 7690000

nrow(dist) #88
nrow(dist[!is.na(dist$age),]) #87
nrow(dist[!is.na(dist$foss.avg.age),]) #71

dist %>%
  group_by(order) %>%
  summarise(n.fam.foss = length(unique(family[!is.na(foss.avg.age)])),
            n.sp.foss = length(unique(binomial[!is.na(foss.avg.age)])),
            n.fam.phyl = length(unique(family[!is.na(age)])),
            n.sp.phyl = length(unique(binomial[!is.na(age)])))
# order              n.fam.foss n.sp.foss n.fam.phyl n.sp.phyl
# Artiodactyla             2         2          2         3
# Carnivora                8        50          9        60
# Didelphimorphia          1         2          1         2
# Diprotodontia            2         2          3         5
# Perissodactyla           2         3          2         3
# Primates                 0         0          1         1
# Rodentia                 5        12          5        13

## FOSSIL
length(dist$binomial[dist$disp.dist.foss >= Australia &
                     !is.na(dist$foss.avg.age)])
#70/71 (98.59%)
#Potos flavus can't make it

length(dist$binomial[dist$disp.dist.foss >= South.America &
                     !is.na(dist$foss.avg.age)])
#69/71 (97.18%)
#Petaurus breviceps can't make it
#Potos flavus can't make it

length(dist$binomial[dist$disp.dist.foss >= North.America &
                     !is.na(dist$foss.avg.age)])
#67/71 (94.37%)
#Petaurus breviceps can't make it  
#Potos flavus can't make it
#Peromyscus leucopus can't make it     
#Philander opossum can't make it

length(dist$binomial[dist$disp.dist.foss >= Africa &
                     !is.na(dist$foss.avg.age)])
#67/71 (94.37%)
#Petaurus breviceps can't make it  
#Potos flavus can't make it
#Peromyscus leucopus can't make it     
#Philander opossum can't make it

length(dist$binomial[dist$disp.dist.foss >= Eurasia &
                     !is.na(dist$foss.avg.age)])
#66/71 (92.96%)
#Petaurus breviceps can't make it  
#Potos flavus can't make it
#Peromyscus leucopus can't make it     
#Philander opossum can't make it  
#Georychus capensis can't make it

#Potos flavus NEVER makes it   
#Petaurus breviceps can only make Australia
#Peromyscus leucopus only Australia and South America
#Philander opossum only Australia and South America
#Georychus capensis can't make Eurasia only

## PHYLO AGES
length(dist$binomial[dist$disp.dist.phyl >= Australia &
                       !is.na(dist$age)])
#86/87 (98.85%)
#Sciurus carolinensis can't make it

length(dist$binomial[dist$disp.dist.phyl >= South.America &
                       !is.na(dist$age)])
#85/87 (97.7%)
#Sciurus carolinensis can't make it
#Spermophilus richardsonii can't make it

length(dist$binomial[dist$disp.dist.phyl >= North.America &
                       !is.na(dist$age)])
#84/86 (97.67%)
#Petaurus breviceps can't make it  
#Sciurus carolinensis can't make it
#Spermophilus richardsonii can't make it

length(dist$binomial[dist$disp.dist.phyl >= Africa &
                       !is.na(dist$age)])
#84/86 (97.67%)
#Petaurus breviceps can't make it  
#Sciurus carolinensis can't make it
#Spermophilus richardsonii can't make it

length(dist$binomial[dist$disp.dist.phyl >= Eurasia &
                       !is.na(dist$age)])
#83/86 (96.51%)
#Petaurus breviceps can't make it  
#Peromyscus leucopus can't make it     
#Sciurus carolinensis can't make it
#Spermophilus richardsonii can't make it

#Sciurus carolinensis NEVER makes it   
#Spermophilus richardsonii can only make Australia
#Petaurus breviceps only Australia and South America
#Peromyscus leucopus can't make Eurasia only

# what does that look like for those on lots of continents?

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

#### ABOUT DATA ----

## which continents did families originate on?
data %>%
  group_by(continent.family) %>%
  summarise(n = length(unique(family)))
# family origin     n
# Unknown           7
# Africa            24
# Australia         22
# Eurasia           35
# North.America     16
# South.America     31

## missing records
nrow(data[data$continent.family == "",]) #missing information for 862 records

missing.origin <- data %>%
  group_by(order) %>%
  summarise(n.fam = length(unique(family[continent.family == ""])),
            n.sp = length(unique(binomial[continent.family == ""]))) %>%
  as.data.frame()
#most are rodents & chiroptera


#### TEST: how does place of family origin affect which species are on more continents? ----

## do families that originated on more connected continents have more limited dispersers or globe trotters?
data %>%
  group_by(continent.family, n.cont) %>%
  summarise(n = n())

# origin         n.cont    n    per
# Unknown        1        784
# Unknown        2         78
# Africa         1        302     87
# Africa         2         40   11.5
# Africa         3+         5    1.4
# Australia      1        206    100
# Eurasia        1       1691     91
# Eurasia        2        156    8.4
# Eurasia        3+        10     .5
# North.America  1        589   85.4
# North.America  2         92   13.3
# North.America  3+         9    1.3
# South.America  1        575   76.4
# South.America  2        178   23.6

#### TEST: what is the mode that species move? ----
## where are species now?

move <- data %>%
  group_by(binomial) %>%
  summarise(n.cont = n.cont[1],
            stay = any(continent.family == continent),
            jump = any(continent.family != continent),
            spread = any(continent.family[n.cont != 1] == continent[n.cont != 1])) %>%
  as.data.frame()

#jump if stay = false & spread = false; stay if n.cont = 1 & stay = TRUE, spread if spread = true & n.cont > 1
move$move <- ""
for(i in 1:nrow(move)){
  if(isTRUE(move$stay[i]) & isTRUE(move$n.cont[i] == "1")){
    move$move[i] <- "stay"
  }
  else if(isTRUE(move$spread[i])){
    move$move[i] <- "spread"
  }
  else if(isTRUE(move$jump[i]) & !isTRUE(move$stay[i]) & !isTRUE(move$spread[i])){
    move$move[i] <- "jump"
  }
  else{
    next
  }
}


move.trim <- move %>%
  select(binomial, move)

data <- left_join(data, move.trim,
                  "binomial")

##### do highly connected continents tend to have spreaders or jumpers? -----

data %>%
  group_by(continent.family, n.cont) %>%
  summarise(n.spread = length(unique(binomial[move == "spread"])),
            n.jump = length(unique(binomial[move == "jump"])),
            n.stay = length(unique(binomial[move == "stay"]))) %>%
  as.data.frame()

# origin        n.cont      n.spread  n.jump  n.stay
# Africa         1             0       84     218     #27.8% jump; 72.2% stay 
# Africa         2             7       13             #35% spread; 65% jump
# Africa         3+            1       0
# Australia      1             0       0      206     #100% stay
# Eurasia        1             0       1093   598     #64.6% jump; 35.4% stay
# Eurasia        2            49       29             #62.8% spread; 37.2% jump
# Eurasia        3+            3       0
# North.America  1             0       392    197     #66.6% jump; 33.4% stay
# North.America  2            25       21             #54.3% spread; 45.7%jump
# North.America  3+            2       1
# South.America  1             0       36     539     #6.3% jump; 93.7% spread
# South.America  2            89       0              #ALL SPREAD

###### LIMITED DISPERSERS ------

data.2 <- data[data$n.cont == 2,]

## Africa
unique(data.2$continent[data.2$continent.family == "Africa" &
                 data.2$move == "spread"]) #all 7 spread to Eurasia

unique(data.2$continent[data.2$continent.family == "Africa" &
                          data.2$move == "jump"])
length(data.2$binomial[data.2$continent.family == "Africa" &
                         data.2$move == "jump" &
                         data.2$continent == "North.America"]) #12
length(data.2$binomial[data.2$continent.family == "Africa" &
                         data.2$move == "jump" &
                         data.2$continent == "South.America"]) #11
length(data.2$binomial[data.2$continent.family == "Africa" &
                         data.2$move == "jump" &
                         data.2$continent == "Australia"]) #1
length(data.2$binomial[data.2$continent.family == "Africa" &
                         data.2$move == "jump" &
                         data.2$continent == "Eurasia"]) #2

## North America
unique(data.2$continent[data.2$continent.family == "North.America" &
                          data.2$move == "spread"]) 

length(data.2$binomial[data.2$continent.family == "North.America" &
                         data.2$move == "spread" &
                         data.2$continent == "South.America"]) #22/25

length(data.2$binomial[data.2$continent.family == "North.America" &
                         data.2$move == "spread" &
                         data.2$continent == "Eurasia"]) #3/25

unique(data.2$continent[data.2$continent.family == "North.America" &
                          data.2$move == "jump"]) 
length(data.2$binomial[data.2$continent.family == "North.America" &
                         data.2$move == "jump" &
                         data.2$continent == "Australia"]) #1; Myotis adversus
length(data.2$binomial[data.2$continent.family == "North.America" &
                         data.2$move == "jump" &
                         data.2$continent == "Eurasia"])
length(data.2$binomial[data.2$continent.family == "North.America" &
                         data.2$move == "jump" &
                         data.2$continent == "Africa"])

## South America
unique(data.2$continent[data.2$continent.family == "South.America" &
                          data.2$move == "spread"]) #only North America

## Eurasia
unique(data.2$continent[data.2$continent.family == "Eurasia" &
                          data.2$move == "spread"])
data.2$binomial[data.2$continent.family == "Eurasia" &
                          data.2$move == "spread" &
                  data.2$continent == "Australia"]

unique(data.2$continent[data.2$continent.family == "Eurasia" &
                          data.2$move == "jump"])

###### GLOBETROTTERS ------

data.3 <- data[data$n.cont == "3+",]

unique(data.3$continent.family)

unique(data.3$continent[data.3$continent.family == "North.America" &
                          data.3$move == "spread"]) 
unique(data.3$continent[data.3$continent.family == "Eurasia" &
                          data.3$move == "spread"]) 

unique(data.3$continent[data.3$continent.family == "North.America" &
                          data.3$move == "jump"]) 
data.3$binomial[data.3$continent.family == "North.America" &
                   data.3$move == "jump" &
                  data.3$continent == "Australia"]
unique(data.3$continent[data.3$continent.family == "Eurasia" &
                          data.3$move == "jump"]) #none

## ORIGIN OF FAMILY ----
#jumpers (no longer living where family originated) and spreaders (living where family originated and other continents too)

#gather data

## CREATE LONG CONTINENT VERSION

df <- data %>% 
  mutate(Africa = continent == "Africa",
         North.America = continent == "North.America",
         South.America = continent == "South.America",
         Eurasia = continent == "Eurasia",
         Australia = continent == "Australia")

homies <- df[df$n.cont == 1,]
limited <- df[df$n.cont == 2,]
trotter <- df[df$n.cont == "3+",]


df.sums <- df %>%
  group_by(binomial) %>%
  dplyr::summarise(diet.invertivore.tot = isTRUE(sum(diet.invertivore) > 0),
                   diet.carnivore.tot = isTRUE(sum(diet.carnivore) > 0),
                   diet.browser.tot = isTRUE(sum(diet.browser) > 0), 
                   diet.grazer.tot = isTRUE(sum(diet.grazer) > 0),
                   diet.piscivore.tot = isTRUE(sum(diet.piscivore) > 0),
                   diet.frugivore.tot = isTRUE(sum(diet.frugivore) > 0),
                   avg.mass = mean(mass),
                   n.cont = length(unique(continent)))

df.continent <- df %>%
  group_by(binomial) %>%
  dplyr::summarise(continent.Africa = as.logical(sum(Africa)),
                   continent.North.America = as.logical(sum(North.America)),
                   continent.South.America = as.logical(sum(South.America)),
                   continent.Eurasia = as.logical(sum(Eurasia)),
                   continent.Australia = as.logical(sum(Australia)))

df.taxa <- df[!duplicated(df$binomial),] %>%
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

length(unique(df$binomial))
nrow(df.contTaxaSums)

df <- df.contTaxaSums

#homies
homies.origin <- homies %>%
  group_by(continent.family) %>%
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
