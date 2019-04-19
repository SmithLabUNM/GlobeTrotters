#load libraries
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)

#load data
options(stringsAsFactors = FALSE)

data <- read.table("MOM.global.mammals.csv", header = TRUE, sep = ",")
## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data$num.conts <- data$n.cont
data$num.conts[data$num.conts == 4] <- "3+"
data$num.conts[data$num.conts == 3] <- "3+"
data$num.conts <- as.factor(data$num.conts)

pan <- read.table("pantheria.csv", header = TRUE, sep = ",")
pan1 <- subset(pan, select = c("MSW05_Binomial", "X26.1_GR_Area_km2", "X22.1_HomeRange_km2"))
colnames(pan1)[1] <- "binomial"
colnames(pan1)[2] <- "GR_Area_km2"
colnames(pan1)[3] <- "HomeRange_km2"

ranges <- read.table("ranges.csv", header = TRUE, sep = ",")
colnames(ranges)[1] <- "binomial"

#H5 spp on mult cont are omnivores
spp = data[!duplicated(data$binomial),]

bats <- subset(spp, spp$order == "Chiroptera")
mean(bats$mass, na.rm = TRUE)
length(bats$binomial[bats$diet.invertivore == 1])
length(bats$binomial[bats$n.cont == 3])

invert.mass <- mean(spp$mass[spp$diet.invertivore == 1], na.rm = TRUE)
carn.mass <- mean(spp$mass[spp$diet.carnivore == 1], na.rm = TRUE)
browse.mass <- mean(spp$mass[spp$diet.browser == 1], na.rm = TRUE)
graze.mass <- mean(spp$mass[spp$diet.grazer == 1], na.rm = TRUE)
frug.mass <- mean(spp$mass[spp$diet.frugivore == 1], na.rm = TRUE)
pisc.mass <- mean(spp$mass[spp$diet.piscivore == 1], na.rm = TRUE)

invert.massD <- spp$mass[spp$diet.invertivore == 1]
carn.massD <- spp$mass[spp$diet.carnivore == 1]
browse.massD <- spp$mass[spp$diet.browser == 1]
graze.massD <- spp$mass[spp$diet.grazer == 1]
frug.massD <- spp$mass[spp$diet.frugivore == 1]
pisc.massD <- spp$mass[spp$diet.piscivore == 1]

ks.test(invert.massD, frug.massD)

#understand data
length(unique(data$binomial[data$diet.invertivore == TRUE])) #1986
length(unique(data$binomial[data$diet.carnivore == TRUE])) #333
length(unique(data$binomial[data$diet.browser == TRUE])) #1619
length(unique(data$binomial[data$diet.grazer == TRUE])) #610
length(unique(data$binomial[data$diet.frugivore == TRUE])) #1935
length(unique(data$binomial[data$diet.piscivore == TRUE])) #31

length(unique(data$binomial[data$diet.breadth == 1])) #2481
length(unique(data$binomial[data$diet.breadth == 2])) #1734
length(unique(data$binomial[data$diet.breadth == 3])) #187
length(unique(data$binomial[data$diet.breadth == 4])) #1
length(unique(data$binomial[data$diet.breadth == 5])) #0
length(unique(data$binomial[data$diet.breadth == 6])) #0

data$binomial[data$diet.breadth == 4] #Sigmodon hispidus

unique.data <- data %>%
  group_by(binomial) %>%
  dplyr:: summarise(dietbreadth = diet.breadth[1], n.cont = n.cont[1])
unique.data$dietbreadth <- as.factor(unique.data$dietbreadth)
anova(lm(unique.data$n.cont ~ unique.data$dietbreadth)) #<0.001

global.definition <- 1
data$global <- data$n.cont > global.definition

not.glob <- filter(data, !global) %>% select(diet.breadth) %>% .[[1]]
glob <- filter(data, global) %>% select(diet.breadth) %>% .[[1]]

# Is variance equal? Yes
var.test(not.glob, glob) #0.2522
#variance is equal, meaning that we can run a t-test
#but data is not normal, so can't run a t-test

# T-test is significant but we violate assumption of normality of data
t.test(not.glob, glob) #p-value = 1.919e-06

# Wilcoxon rank-sum test (equivalent to the Mann-Whitney U test)
# Does not assume normality
wilcox.test(not.glob, glob, paired = FALSE) #p-value = 4.694e-07
median(not.glob) - median(glob) #medians are so close it doesn't make sense to report the difference (0)
mean(not.glob) - mean(glob) #0.12 difference in means

#percentage difference
(mean(not.glob) - mean(glob))/mean(not.glob)*100 #7.908595 

df <- data

#1 v 2 cont
df <- filter(df, !duplicated(binomial))

# Assign if species is global or not by if it occurs on more than 1 continent
df$global <- df$n.cont == 2

# Create a data frame with all species
diets <- names(dplyr::select(df, starts_with("diet"), -diet.src, -diet.breadth))
res <- data.frame(diet = diets)

res["count"] <- colSums(df[diets])
res["on2"] <- colSums(df[df$global, diets])

# Count how large a proportion we expect of a given diet
res <- mutate(res, glob.proportion = count/nrow(df))

# Find the total species number on 2+ continents
on2.tot <- sum(df$global)
res <- mutate(res, on2.proportion = on2/on2.tot)
# Run binomial test and add that to the result
# The test compares each diet presence globally to their overall precense in mammalia
res <- purrrlyr::invoke_rows(.d = res,
                   .f = function(on2, glob.proportion, ...) {
                     test <- binom.test(on2, on2.tot, p = glob.proportion, alternative="greater")
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
res
#write.csv(res, "onecontvtwo.diets.csv", row.names = FALSE)

#1 v all
df <- filter(df, !duplicated(binomial))

# Assign if species is global or not by if it occurs on more than 1 continent
df$global <- df$n.cont > 1

# Create a data frame with all species
diets <- names(select(df, starts_with("diet"), -diet.src, -diet.breadth))
res <- data.frame(diet = diets)

res["count"] <- colSums(df[diets])
res["on2"] <- colSums(df[df$global, diets])

# Count how large a proportion we expect of a given diet
res <- mutate(res, glob.proportion = count/nrow(df))

# Find the total species number on 2+ continents
on2.tot <- sum(df$global)
res <- mutate(res, on2.proportion = on2/on2.tot)
# Run binomial test and add that to the result
# The test compares each diet presence globally to their overall precense in mammalia
res <- purrrlyr::invoke_rows(.d = res,
                   .f = function(on2, glob.proportion, ...) {
                     test <- binom.test(on2, on2.tot, p = glob.proportion, alternative="greater")
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
res
#write.csv(res, "onecontvall.diets.csv", row.names = FALSE)

#1+2 v all
df <- filter(df, !duplicated(binomial))

# Assign if species is global or not by if it occurs on more than 1 continent
df$global <- df$n.cont > 2

# Create a data frame with all species
diets <- names(select(df, starts_with("diet"), -diet.src, -diet.breadth))
res <- data.frame(diet = diets)

res["count"] <- colSums(df[diets])
res["on2"] <- colSums(df[df$global, diets])

# Count how large a proportion we expect of a given diet
res <- mutate(res, glob.proportion = count/nrow(df))

# Find the total species number on 2+ continents
on2.tot <- sum(df$global)
res <- mutate(res, on2.proportion = on2/on2.tot)
# Run binomial test and add that to the result
# The test compares each diet presence globally to their overall precense in mammalia
res <- purrrlyr::invoke_rows(.d = res,
                   .f = function(on2, glob.proportion, ...) {
                     test <- binom.test(on2, on2.tot, p = glob.proportion, alternative="greater")
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
res
filter(res, signif)
#write.csv(res, "oneandtwocontvall.diets.csv", row.names = FALSE)

#need to group by binomial so don't double count
unique.data <- as.data.frame(unique.data)
unique.data$num.conts <- unique.data$n.cont
unique.data$num.conts[unique.data$num.conts == 4] <- "3+"
unique.data$num.conts[unique.data$num.conts == 3] <- "3+"
unique.data$num.conts <- as.factor(unique.data$num.conts)

#dietbreadth
#need to create values for NAs
dietbreadth_bargraph <- ddply(unique.data, .(num.conts, dietbreadth), function(x){
  nrow(x)
})
dietbreadth_bargraph_full <- tidyr::complete(dietbreadth_bargraph, num.conts, dietbreadth)

sum(dietbreadth_bargraph_full$V1[dietbreadth_bargraph_full$num.conts == "1"], na.rm = TRUE) #4105
sum(dietbreadth_bargraph_full$V1[dietbreadth_bargraph_full$num.conts == "2"], na.rm = TRUE) #292
sum(dietbreadth_bargraph_full$V1[dietbreadth_bargraph_full$num.conts == "3+"], na.rm = TRUE) #6 #phew, they match!

dietbreadth_bargraph_full$tots[dietbreadth_bargraph_full$num.conts == "1"] <- 4105
dietbreadth_bargraph_full$tots[dietbreadth_bargraph_full$num.conts == "2"] <- 292
dietbreadth_bargraph_full$tots[dietbreadth_bargraph_full$num.conts == "3+"] <- 6

dietbreadth_bargraph_full$prop <- dietbreadth_bargraph_full$V1 / dietbreadth_bargraph_full$tots

#diettype

require(reshape2)
data.melt <- melt(data, id.vars = c("binomial", "num.conts"), 
               measure.vars = c("diet.carnivore", "diet.browser", "diet.grazer", "diet.invertivore", "diet.piscivore", "diet.frugivore"),
               variable.name = "diet.type")

new.data <- subset(data.melt, data.melt$value == TRUE)

#group by binomial so don't recount
unique.new.data <- new.data %>%
  group_by(binomial) %>%
  dplyr:: summarise(diettype = diet.type[1], numconts = num.conts[1])
unique.data$diettype <- as.factor(unique.new.data$diettype)
unique.data <- as.data.frame(unique.data)

diettype_bargraph <- ddply(unique.new.data, .(numconts, diettype), function(x){
  nrow(x)
})
diettype_bargraph_full <- complete(diettype_bargraph, numconts, diettype)
diettype_bargraph_full$diet.type <- factor(diettype_bargraph_full$diettype, levels = c("diet.carnivore", "diet.piscivore", "diet.invertivore", "diet.frugivore", "diet.browser", "diet.grazer"))

sum(diettype_bargraph_full$V1[1:6]) #4105
sum(diettype_bargraph_full$V1[7:12]) #292
sum(diettype_bargraph_full$V1[13:18], na.rm = TRUE) #6

diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "1"] <- 4105
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "2"] <- 292
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "3+"] <- 6

diettype_bargraph_full$prop <- diettype_bargraph_full$V1 / diettype_bargraph_full$tots

## diet type
#show as proportions
ggplot(diettype_bargraph_full, aes(x = diettype, y = prop, fill = as.factor(numconts))) + 
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_fill_manual("Continents", values = col) +
  xlab("Diet Type") + ylab("Proportion") + 
  scale_x_discrete(labels=c("diet.carnivore" = "Carnivore", "diet.piscivore" = "Piscivore", 
                            "diet.invertivore" = "Invertivore", "diet.browser" = "Browser", 
                            "diet.grazer" = "Grazer", "diet.frugivore" = "Frugivore")) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=14)) + 
  theme_jmg+theme(panel.border = element_rect(fill=NA),
                  strip.background = element_rect(fill=NA),
                  legend.position = c(1.15, 0.5))+
  theme(axis.title.y = element_text(margin = margin(r = 5)))

## diet breadth
#show as proportions
ggplot(dietbreadth_bargraph_full, aes(x = dietbreadth, y = prop, fill = as.factor(num.conts))) + 
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_fill_manual("Continents", values = col) +
  xlab("\n\nDietary Breadth") + ylab("Proportion") + 
  theme(legend.position = "none") +theme_jmg
