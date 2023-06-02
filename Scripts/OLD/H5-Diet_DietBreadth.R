#Are animals with certain diet types or dietary breadths more likely to be on more than one continent?
#meghan.balk@gmail.com
#Rasmus Ø. Pedersen

#### LOAD DATA ----

options(stringsAsFactors = FALSE)

## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data <- read.table("../Data/MOM.global.mammals.csv", 
                   header = TRUE, sep = ",")

#### LOAD LIBRARIES ----
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)

#### SET GRAPHING THEME ----

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

#### ABOUT DATA ----

data = data[!duplicated(data$binomial),]
length(data$binomial) #4426

#understand data
length(data$binomial[data$diet.invertivore == TRUE]) #1998
length(data$binomial[data$diet.carnivore == TRUE]) #333
length(data$binomial[data$diet.browser == TRUE]) #1631
length(data$binomial[data$diet.grazer == TRUE]) #618
length(data$binomial[data$diet.frugivore == TRUE]) #1941
length(data$binomial[data$diet.piscivore == TRUE]) #30

length(data$binomial[data$diet.breadth == 1]) #2490
length(data$binomial[data$diet.breadth == 2]) #1747
length(data$binomial[data$diet.breadth == 3]) #189
length(data$binomial[data$diet.breadth == 4]) #0
length(data$binomial[data$diet.breadth == 5]) #0
length(data$binomial[data$diet.breadth == 6]) #0

df.clean <- subset(data, data$diet.breadth > 0 & !is.na(data$diet.breadth))
length(unique(df.clean$binomial)) #4426
length(unique(df.clean$binomial[df.clean$n.cont == 1])) #4147
length(unique(df.clean$binomial[df.clean$n.cont == 2])) #272
length(unique(df.clean$binomial[df.clean$n.cont == "3+"])) #6

plot(as.factor(df.clean$n.cont), df.clean$diet.breadth)

#### TEST: spp on mult cont are have specific diets ----

##### 1 v 2+ cont -----
data <- filter(df.clean, !duplicated(binomial))

# Assign if species is global or not by if it occurs on more than 1 continent
data$global <- data$n.cont != 1

# Create a data frame with all species
diets <- names(dplyr::select(data, starts_with("diet"), -diet.src, -diet.breadth))
res <- data.frame(diet = diets)

res["count"] <- colSums(data[diets])
res["onMult"] <- colSums(data[data$global, diets])

# Count how large a proportion we expect of a given diet
res <- mutate(res, glob.proportion = count/nrow(data))

# Find the total species number on 2+ continents
onMult.tot <- sum(data$global)
res <- mutate(res, onMult.proportion = onMult/onMult.tot)
# Run binomial test and add that to the result
# The test compares each diet presence globally to their overall precense in mammalia
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
res
write.csv(res, "../Results/onecontvmult.diets.csv", row.names = FALSE)

##### 1+2 v 3+ -----
data <- filter(data.clean, !duplicated(binomial))

# Assign if species is global or not by if it occurs on more than 1 continent
data$global <- data$n.cont == "3+"

# Create a data frame with all species
diets <- names(select(data, starts_with("diet"), -diet.src, -diet.breadth))
res <- data.frame(diet = diets)

res["count"] <- colSums(data[diets])
res["on3"] <- colSums(data[data$global, diets])

# Count how large a proportion we expect of a given diet
res <- mutate(res, glob.proportion = count/nrow(data))

# Find the total species number on 3+ continents
on3.tot <- sum(data$global)
res <- mutate(res, on3.proportion = on3/on3.tot)
# Run binomial test and add that to the result
# The test compares each diet presence globally to their overall precense in mammalia
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
res
write.csv(res, "../Results/onetwocontvthree.diets.csv", row.names = FALSE)

##### PLOT -----
data.melt <- melt(df.clean, id.vars = c("binomial", "n.cont"), 
                  measure.vars = c("diet.carnivore", "diet.browser", "diet.grazer", "diet.invertivore", "diet.piscivore", "diet.frugivore"),
                  variable.name = "diet.type")

new.data <- subset(data.melt, data.melt$value == TRUE)

#group by binomial so don't recount
unique.new.data <- new.data %>%
  group_by(binomial) %>%
  dplyr:: summarise(diettype = diet.type[1], numconts = n.cont[1])
unique.data$diettype <- as.factor(unique.new.data$diettype)
unique.data <- as.data.frame(unique.data)

diettype_bargraph <- plyr::ddply(unique.new.data, c("numconts", "diettype"), function(x){
  nrow(x)
})
diettype_bargraph_full <- complete(diettype_bargraph, numconts, diettype)
diettype_bargraph_full$diet.type <- factor(diettype_bargraph_full$diettype, levels = c("diet.carnivore", "diet.piscivore", "diet.invertivore", "diet.frugivore", "diet.browser", "diet.grazer"))

diettype_bargraph_full$tots <- NA
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "1"] <- sum(diettype_bargraph_full$V1[1:6]) #4147
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "2"] <- sum(diettype_bargraph_full$V1[7:12]) #272
diettype_bargraph_full$tots[diettype_bargraph_full$numconts == "3+"] <- sum(diettype_bargraph_full$V1[13:18], na.rm = TRUE) #6

diettype_bargraph_full$prop <- diettype_bargraph_full$V1 / diettype_bargraph_full$tots

## diet type
#show as proportions
ggplot(diettype_bargraph_full[diettype_bargraph_full$numconts != 5,], aes(x = diettype, y = prop, fill = as.factor(numconts))) + 
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_fill_manual("Continents", values = col) +
  xlab("Diet Type") + ylab("Proportion") + 
  scale_x_discrete(labels=c("diet.carnivore" = "Carnivore", "diet.piscivore" = "Piscivore", 
                            "diet.invertivore" = "Invertivore", "diet.browser" = "Browser", 
                            "diet.grazer" = "Grazer", "diet.frugivore" = "Frugivore")) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=14)) + 
  plot_theme +theme(panel.border = element_rect(fill=NA),
                    strip.background = element_rect(fill=NA),
                    legend.position = c(1.15, 0.5)) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))

#### TEST: spp on mult cont are have wider/narrower dietary breadth ----

##### 1 v 2+ diet breadth -----
not.glob <- filter(df.clean, n.cont == 1) %>% 
  dplyr::select(diet.breadth) %>% .[[1]]
glob <- filter(df.clean, n.cont != 1) %>% 
  dplyr::select(diet.breadth) %>% .[[1]]

# Is variance equal? No
var.test(not.glob, glob) #1.165507, p = 0.09224
#variance is not equal, meaning that we cannot run a t-test

# T-test is significant but we violate assumption of normality of data
t.test(not.glob, glob) #p-value = 0.0001945

# Wilcoxon rank-sum test (equivalent to the Mann-Whitney U test)
# Does not assume normality
wilcox.test(not.glob, glob, paired = FALSE) #p-value = 0.0002889
median(not.glob) - median(glob) #medians are so close it doesn't make sense to report the difference (0)
mean(not.glob) - mean(glob) #0.13 difference in means

#percentage difference
(mean(not.glob) - mean(glob))/mean(not.glob)*100 #8.471176 

##### 1+2 v 3+ diet breadth -----
not.glob <- filter(df.clean, n.cont == 1 | n.cont == 2) %>% 
  select(diet.breadth) %>% .[[1]]
glob <- filter(df.clean, n.cont == "3+") %>% 
  select(diet.breadth) %>% .[[1]]

# Is variance equal? No
var.test(not.glob, glob) #1.117382, p = 0.9667
#variance is equal, meaning that we cannot run a t-test
#but data is not normal, so can't run a t-test

# T-test is significant but we violate assumption of normality of data
t.test(not.glob, glob) #p-value = 0.9322

# Wilcoxon rank-sum test (equivalent to the Mann-Whitney U test)
# Does not assume normality
wilcox.test(not.glob, glob, paired = FALSE) #p-value = .8406
median(not.glob) - median(glob) #medians are so close; -0.5
mean(not.glob) - mean(glob) # basically 0 difference in means (-0.02002716)

#percentage difference
(mean(not.glob) - mean(glob))/mean(not.glob)*100 #-1.353211

#### WHICH DIET BREADTHS ARE UNIQUE? ----

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


##### PLOT ----

#need to create values for NAs
dietbreadth_bargraph <- plyr::ddply(df.clean, c("n.cont", "diet.breadth"), function(x){
  nrow(x)
})

dietbreadth_bargraph_full <- tidyr::complete(dietbreadth_bargraph, n.cont, diet.breadth)

dietbreadth_bargraph_full$tots <- NA
dietbreadth_bargraph_full$tots[dietbreadth_bargraph_full$n.cont == "1"] <- sum(dietbreadth_bargraph_full$V1[dietbreadth_bargraph_full$n.cont == "1"], na.rm = TRUE) #4147
dietbreadth_bargraph_full$tots[dietbreadth_bargraph_full$n.cont == "2"] <- sum(dietbreadth_bargraph_full$V1[dietbreadth_bargraph_full$n.cont == "2"], na.rm = TRUE) #272
dietbreadth_bargraph_full$tots[dietbreadth_bargraph_full$n.cont == "3+"] <-sum(dietbreadth_bargraph_full$V1[dietbreadth_bargraph_full$n.cont == "3+"], na.rm = TRUE) #6 #phew, they match!

dietbreadth_bargraph_full$prop <- dietbreadth_bargraph_full$V1 / dietbreadth_bargraph_full$tots

## diet breadth
#show as proportions
ggplot(dietbreadth_bargraph_full[dietbreadth_bargraph_full$n.cont != "5",], aes(x = diet.breadth, y = prop, fill = as.factor(n.cont))) + 
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_fill_manual("Continents", values = col) +
  xlab("\n\nDietary Breadth") + ylab("Proportion") + 
  theme(legend.position = "none") + plot_theme

#### WHICH DIET TYPES ARE UNIQUE? ----

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
