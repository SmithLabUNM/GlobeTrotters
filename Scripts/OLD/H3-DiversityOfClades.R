#Do certain clades have more species that are globe trotters than others?
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

#### TEST: species that come from diverse clades are on more continents ----

df <- data
df <- filter(df, !duplicated(binomial))

length(unique(df$family)) #135
length(unique(df$family[df$n.cont == 1])) #135
length(unique(df$family[df$n.cont == 2])) #54
length(unique(df$family[df$n.cont == "3+"])) #6

##### 1 v 2+ cont -----
df$global <- df$n.cont != 1

# Create a data frame with all species
res <- data.frame(family = unique(df$family))

# Create a dataframe with famalies and count how many species are in each family
res0 <- df %>% 
  group_by(family) %>% 
  dplyr::summarise(count = n())

# Create a dataframe with global famalies and count how many species are global
res1 <- filter(df, global) %>% 
  group_by(family) %>% 
  dplyr::summarise(onMult = n())

# Merge the data
res <- left_join(res, res0, by = "family") %>% 
  left_join(res1, by = "family")

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

write.csv(res, "../Results/onecontvmult.fams.csv", row.names = FALSE)

##### 1+2 v 3+ -----
df$global <- df$n.cont == "3+"

# Create a data frame with all species
res <- data.frame(family = unique(df$family))

# Create a dataframe with famlies and count how many species are in each family
res0 <- df %>% 
  group_by(family) %>% 
  dplyr::summarise(count = n())

# Create a dataframe with global famlies and count how many species are global
res1 <- filter(df, global) %>% 
  group_by(family) %>% 
  dplyr::summarise(on3 = n())

# Merge the data
res <- left_join(res, res0, by = "family") %>% 
  left_join(res1, by = "family")

# Change NA's to zero
res[is.na(res)] <- 0

# Count how large a proportion we expect of a given family
res <- mutate(res, glob.proportion = count/sum(count))

# Find the total species number on 3+ continents
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

write.csv(res, "../Results/onetwocontvthree.fams.csv", row.names = FALSE)

## how many bats are there?
length(unique(data$binomial[data$order == "Chiroptera"]))

#### WHICH GROUPS ARE UNIQUE ----

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

