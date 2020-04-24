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

#H3 spp that come from diverse grps are on more continents
df <- data
df <- filter(df, !duplicated(binomial))

####1 v 2 cont####
df$global <- df$n.cont == 2

# Create a data frame with all species
res <- data.frame(family = unique(df$family))

# Create a dataframe with famlies and count how many species are in each family
res0 <- df %>% 
  group_by(family) %>% 
  dplyr::summarise(count = n())

# Create a dataframe with global famlies and count how many species are global
res1 <- filter(df, global) %>% 
  group_by(family) %>% 
  dplyr::summarise(on2 = n())

# Merge the data
res <- left_join(res, res0, by = "family") %>% 
  left_join(res1, by = "family")

# Change NA's to zero
res[is.na(res)] <- 0

# Count how large a proportion we expect of a given family
res <- mutate(res, glob.proportion = count/sum(count))

# Find the total species number on 2+ continents
on2.tot <- sum(res$on2)
res <- mutate(res, on2.proportion = on2/on2.tot)
# Run binomial test and add that to the result
# The test compares each familys presence globally to their overall precense in mammalia
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
filter(res, signif)

#write.csv(res, "onecontvtwo.fams.csv", row.names = FALSE)

####1 v the rest cont####
df$global <- df$n.cont > 1

# Create a data frame with all species
res <- data.frame(family = unique(df$family))

# Create a dataframe with famlies and count how many species are in each family
res0 <- df %>% 
  group_by(family) %>% 
  dplyr::summarise(count = n())

# Create a dataframe with global famlies and count how many species are global
res1 <- filter(df, global) %>% 
  group_by(family) %>% 
  dplyr::summarise(on2 = n())

# Merge the data
res <- left_join(res, res0, by = "family") %>% 
  left_join(res1, by = "family")

# Change NA's to zero
res[is.na(res)] <- 0

# Count how large a proportion we expect of a given family
res <- mutate(res, glob.proportion = count/sum(count))

# Find the total species number on 2+ continents
on2.tot <- sum(res$on2)
res <- mutate(res, on2.proportion = on2/on2.tot)
# Run binomial test and add that to the result
# The test compares each familys presence globally to their overall precense in mammalia
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
filter(res, signif)

#write.csv(res, "onecontvall.fams.csv", row.names = FALSE)

####1+2 v the rest####
df$global <- df$n.cont > 2

# Create a data frame with all species
res <- data.frame(family = unique(df$family))

# Create a dataframe with famlies and count how many species are in each family
res0 <- df %>% 
  group_by(family) %>% 
  dplyr::summarise(count = n())

# Create a dataframe with global famlies and count how many species are global
res1 <- filter(df, global) %>% 
  group_by(family) %>% 
  dplyr::summarise(on2 = n())

# Merge the data
res <- left_join(res, res0, by = "family") %>% 
  left_join(res1, by = "family")

# Change NA's to zero
res[is.na(res)] <- 0

# Count how large a proportion we expect of a given family
res <- mutate(res, glob.proportion = count/sum(count))

# Find the total species number on 2+ continents
on2.tot <- sum(res$on2)
res <- mutate(res, on2.proportion = on2/on2.tot)
# Run binomial test and add that to the result
# The test compares each familys presence globally to their overall precense in mammalia
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
filter(res, signif)

#write.csv(res, "oneandtwocontvall.fams.csv", row.names = FALSE)
