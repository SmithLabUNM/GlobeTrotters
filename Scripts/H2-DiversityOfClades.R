#load libraries
require(dplyr)
require(purrrlyr)
require(tidyverse)
require(tidyr)
require(reshape2)
require(ggplot2)

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

#load data
options(stringsAsFactors = FALSE)

## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data <- read.table("MOM.global.mammals.csv", header = TRUE, sep = ",")

data$n.cont[data$n.cont == 4] <- "3+"
data$n.cont[data$n.cont == 3] <- "3+"
data$n.cont <- as.factor(data$n.cont)

## calculate sørensen index
sorensen <- function(x,y) {
  index = (2*(length(intersect(x, y))))/(length(x) + length(y))
  return(index)
}

continents <- c("North.America", "South.America", "Eurasia", "Africa", "Australia")
indeces <- matrix(nrow = 5, ncol = 5, dimnames = list(continents, continents))

indeces[1,2] <- sorensen(x = data$binomial[data$continent == "North.America"], 
         y = data$binomial[data$continent == "South.America"])
indeces[1,3] <- sorensen(x = data$binomial[data$continent == "North.America"], 
         y = data$binomial[data$continent == "Eurasia"])
indeces[1,4] <- sorensen(x = data$binomial[data$continent == "North.America"], 
         y = data$binomial[data$continent == "Africa"])
indeces[1,5] <- sorensen(x = data$binomial[data$continent == "North.America"], 
         y = data$binomial[data$continent == "Australia"])
indeces[2, 3] <- sorensen(x = data$binomial[data$continent == "South.America"], 
         y = data$binomial[data$continent == "Eurasia"])
indeces[2,4] <- sorensen(x = data$binomial[data$continent == "South.America"], 
         y = data$binomial[data$continent == "Africa"])
indeces[2,5] <- sorensen(x = data$binomial[data$continent == "South.America"], 
         y = data$binomial[data$continent == "Australia"])
indeces[3,4] <- sorensen(x = data$binomial[data$continent == "Eurasia"], 
         y = data$binomial[data$continent == "Africa"])
indeces[3,5] <- sorensen(x = data$binomial[data$continent == "Eurasia"], 
         y = data$binomial[data$continent == "Australia"])
indeces[4,5] <- sorensen(x = data$binomial[data$continent == "Africa"], 
         y = data$binomial[data$continent == "Australia"])
#write.csv(indeces, "sorensen.index.csv")

#H2 spp that come from diverse grps are on more continents
df <- data
df <- filter(df, !duplicated(binomial))

####1 v 2+ cont####
df$global <- df$n.cont != 1

# Create a data frame with all species
res <- data.frame(family = unique(df$family))

# Create a dataframe with famlies and count how many species are in each family
res0 <- df %>% 
  group_by(family) %>% 
  dplyr::summarise(count = n())

# Create a dataframe with global famlies and count how many species are global
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

#write.csv(res, "onecontvmult.fams.csv", row.names = FALSE)

####1+2 v 3+####
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

#write.csv(res, "onetwocontvthree.fams.csv", row.names = FALSE)
