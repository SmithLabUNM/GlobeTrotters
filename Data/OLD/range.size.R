library(raster)
library(tidyverse)

mam <- read.csv("Trait_data.csv", header = TRUE)
mam <- mam %>% filter(Terrestrial == 1)
mam <- mam %>% select(Binomial.1.2, Order.1.2, Family.1.2)

load("../PHYLACINE_1.1/Temporary_builds/current.maps.RData")
load("../PHYLACINE_1.1/Temporary_builds/present.natural.maps.RData")

r <- raster("../PHYLACINE_1.1/Data/Ranges/Current/Abditomys_latidens.tif")
r[] <- NA

current.range <- rowSums(current.maps)
present.natural.range <- rowSums(present.natural.maps)

all.equal(row.names(current.maps), row.names(present.natural.maps))
s <- which(row.names(current.maps) %in% mam$Binomial.1.2)
sum(colSums(current.maps[s, ]) > 0)
sum(colSums(present.natural.maps[s, ]) > 0)

current.range <- bind_cols(Binomial.1.2 = names(current.range), current.range = current.range)
present.natural.range <- bind_cols(Binomial.1.2 = names(present.natural.range), present.natural.range = present.natural.range)

mam <- mam %>%
  left_join(current.range) %>% 
  left_join(present.natural.range)

cell.size <- prod(res(r))/1000/1000

mam <- mam %>% mutate(current.range.km2 = current.range * cell.size, present.natural.range.km2 = present.natural.range * cell.size)

write_csv(mam, "ranges.csv")
