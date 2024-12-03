library(raster)
library(data.table)
library(tidyverse)
library(tiff)

#downloaded from https://datadryad.org/stash/dataset/doi:10.5061/dryad.bp26v20
#Trait_data.csv, read in as mam
#Current.zip, unzip file on computer and read in list from folder called "Current"
#Present_natural.zip, unzip file on computer and read in list from folder called "Present_natural"

mam <- read.csv("Trait_data.csv", header = TRUE)
mam <- mam %>% filter(Terrestrial == 1)
mam <- mam %>% 
    dplyr::select(Binomial.1.2, Order.1.2, Family.1.2)

#read in all tif data for current and present natural maps
list.current = list.files(path = "Current",
                          pattern = "*.tif",
                          full.names = TRUE,
                          recursive = TRUE)
current.names <- gsub("Current/",
                      "",
                      list.current)
current.names <- gsub(".tif$",
                      "",
                      current.names)

current.maps <- do.call(list, lapply(list.current, function(x) raster(x)))
current.cells <- do.call(rbind, lapply(current.maps, function(x) rowSums(x)))
row.names(current.cells) <- current.names

list.present.nat = list.files(path = "Present_natural",
                              pattern = "*.tif",
                              full.names = TRUE,
                              recursive = TRUE)

present.nat.names <- gsub("Present_natural/",
                      "",
                      list.present.nat)
present.nat.names <- gsub(".tif$",
                      "",
                      present.nat.names)

present.natural.maps <- do.call(list, lapply(list.present.nat, function(x) raster(x)))
present.natural.cells <- do.call(rbind, lapply(present.natural.maps, function(x) rowSums(x)))
row.names(present.natural.cells) <- present.nat.names

#get resolution of maps, using the first tif file
r <- raster("Current/Abditomys_latidens.tif")
r[] <- NA
cell.size <- prod(res(r))/1000/1000

#sums = number of cells
current.range <- rowSums(current.cells)
present.natural.range <- rowSums(present.natural.cells)

all.equal(row.names(current.cells), row.names(present.natural.cells))
s <- which(row.names(current.cells) %in% mam$Binomial.1.2)
sum(colSums(current.cells[s, ]) > 0)
sum(colSums(present.natural.cells[s, ]) > 0)

current.range <- bind_cols(Binomial.1.2 = names(current.range), current.range = current.range)
present.natural.range <- bind_cols(Binomial.1.2 = names(present.natural.range), present.natural.range = present.natural.range)

mam <- mam %>%
  left_join(current.range) %>% 
  left_join(present.natural.range)

mam <- mam %>% mutate(current.range.km2 = current.range * cell.size, present.natural.range.km2 = present.natural.range * cell.size)

write_csv(mam, "mam.csv")

#check it's the same as the old one:
ranges <- read.csv("../Documents/GitHub/SmithLabUNM/GlobeTrotters/Data/ranges.csv", 
                   header = TRUE, stringsAsFactors = FALSE)

colnames(mam)
colnames(ranges)

match

setdiff(mam$current.range.km2, ranges$current.range.km2) #one that is 161783778
setdiff(ranges$current.range.km2, mam$current.range.km2) #one that is 158664134

setdiff(mam$present.natural.range.km2, ranges$present.natural.range.km2) #none
setdiff(ranges$present.natural.range.km2, mam$present.natural.range.km2) #none
