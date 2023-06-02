#How many species are shared between continents
#meghan.balk@gmail.com

#### LOAD DATA ----
options(stringsAsFactors = FALSE)

## Data does not include oceanic (marine) species; does include aquatic spp.
## Data does not include introduced species (only native ranges)
data <- read.table("../Data/MOM.global.mammals.csv", 
                   header = TRUE, sep = ",")

#### TEST: Connectivity and shared species ----
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
write.csv(indeces, 
          "../Results/sorensen.index.csv",
          row.names = FALSE)