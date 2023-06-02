### Phylogeny work >>>
require(ape)
require(doParallel)

setwd("C:\\Files\\Projects\\PhD\\project Felisas group\\analysis\\age")

phyl1 <- read.nexus("Fully_resolved_phylogeny_1.nex")
phyl2 <- read.nexus("Fully_resolved_phylogeny_2.nex")
phyl3 <- read.nexus("Fully_resolved_phylogeny_3.nex")
phyls <- c(phyl1, phyl2, phyl3)

cl <- makeCluster(8)
registerDoParallel(cl)
age <- foreach(i = 1:1000, .packages = c("ape"), .combine = cbind) %dopar% {
  tree <- phyls[[i]]

  spec.age <- function(x) {
    return(tree$edge.length[which.edge(tree, x)])
  }
  ages <- sapply(tree$tip.label, spec.age)
  ages <- ages[order(names(ages))]
}
stopCluster(cl)
gc()

write.csv(age, "species.age_Faurby.csv")


age <- read.csv("species.age_Faurby.csv", header = TRUE, row.names = 1)
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
