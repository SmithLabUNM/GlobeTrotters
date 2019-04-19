## Q1: How many spp are on ea continent
length(unique(data$binomial[data$n.cont == 1])) #4105
length(unique(data$binomial[data$n.cont == 2])) #292
length(unique(data$binomial[data$n.cont == 3])) #5
length(unique(data$binomial[data$n.cont == 4])) #1

data[which(data$n.cont == 3), "binomial"]
data[which(data$n.cont == 4), "binomial"]

#about the spp on 2 continents
two.cont <- subset(data, data$n.cont == 2)
two.cont %>%
  group_by(order) %>%
  summarise(n = length(unique(binomial))) %>%
  as.data.frame

length(unique(two.cont$order)) #14
length(unique(two.cont$family)) #55
length(unique(two.cont$genus)) #186

#chiroptera
161/292 #(55%)
798/4403 #compared to total dataset (18%)
