####H6. Sp. w greater geographic ranges are found on more continents ####
data2 <- merge(data, pan1, by = "binomial", all.x = TRUE, all.y = FALSE)

#Eurasia = 54.752349 km2
#Africa = 30.380561 km2
#North America = 24.70849 km2
#South America = 17.839838 km2
#Australia = 7.692265 km2

#want to add "tot.area" column
for(i in 1:length(data2$binomial)){
  if(data2$continent[i] == "Eurasia"){
    data2$tot.area[i] <- (54.75*10^6)
  }else if(data2$continent[i] == "Africa"){
    data2$tot.area[i] <- (30.38*10^6)
  }else if(data2$continent[i] == "North.America"){
    data2$tot.area[i] <- (24.70*10^6)
  }else if(data2$continent[i] == "South.America"){
    data2$tot.area[i] <- (17.83*10^6)
  }else{
    data2$tot.area[i] <- (7.69*10^6)
  }
}

#want to combine to unique spp
df <- data2 %>%
  dplyr::group_by(binomial) %>%
  dplyr::summarise(cont.tot.area = sum(tot.area), gr.area = GR_Area_km2[1], num.cont = num.conts[1], home.range = HomeRange_km2[1], size = mean(mass)) %>%
  as.data.frame()

#get cleanest dataset
df.clean <- subset(df, !is.na(df$gr.area) & !is.na(df$size))

#get ratio of geog range out of total area
df.clean$ratio <- df.clean$gr.area/df.clean$cont.tot.area

#change to log units
df.clean$logSize <- log10(df.clean$size)
df.clean$logRatio <- log10(df.clean$ratio)

#Do species on multiple continents occupy a larger area than available when taking body size into account? Yes!
summary(glm(lm(log10(df.clean$ratio) ~ log10(df.clean$size) + as.factor(df.clean$num.cont))))
summary(lm(log10(df.clean$ratio[df.clean$num.cont == "1"]) ~ log10(df.clean$size[df.clean$num.cont == "1"])))
summary(lm(log10(df.clean$ratio[df.clean$num.cont == "2"]) ~ log10(df.clean$size[df.clean$num.cont == "2"])))
summary(lm(log10(df.clean$ratio[df.clean$num.cont == "3+"]) ~ log10(df.clean$size[df.clean$num.cont == "3+"])))

#Do species have a lager geo-range for their body size? Yes!
summary(glm(lm(log10(df.clean$gr.area) ~ log10(df.clean$size) + as.factor(df.clean$num.cont))))

# ##using faurby "natural ranges"
# data3 <- data
# data3$binomial <- gsub(" ", "_", data$binomial, fixed = TRUE)
# data3 <- merge(data3, ranges, 
#                by = "binomial", 
#                all.x = FALSE, all.y = FALSE)
# 
# length(unique(data3$binomial)) #3149 spp
# 
# #want to add "tot.area" column
# for(i in 1:length(data3$binomial)){
#   if(data3$continent[i] == "Eurasia"){
#     data3$tot.area[i] <- 54.75
#   }else if(data3$continent[i] == "Africa"){
#     data3$tot.area[i] <- 30.38
#   }else if(data3$continent[i] == "North.America"){
#     data3$tot.area[i] <- 24.70
#   }else if(data3$continent[i] == "South.America"){
#     data3$tot.area[i] <- 17.83
#   }else{
#     data3$tot.area[i] <- 7.69
#   }
# }
# 
# df <- data3 %>%
#   group_by(binomial) %>%
#   summarise(cont.tot.area = sum(tot.area), gr.area = present.natural.range.km2[1], num.cont = n.cont[1], curr.range = current.range.km2[1], size = mean(mass)) %>%
#   as.data.frame()
# 
# #get cleanest dataset
# df.clean <- subset(df, !is.na(df$gr.area) & !is.na(df$size))
# 
# #get ratio of geog range out of total area
# df.clean$ratio <- df.clean$gr.area/df.clean$cont.tot.area
# df.clean$ratio2 <- df.clean$curr.range/df.clean$cont.tot.area
# model <- lm(log10(df.clean$ratio + 1) ~ log10(df.clean$size))
# summary(model)
# 
# model2 <- anova(lm(log10(df.clean$ratio + 1) ~ log10(df.clean$size) + as.factor(df.clean$num.cont)))
# 
# model <- lm(log10(df.clean$ratio2 + 1) ~ log10(df.clean$size))
# summary(model)
# 
# model2 <- anova(lm(log10(df.clean$ratio2 + 1) ~ log10(df.clean$size) + as.factor(df.clean$num.cont)))
