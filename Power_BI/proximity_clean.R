


housing = read.csv('housing.csv')
head(housing)

#$ocean_proximity = replace(as.character(housing$ocean_proximity), housing$ocean_proximity=="<1H OCEAN", "ONEHOUR OCEAN")
unique(housing$ocean_proximity)

prox = c(unique(housing$ocean_proximity))


near_bay = subset(housing, subset =ocean_proximity== prox[1])
onehour_ocean = subset(housing, subset =ocean_proximity== prox[2])
inland = subset(housing, subset =ocean_proximity== prox[3])
near_ocean = subset(housing, subset =ocean_proximity== prox[4])
island = subset(housing, subset =ocean_proximity== prox[5])

head(near_bay)
head(onehour_ocean)
head(inland)
head(near_ocean)
head(island)

near_bay_sum = sum(near_bay$households)
onehour_ocean_sum = sum(onehour_ocean$households)
inland_sum = sum(inland$households)
near_ocean_sum = sum(near_ocean$households)
island_sum = sum(island$households)

prox

df_sum_proximity = data.frame(
  proximity = prox,
  sumOfHouseholds = c(near_bay_sum, onehour_ocean_sum, inland_sum, near_ocean_sum, island_sum))

df_sum_proximity

write.csv(df_sum_proximity, file="proximity_sum.csv")
