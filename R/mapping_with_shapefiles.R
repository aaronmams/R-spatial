
library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(maptools)
library(rgeos)


#install.packages('rgdal', type='source')

london <- readOGR(dsn = "/Users/aaronmamula/Documents/R projects/spatial/London_Sport", layer = "london_sport")

str(london)
london@data

# change coordinate system to WGS 84
london<-spTransform(london, CRS("+proj=longlat +datum=WGS84"))

#find zones where sports participation is more than 25%
london@data[which(london@data$Partic_Per>25),]

#plot the London Map in Sky Blue with a Title
plot(london, col="skyblue")
title("Map of London")

#can I use ggplot instead
l.new <- fortify(london, region="name")

#############################################################
#############################################################
#############################################################
#############################################################
#############################################################

#let's try with the Census Tract stuff
tract <- readOGR(dsn = "/Users/aaronmamula/Documents/R projects/spatial/gz_2010_06_140_00_500k", 
                 layer = "gz_2010_06_140_00_500k")
tract <- fortify(tract, region="GEO_ID")

ins <- read.csv("/Users/aaronmamula/Documents/R projects/spatial/data/CA_insured_tract.csv")
ins$id <- as.character(ins$GEO.id)
ins$percent <- ins$Insured18_64/ins$Pop18_64

plotData <- left_join(tract, ins)

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent), color = "black", size = 0.25) + 
        theme_bw()

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent), color = "black", size = 0.25) +
  coord_map() + theme_bw()

#-------------------------------------------------------------
#county boundaries
county <- readOGR(dsn = "/Users/aaronmamula/Documents/R Projects/spatial/data/gz_2010_06_060_00_500k", 
                  layer = "gz_2010_06_060_00_500k")
county <- fortify(county, region="COUNTY")

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() + theme_bw()

#now let's try binning the data to see where 'hotspots' are
plotData <- plotData %>% mutate(pct = ifelse(percent<0.25,0,
                                ifelse(percent>0.25 & percent <= 0.5,1,
                                       ifelse(percent > 0.5 & percent <= 0.75,2,
                                              ifelse(percent > 0.75,3,NA)))))

ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) + theme_bw()


ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = factor(pct))) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() + scale_fill_theme_bw()


ggsave(p, file = "map1.png", width = 6, height = 4.5, type = "cairo-png")


