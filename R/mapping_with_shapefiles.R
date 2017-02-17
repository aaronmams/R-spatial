
library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(maptools)
library(rgeos)
library(dplyr)



#############################################################
#############################################################
#############################################################
#############################################################
#############################################################

#let's try with the Census Tract stuff
tract <- readOGR(dsn = "data/gz_2010_06_140_00_500k", 
                 layer = "gz_2010_06_140_00_500k")
tract <- fortify(tract, region="GEO_ID")

ins <- read.csv("data/CA_insured_tract.csv")
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
county <- readOGR(dsn = "data/gz_2010_06_060_00_500k", 
                  layer = "gz_2010_06_060_00_500k")
county <- fortify(county, region="COUNTY")

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() + theme_bw()


ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) + theme_bw() + 
    ggtitle("Percent Insured by Census Tract")
#-------------------------------------------------------------------

#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#let's see if we can map some shit by congressional district
congress <- readOGR(dsn = "data/cb_2015_us_cd114_500k", 
                  layer = "cb_2015_us_cd114_500k")
congress <- fortify(congress, region="AFFGEOID")

#population by sex and age
pop.dist <- read.csv('data/ACS_15_5YR_B01001/ACS_15_5YR_B01001.csv')
#rename columns because the names currently are nonsense
names(pop.dist) <- c('GEO.id','GEO.id2','GEO.display.label','Tpop','moepop','popmale','moemale',
                     'popm18_19','moem18_19','popm20','moem20','popm21','moem21','popm22_24','moem22_24',
                     'popm25_29','moem25_29','popm30_34','moem30_34',
                     'popm35_39','moem35_39','popm40_44','moem40_44',
                     'popm45_49','moem45_49','popm50_54','moem50_54',
                     'popm55_59','meom55_59','popm60_61','moem60_61',
                     'popm62_64','moem62_64','popm65_66','moem65_66',
                     'popm67_69','moem67_69','popm70_74','moem70_74',
                     'popm75_79','moem75_79','popm80_84','moem80_84',
                     'popm85','moem85','popf','moef',
                     'popf18_19','moef18_19','popf20','moef20','popf21',
                     'moef21','popf22_24','moef22_24',
                     'popf25_29','moef25_29','popf30_34','moef30_34',
                     'popf35_39','moef35_39','popf40_44','moef40_44',
                     'popf45_49','moef45_49','popf50_54','moef50_54',
                     'popf55_59','meof55_59','popf60_61','moef60_61',
                     'popf62_64','moef62_64','popf65_66','moef65_66',
                     'popf67_69','moef67_69','popf70_74','moef70_74',
                     'popf75_79','moef75_79','popf80_84','moef80_84',
                     'popf85','moef85')

#now I'm going to clean this population data frame up a lot
pop.dist <- pop.dist %>% mutate(male.group1 = popm18_19+popm20+popm21+popm22_24+popm25_29,
                                male.group2 = popm30_34+popm35_39+popm40_44+popm45_49,
                                male.group3 = popm50_54+popm55_59+popm60_61+popm62_64+popm65_66+
                                                    popm67_69+popm70_74+popm75_79+popm80_84+popm85,
                                female.group1 = popf18_19+popf20+popf21+popf22_24+popf25_29,
                                female.group2 = popf30_34+popf35_39+popf40_44+popf45_49,
                                female.group3 = popf50_54+popf55_59+popf60_61+popf62_64+popf65_66+
                                                    popf67_69+popf70_74+popf75_79+popf80_84+popf85) %>%
            select(GEO.id, GEO.id2, GEO.display.label, Tpop, popmale, popf,
                   male.group1,male.group2,male.group3, female.group1,female.group2,female.group3)
            
pop.dist$id <- as.character(pop.dist$GEO.id)

plotData2 <- left_join(congress, pop.dist)
plotData2$mf <- plotData2$male.group2/plotData2$female.group2

#get rid of alaska and hawaii
state.tmp <- strsplit(as.character(plotData2$GEO.display.label),",")
state <- sapply(state.tmp,function(x) x[2])

plotData2$state <- str_trim(state,side=c('both'))


plotData2 <- plotData2 %>% filter(!is.na(state)) %>% filter(! state %in% c('Alaska','Hawaii'))


p <- ggplot() +
  geom_polygon(data = plotData2, aes(x = long, y = lat, group = group,
                                    fill = mf), color = "black", size = 0.25) + 
  theme_bw()

##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################



