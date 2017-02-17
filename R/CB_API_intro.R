library(RJSONIO)

library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(maptools)
library(rgeos)
library(dplyr)

##################################################################
#Try to parse out the variables list so I can do some manner
# of informed search for searies I'm looking for:
library(XML)
#theurl <- "http://api.census.gov/data/2015/acs1/variables.html"
#tables <- readHTMLTable(theurl)
#################################################################


#################################################################
# quick example using a stock API call for population
key <- 'f5a32f694a14b28acf7301f4972eaab8551eafda'
resURL <- paste('http://api.census.gov/data/2015/acs1?get=NAME,B01001_001E&for=state:*&key=',
                key,sep="")
ljson<-fromJSON(resURL)
ljson<-ljson[2:length(ljson)]
pop <- data.frame(statename=unlist(lapply(ljson,function(x){x[1]})),
                  totalpop=unlist(lapply(ljson,function(x){x[2]})),
                  state=unlist(lapply(ljson,function(x){x[3]})))
#################################################################
#################################################################
#################################################################
#################################################################

################################################################
################################################################
################################################################
#Age of population....let's keep it simple and just get total pop 18 - 30, 31-50, and 50 and over

series.males <- c('001E','002E','007E','008E','009E','010E','011E','012E','013E','014E','015E','016E')
series.females <- c('026E','031E','032E','033E','034E','035E','036E','037E','038E','039E','040E')
series <- c(series.males,series.females)

series <- paste('B01001_',series,sep="")
series.names<- c('total pop','total male','m18_19','m20','m21','m22_24','m25_29','m30_34',
                 'm35_39','m40_44','m45_49','m50_54','total female','f18_19','f20','f21','f22_24',
                 'f25_29','f30_34',
                 'f35_39','f40_44','f45_49','f50_54')


pop.fn <- function(i,yr){
  resURL <- paste('http://api.census.gov/data/',yr,
                  '/acs1?get=NAME,',
                  series[i],
                  '&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda',
                  sep="")
  ljson <- fromJSON(resURL)
  ljson <- ljson[2:length(ljson)]
  tmp <- data.frame(unlist(lapply(ljson,function(x)x[1])),
                    unlist(lapply(ljson,function(x)x[2])),
                    unlist(lapply(ljson,function(x)x[3])),
                    unlist(lapply(ljson,function(x)x[4])),
                    series.names[i])
  names(tmp) <- c('name','variable','state','congressional district','series_name')
  return(tmp)
}

pop.df2015 <- data.frame(rbindlist(lapply(c(1:length(series)),pop.fn,yr=2015)))
pop.df2015$source <- 'ACS 2015 1 yr'
pop.df2014 <- data.frame(rbindlist(lapply(c(1:length(series)),pop.fn,yr=2014)))
pop.df2014$source <- 'ACS 2014 1 yr'
pop.df2012 <- data.frame(rbindlist(lapply(c(1:length(series)),pop.fn,yr=2012)))
pop.df2012$source <- 'ACS 2012 1 yr'
################################################################
################################################################
################################################################


################################################################
################################################################
################################################################

#````````````````````````````````````````````````````````````````
#total population and black population by congressional district
 # from the American Communities Survey, 1 year estimates

#2015 1 yr ACS estimate
#black population
resURL <- 'http://api.census.gov/data/2015/acs1?get=NAME,B02001_003E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)
#total population
resURL <- 'http://api.census.gov/data/2015/acs1?get=NAME,B02001_001E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)

#2014 1 yr ACS estimates
#total pop
resURL <- 'http://api.census.gov/data/2014/acs1?get=NAME,B02001_001E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)
#black pop
resURL <- 'http://api.census.gov/data/2014/acs1?get=NAME,B02001_003E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)

#2013 1 yr ACS estimate
#black pop
resURL <- 'http://api.census.gov/data/2013/acs1?get=NAME,B02001_003E&for=state:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)

#total pop
resURL <- 'http://api.census.gov/data/2013/acs1?get=NAME,B02001_001E&for=state:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)

#2012 1 yr ACS estimate
#total pop
resURL <- 'http://api.census.gov/data/2012/acs1?get=NAME,B02001_001E&for=state:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)

#total pop
resURL <- 'http://api.census.gov/data/2012/acs1?get=NAME,B02001_003E&for=state:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)
#````````````````````````````````````````````````````````````````````````````````````````````
################################################################
################################################################
################################################################


################################################################
################################################################
################################################################
#-----------------------------------------------------------------
#Educational Attainment


#set up a function for this too...just for compactness
edu.fn <- function(yr){
  resURL <- paste('http://api.census.gov/data/',yr,
                  '/acs1/subject?get=NAME,S1501_C01_006E&for=congressional+district:*&',
                  'key=f5a32f694a14b28acf7301f4972eaab8551eafda',sep="")
  ljson <- fromJSON(resURL)
  name <- unlist(lapply(ljson[2:length(ljson)],function(x)x[1]))
  pop25 <- unlist(lapply(ljson[2:length(ljson)],function(x)x[2]))
  state <- unlist(lapply(ljson[2:length(ljson)],function(x)x[3]))
  congressional_district <- unlist(lapply(ljson[2:length(ljson)],function(x)x[4]))
  df <- data.frame(name=name,pop25=value,state=state,congressional_district=congressional_district,
                   source=paste('ACS_1yr_',yr,sep="")) 
  
  #add in the number of people 25 and over with a bachelor's degree
  resURL <- paste('http://api.census.gov/data/',yr,
                  '/acs1/subject?get=NAME,S1501_C01_012E&for=congressional+district:*&',
                  'key=f5a32f694a14b28acf7301f4972eaab8551eafda',sep="")
  ljson <- fromJSON(resURL)
  pop25_bachelors <- unlist(lapply(ljson[2:length(ljson)],function(x)x[2]))
  
  df$pop25_bachelors <- pop25_bachelors
  return(df)
}  

edu2015 <- edu.fn(yr=2015)
edu2014 <- edu.fn(yr=2014)
edu2012 <- edu.fn(yr=2012)
################################################################
################################################################
################################################################


################################################################
################################################################
################################################################
# A quick mapping example
edu.fn <- function(yr){
  resURL <- paste('http://api.census.gov/data/',yr,
                  '/acs1/subject?get=NAME,S1501_C01_006E&for=congressional+district:*&',
                  'key=f5a32f694a14b28acf7301f4972eaab8551eafda',sep="")
  ljson <- fromJSON(resURL)
  name <- unlist(lapply(ljson[2:length(ljson)],function(x)x[1]))
  pop25 <- unlist(lapply(ljson[2:length(ljson)],function(x)x[2]))
  state <- unlist(lapply(ljson[2:length(ljson)],function(x)x[3]))
  congressional_district <- unlist(lapply(ljson[2:length(ljson)],function(x)x[4]))
  df <- data.frame(name=name,pop25=value,state=state,congressional_district=congressional_district,
                   source=paste('ACS_1yr_',yr,sep="")) 
  
  #add in the number of people 25 and over with a bachelor's degree
  resURL <- paste('http://api.census.gov/data/',yr,
                  '/acs1/subject?get=NAME,S1501_C01_012E&for=congressional+district:*&',
                  'key=f5a32f694a14b28acf7301f4972eaab8551eafda',sep="")
  ljson <- fromJSON(resURL)
  pop25_bachelors <- unlist(lapply(ljson[2:length(ljson)],function(x)x[2]))
  
  df$pop25_bachelors <- pop25_bachelors
  return(df)
}  

edu2015 <- edu.fn(yr=2015)

#read the shapefile with cartographic boundaries for congressional districts
#let's see if we can map some shit by congressional district
congress <- readOGR(dsn = "data/cb_2015_us_cd114_500k", 
                    layer = "cb_2015_us_cd114_500k")
#pull out state, CD, and AFFGEOID so we can merge with the education stuff
df.tmp <- congress@data[,c('STATEFP','CD114FP','AFFGEOID')]
df.tmp$state_cd <- paste(df.tmp$STATEFP,df.tmp$CD114FP,sep="")

congress <- fortify(congress, region="AFFGEOID")


#keep edu from 
edu.tmp <- edu2015 %>% mutate(state_cd=paste(state,congressional_district,sep="")) %>% 
            inner_join(df.tmp,by=c('state_cd')) %>%
            filter(state %in% c('06','04','49','32')) %>%
            rename(id=AFFGEOID) %>%
            mutate(pct_college=as.numeric(as.character(pop25_bachelors))/
                     as.numeric(as.character(pop25))) 
         

plotData <- inner_join(congress,edu.tmp)

#trim the congress data frame
ids <- unique(edu.tmp$id)
congress <- congress %>% filter(congress$id %in% ids)

ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = 1-pct_college)) +
  geom_polygon(data = congress, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "YlOrRd", labels = percent,
                       breaks = pretty_breaks(n = 15)) +
  guides(fill = guide_legend(reverse = FALSE)) + theme_bw() 

#I'm pretty sure we could grab a static google map to use as a basemap...
# but I don't particularly want to deal with that right now so I'm just going
# to layer in a few big cities for context
cities <- data.frame(name=c('San Francisco','Fresno','Los Angeles','Reno','Las Vegas','Phoenix'),
                     long=c(-122.49,-119.772,-118.2437,-119.8138,-115.139,-112.0740),
                     lat=c(37.77,36.74,34.052,39.529,36.169,33.448))

ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = pct_college)) +
  geom_polygon(data = congress, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
#  geom_point(data=cities,aes(x=long,y=lat,label=name)) + 
#  geom_text(data=cities,aes(x=long,y=lat,label=name),hjust=0, vjust=0) +
  coord_map() +
  scale_fill_distiller(palette = "Blues", labels = percent,
                       breaks = pretty_breaks(n = 5)) +
  guides(fill = guide_legend(reverse = TRUE)) + theme_bw() 

ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = pct_college)) +
  geom_polygon(data = congress, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "RdYlGn", labels = percent,
                       breaks = pretty_breaks(n = 5)) +
  guides(fill = guide_legend(reverse = FALSE)) + theme_bw() 

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################




resURL <- paste("http://api.census.gov/data/2015/acs1?get=NAME,B01001I_001E&for=congressional+district:*&key=",
                key,sep="")
latinos <- fromJSON(resURL)
latinos <- latinos[2:length(latinos)]
resURL <- paste("http://api.census.gov/data/2015/acs1?get=NAME,B01001_001E&for=congressional+district:*&key=",
                key,sep="")
pop <- fromJSON(resURL)
pop <- pop[2:length(pop)]
pop.L <- data.frame(name=unlist(lapply(pop,function(x){unlist(x)[1]})),
                    pop=unlist(lapply(pop,function(x){unlist(x)[2]})),
                    state=unlist(lapply(pop,function(x){unlist(x)[3]})),
                    cd=unlist(lapply(pop,function(x){unlist(x)[4]})))

latinos <- data.frame(name=unlist(lapply(latinos,function(x){unlist(x)[1]})),
                      latino.pop=unlist(lapply(latinos,function(x){unlist(x)[2]})))

pop.L <- pop.L %>% inner_join(latinos,by=c('name')) %>%
            mutate(pct_latino=as.numeric(as.character(latino.pop))/
                     as.numeric(as.character(pop)))

house_election_2016 <- read.csv('data/congressionalelections2016.csv')

state.codes <- read.csv('data/state_fips_codes.csv')
names(state.codes) <- c('statename','state.num','state')

votes <- house_election_2016 %>% 
          inner_join(state.codes,by=c('state'))

#fix pop.L congressional districts
pop.L$cd <- as.numeric(as.character(pop.L$cd))
pop.L$state <- as.numeric(as.character(pop.L$state))

votes <- votes %>% select(state.num,cd,winning_party)
names(votes) <- c('state','cd','party')

votes <- votes %>% inner_join(pop.L,by=c('state','cd'))

#order it by latino population and see if there are any notable 'R's
votes %>% arrange(-pct_latino)
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################

#now try it with a base map

#load the raster library
library(raster)
#library(rgeos)
#go back to California Census Tract data because the map 
tract <- readOGR(dsn = "data/gz_2010_06_140_00_500k", 
                 layer = "gz_2010_06_140_00_500k")
#load back in the insurance data we pulled
ins <- read.csv("data/CA_insured_tract.csv")
ins$id <- as.character(ins$GEO.id)
ins$percent <- ins$Insured18_64/ins$Pop18_64

#get a basemap
map <- get_map("Fresno", zoom = 11, maptype = "roadmap")
p <- ggmap(map)
p

#establish a bounding box around our static google map
box <- as(extent(as.numeric(attr(map, 'bb'))[c(2,4,1,3)] + 
                   c(.001,-.001,.001,-.001)), "SpatialPolygons")
proj4string(box) <- CRS(summary(tract)[[4]])
tractSub <- gIntersection(tract, box, byid = TRUE, 
                          id = as.character(tract$GEO_ID))
tractSub <- fortify(tractSub, region = "GEO_ID")
plotData <- left_join(tractSub, ins, by = "id")

ggmap(map) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group, 
                                    fill = percent), colour = NA, alpha = 0.5) +
  scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), 
                       labels = percent) +
  labs(fill = "") +
  theme_nothing(legend = TRUE) +
  guides(fill = guide_legend(reverse = TRUE, override.aes = 
                               list(alpha = 1)))
################################################################
################################################################
################################################################

#---------------------------------------------------------------------------------


##################################################################################
##################################################################################
##################################################################################
##################################################################################
# get members of the 113th congress from the pdf file
congress.members <- read.csv('data/Congress114_members.csv')
test <- readLines(con='data/Congress114_members.csv',n=3)

members <- read.csv('data/Congress114_members.txt')
members<- strsplit(as.character(members[,1]),"\\s+")

#first just take out all the list elements that have exactly 5 elements because
# these will be easy to deal with
goods <- which(lapply(members,function(x)length(x))==5)

good.members <- data.frame(rbindlist(lapply(goods,function(i){
  tmp <- members[[i]]
  cd <- unlist(strsplit(tmp[[1]],"[.]"))
  data.frame(cd,firstname=tmp[2],lastname=tmp[3],party=tmp[4],state=tmp[5])
})))

#probably have to do the others by hand
bads <- which(lapply(members,function(x)length(x))!=5)

#there are only 13 bad ones at this point...
r1 <- data.frame(cd=23,firstname='Debbie', lastname='Wasserman Schultz', party='D',state='FL')
r2 <- data.frame(cd=8,firstname='Chris Van', lastname='Hollen', party='D',state='MD')
r3 <- data.frame(cd=2,firstname='Ann McLane', lastname='Kuster', party='D',state='NH')
r4 <- data.frame(cd=12,firstname='Bonnie', lastname='Watson Coleman', party='D',state='NJ')
r5 <- data.frame(cd=1,firstname='Michelle', lastname='Lujan Grisham', party='D',state='NM')
r6 <- data.frame(cd=3,firstname='Ben Ray', lastname='Lujan', party='D',state='NM')
r7 <- data.frame(cd=15,firstname='Jose E', lastname='Serrano', party='D',state='NY')
r8 <- data.frame(cd=18,firstname='Sean Patrick', lastname='Maloney', party='D',state='NY')
r9 <- data.frame(cd=18,firstname='Sheila', lastname='Jackson Lee', party='D',state='TX')
r10 <- data.frame(cd=1,firstname='G.K.', lastname='Butterfield', party='D',state='NC')
r11 <- data.frame(cd=30,firstname='Eddie Bernice', lastname='Johnson', party='D',state='TX')
r12 <- data.frame(cd=3,firstname='Jamie', lastname='Herrera Beutler', party='R',state='WA')
r13 <- data.frame(cd=5,firstname='Cathy', lastname='McMorris Rodgers', party='R',state='WA')

df <- rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13)

members <- rbind(good.members,df)


##################################################################################
##################################################################################
##################################################################################
##################################################################################

#now we gotta match the list of reps to the list of reps voting yay and nay
# on the defund PP bill

defund <- read.csv('data/PP_defundvote_aye.csv')

#fix this df up a little
defund <- defund[,c(1,2)]
#there were only 2 Dems voting to defund
defund$party<- 'R'
defund$party[which(defund$Last.Name %in% c('Lipinski','Peterson'))] <- 'D'

support <- read.csv('data/PP_defundvote_nay.csv')
support <- support[,c(1,2)]
support$party<- 'D'
support$party[which(support$Last.Name %in% c('Dold','Hanna'))] <- 'R'

#a couple names got messed up
support$Last.Name <- as.character(support$Last.Name)
support$Last.Name[20] <- 'Cardenas'
support$Last.Name[75] <- 'Guiterrez'
support$Last.Name[113] <- 'Lujan'
support$Last.Name[152] <- 'Sanchez'
support$Last.Name[179] <- 'Velazquez'

#now we need to format the names in the roll call
# split anything with a comma
names.defund <- strsplit(as.character(defund$Last.Name),"[,]")
names.defund <- data.frame(rbindlist(lapply(names.defund,function(x){
  if(length(unlist(x))==2){
    lastname <- gsub("^\\s+|\\s+$", "", x[1])
    firstname <- gsub("^\\s+|\\s+$", "", x[2])
  }else{
    lastname <- gsub("^\\s+|\\s+$", "", x[1])
    firstname <- NA
  }
  return(data.frame(lastname=lastname,firstname=firstname))
})))


#do the same thing with the aye data frame
names.support <- strsplit(as.character(support$Last.Name),"[,]")
names.support <- data.frame(rbindlist(lapply(names.support,function(x){
  if(length(unlist(x))==2){
    lastname <- gsub("^\\s+|\\s+$", "", x[1])
    firstname <- gsub("^\\s+|\\s+$", "", x[2])
  }else{
    lastname <- gsub("^\\s+|\\s+$", "", x[1])
    firstname <- NA
  }
  return(data.frame(lastname=lastname,firstname=firstname))
})))

#merge these fixed names back with the original data frames
defund <- cbind(defund,names.defund)
support <- cbind(support,names.support)






# first pass summary stats on congressional districts

# age structure of the population
# ethnic make-up
# education level
# male/female

#-------------------------------------------------------------
#education series from ACS 2015 1 yr. estimates
edu2015 <- read.csv('data/ACS_education_cd/ACS_15_1YR_S1501.csv')
names(edu2015)

series <- c(1,2,3,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,
            124,128,132,136,140,144)

edu2015 <- edu[,series]
names(edu2015) <- c('id','id2','geography','totalpop_1824','males1824','females1824','less_than_hs_total1824',
                    'less_than_hs_males1824','less_than_hs_female1824',
                    'hs_grad_total1824','hs_grad_male1824','hs_grad_female1824',
                    'somecollege_total1824','somecollege_male1824','somecollege_female1824',
                    'bachelorsdegree_total1824','bachelorsdegree_male1824',
                    'bachelorsdegree_female1824','totalpop_25','males25','females25',
                    'totalpop25_associates','males25_associates','females25_associates',
                    'totalpop25_bachelorsdegree','males25_bachelorsdegree','females25_bachelorsdegree')

#get population with a bachelor's degree from
# 1. 2010 census
# 2. 2010 census 2012 estimates
# 3. 
#---------------------------------------------------------------
