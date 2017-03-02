library(data.table)
library(dplyr)
library(ggplot2)

#get the results of the 
house2012 <- tbl_df(read.csv('data/HouseElectionResults2012.csv')) %>%
              filter(GE.WINNER.INDICATOR=='W' & D!='S') %>%
              filter(! STATE %in% c('American Samoa','Puerto Rico','Northern Mariana Islands','District of Columbia'))

#----------------------------------------------------
house2016 <- read.csv('data/Congress114_members.txt')
house2016 <- strsplit(as.character(house2016[,1]),"\\s+")

#first just take out all the list elements that have exactly 5 elements because
# these will be easy to deal with
goods <- which(lapply(house2016,function(x)length(x))==5)

good.members <- data.frame(rbindlist(lapply(goods,function(i){
  tmp <- house2016[[i]]
  cd <- unlist(strsplit(tmp[[1]],"[.]"))
  data.frame(cd,firstname=tmp[2],lastname=tmp[3],party=tmp[4],state=tmp[5])
})))

#probably have to do the others by hand
bads <- which(lapply(house2016,function(x)length(x))!=5)

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

house2016 <- rbind(good.members,df)

#last step is to get the party afiliation
party.afil <- lapply(house2016$party,function(x){
  tmp <- strsplit(as.character(x),'[())]')
  if(length(unlist(tmp))==2){
    party <- as.character(unlist(tmp)[2])
  }else{
    party <- as.character(tmp)
  }
return(party)  
}) 

house2016$party <- unlist(party.afil)

#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
key <- 'f5a32f694a14b28acf7301f4972eaab8551eafda'
#Now get 3 simple demographics for 2012 and 2016

#1. percent female
#2. percent black
#3. percent with a college degree 
#4. age structure...let's just use percent 25 - 50

# by congressional district


# age and sex
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
  names(tmp) <- c('name','variable','state','congressional_district','series_name')
  return(tmp)
}

age.sex <- pop.fn(i=1,yr=2015)
names(age.sex) <- c('name','total_pop','state','congressional_district','series')
age.sex$total_female <- pop.fn(i=13,yr=2015)[,2]
age.sex$f25 <- pop.fn(i=18,yr=2015)[,2]
age.sex$f30 <- pop.fn(i=19,yr=2015)[,2]
age.sex$f35 <- pop.fn(i=20,yr=2015)[,2]
age.sex$f40 <- pop.fn(i=21,yr=2015)[,2]
age.sex$f45 <- pop.fn(i=22,yr=2015)[,2]
age.sex$f50 <- pop.fn(i=23,yr=2015)[,2]



df12 <- pop.fn(i=1,yr=2012)
names(pop.fn) <- c('name','total_pop','state','congressional district','series')
df12$total_female <- pop.fn(i=13,yr=2012)[,2]
df12$f25 <- pop.fn(i=18,yr=2012)[,2]
df12$f30 <- pop.fn(i=19,yr=2012)[,2]
df12$f35 <- pop.fn(i=20,yr=2012)[,2]
df12$f40 <- pop.fn(i=21,yr=2012)[,2]
df12$f45 <- pop.fn(i=22,yr=2012)[,2]
df12$f50 <- pop.fn(i=23,yr=2012)[,2]


#percent black
#2015 1 yr ACS estimate
#black population
resURL <- 'http://api.census.gov/data/2015/acs1?get=NAME,B02001_003E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)
pct.black <- data.frame(rbindlist(lapply(ljson,function(x){
  tmp <- unlist(x)
  return(data.frame(name=tmp[1],pop.black=tmp[2],state=tmp[3],cd=tmp[4]))
})))

#total population
resURL <- 'http://api.census.gov/data/2015/acs1?get=NAME,B02001_001E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)
total.pop <- data.frame(rbindlist(lapply(ljson,function(x){
  return(data.frame(name=unlist(x)[1],total.pop=unlist(x)[2]))
})))

pct.black <- tbl_df(pct.black) %>%
              inner_join(total.pop,by=c('name'))


#2012 1 yr ACS estimate
#black population
resURL <- 'http://api.census.gov/data/2012/acs1?get=NAME,B02001_003E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)
pct.black2012 <- data.frame(rbindlist(lapply(ljson,function(x){
  tmp <- unlist(x)
  return(data.frame(name=tmp[1],pop.black=tmp[2],state=tmp[3],cd=tmp[4]))
})))

#total population
resURL <- 'http://api.census.gov/data/2012/acs1?get=NAME,B02001_001E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)
total.pop <- data.frame(rbindlist(lapply(ljson,function(x){
  return(data.frame(name=unlist(x)[1],total.pop=unlist(x)[2]))
})))

pct.black2012 <- tbl_df(pct.black2012) %>% 
                    inner_join(total.pop,by=c('name'))

#---------------------------------------------------------------------------------
#poverty percentages
resURL <- 'http://api.census.gov/data/2015/acs1?get=NAME,B17001_025E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)
poverty.f25 <- data.frame(rbindlist(lapply(ljson,function(x){
  return(data.frame(name=unlist(x)[1],pop.poverty=unlist(x)[2]))
})))

resURL <- 'http://api.census.gov/data/2015/acs1?get=NAME,B17001_026E&for=congressional+district:*&key=f5a32f694a14b28acf7301f4972eaab8551eafda'
ljson <- fromJSON(resURL)
poverty.f35 <- data.frame(rbindlist(lapply(ljson,function(x){
  return(data.frame(name=unlist(x)[1],pop.poverty=unlist(x)[2]))
})))

pov.df <- data.frame(cbind(poverty.f25[2:nrow(poverty.f25),],poverty.f35$pop.poverty[2:nrow(poverty.f35)]))
names(pov.df) <- c('name','pov25','pov35')
#---------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------
#Education Level

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
  df <- data.frame(name=name,pop25=pop25,state=state,congressional_district=congressional_district,
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


#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

#add state abbreviations to the age.sex data frame
state.codes <- read.csv('data/state_fips_codes.csv') %>% select(code,abb)
names(state.codes) <- c('state.code','abb')

names(house2016) <- c('cd','firstname','lastname','party','abb')
house2016$cd <- as.numeric(as.character(house2016$cd))
house2016$cd[is.na(house2016$cd)] <-  0 

age.sex <- age.sex %>% mutate(state.code = as.numeric(as.character(state))) %>% 
            inner_join(state.codes,by=c('state.code')) %>%
            mutate(cd=as.numeric(as.character(congressional_district))) %>%
            inner_join(house2016,by=c('abb','cd'))

#now bring in the % black
pct.black <- pct.black %>% filter(row_number() > 1) %>% 
              mutate(cd=as.numeric(as.character(cd)),
                     state.code=as.numeric(as.character(state)),
                     pop.black=as.numeric(as.character(pop.black)),
                     total.pop=as.numeric(as.character(total.pop)),
                     pct.black=pop.black/total.pop) %>%
              select(cd,state.code,pct.black)

age.sex <- age.sex %>% inner_join(pct.black,by=c('state.code','cd'))


#bring in education
edu2015 <- edu2015 %>% mutate(state.code=as.numeric(as.character(state)),
                              cd=as.numeric(as.character(congressional_district)),
                              pop25=as.numeric(as.character(pop25)),
                              pop25_bachelors=as.numeric(as.character(pop25_bachelors)),
                              pct.bachelors=pop25_bachelors/pop25) %>%
          select(state.code,cd,pct.bachelors)

age.sex <- age.sex %>% inner_join(edu2015,by=c('state.code','cd'))

#add in the female poverty
age.sex <- age.sex %>% left_join(pov.df,by=c('name')) %>%
              mutate(pov.female=as.numeric(as.character(pov25))+
                       as.numeric(as.character(pov35)),
                pct.pov.female=as.numeric(as.character(pov.female))/as.numeric(as.character(total_female)))

############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
#look at republican districts with highest female poverty rates
age.sex %>% select(name,party,pct.pov.female,pct.female) %>% 
   filter(party=='R') %>% arrange(-pct.pov.female,-pct.female)

############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################

#propensity to vote for Clinton over Trump by demographic make-up

#get 2016 presidential election results by CD
#http://www.dailykos.com/story/2012/11/19/1163009/-Daily-Kos-Elections-presidential-results-by-congressional-district-for-the-2012-2008-elections
pres.2016 <- read.csv('data/2016presidential_by_cd.csv')
#1st value after party affiliation is Clinton vote share
# 2nd value is Trump vote share

head(pres.2016[pres.2016$Party=='(R)' & pres.2016$Clinton.2016>pres.2016$Trump.2016,])

pres.2016 <- pres.2016 %>% mutate(d.margin=Clinton.2016 - Trump.2016) %>%
  arrange(Party,-d.margin) 

p16 <- pres.2016 %>% mutate(Dem=ifelse(Clinton.2016>Trump.2016,1,0)) %>%
        select(CD,Dem) 
cd.new <- strsplit(as.character(p16$CD),"-")
p16$abb <- unlist(lapply(cd.new,function(x){x[1]}))
p16$cd <- unlist(lapply(cd.new,function(x){x[2]}))

#fix 'at large' values
p16 <- p16 %>% mutate(cd=ifelse(cd=='AL','00',cd),cd=as.numeric(cd))

#merge these results with our demographics
age.sex <- age.sex %>% left_join(p16,by=c('abb','cd'))

#something simple
#logit model
vote.red <- glm(Dem~pct.bachelors+pct.female+pct.black,data=age.sex,family=binomial(link='logit'))
age.sex$pr.D <- predict.glm(vote.red, newdata = age.sex, type = "response")

#sort by propensity and look
age.sex %>% arrange(-pr.D) %>% select(name,pr.D,party,pct.black,pct.bachelors,pct.female)
              
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
# try a classification tree application just for fun

# here we will use a classification tree to predict 2016 presidential election outcomes
# by congressional district using demographic data

library(tree)

#recod Republican districts = 1
age.sex <- age.sex %>% mutate(z = ifelse(party=='R',1,0),
                              female.votingage=as.numeric(as.character(f25))+
                                               as.numeric(as.character(f30))+
                                               as.numeric(as.character(f40))+
                                               as.numeric(as.character(f50)),
                              total_pop=as.numeric(as.character(total_pop)),
                              pct.female=female.votingage/total_pop) 


#have a quick look at some factor distributions
ggplot(age.sex,aes(x=party,y=pct.bachelors)) + geom_boxplot() + theme_bw()
ggplot(age.sex,aes(x=party,y=pct.female)) + geom_boxplot() + theme_bw()


#a classification tree
tree.model <- tree(factor(party) ~ pct.black + pct.bachelors + pct.female, data=age.sex)
tree.model

my.prediction <- predict(tree.model, age.sex) # gives the probability for each class
head(my.prediction)
my.prediction <- data.frame(pDem=my.prediction[,1],pRepub=my.prediction[,2])

#Create a simplied postestimation data frame
tree.df <- age.sex %>% select(name,party,pct.black,pct.bachelors,pct.female,party,Dem)
tree.df <- cbind(tree.df,my.prediction)


plot(tree.model)
text(tree.model)

#look at number correctly classified
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(my.prediction, c(1), maxidx)
prediction <- c('D', 'R')[idx]

tree.df$tree.pred <- prediction

#compare number correctly classified to what we would get from a logit model
#logit model
vote.red <- glm(z~pct.bachelors+pct.female+pct.black,data=age.sex,family=binomial(link='logit'))
tree.df$pr.R <- predict.glm(vote.red, newdata = tree.df, type = "response")

tree.df <- tree.df %>% mutate(logit.correct=ifelse(pr.R>0.5 & party=='R',1,
                                                   ifelse(pr.R<0.5 & party=='D',1,0)),
                              tree.correct=ifelse(tree.pred==party,1,0))

logit.bad <- nrow(tree.df) - sum(tree.df$logit.correct)
tree.bad <- nrow(tree.df) - sum(tree.df$tree.correct)


#First priority: Red districts where the prediction didn't work well & that 
# voted for Clinton over Trump 
tree.df %>% filter(tree.pred=='D' & party=='R' & Dem==1) %>% arrange(-pDem)

#which Republican Districts were predicted to vote Democrate but voted Trump in the National Election:
tree.df %>% filter(tree.correct==0 & party=='R' & Dem==0) %>% arrange(-pDem)

