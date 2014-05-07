
##  Question to be answered

##------------------------------------------------------------------------------------
# Are there any airports with flight departure delays that are systematically worse  -
# or better than other airports? Be sure to consider seasonal fluctuations that might -
# be due to differential whether patterns, using 25 years of flight data for the 
# entire country.

library(dplyr)
setwd( "/home/zhuob/Project2-Bigdata")

spring <- read.csv("SpringPop.csv", header=T)
summer <- read.csv("SummerPop.csv", header=T)
fall <- read.csv("FallPop.csv", header=T)
winter <- read.csv("WinterPop.csv", header=T)
colnames(winter)[1] <- "origin"
  
# don't see why this is not working
# season <- join_all(list(spring, summer, fall, winter), by = "origin")

sea1 <- merge(spring, summer, by = "origin")
sea2 <- merge(sea1, fall, by = "origin")
season <- merge(sea2, winter, by = "origin")
colnames(season)[-1] <- c("pop.spring", "pop.summer", "pop.fall", "pop.winter")



# this is the dataset that took us more than 5 hours to obtain
sample <- read.csv("/home/zhuob/Project2-Bigdata/dataset/combinedSample.csv",header=T)

# filter by seasons
spring.s <- sample[which(sample$month %in% c(4, 5)), ]
summer.s <- sample[which(sample$month %in% c(6, 7, 8)), ]
fall.s <- sample[which(sample$month %in% c(9, 10, 11)), ]
winter.s <- sample[which(sample$month %in% c(12, 1, 2, 3)), ]

# sample size in each season
c(dim(spring.s)[1], dim(summer.s)[1], dim(fall.s)[1], dim(winter.s)[1])


# calculate the prop. of delay for each season
spr.s <-  summarise(group_by(spring.s, origin), prob = mean(depdelay> 0, na.rm=T))
sum.s <-  summarise(group_by(summer.s, origin), prob = mean(depdelay> 0, na.rm=T))
fal.s <-  summarise(group_by(fall.s, origin), prob = mean(depdelay> 0, na.rm=T))
win.s <-  summarise(group_by(winter.s, origin), prob = mean(depdelay> 0, na.rm=T))

# merge them 
samp1 <- merge(spr.s, sum.s, by ="origin")
samp2 <- merge(samp1, fal.s, by = "origin")
colnames(samp2)[-1] <- c("spr.s","sum.s", "fal.s")
season.samp <- merge(samp2, win.s, by = "origin")
colnames(season.samp)[5] <- "win.s"

# merge the sample and population for prop. of delay
s_samp <- merge(season, season.samp, by = "origin")



## use the following functions to obtain the results

source("/home/zhuob/Project2-Bigdata/dataset/rankDelay.R")
source("/home/zhuob/Project2-Bigdata/dataset/conf.interv.R")

#-------------------- parameter ------------------------------
##  rankDelay ranks the airports by prop of delays, can have multiple choice
##  
## type :  "pop" or "samp"
## order:  "ascending" or "descending"
## num:  how many (ranked) airports you want to get

## alpha:  CI you want to achieve
## season: "all" for non-season, "srping", "summer", "fall", "winter"

ss <- rankDelay(s_samp, type="pop",order="descending", num=dim(s_samp)[1])
sprg <- ss$spr

co <- conf.interv(alpha = 0.95, season="spring")
result <- merge(sprg, co, by ="origin")
## creating a new column, indicating if our CI captures the population prop.

result$inCI <- ifelse( (result[, 2]<= result[, 6]) & (result[, 2]<= result[, 6]),"Yes", "No")
result[which(result$inCI =="No"),]

## probably you might noticed that CWA has both estimated prob and sd 0. That is
## because the estimate is truly 0, i.e. it never delays in spring. 
# try the following code

# co[which(co$origin=="CWA"),]

## this is all the sample for "CWA"
# sample[which(sample$origin=="CWA"),]

# also might be intersting to check if the true prop is captured by our CI 
# regardless of seasonality

year.pop <- read.csv("PopProp.csv", header=T)[-3]
head(year.pop)
co1 <- conf.interv(alpha=0.95, season="all")
sampVsPopu <- merge(year.pop, co1, by="origin")
sampVsPopu$inCI <- ifelse( (sampVsPopu[, 2]<= sampVsPopu[, 6]) & 
                            (sampVsPopu[, 2]<= sampVsPopu[, 6]),"Yes", "No")

# the following gives you the type I error rate
sum(sampVsPopu$inCI =="No")/dim(sampVsPopu)[1] # 0.04126984
# might be a bit conservative, but good enough. 


