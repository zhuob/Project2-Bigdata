
##  Question to be answered

##------------------------------------------------------------------------------------
# Are there any airports with flight departure delays that are systematically worse  -
# or better than other airports? Be sure to consider seasonal fluctuations that might -
# be due to differential whether patterns, using 25 years of flight data for the 
# entire country.

library(dplyr)
library(ggplot2)
setwd( "/home/zhuob/Project2-Bigdata")

spring <- read.csv("SpringPop.csv", header=T)
summer <- read.csv("SummerPop.csv", header=T)
fall <- read.csv("FallPop.csv", header=T)
winter <- read.csv("WinterPop.csv", header=T)
winter <- winter[, -3] ## don't want the mean wait time
colnames(winter)[1] <- "origin"
# don't see why this is not working
# season <- join_all(list(spring, summer, fall, winter), by = "origin")

sea1 <- merge(spring, summer, by = "origin")
sea2 <- merge(sea1, fall, by = "origin")
season <- merge(sea2, winter, by = "origin")
colnames(season)[-1] <- c("pop.spring", "pop.summer", "pop.fall", "pop.winter")
head(season)


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


s1 <- rankDelay(s_samp, type="samp", order ="ascending", num=5)

s1

head(s_samp)
co <- conf.interv(alpha = 0.95, season="spring")
result <- merge(sprg, co, by ="origin")
## creating a new column, indicating if our CI captures the population prop.
head(result)
# it's better to view p values

stat0 <- (result$prob-result$pop.spring)/result$sd
result$p <- 2*(1-pnorm(abs(stat0)))
mean(result$p <0.05)
## this histogram shows that our p value is not significantly different from
## uniform distribution, which is good.
m <- ggplot(result, aes(x=p))
  m + geom_histogram(aes(fill = ..count..), binwidth= 0.1) +
scale_fill_gradient("Count", low = "green", high = "red")



# result$inCI <- ifelse( (result[, 2]<= result[, 6]) & (result[, 2]>= result[, 5]),"Yes", "No")
# result[which(result$inCI =="No"),]

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
co1 <- conf.interv(alpha=0.90, season="all")
head(co1)
sampVsPopu <- merge(year.pop, co1, by="origin")
colnames(sampVsPopu)[c(2:3)] <- c("popu.p", "samp.p")

#sampVsPopu$inCI <- ifelse( (sampVsPopu[, 2]<= sampVsPopu[, 6]) & 
#                            (sampVsPopu[, 2]>= sampVsPopu[, 5]),"Yes", "No")

# the following gives you the type I error rate
# sum(sampVsPopu$inCI =="No")/dim(sampVsPopu)[1] # 0.04126984
# might be a bit conservative, but good enough. 


stat <- (sampVsPopu$samp.p-sampVsPopu$popu.p)/(sampVsPopu$sd)
sampVsPopu$p <- 2*(1-pnorm(abs(stat)))
mean(sampVsPopu$p<0.1)
# the p value looks uniform

m <- ggplot(sampVsPopu, aes(x=p))
  m + geom_histogram(aes(fill = ..count..), binwidth= 0.1) 

# change type, and order, number for different interest
s1 <- rankDelay(s_samp, type="samp", order ="descending", num=10) 
# change s1$sum[, 1] to  s1$win[, 1] if you want to see winter
apt <- data.frame(s1$sum[,1]) 
colnames(apt) <- "origin"
s2 <- merge(apt, s_samp, by = "origin", all.x= T)
s3 <- s2[, c(1, 6:9)] # keep sample proportions grouped by season
# s3 <- s2[, c(1:5)] # if you want to see rank for population

library(reshape2)
md <- melt(s3, id=(c("origin")))

## this shows the graph Martin suggests 
ggplot(md, aes(variable, value, group = origin, colour = origin)) +
  geom_path(alpha = 1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Top 10 airports with worst delay(population level)")


## We probably want to make some table as well

# The following shows which airports are most likely to delay
rr <- rankDelay(s_samp, type="pop", order ="descending", num=20) 
r1 <- merge(rr$spr, rr$sum, by = "origin", all= F)
r1 <- merge(r1, rr$fal, by="origin", all=F)
r1 <- merge(r1, rr$win, by="origin", all=F)
r1 
##---------------------------------------------------------------------
## by ranking the first 20
## conclusion:  population level , 13 airports are consistently  worse
## --------------------------------------------------------------------

rr <- rankDelay(s_samp, type="pop", order ="ascending", num=20) 
r1 <- merge(rr$spr, rr$sum, by = "origin", all= F)
r1 <- merge(r1, rr$fal, by="origin", all=F)
r1 <- merge(r1, rr$win, by="origin", all=F)
r1 

##---------------------------------------------------------------------
## conclusion:  population level , 3 airport are consistently  better
## --------------------------------------------------------------------


rr <- rankDelay(s_samp, type="samp", order ="ascending", num=30) 

r1 <- merge(rr$sum, rr$fal, by="origin", all=F)
r1 <- merge(r1, rr$win, by="origin", all=F)
r1



rr <- rankDelay(s_samp, type="samp", order ="descending", num=30) 
r1 <- merge(rr$spr, rr$sum, by = "origin", all= F)
r1 <- merge(r1, rr$fal, by="origin", all=F)
r1 <- merge(r1, rr$win, by="origin", all=F)
r1 

## I don't think we can rank them overally, it should be better if we just
## categorize them by season
