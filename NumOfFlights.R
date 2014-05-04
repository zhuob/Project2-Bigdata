###############################################
#Getting a data set that gives us the average #
#number of flights that an airport has a year #
###############################################
Num.flights_year <- flights %.%
  select(origin, year) %.%
  group_by(origin, year) %.%
  summarise(flights=n()) 
explain(Num.flights_year)

Num.Flights_avg <- Num.flights_year %.%
  summarise(Avg_Num.flights = mean(flights))
explain(Num.Flights_avg)

Num.Flights_data <- collect(Num.Flights_avg)
write.table(Num.Flights_data, "FlightNumber.csv", sep=",",
            col.names=TRUE)

Num.Flights_data <- read.csv("FlightNumber.csv", header=TRUE)
arrange(Num.Flights_data, desc(Avg_Num.flights))

################################################
# Sampling by Day in each month: A little bit  #
# better we will Stratify by month             #
################################################
jan <- sample(1:31, 5)
feb <- sample(1:29, 5)
mar <- sample(1:31, 5)
apr <- sample(1:30, 5)
may <- sample(1:31, 5)
jun <- sample(1:30, 5)
jul <- sample(1:31, 5)
aug <- sample(1:31, 5)
sep <- sample(1:30, 5)
oct <- sample(1:31, 5)
nov <- sample(1:30, 5)
dec <- sample(1:31, 5)
samples <- cbind(jan, feb, mar, apr, may, jun, 
                 jul, aug, sep, oct, nov, dec)
write.table(samples, file="SampleNumbers.csv", sep=",", 
            col.names=TRUE)


#Now Creating the Data Set from the samples
###########################################
#January 
Jan.days_prop <- flights %.%
  filter(month==1L & dayofmonth==25L | dayofmonth==10L |
           dayofmonth==6L | dayofmonth==8L | dayofmonth == 27L) %.%
  select(origin, depdelay) %.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Jan.days)

Jan_data <- collect(Jan.days)
write.table(Jan_data, file="JanSamp.csv", sep=",", col.names=TRUE)

#February
Feb.days <- flights %.%
  filter(month==2L & dayofmonth==12L | dayofmonth==14L |
           dayofmonth==23L | dayofmonth==18L | dayofmonth==19L) %.%
  select(origin, depdelay) %.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Feb.days)

Feb_data <- collect(Feb.days)
write.table(Feb_data, file="FebSamp.csv", sep=",", col.names=TRUE)

#March
Mar.days <- flights %.%
  filter(month==3L & dayofmonth==7L | dayofmonth==9L | 
           dayofmonth==31L | dayofmonth==28L | dayofmonth==14L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Mar.days)

Mar_data <- collect(Mar.days)
write.table(Mar_data, file="MarSamp.csv", sep=",", col.names=TRUE)

#April
Apr.days <- flights %.%
  filter(month==4L & dayofmonth==10L | dayofmonth==26L |
           dayofmonth==23L | dayofmonth==15L | dayofmonth==4L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Apr.days)

Apr_data <- collect(Apr.days)
write.table(Apr_data, file="AprSamp.csv", sep=",", col.names=TRUE)

#May
May.days <- flights %.%
  filter(month==5L & dayofmonth==16L | dayofmonth==18L |
           dayofmonth==23L | dayofmonth==20L | dayofmonth==3L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(May.days)

May_data <- collect(May.days)
write.table(May_data, file="MaySamp.csv", sep=",", col.names=TRUE)

#June
Jun.days <- flights %.%
  filter(month==6L & dayofmonth==9L | dayofmonth==25L | 
           dayofmonth==16L | dayofmonth==8L | dayofmonth==18L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Jun.days)

Jun_data <- collect(Jun.days)
write.table(Jun_data, file="JunSamp.csv", sep=",", col.names=TRUE)

#July
Jul.days <- flights %.%
  filter(month==7L & dayofmonth==4L | dayofmonth==2L |
           dayofmonth==29L | dayofmonth==3L | dayofmonth == 8L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Jul.days)

Jul_data <- collect(Jul.days)
write.table(Jul_data, file="JulSamp.csv", sep=",", col.names=TRUE)

#August
Aug.days <- flights %.%
  filter(month==8L & dayofmonth==29L | dayofmonth==10L |
           dayofmonth==9L | dayofmonth==24L | dayofmonth==20L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Aug.days)

Aug_data <- collect(Aug.days)
write.table(Aug_data, file="AugSamp.csv", sep=",", col.names=TRUE)

#September
Sep.days <- flights %.%
  filter(month==9L & dayofmonth==28L | dayofmonth==18L | 
           dayofmonth==5L | dayofmonth==25L | dayofmonth==29L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Sep.days)

Sep_data <- collect(Sep.days)
write.table(Sep_data, file="SepSamp.csv", sep=",", col.names=TRUE)

#October
Oct.days <- flights %.%
  filter(month==10L & dayofmonth==18L | dayofmonth==27L |
           dayofmonth==31L | dayofmonth==4L | dayofmonth==13L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Oct.days)

Oct_data <- collect(Oct.days)
write.table(Oct_data, file="OctSamp.csv", sep=",", col.names=TRUE)

#November
Nov.days <- flights %.%
  filter(month==11L & dayofmonth==9L | dayofmonth==18L |
           dayofmonth==25L | dayofmonth==10L | dayofmonth==24L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Nov.days)

Nov_data <- collect(Nov.days)
write.table(Nov_data, file="NovSamp.csv", sep=",", col.names=TRUE)

#December
Dec.days <- flights %.%
  filter(month==12L & dayofmonth==3L | dayofmonth==14L | 
           dayofmonth==31L | dayofmonth==19L | dayofmonth==22L) %.%
  select(origin, depdelay)%.%
  group_by(origin) %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Dec.days)

Dec_data <- collect(Dec.days)
write.table(Dec_data, file="DecSamp.csv", sep=",", col.names=TRUE)

SampleOfDays <- read.csv("SampleDays.csv", header=TRUE)

##########################################################
# We can also try to sample by months (Selecting 1 month #
# from each of the 4 seasons)                            #
##########################################################
winter = c(12, 1, 2, 3)
spring = c(4, 5)
summer = c(6, 7, 8)
fall = c(9, 10, 11)
month.sample = c(sample(winter, 1), sample(spring, 1), 
                 sample(summer, 1), sample(fall, 1))

Samp.month <- flights %.%
  filter(month==12L | month==4L | month==8L | month==9L) %.%
  select(depdelay, origin) %.%
  group_by(origin) %.%
  summarise(props.month = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Samp.month)

Samp.month_data <- collect(Samp.month)
write.table(Samp.month_data, file="MonthSample.csv", sep=",",
            col.names=TRUE)

arrange(Samp.month_data, desc(props.month))
