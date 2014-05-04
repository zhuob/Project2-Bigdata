##  Question to be asked

##------------------------------------------------------------------------------------
# Are there any airports with flight departure delays that are systematically worse  -
# or better than other airports? Be sure to consider seasonal fluctuations that might -
# be due to differential whether patterns, using 25 years of flight data for the 
# entire country.


library(dplyr)
library(RPostgreSQL)  #added by Feifei

endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", host = endpoint, port = 5432,
                      user = user, password = password)

flights = tbl(ontime, "flights")
as.tbl(head(flights))

# dim(flights)  #  154986896        30

hou <- filter(flights, (year == "2011" & month == 1) &  
                (origin == "SEA" | origin == "IAH") & (weatherdelay == 1))
head(hou, 20L)
dim(hou)
explain(hou)

summerise(group_by(fligts, origin), )
a <- summarise(group_by(flights, origin), prob = mean(as.integer(depdelay>0)))

collect(a)
arrange(a, desc(prob))



y <- summarise(group_by(flights, origin), ave = mean(depdelay))
ave.delay <- collect(y)
write.csv(ave.delay,"aveDelay.byAirport.csv")
mean(ave.delay$ave)

