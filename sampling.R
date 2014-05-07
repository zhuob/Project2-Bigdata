#################################################
# Figuring out the sample size needed for each  #
# of the 367 airports (e = 1.96 sqrt(p(1-p)/n)) #
# so n = [1.96^2*(p(1-p))]/e^2                    #
#################################################

#Assuming our needed error is .01
##################################
Prop.Delay_Data = read.csv("PopPropMean.csv", header=TRUE)
n.01 = ceiling(((1.96^2)*(Prop.Delay_Data[,2]*(1-Prop.Delay_Data[,2])))/
                 (.01^2))
n.05 =  ceiling(((1.96^2)*(Prop.Delay_Data[,2]*(1-Prop.Delay_Data[,2])))/
                  (.05^2))
n.02 =  ceiling(((1.96^2)*(Prop.Delay_Data[,2]*(1-Prop.Delay_Data[,2])))/
                  (.02^2))
sample.size_airport = cbind(Prop.Delay_Data, n.01, n.02, n.05)
sample.size_airport <- arrange(sample.size_airport, origin)
write.csv(sample.size_airport, "NeedSampleSize.csv", row.names=FALSE)

#That is the numbers we need for each airport. We can from here use 
#the code that is necessary for each of the airports individually or
# we can take the max of these numbers and take tha sample size for
# all of the airports
####################################################################
n.01_max = as.integer(max(n.01))
n.02_max = as.integer(max(n.02))
n.05_max = as.integer(max(n.05))

# Trying a for loop with just the maximum number for an
# error of .02 in proportion 
########################################################
Num.flights_year <- flights %.%
  select(origin) %.%
  group_by(origin) %.%
  summarise(flights=n()) 
explain(Num.flights_year)
Num.flights_data = collect(Num.flights_year)
write.csv(Num.flights_data, "TotalFlights.csv", row.names=FALSE)
flights.num=Num.flights_data[,2]
Sample.size_Num.flights <- cbind(sample.size_airport, flights.num)

write.csv(Sample.size_Num.flights , file="Flights_SampleSize.csv", row.names=FALSE)


setwd("/home/zhuob/Project2-Bigdata")
Sample.size_Num.flights <- read.csv("Flights_SampleSize.csv", header=T)

Num.Flights_02 <- filter(Sample.size_Num.flights, flights.num > n.02_max)
num_02 = length(Num.Flights_02[,2])
write.csv(Num.Flights_02, file="Airports_to_Sample.csv", row.names=FALSE)

Num.Flights_02=read.csv("Airports_to_Sample.csv", header=T)
origins_02 = Num.Flights_02[,1]
ns_02 = Num.Flights_02[,6]

origin_1 = toString(origins_02[1])

origin.1 = flights %.%
  filter(origin==origin_1) %.%
  select(depdelay, month, year, origin) %.%
  arrange(random())
explain(origin.1)

n1=as.integer(ns_02[1])
sample= collect(head(origin.1, n=n1))
setwd("/home/zhuob/Project2-Bigdata/dataset/")

write.csv(sample, file="combinedSample.csv", row.names=FALSE)



for(i in 2:length(ns_02)){
  airport=toString(origins_02[i])
  origin = flights %.%
    filter(origin==airport) %.%
    select(depdelay, month, year, origin) %.%
    arrange(random())
  explain(origin)
  
  n1 = as.integer(ns_02[i])
  sample2=collect(head(origin, n=n1))
  
  write.csv(sample2, file="sample2.csv", row.names=FALSE)
  system("cat sample2.csv | sed '1 d' >> combinedSample.csv")
  system("rm sample2.csv")
}

dddd <- read.csv("combinedSample.csv", header=T)

sample_data = data.frame(samples, col.names=origins_02)
