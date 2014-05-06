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

Num.Flights_02 <- filter(Sample.size_Num.flights, flights.num > n.02_max)
num_02 = length(Num.Flights_02[,2])
write.csv(Num.Flights_02, file="Airports_to_Sample.csv", row.names=FALSE)

origins_02 = Num.Flights_02[,1]
ns_02 = Num.Flights_02[,7]

origin_1 = toString(origins_02[1])

origin.1 = flights %.%
  filter(origin==origin_1) %.%
  select(depdelay, month, year, origin) %.%
  arrange(random())
explain(origin.1)

sample= collect(head(origin.1, n=as.integer(ns_02[1]))
write.csv(sample, file="combinedSample.csv", row.names=FALSE)

for(i in 2:num_02){
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
}


sample_data = data.frame(samples, col.names=origins_02)

#####################################################################

origin_PMD = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="PMD") %.%
  arrange(random()) 
explain(origin_PMD)

sample_PMD = collect(head(origin_PMD, n=n.02_max))

origin_ANC = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="ANC") %.%
  arrange(random())
explain(origin_ANC)

sample_ANC = collect(head(origin_ANC, n=n.02_max))

origin_ELP = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="ELP") %.%
  arrange(random())
explain(origin_ELP)

sample_ELP = collect(head(origin_ELP, n=n.02_max))

origins_02[4:6]

origin_CAE = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="CAE") %.%
  arrange(random()) 
explain(origin_CAE)

sample_CAE = collect(head(origin_CAE, n=n.02_max))

origin_BNA = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="BNA") %.%
  arrange(random())
explain(origin_BNA)

sample_BNA = collect(head(origin_BNA, n=n.02_max))

origin_IMT = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="IMT") %.%
  arrange(random())
explain(origin_IMT)

sample_IMT = collect(head(origin_IMT, n=n.02_max)) #too small

origins_02[7:9]

origin_SGF = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="SGF") %.%
  arrange(random()) 
explain(origin_SGF)

sample_SGF = collect(head(origin_SGF, n=n.02_max))

origin_HTS = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="HTS") %.%
  arrange(random())
explain(origin_HTS)

sample_HTS = collect(head(origin_HTS, n=n.02_max))

origin_ACV = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="ACV") %.%
  arrange(random())
explain(origin_ACV)

sample_ACV = collect(head(origin_ACV, n=n.02_max))

origins_02[10:12]

origin_LNY = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="LNY") %.%
  arrange(random()) 
explain(origin_LNY)

sample_LNY = collect(head(origin_LNY, n=n.02_max)) #too small

origin_KOA = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="KOA") %.%
  arrange(random())
explain(origin_KOA)

sample_KOA = collect(head(origin_KOA, n=n.02_max))

origin_LMT = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="LMT") %.%
  arrange(random())
explain(origin_LMT)

sample_LMT = collect(head(origin_LMT, n=n.02_max))
  
write.csv(sample_LMT, file="LMT_Sample.csv")
write.csv(sample_PMD, file="PMD_Sample.csv")
write.csv(sample_SGF, file="SGF_Sample.csv")
write.csv(sample_KOA, file="KOA_Sample.csv")
write.csv(sample_HTS, file="HTS_Sample.csv")
write.csv(sample_ELP, file="ELP_Sample.csv")
write.csv(sample_CAE, file="CAE_Sample.csv")
write.csv(sample_BNA, file="BNA_Sample.csv")
write.csv(sample_ANC, file="ANC_Sample.csv")
write.csv(sample_ACV, file="ACV_Sample.csv")

origins_02[13:15]

origin_CID = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="CID") %.%
  arrange(random()) 
explain(origin_CID)

sample_CID = collect(head(origin_CID, n=n.02_max)) 
write.csv(sample_CID, file="CID_Sample.csv")

origin_CRP = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="CRP") %.%
  arrange(random())
explain(origin_CRP)

sample_CRP = collect(head(origin_CRP, n=n.02_max))
write.csv(sample_CRP, file="CRP_Sample.csv")

origin_CHA = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="CHA") %.%
  arrange(random())
explain(origin_CHA)

sample_CHA = collect(head(origin_CHA, n=n.02_max))
write.csv(sample_CHA, file="CHA_Sample.csv")

origins_02[16:18]

origin_BET = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="BET") %.%
  arrange(random()) 
explain(origin_BET)

sample_BET = collect(head(origin_BET, n=n.02_max)) 
write.csv(sample_BET, file="BET_Sample.csv")

origin_MFE = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="MFE") %.%
  arrange(random())
explain(origin_MFE)

sample_MFE = collect(head(origin_MFE, n=n.02_max))
write.csv(sample_MFE, file="MFE_Sample.csv")

origin_SMF = flights %.%
  select(depdelay, origin, month, year) %.%
  filter(origin=="SMF") %.%
  arrange(random())
explain(origin_SMF)

sample_SMF = collect(head(origin_SMF, n=n.02_max))
write.csv(sample_SMF, file="SMF_Sample.csv")
