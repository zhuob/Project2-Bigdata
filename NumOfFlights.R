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
