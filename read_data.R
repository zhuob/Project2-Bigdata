##  Question to be asked

##------------------------------------------------------------------------------------
# Are there any airports with flight departure delays that are systematically worse  -
# or better than other airports? Be sure to consider seasonal fluctuations that might -
# be due to differential whether patterns, using 25 years of flight data for the 
# entire country.


library(dplyr)

endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", host = endpoint, port = 5432,
                      user = user, password = password)

flight = tbl(ontime, "flights")
as.tbl(head(flights))

