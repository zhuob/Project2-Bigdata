library(dplyr)
library(DBI)
library(RPostgreSQL)

endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime",host = endpoint, port = 5432, user = user, password= password)
flights <- tbl(ontime, "flights")
as.tbl(head(flights))
head(flights)
tbl_df(head(flights))
