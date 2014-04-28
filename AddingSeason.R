#First we only need, (Year, Day, Month, Origin, and DepDelay)
#Then, we need it organized by the Origin (Airport), the Year,
#the Month, the Day of the  Month. 
Depart.Delay <- flights %.% 
  select(Year, Month, DayOfWeek, DepDelay) %.%
  group_by(Origin)

#We can then add a column, which an be for season. 
#(Spring, Summer, Fall, Winter)
Seasons <- function(months){
  season = "Winter"
  if(months > 3 && months < 6){
    season = "Spring"
  }
  else if(months >5 && months < 9){
    season = "Summer"
  }
  else if(months > 8 && months < 12){
    season ="Fall"
  }
  else {
    season = season
  }
  return(season) 
}

#This may need to be modified, since I don't think that
#my function can handel a vector
Depart.Delay_Seasons <- Depart.Delay %.%
  mutate(Season = Seasons(Month)) %.%
  group_by(Season)

#It may need to be used this way
Depart.Delay_Seasons <- Depart.Delay %.%
  mutate(Season = lapply(Month, Seasons)) %.%
  group_by(Season)
