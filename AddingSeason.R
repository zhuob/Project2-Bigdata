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
  if(month > 3 && month < 6){
    season = "Spring"
  }
  else if(month >5 && month < 9){
    season = "Summer"
  }
  else if(month > 8 && month < 12){
    season ="Fall"
  }
  else {
    season = season
  }
  return(season) 
}

Depart.Delay_Seasons <- Depart.Delay %.%
  mutate(Season = Seasons(Month)) %.%
  group_by(Season)
