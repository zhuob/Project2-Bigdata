Depart.Delay1 <- flights %.% 
  select(depdelay, origin) %.%
  group_by(origin)
explain(Depart.Delay1)

Prop.Delay <- Depart.Delay1 %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Prop.Delay)
  
Prop.Delay_Data <- collect(Prop.Delay)
write.table(Prop.Delay_Data, file="PopPropMean.csv", sep=",", 
            col.names=TRUE)
Prop.Delay_Data = read.csv("PopPropMean.csv", header=TRUE)

arrange(Prop.Delay_Data, desc(props))

##########################################################
# Accounting for Season by looking at Seasonal Data Sets #
##########################################################

#Winter Considered Dec., Jan., Feb., March
###########################################
#Creating the Data Set
Winter.Delay <- flights %.%
  filter(month==12L | month < 4L) %.%
  select(depdelay, origin) %.%
  group_by(origin)
explain(Winter.Delay)

#Creating the Proportions for Winter
Prop.Delay_Winter <- Winter.Delay %.%
  summarise(win.props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Prop.Delay_Winter)

#Writing a CSV for Winter
Winter.Delay_Data <- collect(Prop.Delay_Winter)
write.table(Winter.Delay_Data, file="WinterPop2.csv", sep=",", 
            col.names=TRUE)

#Arranging them to see which is the highest proportion
Winter.Delay_Data = read.csv("WinterPop2.csv", header=TRUE)
arrange(Winter.Delay_Data, desc(win.props))


#Spring Considered April and May
################################
#Creating the Data set for Spring
Spring.Delay <- flights %.%
  filter(month > 3L & month < 6L) %.%
  select(depdelay, origin) %.%
  group_by(origin)
explain(Spring.Delay)

#Creating the Proportions for Spring
Prop.Delay_Spring <- Spring.Delay %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Prop.Delay_Spring)

#Writing a CSV for Spring
Spring.Delay_Data <- collect(Prop.Delay_Spring)
write.table(Spring.Delay_Data, file="SpringPop2.csv", sep=",", 
            col.names=TRUE)

#Arranging them to see which is the highest proportion
Spring.Delay_Data <- read.csv("SpringPop2.csv", header=TRUE)
arrange(Spring.Delay_Data, desc(props))

#Summer is considered June, July, and August
############################################
#Creating the Data set for Spring
Summer.Delay <- flights %.%
  filter(month > 5L & month < 9L) %.%
  select(depdelay, origin) %.%
  group_by(origin)
explain(Summer.Delay)

#Creating the Proportions for Summer
Prop.Delay_Summer <- Summer.Delay %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Prop.Delay_Summer)

#Writing a CSV for Summer
Summer.Delay_Data <- collect(Prop.Delay_Summer)
write.table(Summer.Delay_Data, file="SummerPop2.csv", sep=",", 
            col.names=TRUE)

#Arranging them to see which is the highest proportion
Summer.Delay_Data <- read.csv("SummerPop2.csv", header=TRUE)
arrange(Summer.Delay_Data, desc(props))


#Fall is considered to be Sept., Oct., Nov.
############################################
#Creating the data set for Fall
Fall.Delay <- flights %.%
  filter(month > 8L & month < 12L) %.%
  select(depdelay, origin) %.%
  group_by(origin)
explain(Fall.Delay)

#Creating the Proportions for Fall
Prop.Delay_Fall <- Fall.Delay %.%
  summarise(props = mean(as.integer(depdelay > 0)), 
            mean.wait=mean(depdelay))
explain(Prop.Delay_Fall)

#Writing a CSV for Fall
Fall.Delay_Data <- collect(Prop.Delay_Fall)
write.table(Fall.Delay_Data, file="FallPop2.csv", sep=",", 
            col.names=TRUE)

#Arranging them to see which is the highest proportion
Fall.Delay_Data <- read.csv("FallPop2.csv", header=TRUE)
arrange(Fall.Delay_Data, desc(props))

########################################################
# Considering taking Season into account by Taking the #
# mean of the entire year's median delay time          #
########################################################

#Creating a data set that has one mean value for each year
#for each of the different Airports
##########################################################
Year_Mean.Delay <- flights %.% 
  select(depdelay, origin, year) %.%
  group_by(origin, year) %.%
  summarise(Mean_Delay = mean(depdelay))
explain(Year_Mean.Delay)

Origin_Year.Delay <- Year_Mean.Delay %.%
  summarise(Year_props=mean(as.integer(Mean_Delay > 0)), 
            Year_mean = mean(Mean_Delay))
explain(Origin_Year.Delay)

Origin.Year_Data <- collect(Origin_Year.Delay)
write.table(Origin.Year_Data, file="YearlyPop.csv", sep=",",
            col.names=TRUE)

Origin.Year_Data <- read.csv("YearlyPop.csv", header=TRUE)
arrange(Origin.Year_Data, desc(Year_mean))

