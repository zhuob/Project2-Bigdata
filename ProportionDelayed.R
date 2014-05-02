Depart.Delay1 <- flights %.% 
  select(depdelay, origin) %.%
  group_by(origin)
explain(Depart.Delay1)

Prop.Delay <- Depart.Delay1 %.%
  summarise(props = mean(as.integer(depdelay > 0)))
explain(Prop.Delay)
  
Prop.Delay_Data <- collect(Prop.Delay)
write.table(Prop.Delay_Data, file="PopProp.csv", sep=",", col.names=TRUE)

arrange(Prop.Delay_Data, desc(props))
