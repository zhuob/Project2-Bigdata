Depart.Delay1 <- flights %.% 
  select(depdelay, origin) %.%
  group_by(origin)
explain(Depart.Delay1)

Prop.Delay <- Depart.Delay1 %.%
  summarise(props = mean(as.integer(depdelay > 0)) %.%
  arrange(desc(props))
  
explain(Prop.Delay)
head(Prop.Delay)
