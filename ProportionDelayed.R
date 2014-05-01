Prop.Delay <- Depart.Delay %.%
  summarise(props = mean(as.integer(depdelay > 0)) %.%
  arrange(desc(props))
  
explain(Prop.Delay)
head(Prop.Delay)
