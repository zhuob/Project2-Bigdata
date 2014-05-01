Prop.Delay <- Depart.Delay %.%
  mutate(delayed = as.integer(depdelay > 0)) %.%
  summarise(props = mean(delayed)) %.%
  arrange(desc(props))
  
explain(Prop.Delay)
head(Prop.Delay)
