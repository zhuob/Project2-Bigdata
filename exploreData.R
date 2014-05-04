## http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.81.9924&rep=rep1&type=pdf

prop <- read.csv("PopProp.csv", header=T)
attach(prop)
prop1 <- prop[order(origin),]

head(prop1, 30)
FMN <- filter(flights, origin=="FMN")
BFF <- filter(flights, origin=="BFF")
MKC <- filter(flights, origin=="MKC")
BFI <- filter(flights, origin=="BFI")
OGD <- filter(flights, origin=="OGD")
PIR <- filter(flights, origin=="PIR")

###   # of obs corresponding to the highest delay rate. These numbers are pretty small, 
### So I don't think we should include these airports in our analysis
c(dim(FMN)[1], dim(BFF)[1], dim(MKC)[1], dim(BFI)[1], dim(OGD)[1], dim(PIR)[1])


nobs <- read.csv("FlightNumber.csv", header=T)
attach(nobs)
nobs1 <- nobs[order(origin),]
tail(nobs1, 20)



obs.prop <- cbind(prop1, nobs1)[, -3]
attach(obs.prop)
obs.prop.ordered <- obs.prop[order(Avg_Num.flights),]
## now we see, 9 of the airports have only less than 10 flights per year
head(obs.prop.ordered,20)

##  remove these 9 airports
airports <- obs.prop.ordered[-c(1:9),] 
dim(airports)

##  see if there is relationship between delay rates and # of flights/year
plot( log(airports$Avg_Num.flights),airports$props, pch= 20)
mod1 <- lm(airports$props~ log(airports$Avg_Num.flights))

# a strong linear relationship!  Do we need to redefine "delay"??
summary(mod1)
abline(mod1, col=2)
cor(airports$props, log(airports$Avg_Num.flights))











