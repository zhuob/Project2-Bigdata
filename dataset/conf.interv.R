
## alpha, the significant level you want to achieve
## season you need to specify, if you 

conf.interv <- function(alpha, season="all")
{
  sample <- read.csv("/home/zhuob/Project2-Bigdata/dataset/combinedSample.csv",header=T)
  
  if (season=="all"){
  s<- sample
  p <-  summarise(group_by(s, origin),prob = mean(depdelay> 0, na.rm=T))
  n <- s%.%
    group_by(origin)%.%
    summarise(s = n())
  sd <- sqrt(p[,2]*(1-p[,2])/n[,2])
  Upper <- p[, 2] + abs(qnorm( (1-alpha) /2))*sd
  Lower <- p[, 2] - abs(qnorm( (1-alpha) /2))*sd
  }

  
  else if (season=="spring")
  {
    s <- sample[which(sample$month %in% c(4, 5)), ]
    p <-  summarise(group_by(s, origin),prob = mean(depdelay> 0, na.rm=T))
    n <- spring.s%.%
      group_by(origin)%.%
      summarise(s = n())
    sd <- sqrt(p[,2]*(1-p[,2])/n[,2])
    Upper <- p[, 2] + abs(qnorm( (1-alpha) /2))*sd
    Lower <- p[, 2] - abs(qnorm( (1-alpha) /2))*sd
    
  }
  
  
  else if (season=="summer")
  {
    s <- sample[which(sample$month %in% c(6, 7, 8)), ]
    p <-  summarise(group_by(s, origin),prob = mean(depdelay> 0, na.rm=T))
    n <- s%.%
      group_by(origin)%.%
      summarise(s = n())
    sd <- sqrt(p[,2]*(1-p[,2])/n[,2])
    Upper <- p[, 2] + abs(qnorm( (1-alpha) /2))*sd
    Lower <- p[, 2] - abs(qnorm( (1-alpha) /2))*sd
    
  }
  
  
  else if (season=="fall")
  {
    s <- sample[which(sample$month %in% c(9, 10, 11)), ]
    p <-  summarise(group_by(s, origin),prob = mean(depdelay> 0, na.rm=T))
    n <- s%.%
      group_by(origin)%.%
      summarise(s = n())
    sd <- sqrt(p[,2]*(1-p[,2])/n[,2])
    Upper <- p[, 2] + abs(qnorm( (1-alpha) /2))*sd
    Lower <- p[, 2] - abs(qnorm( (1-alpha) /2))*sd
    
  }
  
  
  else if (season=="winter")
  {
    s <- sample[which(sample$month %in% c(12, 1, 2, 3)), ]
    p <-  summarise(group_by(s, origin),prob = mean(depdelay> 0, na.rm=T))
    n <- s%.%
      group_by(origin)%.%
      summarise(s = n())
    sd <- sqrt(p[,2]*(1-p[,2])/n[,2])
    Upper <- p[, 2] + abs(qnorm( (1-alpha) /2))*sd
    Lower <- p[, 2] - abs(qnorm( (1-alpha) /2))*sd
    
  }
  
  ind <- which(Lower<0)
  Lower[ind] <- 0
  ind2 <- which(Upper>1)
  Upper[ind2] <- 1
  
  dat <- cbind(p, sd,Lower, Upper, n[, 2])
  
  colnames(dat)[c(4, 5, 6)] <- c(paste("Lower", alpha, sep="")
                                 , paste("Upper", alpha, sep=""), "samp.size")
  return (dat)
  
}