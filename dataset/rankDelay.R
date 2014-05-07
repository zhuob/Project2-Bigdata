
## the following function will give you the rank as specified
rankDelay <- function(data, type ="pop", order = "ascending", num=10)
{
  if (type == "pop"){
    
    if (order == "ascending"){
      spring <- head(data[order(data[, 2]),c(1,2)], num)
      summer <-  head(data[order(data[, 3]),c(1,3)], num)
      fall <-  head(data[order(data[, 4]),c(1,4)], num)
      winter <-  head(data[order(data[, 5]),c(1,5)], num)
    }
    
    else if(order == "descending"){
      spring <- head(data[order(-data[, 2]),c(1,2)], num)
      summer <-  head(data[order(-data[, 3]),c(1,3)], num)
      fall <-  head(data[order(-data[, 4]),c(1,4)], num)
      winter <-  head(data[order(-data[, 5]),c(1,5)], num)  
    }
    
  }
  
  else if(type =="samp"){
    
    if (order == "ascending"){
      spring <- head(data[order(data[, 6]),c(1,6)], num)
      summer <-  head(data[order(data[, 7]),c(1,7)], num)
      fall <-  head(data[order(data[, 8]),c(1,8)], num)
      winter <-  head(data[order(data[, 9]),c(1,9)], num)
    }
    
    else if(order == "descending"){
      spring <- head(data[order(-data[, 6]),c(1,6)], num)
      summer <-  head(data[order(-data[, 7]),c(1,7)], num)
      fall <-  head(data[order(-data[, 8]),c(1,8)], num)
      winter <-  head(data[order(-data[, 9]),c(1,9)], num)  
    }
    
  }
  
  dat <- list(spr=spring, sum= summer, fal= fall, win= winter)  
  return(dat) 
}