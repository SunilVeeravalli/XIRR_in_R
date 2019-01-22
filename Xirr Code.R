library(tidyverse)

xirr <- function(dataset) {
    
    # creating a function to calculate npv value
    npv <- function(range, dataset){
        for(test.rate in range) {
            
            max.date <- max(dataset$dates)
            
            temp <- dataset %>%
                mutate(npv = amount * ((1 + test.rate/100)^(as.numeric(max.date - dates)/365))) %>%
                select(npv) %>%
                .[1]
            if(sum(dataset$amount) > 0) {
                if(sum(temp) > 0) {
                    min.rate <- test.rate
                    next
                } else {
                    max.rate <- test.rate
                    break
                }
            } else {
                if(sum(temp) < 0) {
                    min.rate <- test.rate
                    next
                } else {
                    max.rate <- test.rate
                    break
                }
            }
        }
        return(list(min.rate = min.rate, max.rate = max.rate))
    }
    
    
    names(dataset) <- c("dates", "amount")

    max.rate <- c()
    min.rate <- c()
    
    if(sum(dataset$amount) > 0) {
        
        range <- seq(from = 0, to = 10000, by = 100)    
        hundreds <- npv(range, dataset)

        range <- seq(from = hundreds$min.rate, to = hundreds$max.rate, by = 10)
        tens <- npv(range, dataset)
    
        range <- seq(from = tens$min.rate, to = tens$max.rate, by = 1)
        ones <- npv(range, dataset)
    
        range <- seq(from = ones$min.rate, to = ones$max.rate, by = 0.01)
        decimals <- npv(range, dataset)
        
        return(paste("XIRR is ", mean(unlist(decimals)), "%", sep = ""))   
        
    } else {
        
        range <- seq(from = 0, to = -10000, by = -100)
        hundreds <- npv(range, dataset)
        
        range <- seq(from = hundreds$min.rate, to = hundreds$max.rate, by = -10)
        tens <- npv(range, dataset)
        
        range <- seq(from = tens$min.rate, to = tens$max.rate, by = -1)
        ones <- npv(range, dataset)
        
        range <- seq(from = ones$min.rate, to = ones$max.rate, by = -0.01)
        decimals <- npv(range, dataset)
        
        return(paste("XIRR is ", mean(unlist(decimals)), "%", sep = "")) 
    }
}





