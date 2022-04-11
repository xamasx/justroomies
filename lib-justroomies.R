#-------------------------------------------------------------------
# App requirements
#-------------------------------------------------------------------
require(dplyr)
require(ggplot2)
require(shiny)
require(shinythemes)


#-------------------------------------------------------------------
# UDFs 
#-------------------------------------------------------------------
deliverRawContribution <- function(aFraction, aVectorOfIncomes) {
  
  sum(aFraction*aVectorOfIncomes)
  
}

deliverIncomeTibb <- function(aRoomieVector, anIncomeVector) {
  
  numberRoomies <- length(aRoomieVector)
  incomeTibb <- tibble(roomie = rep(NA, 10), income = rep(NA, 10))
  roomies <- c(aRoomieVector, rep(NA, 10 - numberRoomies))
  incomes <- c(anIncomeVector, rep(NA, 10 - numberRoomies))
  
  incomeTibb %>%
    mutate(roomie = roomies, income = incomes) %>%
    filter(!is.na(roomie))
}



addRawContribution <- function(anIncomeTibb, aTotalRent, aBareMinimum) {
  
  contributionFunction <- function(x) deliverRawContribution(x, anIncomeTibb$income) - aTotalRent
  
  thisFraction <- tryCatch(uniroot(contributionFunction, lower=0.01, upper=100000000)$root,
                           error = function(e) 0)
  
  anIncomeTibb %>%
    mutate(rawContribution = thisFraction * income,
           isLowerThanMin = rawContribution < aBareMinimum)
}

computeJustDistribution <- function(aRoomieVector, 
                                    anIncomeVector, 
                                    aTotalRent, 
                                    aBareMinimum) {
  
  
  subTibb <- deliverIncomeTibb(aRoomieVector, anIncomeVector) %>% 
    addRawContribution(aTotalRent, aBareMinimum)
                      
  if(0 %in% subTibb$rawContribution) {
    return(deliverDummyTibb(mode = "total"))
  }
  
  if(TRUE %in% subTibb$isLowerThanMin) { 
    
    cappedTibb <- subTibb %>%
      filter(isLowerThanMin) %>%
      transmute(roomie, income, adjustedContribution = aBareMinimum)
    
    newTotalRent <- aTotalRent - (nrow(cappedTibb) * aBareMinimum)
    
    if(newTotalRent < 0) {
      return(deliverDummyTibb(mode = "min"))
    }
    
    newSubTibb <- subTibb %>%
      filter(!isLowerThanMin) %>%
      select(roomie, income) %>%
      addRawContribution(newTotalRent, aBareMinimum) %>%
      transmute(roomie, income, adjustedContribution = rawContribution)
    
    summaryTibb <- cappedTibb %>%
      rbind(newSubTibb) %>%
      transmute(roomie, 
                income, 
                adjustedContribution = round(adjustedContribution, digits = 2),
                percentageUsed = round(100 * adjustedContribution/income, digits = 1),
                notification = paste0(roomie, " chips in ", percentageUsed, "% of his/her income"))
    
    return(summaryTibb)
  }
  
  summaryTibb <- subTibb %>%
    transmute(roomie, 
              income, 
              adjustedContribution = round(rawContribution, digits = 2),
              percentageUsed = round(100*adjustedContribution/income, digits = 1),
              percentageOfRent = round(100*adjustedContribution/aTotalRent, digits = 1),
              notification = paste0(roomie, " chips in ", percentageUsed, "% of his/her income"))
  
  return(summaryTibb)
}

deliverDummyTibb <- function(mode = c("min", "total")) {
  if(mode == "min") {
    tibble(msg = c('The bare minimum is too high!'), income = c(0))
  } else {
    tibble(msg = c('The total rent is too low!'), income = c(0))
  }
}


deliverJustDistribution <- function(aRoomieVector, 
                                    anIncomeVector, 
                                    aColoursVector,
                                    aTotalRent, 
                                    aBareMinimum) {
  
  result <- computeJustDistribution(aRoomieVector, anIncomeVector, aTotalRent, aBareMinimum)
  
  if(nrow(result)==1 && result$income==0) {
    
    plot <- ggplot(result, aes(msg, income, label = msg)) +
      geom_point() +
      geom_label(size = 10) +
      labs(x = "", y = "", title = "") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())
    
    return(list(result, plot))
  }
  
  plot <- ggplot(result, aes(roomie, adjustedContribution, label = adjustedContribution)) + 
    geom_bar(stat = "identity", fill = aColoursVector, color = 'black') + 
    coord_flip() +
    theme(panel.background = element_rect(fill = "gray",
                                          colour = "black",
                                          size = 0.5, 
                                          linetype = "solid"),
          plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
          axis.text = element_text(color = "black", size = 16),
          axis.title.x = element_text(color = "black", size = 16),
          axis.title.y = element_text(color = "black", size = 16)
    ) +
    labs(x = "", y = "MUCHes",
         title = "Income-based contributions") +
    geom_text(size = 7, position = position_stack(vjust = 0.5), color = "#F8F8FF")
  
  return(list(result, plot))
}