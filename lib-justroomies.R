#-------------------------------------------------------------------------------
# App requirements
#-------------------------------------------------------------------------------
require(dplyr)
require(ggplot2)
require(shiny)
require(shinythemes)


#-------------------------------------------------------------------------------
# UDFs 
#-------------------------------------------------------------------------------
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
                                    aColourVector,
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
      buildSummary(aTotalRent, aColourVector)
    
    return(summaryTibb)
  }
  
  summaryTibb <- subTibb %>%
    buildSummary(aTotalRent, aColourVector)
  
  return(summaryTibb)
}

buildSummary <- function(aSubTibble, aTotalRent, aColourVector) {
  
  aSubTibble %>%
    transmute(roomie, 
              income, 
              adjustedContribution = round(rawContribution, digits = 2),
              percentageUsed = round(100*adjustedContribution/income, digits = 1),
              percentageOfRent = round(100*adjustedContribution/aTotalRent, digits = 1),
              percentageLabel = paste0(percentageOfRent, ' %'),
              notification = paste0(roomie, " chips in ", percentageUsed, "% of his/her income"),
              colour = aColourVector)
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
                                    aColourVector,
                                    aTotalRent, 
                                    aBareMinimum,
                                    aLanguage) {
  
  result <- computeJustDistribution(aRoomieVector, anIncomeVector, aColourVector, aTotalRent, aBareMinimum)
  
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
    geom_bar(stat = "identity", fill = result$colour, color = 'black') + 
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
         title = translatePlotTitle(aLanguage)) +
    geom_text(size = 7, position = position_stack(vjust = 0.5), color = "#F8F8FF")
  
  return(list(result, plot))
}

isSingleString <- function(input) {
  is.character(input) & length(input) == 1
}

translatePlotTitle <- function(aLanguage) {
  if(isSingleString(aLanguage)) {
    switch(aLanguage,
           "en" = "Income-based contributions!",
           "es" = "Contribuciones ajustadas al ingreso",
           "de" = "Einkommensbezogene Beiträge",
           "fr" = "Contributions adjustées au revenu",
           "nl" = "Inkomensgerelateerde bijdragen")
  } else {
    "Ok, thanks!"
  }
}

translateOK <- function(aLanguage) {
  if(isSingleString(aLanguage)) {
    switch(aLanguage,
           "en" = "Ok, thanks!",
           "es" = "Ok, gracias",
           "de" = "Alles klar, danke!",
           "fr" = "D'ac, merci",
           "nl" = "Ok, bedankt")
  } else {
    "Ok, thanks!"
  }
}

deliverPieChartTitle <- function(aLanguage) {
  if(isSingleString(aLanguage)) {
    switch(aLanguage,
           "en" = "Fraction of total rent per person",
           "es" = "Fracción del total por persona",
           "de" = "Bruchteil der Gesamtmiete per Mitbewohner",
           "fr" = "Fraction du total par personne",
           "nl" = "Deel van de huur per persoon")
  } else {
    "Fraction of total rent per roomie"
  }
} 


translateHowDoesItWork <- function(aLanguage) {
  if(isSingleString(aLanguage)) {
    switch(aLanguage,
           "en" = "How does it work?",
           "es" = "¿Cómo funciona?",
           "de" = "Wie funktioniert's?",
           "fr" = "Comment ça marche?",
           "nl" = "Hoe werkt het?")
  } else {
    "How does it work?"
  }
}

#-------------------------------------------------------------------------------
# HTML
#-------------------------------------------------------------------------------

translateExplanation <- function(aLanguage) {
  if(isSingleString(aLanguage)) {
    switch(aLanguage,
           "en" = "<p> When I lived in Berlin I shared my flat with three other people. 
      We all had different incomes. Our rooms had all diffeent surfaces. 
      One of my roomies said, 'Well, if you want to be fair, you'd need to split 
      the costs according to the surface each one of us lives in'. And we did just that. 
      But, deep down, it did not feel right. And when my girlfirend at the time
      started struggling, I was convinced there had to be another way.<br>
      
      So, for those who currently live in a shared flat and would like to split 
      the costs according to what each of you earn, here is a simple calculator
      that can help you do just that. <br><br>
      
      Under <b>Financials</b> set the total amount that you and your roomies would like
      to split in a <u>M</u>onetary <u>U</u>nit of your <u>CH</u>oice (MUCH). 
      It could be USD, EUR or any other currency. Under <b>Income per roommate</b> 
      choose the number of people who share the flat. <br><br>
      
      A number of hypothetical names will appear. Each one of your roomies should pick a name
      and type in their corresponding monthly income into the box - if one of your roomies 
      is a freelancer, this will obviously vary month to month. <br>
      Having all entered how much MUCHes you earned this moth, the plot will 
      automatically update and you will be able to see how much MUCHes each one of 
      you ought to pay this month. <br><br>
      
      Finally, in cases where there is a stark imbalance among roomies, you may 
      want to consider agreeing upon the bare minimum you will be chipping in. 
      This way, you ensure that any given month where, for example, Maurice 
      happened to struggle does not take a stark toll on high-earner Lina. </p>",
           
           "es" = "<p> A mi paso por Berlín, compartí mi departamento con tres otros
           roomies. Todos con ingresos diferentes. Todos viviendo en cuartos de
           diferentes tamaños. Uno de mis roomies propuso que dividieramos los
           costos de la renta de acuerdo a la superficie que cada quien habitaba,
           después de todo 'queríamos ser justos'. Así que lo hicimos. No obstante,
           a mí en el fondo no me convencía, y fue cuando mi novia de ese entonces
           empezó a entrar en problemas que me empeñé en encontrar alguna alternativa.<br>
           
           Así que para ti que compartes tu espacio con alguien más y que quieres
           distribuir los costos de acuerdo a los ingresos respectivos, he aquí
           una calculadora que hace justo eso. <br><br>
           
           Bajo el apartado de <b>Finanzas</b> ingresa el monto que tú y tus roomies
           tienen que dividir en cualquier <u>U</u>nidad <u>M</u>onetaria de tu
           <u>P</u>referencia (MUCH, por sus siglas en inglés). Esto serán dólares,
           euros o lo que sea. Bajo el apartado de <b>Ingreso por compañero</b> elige
           el número de roomies que son en el departamento.
           
           Verás aparecer una lista de roomies hipotéticos. Cada uno elija un nombre
           e ingrese el monto del ingreso correspondiente al mes actual. Se alguien
           es freelancer, el ingreso variará mes con mes. <br>
           Una vez todos han escrito la cantidad de MUCHes mensuales que ganan, verás
           que la gráfica se actualiza automáticamente e indica cuántos MUCHes cada
           cabeza habrá de pagar este mes.<br><br>
           
           Como nota final: en caso de un fuerte desequilibrio en los ingresos,
           considérese acordar un monto mínimo con el que contribuir independientemente
           de cuánto se haya ganado. Así, aseguras que si, por ejemplo, Maurice un buen
           día tiene un resbalón en sus finanzas, no recaiga todo sobre Lina, que gana
           bien. </p>",
           "de" = "Wie funktioniert's?",
           "fr" = "Comment ça marche?",
           "nl" = "Hoe werkt het?")
  } else {
    "<p> When I lived in Berlin I shared my flat with three other people. 
      We all had different incomes. Our rooms had all diffeent surfaces. 
      One of my roomies said, 'Well, if you want to be fair, you'd need to split 
      the costs according to the surface each one of us lives in'. And we did just that. 
      But, deep down, it did not feel right. And when my girlfirend at the time
      started struggling, I was convinced there had to be another way.<br>
      
      So, for those who currently live in a shared flat and would like to split 
      the costs according to what each of you earn, here is a simple calculator
      that can help you do just that. <br><br>
      
      Under <b>Financials</b> set the total amount that you and your roomies would like
      to split in a <u>M</u>onetary <u>U</u>nit of your <u>CH</u>oice (MUCH). 
      It could be USD, EUR or any other currency. Under <b>Income per roommate</b> 
      choose the number of people who share the flat. <br><br>
      
      A number of hypothetical names will appear. Each one of your roomies should pick a name
      and type in their corresponding monthly income into the box - if one of your roomies 
      is a freelancer, this will obviously vary month to month. <br>
      Having all entered how much MUCHes you earned this moth, the plot will 
      automatically update and you will be able to see how much MUCHes each one of 
      you ought to pay this month. <br><br>
      
      Finally, in cases where there is a stark imbalance among roomies, you may 
      want to consider agreeing upon the bare minimum you will be chipping in. 
      This way, you ensure that any given month where, for example, Maurice 
      happened to struggle does not take a stark toll on high-earner Lina. </p>"
  }
  
}