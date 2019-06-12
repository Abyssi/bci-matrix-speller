#----------------Predicting Soccer Match Outcome----------------
#                  Analisi Esplorativa dei dati
#---------------------------------------------------------------
library("ggplot2")#package per realizzare grafici
library("dplyr")#package per la manipolazione dati
source("outliers_detection.R") #funzione per individuare gli outliers


data_understanding <- function(my_data, plot_folder){
  set.seed(123)
  
  data_rows <- nrow(my_data)
  
  #Analisi degli attributi:
  #Tipo
  #Principali grandezze statistiche
  #creazione di un boxplot per ogni atributo numerico raggruppando i valori in base 
  #alla classe
  #Valori anomali per ogni attributo numerico
  type_attribute <- lapply(my_data, class)
  summary_attribute <- lapply(my_data, summary)
  
  #La funzione filter() di dplyr serve a selezionare un gruppo di casi in un dataset 
  #in questo caso gli gli attributi numerici
  numeric_attribute <- names(Filter(is.numeric,my_data))
  
  my_plot <- function(varName) {
    
      ggplot(my_data, aes_string("risultato",varName)) +
      geom_boxplot() +
      ggtitle(paste("Distribuzione statistica dell'attributo:",varName,sep=" "))
      theme_bw()
      
      ggsave(paste(plot_folder, paste(varName,"png", sep="."),sep=""))
    
    
  }
  
  lapply(numeric_attribute,my_plot)
  
  numeric_attribute <- names(Filter(is.numeric,my_data))
  outlier_list <- lapply(numeric_attribute,outliers_detection,dataset=my_data)
  names(outlier_list) <- numeric_attribute 
  
  
  
  #Analisi delle istanze
  #duplicati: numero e percentuale
  #missing value: numero complessivo e percentuale
  
  n_duplicate <- sum(duplicated(my_data))
  perc_duplicate <- round(n_duplicate/data_rows,4)
  
  n_complete <- sum(complete.cases(my_data))
  n_incomplete <- nrow(my_data)-n_complete
  perc_incomplete <- 1-(round((n_complete/data_rows),4))
  
  
  #inseriamo questi parametri in una matrice
  aggregate_information <- rbind(n_duplicate,perc_duplicate,n_incomplete,perc_incomplete)
  rownames(aggregate_information) <- c("Numero di duplicati","percentuale di duplicati","Numero istanze incomplete","Percentuale istanze incomplete")

  
  
  
  output <- list(aggregate_information,type_attribute,summary_attribute,outlier_list)
  names(output) <- c("Analisi istanze","Attribute class","Attribute Summary","Lista degli outliers")
  return(output)
  
}



