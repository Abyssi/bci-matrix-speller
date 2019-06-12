#----------------Predicting Soccer Match Outcome----------------
#     Obiettivo: Predire l'esito finale di alcune partite
#     del campionato di calcio di serie A, terminate con
#     un pareggio alla fine del primo tempo di gioco
#     utilizzando statistiche descrittive collezionate durante
#     i primi 45 minuti.
#---------------------------------------------------------------
library("dplyr")
library("LiblineaR")
library("e1071")
library(CORElearn)
source("data_understanding.R")
source("data_preparation.R")
source("split.R")
source("data_normalization.R")
source("choosing_classifier.R")
source("linear_SVM.R")
source("gaussian_SVM.R")
source("polinomial_SVM.R")
source("setting_linear_SVM.R")
source("c_linear_SVM.R")
source("opt_classifier.R")

#Caricamento Dati
df <- read.csv("./dataset/raw/DatasetCalcio.csv",header = T, stringsAsFactors = F)

#Crezione di una o più cartelle in cui salvare il risultato di alcune operazioni
plot_dir <- paste(getwd(), "grafici/", sep="/" )
dir.create(plot_dir)

data_summary <- data_understanding(my_data = df, plot_folder = plot_dir)

outlier <- data_summary$`Lista degli outliers`

#Pulizia dei dati
cleaned_data <- data_preparation(my_data = df)
new_summary_attribute <- lapply(cleaned_data,summary)
#dal momento che le classi non sono perfettamente bilanciate
#? preferibile utilizzare uno split training-test bilanciato 
#my_split <- split(cleaned_data, cleaned_data$label)
my_split <- split(cleaned_data)
normalized_split <- data_normalization(my_split)
#supponiamo di aver individuato (mediante ricerca in letteratura tre classificatori adatti al nostro 
#problema). Utilizziamo quindi la cross-validation sui dati di training per scegliere il/i migliori.
cross_validation_output <- choosing_classifier(normalized_split) 
cross_validation_output
#il classificatore che sembra risultare pi? robusto ? una Linear SVM (nonostante la SVM con kernel 
#polinomiale presenti un accuratezza media pi? alta il TPR relativo alla classe B ? zero)
#Una volta scelto il classificatore possiamo procedere con il tuning dei parametri modificando ad esempio
#la c del kernel lineare oppure utilizzando un vettore di pesi per penalizzare in modo diverso gli errori rispetto alle tre classi
c_setting <-setting_linear_SVM(normalized_split)
c_setting
#A seguito della modifica della c decidiamo di utilizzare c=0.1 per le successive prove
#una volta ottimizzate le performance del classificatore si pu? procedere al check dei risultati 
#sul test set che fino a questo punto non era mai stato utilizzato
test_result <- opt_classifier(normalized_split$scaled_training,normalized_split$label_train,normalized_split$scaled_test,normalized_split$label_test)
test_result

#A fronte dei risultati ottenuti proviamo a lavorare sul sottoinsieme degli attributi a nostra disposizione
#La valutazione degli attributi deve essere realizzata sui dati di training
ReliefFNumNeighbour<- 70 #suggestion: if ReliefFType == ReliefFequalK  than 10, else 70. 
ReliefFType<- "ReliefFexpRank"
NumIterations<-5

FS<-attrEval(label ~ ., my_split$training_set, estimator=ReliefFType, 
             kNearestExpRank=ReliefFNumNeighbour,
            ReliefIterations=NumIterations)
FS_ordered<- sort(FS, decreasing = T)
#Scartiamo tutti gli attributi con indice di rilevanza minore di zero
feature_subset <- c(names(FS_ordered)[which(FS_ordered>=0)],"label")

#Selezioniamo le feature più rilevanti sia dal training che dal test

fs_split <- lapply(my_split, function(x){
  x <- x[,feature_subset]
})
normalized_fs_split <- data_normalization(fs_split)
new_test <- opt_classifier(normalized_fs_split$scaled_training,normalized_fs_split$label_train,normalized_fs_split$scaled_test,normalized_fs_split$label_test)



#
#     A B C D E F  
#     G H I J K L
#     M N O P Q R
#     S T U V W Y
#     Y Z 1 2 3 4
#     5 6 7 8 9 0
#




