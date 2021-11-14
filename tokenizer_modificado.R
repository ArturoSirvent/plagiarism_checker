#Miguel Hortelano 2/11/2021#
library(tidyr)
library(tidytext)
library(dplyr)
library(ngram)

#### funciones ####
# Devuelve el texto introducido en forma de lista de Ngramas 
agrupa <-function(text){
  temp <-unlist(text)
  temptemp <- temp[temp != ""]
  
  temp2 <- temp[1]
  for(i in 2:length(temp)){
    temp2 <- paste(temp2,temp[i])
  }
  temp2 <- gsub('[[:punct:] ]+',' ',temp2)%>%
    tolower()
  #%>%
  #  strsplit(split="\\s")
  
  temp <- tibble(word = unlist(temp2))%>%
    unnest_tokens(trigram, word,token = "ngrams",n=3)#cambiar el número para tamaño del ngrama
}

# Cuenta las veces que se repiten las palabras entre dos textos
compara <- function(original, sospechoso){
  
  coinc <- c()
  coinc2 <- c()
  
  or <- tibble(trigram = unlist(original))
  sus <- tibble(trigram = unlist(unlist(sospechoso)))
  
  sus <- separate(sus,trigram,c("P1","P2","P3"),sep=" ")
  or <- separate(or,trigram,c("P1","P2","P3"),sep=" ")
  
  for( i in 1:length(sus$P1)){#aquí es donde hay que poner un modelo de verdad, una distancia o métrica
    count <- 0;
    count2 <- 0
    sus_str <- c(sus[i,]$P1,sus[i,]$P2,sus[i,]$P3)
    
    for( j in 1:length(or$P1)){
      or_str <- c(or[j,]$P1,or[j,]$P2,or[j,]$P3)
      count <- count + all(or_str == sus_str)
      count2 <- count2 + sum(or_str==sus_str)
    }
    
    coinc <- append(coinc,count)     #simplemente un vector con el número de veces que se ha repetido cada palabra del ngrama
    coinc2 <- append(coinc2,count2)
    score <- sum(coinc)/length(sus$P1)
    salida <- c(score,coinc2)
  }
  return(salida)
}

compara_djabbard <- function(original, sospechoso){
  
  or <- unlist(original)
  sus <- unlist(sospechoso)
  
  dj <- length(intersect(sus,or))/length(union(sus,or))
  return(dj)
}

#### Limpieza de los datos ####
load(file = "corpus_as_dataframe.Rda")
Ta <- corpus[corpus$Task=="a",]


#La columna texto ahora está en forma de ngramas
Ta$Text <- lapply(Ta$Text,FUN = agrupa)


#### comparamos Ngrams ###
Original <- unlist(Ta$Text[20])

Ta$Djaccard <- lapply(Ta$Text,FUN = compara_djabbard,original = Original)
aux <- lapply(Ta$Text,FUN = compara,original = Original) #obtenemos lista con número de concidenca por ngrama
for (i in 1:length(Ta$Text)){
  Ta$score[i] <- aux[[i]][1]
  Ta$Coincount[i] <- list(aux[[i]][2: length(aux[[i]])])
}
View(Ta)

# visualizamos pasando los vectores de coincidencias a matrices y haciendo heatmap
Ta$Ccmatrix <- lapply(Ta$Coincount, matrix, ncol = 20)

sapply(Ta$Ccmatrix,heatmap)
heatmap(Ta$Ccmatrix[[2]],Rowv=NA,Colv=NA)



# tabla resumen

#install.packages("kableExtra")
library("kableExtra")

variables <- c("Task","Category","djaccard","containment")
tabla_resumen <- Ta[,variables]
kable(tabla_resumen)
