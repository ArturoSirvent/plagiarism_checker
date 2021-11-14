---
title: "Trabajo Plagio Master"
author: "Ángel Guevara Ros, Arturo  Sirvent Fresneda , Miguel Hortelano Busto , Francisco Olayo González Rodríguez"
date:  "`r Sys.Date()`" 
output:
  html_document:
    echo: yes
    number_sections: yes
    theme: lumen
    toc: yes
  html_notebook:
    echo: yes
    number_sections: yes
    toc: yes
  pdf_document:
    toc: yes
    toc_depth: 3
    number_sections: yes
language:
  label:
    fig: 'Figura '
    tab: 'Tabla '
    eq: 'Ecuación '
    thm: 'Teorema '
    lem: 'Lema '
    def: 'Definición '
    cor: 'Corolario '
    prp: 'Proposición '
    exm: 'Ejemplo '
    exr: 'Ejercicio '
    proof: 'Demostración. '
    remark: 'Nota: '
    solution: 'Solución. '
bibliography: bibliography.bib
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r include=FALSE}
# Especificamos las librerías necesarias en esta lista

packages = c("dplyr", "kableExtra","ngram","readr", "tidytext","tidyr","tm","stringr","caret","knitr")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()

```


# Introducción y motivación

En los últimos años, el plagio y su detección se han posicionado como un problema en crecimiento dentro del ámbito académico. Según la Real Academia Española (RAE), plagiar es la acción de copiar en lo sustancial obras ajenas dándolas como propias. En el caso de textos, plagiar es la acción de atribuirse la autoría de fragmentos o ideas cuya propiedad intelectual no le corresponde al autor. 

Debido al aumento de este problema, han surgido diferentes soluciones por parte del mundo empresarial y del educativo. Entre otras, el desarrollo de sistemas para ayudar a los educadores en la ardua tarea de identificar trabajos plagiados, o incluso el desarrollo de sistemas que son capaces de detectar el plagio automáticamente.

Uno de los problemas de estos sistemas de detección de plagio es la falta de datos para entrenar los modelos ya que no hay muchos repositorios con textos que estén admitidos como plagiados. Esto se debe a la ilegalidad de publicar textos plagiados. Por eso, en este trabajo usaremos un corpus de texto plagiado que ha sido simulado por los investigadores y que está disponible para uso libre. 

# Teoria


En este apartado desarrollamos los conceptos y técnicas necesarias para llevar a cabo más tarde en un caso práctico la construcción de un detector de plagio. Esta teoría esta subdividida en los diferentes pasos a seguir en la práctica para el tratado de los datos y la construcción y evaluación del modelo.

Atendiendo los diferentes tipos de plagio y los métodos usados para su detección, podemos realizar distintas clasificaciones. A continuación se exponen algunas de ellas [@tipos1] [@tipos2]. 
_Nota: También es muy importante diferenciar respecto al objeto de estudio. Si se está analizando plagio en código, las técnicas no serás igual que si estamos tratando con un artículo científico. Nuestro interés se centra en texto plano y no en código._

La clasificación menos relevante para nuestro trabajo sería la que atiende a la intención de quien escribe el texto. Dicha clasificación podría ser [@intencion]:

![Clasificación del pragio según la intención del escritor](diagrama_plagio_intencion.png)

Una de las clasificaciones que se hacen más comúnmente sobre el plagio, son las de plagio intrínseco y el plagio extrínseco.
* Plagio intrínseco: Se habla de plagio intrínseco cuando en un mismo documento o texto, cambia el estilo de redacción y se distinguen diferentes autores. Su nombre viene del hecho que no se requieren de bases de datos ni fuentes externas para la comparación. Como consecuencia, de dicho método solo podremos sacar una sospecha del plagio, pues no contaremos con la fuente del plagio.

* Plagio extrínseco: Donde los recursos originales o fuente del plagio son usados y comparados con el texto a analizar. El plagio no solo puede venir de una copia textual entre documento original y ejemplar (llamado _plagio literal_ o _copia-pega_), si se usan sinónimos y se reescriben las oraciones de manera similar, deberemos atender a la similitud entre ambos documentos (este tipo de plagio es llamado _plagio inteligente_). Esta similitud puede separarse en:
  * Similitud Semántica: Considerando el uso de sinónimos, se busca la similitud de la estructura por **palabras**.
  * Similitud Sintáctica: Se busca una similitud por oraciones, donde se le da relevancia a similitud entre oraciones a nivel sintáctico.
 
![Clasificación del plagio](plagio_tax.png)
 
## Corpus
Un corpus lingüístico es un conjunto de textos variados que cumple la función de representar el uso real de la lengua que se quiere examinar. En el caso del procesamiento de texto es necesario contar con un corpus adecuado al campo y características que se buscan extraer. En nuestro caso hacemos uso del corpus proporcionado por miembros de la universidad de Sheffield [@corpus_source]. Consta de un set de 5 cuestiones que veinte alumnos tuvieron que responder. Los alumnos se dividieron en cuatro grupos que podían responder las preguntas con materiales externos o consultando páginas de Wikipedia, permitiéndose niveles diferentes de copia y referencia. De esta forma tenemos diferentes grados de plagio con el documento original, que van del corta y pega al no uso de la fuente, y una fácil comparación. Además de todas estas respuestas, también se proporciona un índice con el alumno que escribió cada texto, cual era la pregunta original y el nivel de copia (near copy, light revision, heavy revision, non- plagiarism) debidamente indexado. EN resumidas cuentas, nuestro corpus cuenta con 100 textos breves (entre 200 y 300 palabras) con diferentes niveles de plagio, de manera que son lo suficientemente largos para realizar un análisis didáctico sin convertirse en una carga difícil de procesar con nuestros medios.

## Preprocesado   

El preporcesado es el primer contacto con los datos, las primeras tareas de limpieza y formateo de los datos suelen ser por lo general sencillas y en su mayoría genéricas. 

Primero exponemos las funciones básicas para este propósito:
```{r}
#librerías para cargar los datos y preprocesarlos 

texto<-" ¡ Esto es    un texto de prueba para demostrar la   limpieza incicial de los textos! (ááéçñ)"
#pasarlo a minúsculas
texto<-tolower(texto) 
texto
```


```{r}
#eliminar signos de puntuación, carácteres problemáticos
texto<-gsub("[[:punct:]]"," ",texto)
texto<-gsub("(\\n)|(\\t)"," ",texto)
texto

#y comprobar que no hay espacios repetidos y al principio y final del string (str_trim())
texto<-gsub("\\s+"," ",str_trim(texto))
#la función str_squish() de la librería stringr hace exactamente 
#elimilar los espacios en blanco al comienzo y al final del string
texto
```

En el caso de que el idioma tenga acentuación, sería recomendable eliminarla para no tener problemas de formato ni considerar diferentes palabras iguales solo por estar acentuada o no. Esto lo podemos hacer de forma sencilla usando stringi::stri_trans_general()
```{r}

texto<-stringi::stri_trans_general(texto,"Latin-ASCII") 
# este proceso será muy dependiente del idioma del texto
texto
```


Una limpieza más intensiva podría seguir con la eliminación de palabras unión `stop-words` y hasta la reducción de palabras hasta su raíz (término _stemming_ en inglés).Para este propósito hay librerías que implementan funciones para dicho proposito en R, algunas de ellas son `tm` y `tidytext`.  
Sin embargo esta sobre-simplificación puede no ser lo más recomendado en todos los casos. La elección será muy dependiente del algoritmo y del tipo de plagio buscado (ya hemos comentado previamente que hay diferentes tipos de plagio a detectar).

```{r}
#eliminar stop-words con la librería tm

texto2<-removeWords(texto,stopwords("es"))
texto2<-str_squish(texto2)
texto2


```

```{r}
#stemming palabras con la libreria tm
texto2<-unlist(str_split(texto2," "))
texto3<-stemDocument(texto2,language = "es")
texto3
```


## Tokenizado

Una vez importado el corpus y depurado los datos, el siguiente paso es tokemizarlos. Esto consiste en separar en elementos más pequeños el conjunto de datos que tenemos. Hablando de datos de tipo texo, tokemizar podría ser separar en palabras o en frases por ejemplo. La función `strsplit()` nos permitiría hacer variando el argumento de separación, `split`  según nos convenga.

  - `split=" "` un espacio podría servir para separar en palabras. 
  - `split="."` un punto podría servir para separar en frases. 

Sin embargo, en este caso nos va a interesar usar N-gramas, que son subconjuntos del conjunto original que van cogiendo combinaciones de n elementos y dónde el elemento siguiente está formado por los (n-1) elementos del anterior más un elemento nuevo. Por ejemplo, si tenemos el texto: "Hoy hemos quedado en la rusa a las seis". Su división en 3-gramas (o trigramas) sería: 

[Hoy hemos quedado], [hemos quedado en], [quedado en la], [en la rusa],[la rusa a], [rusa a las],[a las seis]. 

En R, se puede hacer fácilmente usando el paquete `ngram`, y más concretamente la función `unnest_tokens()`, que toma como argumentos principales los siguientes: 

  - `tbl` un data frame
  - `output` el nombre de la columna que se va  crear
  - `input` los datos que queremos usar.
  - `token` a qué token queremos convertirlos (en este caso el que más nos interesa es "ngrams")
  - `n` argumento adicional para n-gramas para indicar el tamaño de éstos.
  
Los N-gramas son muy utilizados en aplicaciones de análisis de texto, como por ejemplo en la detección de plagio. La similitud entre dos textos se puede calcular contando el número de N-gramas que tienen en común.


## Metricas y modelos

Las métricas y modelos utilizados a la hora de evaluar los niveles de plagio entre textos, ya sean del mismo autor, o de varios, se deben dividir en varios grupos, dependiendo de la previa tokenización o agrupación de las palabras o letras del corpus.

### Métricas basadas en similitudes de tokenes

Una de las formas más utilizadas a la hora de comparar bloques de texto es la de comparaciones vectoriales, en las que, por ejemplo, cada palabra se convierte en el índice de un vector, y la cantidad de veces que aparece es su escalar. Utilizando este proceso, tambien se puede extrapolar de manera que en vez de contar las palabras, se cuente los pares o trios de palabras, lo que vendria a ser separacion en n-gramas(bigramas o trigramas).

Son bastante eficientes en general, y funcionan bien para textos largos a diferencia de los otros tipos de métricas.

Para esta tokenización, las métricas mas utilizadas, son las de medida de distancia entre un texto y otro, comparando vector a vector.

-Una primera métrica seria la de **Jaccard**.

Esta métrica trata de medir los elementos que comparten ambos vectores.

$D_{jaccard}\left ( X,Y \right )=1- \frac{\sum x_{i}\cap y_{i} }{\sum x_{i}\cup y_{j}}$

En términos generales, consiste en dividir el vocabulario común de los documentos entre el vocabulario de la unión de ambos documentos, y le restamos 1 para que dos vectores sean parecidos cuando estan cerca de 0 y lejanos cuando se acercan a 1.

-La siguiente métrica es muy parecida ya que es la **Jaccard pesada**.
En esta métrica se tiene en cuenta tambien la cantidad potencial de coincidencias.

$D_{wjaccard}\left( X,Y\right )=1- \frac{\sum min\left ( x,y \right ) }{\sum max(x,y)}$

-Métrica de **Sorensen**.

$D_{sorensen}\left ( X,Y \right )=1- \frac{2*\sum x_{i}\cap y_{i} }{\sum x_{j}\cap 1+\sum y_{k}\cap 1}$

Consiste en dividir la cantidad de palabras comunes se multiplican por 2, ya que se repiten al ser comunes. Esto se divide por todas las palabras tipo de ambos documentos.

-Con **Jaccard** y **Sorensen**, aparece un problema y es que si uno de los dos textos a comparar tiene un tamaño mucho mayor que otro, las distancias tienden siempre a cero, por ello **Overlap** trata de disminuir este problema al dividirlo por el vocabulario más pequeño que corresponde a uno de los documentos.

$D_{overlap}\left ( X,Y \right )=1- \frac{2*\sum x_{i}\cap y_{i} }{min(\sum x_{j}\cap 1+\sum y_{k}\cap 1)}$

-La métrica *Masi* (Measuring Agreement on Set-valued Items) sigue un proceso parecido al overlap pero tiene en cuenta el mayor.

$D_{masi}\left ( X,Y \right )=1- \frac{2*\sum x_{i}\cap y_{i} }{max(\sum x_{j}\cap 1+\sum y_{k}\cap 1)}$

-Distancia de **contención**.

Se basa en realizar un conteo de la cantidad de de n-gramas unicos que hay en A y en B y se divide entre el número de n-gramas unicos que hay en A.

$C_{n}(A,B)=\frac{\left | S(A,n)\cap S(B,n) \right |}{\left | S(A,n) \right |}$

Podemos usar esta medida de distancia, siempre que el texto fuente sea más grande que los textos posiblemente plagiados a evaluar.

### Métricas basadas en similitudes de edición

Estas métricas en general se basan en comparar palabras mediante el número de trasnformaciones que hay que realizar para llegar de una a otra.
Se suelen utilizar para comparar distancias linguisticas entre diferentes idiomas, duplicación de textos y correción de léxico. Las más utlizadas son:

-Distancia de **Hamming**.

Compara cada letra de dos palabras basadas por su posición, su ventaja es su velocidad y simplicidad, por el contrario, es muy estricto, ya que necesita que ambas palabras sean del mismo tamaño.

-Distancia de **Levenshtein**.

Compara el número de transformaciones necesarias para transformar una palabra en otra, estas transformaciones se resumen en:
    
    -Insertar caracteres.
    -Eliminar caracteres.
    -Sustituir un caracter por otro.
    
Esta distancia en general es más completa y compleja que la distancia de Hamming.

### Métricas basadas en las secuencias de palabras

Compara las diferencias entre dos palabras, teniendo en cuenta las sub-palabras o sub-cadenas de palabras más largas dentro de las palabras inciales. (Longest common subsequence and longest common substring)

La diferencia básica entre las sub-palabras y sub-cadenas de palabras, reside en que el primero utiliza letras dentro de la palabra sin necesidad de que sean contiguas, y el segundo si implica que esté una contigua a la otra.
Por ejemplo entre las palabras 'palabras' y 'paralelas', con el método de sub-palabras tendrían en común : 'palas', mientras que con el método de sub-cadenas sería: 'pala' y 'para'.


# Caso prático

En este apartado, vamos a aplicar los procedimientos descritos a lo largo del trabajo a un caso real de detección de plagio, descrito en el apartado del Corpus.  

Para el análsis, nosotros lo que tendremos será un data frame con 100 muestras,de las cuales las 95 primeras muestras son la respuesta a una de las preguntas por parte de los alumnos, y las 5 últimas muestras son los enlaces a Wikipedia que podían consultar para responder a cada una de las 5 preguntas. Algunas de las columnas de ese data frame serán el tipo de plagio con el que se pedía responder, la pregunta que se responde, el individuo que la responde y la respuesta en forma de cadena de texto.

Una vez que tenemos el data frame, lo primero que hacemos es depurar la columna que contiene la respuesta tal y como se ha explicado en el apartado de preprocesado de los datos. En particular, desarrollamos una función `agrupa()` que elimina los signos de puntuación y convierte todo a minúsculas. Una vez hecho esto, dividimos cada una de las respuestas en N-gramas, en concreto hemos cogido N=3 ya que se ha visto que es la que mejor funciona.

```{r,include=FALSE}
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
```


Una vez tenemos cada respuesta dividida en trigramas, podemos proceder a aplicar algunas de las métricas descritas en el trabajo para ver si hacen buenas predicciones. Hemos escogido las métricas de Jaccard, distancia LCS y la métrica de 'containment'. A continuación, mostramos una pequeña tabla dónde podemos ver el porcentaje de plagio que nos brinda cada una de las métricas, cuando comprobamos el nivel de plagio entre todas las respuestas de la primera pregunta (incluida la fuente de Wikipedia). Por lo tanto, tendremos una porcentaje de plagio para las 20 respuestas del data frame.


```{r,include=FALSE}


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
  
  dj <- 1 - length(intersect(sus,or))/length(union(sus,or))
  return(dj)
}

#### Limpieza de los datos ####
load(file = "corpus_as_dataframe.Rda")
Ta <- corpus[corpus$Task=="a",]


#La columna texto ahora está en forma de ngramas
Ta$Text <- lapply(Ta$Text,FUN = agrupa)


#### comparamos Ngrams ###
Original <- unlist(Ta$Text[20])
Ta$djaccard <- lapply(Ta$Text,FUN = compara_djabbard,original = Original) #obtenemos lista con número de concidenca por ngrama
aux2 <- lapply(Ta$Text,FUN = compara,original = Original)
for (i in 1:length(Ta$Text)){
  Ta$containment[i] <- aux2[[i]][1]
  Ta$Coincidencias_containment[i] <- list(aux2[[i]][2: length(aux2[[i]])])
}
View(Ta)

# visualizamos pasando los vectores de coincidencias a matrices y haciendo heatmap
Ta$Ccmatrix <- lapply(Ta$Coincidencias_containment, matrix, ncol = 20)

sapply(Ta$Ccmatrix,heatmap)
heatmap(Ta$Ccmatrix[[2]],Rowv=NA,Colv=NA)
```


 

```{r,include=FALSE}

# PREGUNTA B 

Tb <- corpus[corpus$Task=="b",]
#La columna texto ahora está en forma de ngramas
Tb$Text <- lapply(Tb$Text,FUN = agrupa)
#### comparamos Ngrams ###
Original <- unlist(Tb$Text[20])
Tb$djaccard <- lapply(Tb$Text,FUN = compara_djabbard,original = Original) #obtenemos lisTb con número de concidenca por ngrama
aux2 <- lapply(Tb$Text,FUN = compara,original = Original)
for (i in 1:length(Tb$Text)){
  Tb$containment[i] <- aux2[[i]][1]
  Tb$Coincidencias_containment[i] <- list(aux2[[i]][2: length(aux2[[i]])])
}
View(Tb)


variables <- c("Task","Category","djaccard","containment")
tabla_resumen_b <- Tb[,variables]
kable(tabla_resumen_b)
```



```{r,include=FALSE}

# PREGUNTA C

Tc <- corpus[corpus$Task=="c",]
#La columna texto ahora está en forma de ngramas
Tc$Text <- lapply(Tc$Text,FUN = agrupa)
#### comparamos Ngrams ###
Original <- unlist(Tc$Text[20])
Tc$djaccard <- lapply(Tc$Text,FUN = compara_djabbard,original = Original) #obtenemos lisTb con número de concidenca por ngrama
aux2 <- lapply(Tc$Text,FUN = compara,original = Original)
for (i in 1:length(Tc$Text)){
  Tc$containment[i] <- aux2[[i]][1]
  Tc$Coincidencias_containment[i] <- list(aux2[[i]][2: length(aux2[[i]])])
}
View(Tc)


variables <- c("Task","Category","djaccard","containment")
tabla_resumen_c <- Tc[,variables]
kable(tabla_resumen_c)
```



```{r,include=FALSE}

# PREGUNTA D

Td <- corpus[corpus$Task=="d",]
#La columna texto ahora está en forma de ngramas
Td$Text <- lapply(Td$Text,FUN = agrupa)
#### comparamos Ngrams ###
Original <- unlist(Td$Text[20])
Td$djaccard <- lapply(Td$Text,FUN = compara_djabbard,original = Original) #obtenemos lisTb con número de concidenca por ngrama
aux2 <- lapply(Td$Text,FUN = compara,original = Original)
for (i in 1:length(Td$Text)){
  Td$containment[i] <- aux2[[i]][1]
  Td$Coincidencias_containment[i] <- list(aux2[[i]][2: length(aux2[[i]])])
}
View(Td)


variables <- c("Task","Category","djaccard","containment")
tabla_resumen_d <- Td[,variables]
kable(tabla_resumen_d)
```



```{r,include=FALSE}

# PREGUNTA E

Te <- corpus[corpus$Task=="e",]
#La columna texto ahora está en forma de ngramas
Te$Text <- lapply(Te$Text,FUN = agrupa)
#### comparamos Ngrams ###
Original <- unlist(Te$Text[20])
Te$djaccard <- lapply(Te$Text,FUN = compara_djabbard,original = Original) #obtenemos lisTb con número de concidenca por ngrama
aux2 <- lapply(Te$Text,FUN = compara,original = Original)
for (i in 1:length(Te$Text)){
  Te$containment[i] <- aux2[[i]][1]
  Te$Coincidencias_containment[i] <- list(aux2[[i]][2: length(aux2[[i]])])
}
View(Te)


variables <- c("Task","Category","djaccard","containment")
tabla_resumen_e <- Te[,variables]
kable(tabla_resumen_e)
```



```{r}
Ta <- Ta[,-13]
View(Ta)
T_abcde <- rbind(Ta,Tb,Tc,Td,Te)
View(T_abcde)

save(T_abcde,file="./Puntuaciones.Rdata")
```
```{r}
#CARGAMOS LOS DATOS

load("Puntuaciones.Rdata")
View(T_abcde)
```

```{r}
#install.packages("kableExtra")
library("kableExtra")
variables <- c("Task","Category","djaccard","containment")
tabla_resumen <- T_abcde[,variables]
kable(tabla_resumen)
```

En la tabla podemos ver el porcentaje de plagio que se le asigna a la respuesta en función de la métrica usada. En general, vemos que todas funcionan bastante bien y nos dan un resultado acorde con la categoría, que recordemos que hace referencia al tipo de plagio que habían usado los alumnos para contestar a la pregunta.

Veamos lo bueno que es nuestro modelo para la tarea "a". Ahora vamos a suponer que tenemos solo dos categorías: PLAGIADO y NO PLAGIADO. Dentro de la categoría PLAGIADO están incluidas las categorías "cut", "light" y "heavy". Vamos a evaluar entonces los aciertos del modelo en función de las dos métricas utilizadas. Para ello, consideramos que una métrica de "containment" superior a 0.50 se considera dentro de la categoría PLAGIADO, y si obtenemos un "containment" por debajo de 0.5 consideraremos que estamos en la categoría NO PLAGIADO.

```{r}
#install.packages("caret")
library("caret")

for (i in 1:dim(T_abcde)[1]){
  if (T_abcde$containment[i]<0.5){
    T_abcde$Prediccion[i] <- "NO PLAGIO"
  } else{
    T_abcde$Prediccion[i] <- "PLAGIO"
  }
}

for (i in 1:dim(T_abcde)[1]){
  if(T_abcde$Category[i]=="non"){
    T_abcde$Realidad[i] <- "NO PLAGIO"
  } else{
    T_abcde$Realidad[i] <- "PLAGIO"
  }
}

T_abcde$Realidad <- factor(T_abcde$Realidad)

T_abcde$Prediccion <- factor(T_abcde$Prediccion)

confusionMatrix(T_abcde$Realidad,T_abcde$Prediccion)
```

Observamos que tiene un accuracy del 78.36%, es decir, un acierto de casi el 80%. Es una medida bastante aceptable, por lo que podemos concluir que nuestro pequeño modelo nos sirve para detectar plagio con cierta fiabilidad en respuestas cortas escritas por alumnos. Tambén podemos visualizar la similitud de los textos gráficamente con esta métrica. En la siguiente ficura cada casilla es un ngrama, dispuesto de la manera en que estaría en un texto escrito, y el color representa la similitud encontrada de este ngrama en el texto original. Se aprecian las diferencias entre un texto con fragmentos copiado y pegado (izquierda), y otro sin plagio (derecha).
![image](convssin.jpg)

# Caso práctico (similitud semántica):


# Comparación semántica.  

Identificación de plagio mediante una sencilla comparación semántica.  
La estrategia para esta técnica será la tokenización del texto en sus raíces semánticas, para luego realizar una comparación con las raíces semánticas del texto original.  

```{r,include=FALSE,echo=FALSE}
#library(tidyr)
#library(tidytext)
#library(knitr)
#library(dplyr)
#library(tm)
#library(stringr)
#library(readr)
```

```{r,include=FALSE}
#cargamos los datos
#??lo comento porque asumo que se carga corpus antes
#load(file = "corpus_as_dataframe.Rda")

```

```{r,include=FALSE}
#Creamos una función que deje el texto completamente limpio:
limpiar<-function(texto){
  aux_texto<-paste(unlist(texto),collapse=" ")
  aux_texto<-tolower(aux_texto) 
  aux_texto<-gsub("([[:punct:]])|(\\n)|(\\t)|(\\W)"," ",aux_texto)
  #quitamos basicamente todo lo que no son letras y numeros
  aux_texto<-str_squish(aux_texto)
  aux_texto<-stringi::stri_trans_general(aux_texto,"Latin-ASCII") 
  return(aux_texto)
}

#creamos una nueva columna para guardar los textos limpios
corpus$Text_clean<-sapply(corpus$Text,limpiar)
#Ahora tenemos que eliminar las stop-words:

eliminar_stopwords<-function(texto,lang="en"){
  #libreria tm
  texto_aux<-removeWords(texto,stopwords(lang))
  return(str_squish(texto_aux))
}

#sobre escribimos en la columna de texto limpio, el texto sin stop-words
corpus$Text_clean<-unname(sapply(corpus$Text_clean,eliminar_stopwords))
#se estaba creando un vector con nombres y no necesitamos eso, 
#por ello applicamos el unname()


#Ahora que tenemos el texto sin stop-words le vamos a aplicar el stemming para quedarnos con los morfemas:  
steeming<-function(texto,lang="en",sep=" "){
  #libreria tm
  #necesita las palabras separadas individualmente
  texto_aux<-unlist(strsplit(texto,sep))
  texto_aux<-stemDocument(texto_aux,language = lang)
  return(as.character(unlist(texto_aux)))
}

#applicamos el stemming a cada texto limpio y obtenemos un vector de morfemas
corpus$Text_clean<-unname(sapply(corpus$Text_clean,steeming))



# Ya tenemos todas las palabras en sus raíces. El siguiente paso será contar todas las coincidencias. 
# 
# 
# _Nota: Un problema con el que nos podríamos encontrar y que requeriría un estudio más profundo, es el tratar con los errores de ortografía. De esta manera, un plagio parcial podría pasar desapercibido por errores de escritura. La sencilla solución que proponemos es usar la función `tm_map()` de la librería `tm` para realizar una sustitución de errores comunes (usando `replaceSynonyms`)._
# 
# Tal como hemos mencionado quedaría realizar una comprobación de las coincidencias que encontramos entre los textos con supuesto plagio y los originales.  
# 
# Hacemos una función para comparar dos vectores de palabras. Esto será usado para comparar el texto supuestamente plagiado con el original.

comparar<- function(original,plagio){
  #devolvemos las palabras que son coincidencias entre ambas listas 
  aux_plagio<-unlist(plagio)
  aux_original<-unlist(original)
  result<-unname(aux_plagio[aux_plagio %in% aux_original ])
  if (length(result)==0){
    return(0)
  }else{
    return(result)
  }
}


# Ahora que ya tenemos la función que nos hace la comparación solo debemos de crear una columna  
#en el dataset con las coincidencias que encontremos con el original:
  
#Hacemos un loop que recorra todos los registros que no son "original" y según su tipo de
#task realizamos la comparación con el original de dicha task
data_no_original<-corpus[corpus$Category!="original",]
#creamos un columna para ir llenando con las coincidencias
data_no_original$coincidencias<-NA

for (i in 1:nrow(data_no_original)){
  aux_task<-as.character(data_no_original$Task[i])
  text_origin_aux<-as.character(unlist(corpus[(corpus$Category=="original") 
                                              & (corpus$Task==aux_task),"Text_clean"]))
  text_plagio_aux<-data_no_original$Text_clean[[i]]
  aux_coincidencias<-comparar(text_origin_aux,text_plagio_aux)
  data_no_original$coincidencias[i]<-list(aux_coincidencias)
  
}

#hacemos la cuenta de las coincidencias al final
data_no_original$coincidencias_count<-sapply(data_no_original$coincidencias,length)
  
# Una vez tenemos las coincidencias, usaremos alguna de las métricas que ya hemos usado 
# para hacer una estimación de un porcentaje de plagio.
#añadimos una nueva columna para hacer la cuenta del total de elementos en text_clean
data_no_original$total_words<-sapply(data_no_original$Text_clean,length)

#Y por ultimo hacemos un porcentaje de coincidencias/total *100
data_no_original$porcentaje<-0
for (i in 1:nrow(data_no_original)){
  data_no_original$porcentaje[i]<-100*(data_no_original$coincidencias_count[i]/
                                         data_no_original$total_words[i])
}
```

El proceso seguido ha sido:   
Pre-procesado del texto para dejarlo limpio, eliminación de las stop-words, y tokenizado según sus raíces. Finalmente realizamos una sencilla comparación elemento a elemento entre el texto original y el que se está analizando. De esta comparación obtedremos una lista con las raices coincidentes. Por último, para realizar un análisis de los resultados, se obtuvieron porcentajes para cada texto, dividiendo las raices comunes entre todas las palabras del texto a analizar.  

Una vez tenemos el porcentaje de palabras plagiadas para cada texto, podemos establecer un umbral para clasificarlo como **plagio** y así determinar la precisión etc. En nuestro caso escogimos un umbral de **50%**, pero esto es un parámetro que se puede _tunear_ para obtener el mejor rendimiento del modelo.

```{r,include=FALSE}
#Matrix de confusion mal

# Resultados
#Veamos solo la columna de categoria, dificultad, task y porcentaje juntas:

head(data_no_original[,c("Category","porcentaje","Task","Difficulty")],10)
# El modelo es muy sencillo pero ha dado resultados bastante prometedores al menos en la clasificación NOPLAGIO--PLAGIO.
# Si ponemos 50 como limite, vemos la cantidad de plagios que se nos han colado:

#Vamos a hacer una matriz de confusión para tener una idea de que
#tan bien funciona el modelo.

#true true
#plagio as plagio
TT<-sum(data_no_original$porcentaje>50 & data_no_original$Category!="non")/sum(data_no_original$porcentaje>50)*100

#true false
#plagio as no-plagio
TF<-sum(data_no_original$porcentaje>50 & data_no_original$Category=="non")/sum(data_no_original$porcentaje>50)*100

#false true
#no-plagio as plagio
FT<-sum(data_no_original$porcentaje<50 & data_no_original$Category!="non")/sum(data_no_original$porcentaje<50)*100

#false false
#no-plagio as no-plagio
FF<-sum(data_no_original$porcentaje<50 & data_no_original$Category=="non")/sum(data_no_original$porcentaje<50)*100

table_aux<-data.frame(matrix(c(TT,FT,TF,FF),ncol=2))
colnames(table_aux)<-c("Real Plagio"," Real No-plagio")
rownames(table_aux)<-c("Predicted Plagio","Predicted No-plagio")

kable(table_aux,"pipe")


```


A continuación mostramos una matriz de confusión con los resultados obtenidos:

```{r,echo=FALSE}
#library("caret")

#le damos las categorias de no plagio y plagio en funcion de si es non o el resto

data_no_original$Prediccion<-NA
for (i in 1:dim(data_no_original)[1]){
  if (data_no_original$porcentaje[i]<50){
    data_no_original$Prediccion[i] <- "NO PLAGIO"
  }else{
    data_no_original$Prediccion[i] <- "PLAGIO"
  }
}
data_no_original$Realidad<-NA
for (i in 1:dim(data_no_original)[1]){
  if(data_no_original$Category[i]=="non"){
    data_no_original$Realidad[i] <- "NO PLAGIO"
  }else{
    data_no_original$Realidad[i] <- "PLAGIO"
  }
}
data_no_original$Realidad <- factor(data_no_original$Realidad)
data_no_original$Prediccion <- factor(data_no_original$Prediccion)
confusionMatrix(data_no_original$Realidad,data_no_original$Prediccion)
```

De la matriz anterior sacamos las siguientes observaciones:

* Se ha confundido un 7.6% de no plagiados como sí plagiados.
* El 63 % de los no plagio están bien clasificados.
* El 96 % de los plagios si se clasifican.
* Solo un 20% se los que no debería clasificarse como plagio se clasifican como plagio.



# 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" alt />
<p class="caption">Clasificación del plagio</p>
</div>
<div id="corpus" class="section level2" number="2.1">
<h2><span class="header-section-number">2.1</span> Corpus</h2>
<p>Un corpus lingüístico es un conjunto de textos variados que cumple la función de representar el uso real de la lengua que se quiere examinar. En el caso del procesamiento de texto es necesario contar con un corpus adecuado al campo y características que se buscan extraer. En nuestro caso hacemos uso del corpus proporcionado por miembros de la universidad de Sheffield <span class="citation">(P Clough 2009)</span>. Consta de un set de 5 cuestiones que veinte alumnos tuvieron que responder. Los alumnos se dividieron en cuatro grupos que podían responder las preguntas con materiales externos o consultando páginas de Wikipedia, permitiéndose niveles diferentes de copia y referencia. De esta forma tenemos diferentes grados de plagio con el documento original, que van del corta y pega al no uso de la fuente, y una fácil comparación. Además de todas estas respuestas, también se proporciona un índice con el alumno que escribió cada texto, cual era la pregunta original y el nivel de copia (near copy, light revision, heavy revision, non- plagiarism) debidamente indexado. EN resumidas cuentas, nuestro corpus cuenta con 100 textos breves (entre 200 y 300 palabras) con diferentes niveles de plagio, de manera que son lo suficientemente largos para realizar un análisis didáctico sin convertirse en una carga difícil de procesar con nuestros medios.</p>
</div>
<div id="preprocesado" class="section level2" number="2.2">
<h2><span class="header-section-number">2.2</span> Preprocesado</h2>
<p>El preporcesado es el primer contacto con los datos, las primeras tareas de limpieza y formateo de los datos suelen ser por lo general sencillas y en su mayoría genéricas.</p>
<p>Primero exponemos las funciones básicas para este propósito:</p>
<!-- rnb-text-end -->
<!-- rnb-chunk-begin -->
<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI2xpYnJlcsOtYXMgcGFyYSBjYXJnYXIgbG9zIGRhdG9zIHkgcHJlcHJvY2VzYXJsb3MgXG5cbnRleHRvPC1cIiDCoSBFc3RvIGVzICAgIHVuIHRleHRvIGRlIHBydWViYSBwYXJhIGRlbW9zdHJhciBsYSAgIGxpbXBpZXphIGluY2ljaWFsIGRlIGxvcyB0ZXh0b3MhICjDocOhw6nDp8OxKVwiXG4jcGFzYXJsbyBhIG1pbsO6c2N1bGFzXG50ZXh0bzwtdG9sb3dlcih0ZXh0bykgXG50ZXh0b1xuYGBgIn0= -->
<pre class="r"><code>#librerías para cargar los datos y preprocesarlos 

texto&lt;-&quot; ¡ Esto es    un texto de prueba para demostrar la   limpieza incicial de los textos! (ááéçñ)&quot;
#pasarlo a minúsculas
texto&lt;-tolower(texto) 
texto</code></pre>
<!-- rnb-source-end -->
<!-- rnb-chunk-end -->
<!-- rnb-text-begin -->
<!-- rnb-text-end -->
<!-- rnb-chunk-begin -->
<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI2VsaW1pbmFyIHNpZ25vcyBkZSBwdW50dWFjacOzbiwgY2Fyw6FjdGVyZXMgcHJvYmxlbcOhdGljb3NcbnRleHRvPC1nc3ViKFwiW1s6cHVuY3Q6XV1cIixcIiBcIix0ZXh0bylcbnRleHRvPC1nc3ViKFwiKFxcXFxuKXwoXFxcXHQpXCIsXCIgXCIsdGV4dG8pXG50ZXh0b1xuXG4jeSBjb21wcm9iYXIgcXVlIG5vIGhheSBlc3BhY2lvcyByZXBldGlkb3MgeSBhbCBwcmluY2lwaW8geSBmaW5hbCBkZWwgc3RyaW5nIChzdHJfdHJpbSgpKVxudGV4dG88LWdzdWIoXCJcXFxccytcIixcIiBcIixzdHJfdHJpbSh0ZXh0bykpXG4jbGEgZnVuY2nDs24gc3RyX3NxdWlzaCgpIGRlIGxhIGxpYnJlcsOtYSBzdHJpbmdyIGhhY2UgZXhhY3RhbWVudGUgXG4jZWxpbWlsYXIgbG9zIGVzcGFjaW9zIGVuIGJsYW5jbyBhbCBjb21pZW56byB5IGFsIGZpbmFsIGRlbCBzdHJpbmdcbnRleHRvXG5gYGAifQ== -->
<pre class="r"><code>#eliminar signos de puntuación, carácteres problemáticos
texto&lt;-gsub(&quot;[[:punct:]]&quot;,&quot; &quot;,texto)
texto&lt;-gsub(&quot;(\\n)|(\\t)&quot;,&quot; &quot;,texto)
texto

#y comprobar que no hay espacios repetidos y al principio y final del string (str_trim())
texto&lt;-gsub(&quot;\\s+&quot;,&quot; &quot;,str_trim(texto))
#la función str_squish() de la librería stringr hace exactamente 
#elimilar los espacios en blanco al comienzo y al final del string
texto</code></pre>
<!-- rnb-source-end -->
<!-- rnb-chunk-end -->
<!-- rnb-text-begin -->
<p>En el caso de que el idioma tenga acentuación, sería recomendable eliminarla para no tener problemas de formato ni considerar diferentes palabras iguales solo por estar acentuada o no. Esto lo podemos hacer de forma sencilla usando stringi::stri_trans_general()</p>
<!-- rnb-text-end -->
<!-- rnb-chunk-begin -->
<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG50ZXh0bzwtc3RyaW5naTo6c3RyaV90cmFuc19nZW5lcmFsKHRleHRvLFwiTGF0aW4tQVNDSUlcIikgXG4jIGVzdGUgcHJvY2VzbyBzZXLDoSBtdXkgZGVwZW5kaWVudGUgZGVsIGlkaW9tYSBkZWwgdGV4dG9cbnRleHRvXG5gYGAifQ== -->
<pre class="r"><code>
texto&lt;-stringi::stri_trans_general(texto,&quot;Latin-ASCII&quot;) 
# este proceso será muy dependiente del idioma del texto
texto</code></pre>
<!-- rnb-source-end -->
<!-- rnb-chunk-end -->
<!-- rnb-text-begin -->
<p>Una limpieza más intensiva podría seguir con la eliminación de palabras unión <code>stop-words</code> y hasta la reducción de palabras hasta su raíz (término <em>stemming</em> en inglés).Para este propósito hay librerías que implementan funciones para dicho proposito en R, algunas de ellas son <code>tm</code> y <code>tidytext</code>.<br />
Sin embargo esta sobre-simplificación puede no ser lo más recomendado en todos los casos. La elección será muy dependiente del algoritmo y del tipo de plagio buscado (ya hemos comentado previamente que hay diferentes tipos de plagio a detectar).</p>
<!-- rnb-text-end -->
<!-- rnb-chunk-begin -->
<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI2VsaW1pbmFyIHN0b3Atd29yZHMgY29uIGxhIGxpYnJlcsOtYSB0bVxuXG50ZXh0bzI8LXJlbW92ZVdvcmRzKHRleHRvLHN0b3B3b3JkcyhcImVzXCIpKVxudGV4dG8yPC1zdHJfc3F1aXNoKHRleHRvMilcbnRleHRvMlxuXG5cbmBgYCJ9 -->
<pre class="r"><code>#eliminar stop-words con la librería tm

texto2&lt;-removeWords(texto,stopwords(&quot;es&quot;))
texto2&lt;-str_squish(texto2)
texto2
</code></pre>
<!-- rnb-source-end -->
<!-- rnb-chunk-end -->
<!-- rnb-text-begin -->
<!-- rnb-text-end -->
<!-- rnb-chunk-begin -->
<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI3N0ZW1taW5nIHBhbGFicmFzIGNvbiBsYSBsaWJyZXJpYSB0bVxudGV4dG8yPC11bmxpc3Qoc3RyX3NwbGl0KHRleHRvMixcIiBcIikpXG50ZXh0bzM8LXN0ZW1Eb2N1bWVudCh0ZXh0bzIsbGFuZ3VhZ2UgPSBcImVzXCIpXG50ZXh0bzNcbmBgYCJ9 -->
<pre class="r"><code>#stemming palabras con la libreria tm
texto2&lt;-unlist(str_split(texto2,&quot; &quot;))
texto3&lt;-stemDocument(texto2,language = &quot;es&quot;)
texto3</code></pre>
<!-- rnb-source-end -->
<!-- rnb-chunk-end -->
<!-- rnb-text-begin -->
</div>
<div id="tokenizado" class="section level2" number="2.3">
<h2><span class="header-section-number">2.3</span> Tokenizado</h2>
<p>Una vez importado el corpus y depurado los datos, el siguiente paso es tokemizarlos. Esto consiste en separar en elementos más pequeños el conjunto de datos que tenemos. Hablando de datos de tipo texo, tokemizar podría ser separar en palabras o en frases por ejemplo. La función <code>strsplit()</code> nos permitiría hacer variando el argumento de separación, <code>split</code> según nos convenga.</p>
<ul>
<li><code>split=&quot; &quot;</code> un espacio podría servir para separar en palabras.</li>
<li><code>split=&quot;.&quot;</code> un punto podría servir para separar en frases.</li>
</ul>
<p>Sin embargo, en este caso nos va a interesar usar N-gramas, que son subconjuntos del conjunto original que van cogiendo combinaciones de n elementos y dónde el elemento siguiente está formado por los (n-1) elementos del anterior más un elemento nuevo. Por ejemplo, si tenemos el texto: “Hoy hemos quedado en la rusa a las seis”. Su división en 3-gramas (o trigramas) sería:</p>
<p>[Hoy hemos quedado], [hemos quedado en], [quedado en la], [en la rusa],[la rusa a], [rusa a las],[a las seis].</p>
<p>En R, se puede hacer fácilmente usando el paquete <code>ngram</code>, y más concretamente la función <code>unnest_tokens()</code>, que toma como argumentos principales los siguientes:</p>
<ul>
<li><code>tbl</code> un data frame</li>
<li><code>output</code> el nombre de la columna que se va crear</li>
<li><code>input</code> los datos que queremos usar.</li>
<li><code>token</code> a qué token queremos convertirlos (en este caso el que más nos interesa es “ngrams”)</li>
<li><code>n</code> argumento adicional para n-gramas para indicar el tamaño de éstos.</li>
</ul>
<p>Los N-gramas son muy utilizados en aplicaciones de análisis de texto, como por ejemplo en la detección de plagio. La similitud entre dos textos se puede calcular contando el número de N-gramas que tienen en común.</p>
</div>
<div id="metricas-y-modelos" class="section level2" number="2.4">
<h2><span class="header-section-number">2.4</span> Metricas y modelos</h2>
<p>Las métricas y modelos utilizados a la hora de evaluar los niveles de plagio entre textos, ya sean del mismo autor, o de varios, se deben dividir en varios grupos, dependiendo de la previa tokenización o agrupación de las palabras o letras del corpus.</p>
<div id="métricas-basadas-en-similitudes-de-tokenes" class="section level3" number="2.4.1">
<h3><span class="header-section-number">2.4.1</span> Métricas basadas en similitudes de tokenes</h3>
<p>Una de las formas más utilizadas a la hora de comparar bloques de texto es la de comparaciones vectoriales, en las que, por ejemplo, cada palabra se convierte en el índice de un vector, y la cantidad de veces que aparece es su escalar. Utilizando este proceso, tambien se puede extrapolar de manera que en vez de contar las palabras, se cuente los pares o trios de palabras, lo que vendria a ser separacion en n-gramas(bigramas o trigramas).</p>
<p>Son bastante eficientes en general, y funcionan bien para textos largos a diferencia de los otros tipos de métricas.</p>
<p>Para esta tokenización, las métricas mas utilizadas, son las de medida de distancia entre un texto y otro, comparando vector a vector.</p>
<p>-Una primera métrica seria la de <strong>Jaccard</strong>.</p>
<p>Esta métrica trata de medir los elementos que comparten ambos vectores.</p>
<p><span class="math inline">\(D_{jaccard}\left ( X,Y \right )=1- \frac{\sum x_{i}\cap y_{i} }{\sum x_{i}\cup y_{j}}\)</span></p>
<p>En términos generales, consiste en dividir el vocabulario común de los documentos entre el vocabulario de la unión de ambos documentos, y le restamos 1 para que dos vectores sean parecidos cuando estan cerca de 0 y lejanos cuando se acercan a 1.</p>
<p>-La siguiente métrica es muy parecida ya que es la <strong>Jaccard pesada</strong>. En esta métrica se tiene en cuenta tambien la cantidad potencial de coincidencias.</p>
<p><span class="math inline">\(D_{wjaccard}\left( X,Y\right )=1- \frac{\sum min\left ( x,y \right ) }{\sum max(x,y)}\)</span></p>
<p>-Métrica de <strong>Sorensen</strong>.</p>
<p><span class="math inline">\(D_{sorensen}\left ( X,Y \right )=1- \frac{2*\sum x_{i}\cap y_{i} }{\sum x_{j}\cap 1+\sum y_{k}\cap 1}\)</span></p>
<p>Consiste en dividir la cantidad de palabras comunes se multiplican por 2, ya que se repiten al ser comunes. Esto se divide por todas las palabras tipo de ambos documentos.</p>
<p>-Con <strong>Jaccard</strong> y <strong>Sorensen</strong>, aparece un problema y es que si uno de los dos textos a comparar tiene un tamaño mucho mayor que otro, las distancias tienden siempre a cero, por ello <strong>Overlap</strong> trata de disminuir este problema al dividirlo por el vocabulario más pequeño que corresponde a uno de los documentos.</p>
<p><span class="math inline">\(D_{overlap}\left ( X,Y \right )=1- \frac{2*\sum x_{i}\cap y_{i} }{min(\sum x_{j}\cap 1+\sum y_{k}\cap 1)}\)</span></p>
<p>-La métrica <em>Masi</em> (Measuring Agreement on Set-valued Items) sigue un proceso parecido al overlap pero tiene en cuenta el mayor.</p>
<p><span class="math inline">\(D_{masi}\left ( X,Y \right )=1- \frac{2*\sum x_{i}\cap y_{i} }{max(\sum x_{j}\cap 1+\sum y_{k}\cap 1)}\)</span></p>
</div>
<div id="métricas-basadas-en-similitudes-de-edición" class="section level3" number="2.4.2">
<h3><span class="header-section-number">2.4.2</span> Métricas basadas en similitudes de edición</h3>
<p>Estas métricas en general se basan en comparar palabras mediante el número de trasnformaciones que hay que realizar para llegar de una a otra. Se suelen utilizar para comparar distancias linguisticas entre diferentes idiomas, duplicación de textos y correción de léxico. Las más utlizadas son:</p>
<p>-Distancia de <strong>Hamming</strong>.</p>
<p>Compara cada letra de dos palabras basadas por su posición, su ventaja es su velocidad y simplicidad, por el contrario, es muy estricto, ya que necesita que ambas palabras sean del mismo tamaño.</p>
<p>-Distancia de <strong>Levenshtein</strong>.</p>
<p>Compara el número de transformaciones necesarias para transformar una palabra en otra, estas transformaciones se resumen en:</p>
<pre><code>-Insertar caracteres.
-Eliminar caracteres.
-Sustituir un caracter por otro.</code></pre>
<p>Esta distancia en general es más completa y compleja que la distancia de Hamming.</p>
</div>
<div id="métricas-basadas-en-las-secuencias-de-palabras" class="section level3" number="2.4.3">
<h3><span class="header-section-number">2.4.3</span> Métricas basadas en las secuencias de palabras</h3>
<p>Compara las diferencias entre dos palabras, teniendo en cuenta las sub-palabras o sub-cadenas de palabras más largas dentro de las palabras inciales. (Longest common subsequence and longest common substring)</p>
<p>La diferencia básica entre las sub-palabras y sub-cadenas de palabras, reside en que el primero utiliza letras dentro de la palabra sin necesidad de que sean contiguas, y el segundo si implica que esté una contigua a la otra. Por ejemplo entre las palabras ‘palabras’ y ‘paralelas’, con el método de sub-palabras tendrían en común : ‘palas’, mientras que con el método de sub-cadenas sería: ‘pala’ y ‘para’.</p>
</div>
</div>
<div id="evaluación-de-los-resultados" class="section level2" number="2.5">
<h2><span class="header-section-number">2.5</span> Evaluación de los resultados</h2>
</div>
</div>
<div id="caso-prático" class="section level1" number="3">
<h1><span class="header-section-number">3</span> Caso prático</h1>
<p>En este apartado, vamos a aplicar los procedimientos descritos a lo largo del trabajo a un caso real de detección de plagio, descrito en el apartado del Corpus.</p>
<p>Para el análsis, nosotros lo que tendremos será un data frame con 100 muestras,de las cuales las 95 primeras muestras son la respuesta a una de las preguntas por parte de los alumnos, y las 5 últimas muestras son los enlaces a Wikipedia que podían consultar para responder a cada una de las 5 preguntas. Algunas de las columnas de ese data frame serán el tipo de plagio con el que se pedía responder, la pregunta que se responde, el individuo que la responde y la respuesta en forma de cadena de texto.</p>
<p>Una vez que tenemos el data frame, lo primero que hacemos es depurar la columna que contiene la respuesta tal y como se ha explicado en el apartado de preprocesado de los datos. En particular, desarrollamos una función <code>agrupa()</code> que elimina los signos de puntuación y convierte todo a minúsculas. Una vez hecho esto, dividimos cada una de las respuestas en N-gramas, en concreto hemos cogido N=3 ya que se ha visto que es la que mejor funciona.</p>
<!-- rnb-text-end -->
<!-- rnb-text-begin -->
<p>Una vez tenemos cada respuesta dividida en trigramas, podemos proceder a aplicar algunas de las métricas descritas en el trabajo para ver si hacen buenas predicciones. Hemos escogido las métricas de Jaccard, distancia LCS y la métrica de ‘containment’. A continuación, mostramos una pequeña tabla dónde podemos ver el porcentaje de plagio que nos brinda cada una de las métricas, cuando comprobamos el nivel de plagio entre todas las respuestas de la primera pregunta (incluida la fuente de Wikipedia). Por lo tanto, tendremos una porcentaje de plagio para las 20 respuestas del data frame.</p>
<!-- rnb-text-end -->
<!-- rnb-text-begin -->
<!-- rnb-text-end -->
<!-- rnb-text-begin -->
<!-- rnb-text-end -->
<!-- rnb-text-begin -->
<!-- rnb-text-end -->
<!-- rnb-text-begin -->
<!-- rnb-text-end -->
<!-- rnb-text-begin -->
<!-- rnb-text-end -->
<!-- rnb-chunk-begin -->
<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuVGEgPC0gVGFbLC0xM11cblZpZXcoVGEpXG5UX2FiY2RlIDwtIHJiaW5kKFRhLFRiLFRjLFRkLFRlKVxuVmlldyhUX2FiY2RlKVxuXG5zYXZlKFRfYWJjZGUsZmlsZT1cIi4vUHVudHVhY2lvbmVzLlJkYXRhXCIpXG5gYGAifQ== -->
<pre class="r"><code>Ta &lt;- Ta[,-13]
View(Ta)
T_abcde &lt;- rbind(Ta,Tb,Tc,Td,Te)
View(T_abcde)

save(T_abcde,file=&quot;./Puntuaciones.Rdata&quot;)</code></pre>
<!-- rnb-source-end -->
<!-- rnb-chunk-end -->
<!-- rnb-chunk-begin -->
<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI0NBUkdBTU9TIExPUyBEQVRPU1xuXG5sb2FkKFwiUHVudHVhY2lvbmVzLlJkYXRhXCIpXG5WaWV3KFRfYWJjZGUpXG5gYGAifQ== -->
<pre class="r"><code>#CARGAMOS LOS DATOS

load(&quot;Puntuaciones.Rdata&quot;)
View(T_abcde)</code></pre>
<!-- rnb-source-end -->
<!-- rnb-chunk-end -->
<!-- rnb-text-begin -->
<!-- rnb-text-end -->
<!-- rnb-chunk-begin -->
<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI2luc3RhbGwucGFja2FnZXMoXCJrYWJsZUV4dHJhXCIpXG5saWJyYXJ5KFwia2FibGVFeHRyYVwiKVxudmFyaWFibGVzIDwtIGMoXCJUYXNrXCIsXCJDYXRlZ29yeVwiLFwiZGphY2NhcmRcIixcImNvbnRhaW5tZW50XCIpXG50YWJsYV9yZXN1bWVuIDwtIFRfYWJjZGVbLHZhcmlhYmxlc11cbmthYmxlKHRhYmxhX3Jlc3VtZW4pXG5gYGAifQ== -->
<pre class="r"><code>#install.packages(&quot;kableExtra&quot;)
library(&quot;kableExtra&quot;)
variables &lt;- c(&quot;Task&quot;,&quot;Category&quot;,&quot;djaccard&quot;,&quot;containment&quot;)
tabla_resumen &lt;- T_abcde[,variables]
kable(tabla_resumen)</code></pre>
<!-- rnb-source-end -->
<!-- rnb-chunk-end -->
<!-- rnb-text-begin -->
<p>En la tabla podemos ver el porcentaje de plagio que se le asigna a la respuesta en función de la métrica usada. En general, vemos que todas funcionan bastante bien y nos dan un resultado acorde con la categoría, que recordemos que hace referencia al tipo de plagio que habían usado los alumnos para contestar a la pregunta.</p>
<p>Veamos lo bueno que es nuestro modelo para la tarea “a”. Ahora vamos a suponer que tenemos solo dos categorías: PLAGIADO y NO PLAGIADO. Dentro de la categoría PLAGIADO están incluidas las categorías “cut”, “light” y “heavy”. Vamos a evaluar entonces los aciertos del modelo en función de las dos métricas utilizadas. Para ello, consideramos que una métrica de “containment” superior a 0.50 se considera dentro de la categoría PLAGIADO, y si obtenemos un “containment” por debajo de 0.5 consideraremos que estamos en la categoría NO PLAGIADO.</p>
<!-- rnb-text-end -->
<!-- rnb-chunk-begin -->
<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI2luc3RhbGwucGFja2FnZXMoXCJjYXJldFwiKVxubGlicmFyeShcImNhcmV0XCIpXG5cbmZvciAoaSBpbiAxOmRpbShUX2FiY2RlKVsxXSl7XG4gIGlmIChUX2FiY2RlJGNvbnRhaW5tZW50W2ldPDAuNSl7XG4gICAgVF9hYmNkZSRQcmVkaWNjaW9uW2ldIDwtIFwiTk8gUExBR0lPXCJcbiAgfSBlbHNle1xuICAgIFRfYWJjZGUkUHJlZGljY2lvbltpXSA8LSBcIlBMQUdJT1wiXG4gIH1cbn1cblZpZXcoVF9hYmNkZSlcblxuZm9yIChpIGluIDE6ZGltKFRfYWJjZGUpWzFdKXtcbiAgaWYoVF9hYmNkZSRDYXRlZ29yeVtpXT09XCJub25cIil7XG4gICAgVF9hYmNkZSRSZWFsaWRhZFtpXSA8LSBcIk5PIFBMQUdJT1wiXG4gIH0gZWxzZXtcbiAgICBUX2FiY2RlJFJlYWxpZGFkW2ldIDwtIFwiUExBR0lPXCJcbiAgfVxufVxuXG5UX2FiY2RlJFJlYWxpZGFkIDwtIGZhY3RvcihUX2FiY2RlJFJlYWxpZGFkKVxuXG5UX2FiY2RlJFByZWRpY2Npb24gPC0gZmFjdG9yKFRfYWJjZGUkUHJlZGljY2lvbilcblxuY29uZnVzaW9uTWF0cml4KFRfYWJjZGUkUmVhbGlkYWQsVF9hYmNkZSRQcmVkaWNjaW9uKVxuYGBgIn0= -->
<pre class="r"><code>#install.packages(&quot;caret&quot;)
library(&quot;caret&quot;)

for (i in 1:dim(T_abcde)[1]){
  if (T_abcde$containment[i]&lt;0.5){
    T_abcde$Prediccion[i] &lt;- &quot;NO PLAGIO&quot;
  } else{
    T_abcde$Prediccion[i] &lt;- &quot;PLAGIO&quot;
  }
}
View(T_abcde)

for (i in 1:dim(T_abcde)[1]){
  if(T_abcde$Category[i]==&quot;non&quot;){
    T_abcde$Realidad[i] &lt;- &quot;NO PLAGIO&quot;
  } else{
    T_abcde$Realidad[i] &lt;- &quot;PLAGIO&quot;
  }
}

T_abcde$Realidad &lt;- factor(T_abcde$Realidad)

T_abcde$Prediccion &lt;- factor(T_abcde$Prediccion)

confusionMatrix(T_abcde$Realidad,T_abcde$Prediccion)</code></pre>
<!-- rnb-source-end -->
<!-- rnb-chunk-end -->
<!-- rnb-text-begin -->
<p>Observamos que tiene un accuracy del 78.36%, es decir, un acierto de casi el 80%. Es una medida bastante aceptable, por lo que podemos concluir que nuestro pequeño modelo nos sirve para detectar plagio con cierta fiabilidad en respuestas cortas escritas por alumnos. Tambén podemos visualizar la similitud de los textos gráficamente con esta métrica. En la siguiente ficura cada casilla es un ngrama, dispuesto de la manera en que estaría en un texto escrito, y el color representa la similitud encontrada de este ngrama en el texto original. Se aprecian las diferencias entre un texto con fragmentos copiado y pegado (izquierda), y otro sin plagio (derecha). <img src="data:image/jpeg;base64,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" alt="image" /></p>
</div>
<div id="bibliofragia" class="section level1" number="4">
<h1><span class="header-section-number">4</span> Bibliofragia</h1>
<!-- rnb-text-end -->
<div id="refs" class="references csl-bib-body hanging-indent">
<div id="ref-tipos1" class="csl-entry">
Hussain Chowdhury, Dhruba Bhattacharyya. 2016. <span>“Plagiarism: Taxonomy, Tools and Detection Techniques.”</span> <em>Knowledge, Library and Information Networking</em>.
</div>
<div id="ref-corpus_source" class="csl-entry">
P Clough, M Stevenson. 2009. <span>“Developing a Corpus of Plagiarised Short Answers, Language Resources and Evaluation: Special Issue on Plagiarism and Authorship Analysis, in Press.”</span>
</div>
<div id="ref-intencion" class="csl-entry">
Shivani, Vishal Goyal. 2016. <span>“A Novel Approach for Plagiarism Detection in English Text.”</span> <em>International Journal of Computer Applications</em>.
</div>
<div id="ref-tipos2" class="csl-entry">
Vani Kanjirangat, Deepa Gupta. 2016. <span>“A Study on Extrinsic Text Plagiarism Detection Techniques and Tools.”</span> <em>Journal of Engineering Science and Technology Review</em>.
</div>
</div>
</div>

<div id="rmd-source-code">LS0tDQp0aXRsZTogIlRyYWJham8gUGxhZ2lvIE1hc3RlciINCmF1dGhvcjogIsOBbmdlbCBHdWV2YXJhIFJvcywgQXJ0dXJvICBTaXJ2ZW50IEZyZXNuZWRhICwgTWlndWVsIEhvcnRlbGFubyBCdXN0byAsIEZyYW5jaXNjbyBPbGF5byBHb256w6FsZXogUm9kcsOtZ3VleiINCmRhdGU6ICAiYHIgU3lzLkRhdGUoKWAiIA0Kb3V0cHV0Og0KICBodG1sX2RvY3VtZW50Og0KICAgIGVjaG86IHllcw0KICAgIG51bWJlcl9zZWN0aW9uczogeWVzDQogICAgdGhlbWU6IGx1bWVuDQogICAgdG9jOiB5ZXMNCiAgaHRtbF9ub3RlYm9vazoNCiAgICBlY2hvOiB5ZXMNCiAgICBudW1iZXJfc2VjdGlvbnM6IHllcw0KICAgIHRvYzogeWVzDQogIHBkZl9kb2N1bWVudDoNCiAgICB0b2M6IHllcw0KICAgIHRvY19kZXB0aDogMw0KICAgIG51bWJlcl9zZWN0aW9uczogeWVzDQpsYW5ndWFnZToNCiAgbGFiZWw6DQogICAgZmlnOiAnRmlndXJhICcNCiAgICB0YWI6ICdUYWJsYSAnDQogICAgZXE6ICdFY3VhY2nDs24gJw0KICAgIHRobTogJ1Rlb3JlbWEgJw0KICAgIGxlbTogJ0xlbWEgJw0KICAgIGRlZjogJ0RlZmluaWNpw7NuICcNCiAgICBjb3I6ICdDb3JvbGFyaW8gJw0KICAgIHBycDogJ1Byb3Bvc2ljacOzbiAnDQogICAgZXhtOiAnRWplbXBsbyAnDQogICAgZXhyOiAnRWplcmNpY2lvICcNCiAgICBwcm9vZjogJ0RlbW9zdHJhY2nDs24uICcNCiAgICByZW1hcms6ICdOb3RhOiAnDQogICAgc29sdXRpb246ICdTb2x1Y2nDs24uICcNCmJpYmxpb2dyYXBoeTogYmlibGlvZ3JhcGh5LmJpYg0KLS0tDQoNCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFfQ0Ka25pdHI6Om9wdHNfY2h1bmskc2V0KGVjaG8gPSBUUlVFKQ0KYGBgDQoNCmBgYHtyIGluY2x1ZGU9RkFMU0V9DQojIEVzcGVjaWZpY2Ftb3MgbGFzIGxpYnJlcsOtYXMgbmVjZXNhcmlhcyBlbiBlc3RhIGxpc3RhDQoNCnBhY2thZ2VzID0gYygiZHBseXIiLCAia2FibGVFeHRyYSIsIm5ncmFtIiwicmVhZHIiLCAidGlkeXRleHQiLCJ0aWR5ciIsInRtIiwic3RyaW5nciIpDQoNCiN1c2UgdGhpcyBmdW5jdGlvbiB0byBjaGVjayBpZiBlYWNoIHBhY2thZ2UgaXMgb24gdGhlIGxvY2FsIG1hY2hpbmUNCiNpZiBhIHBhY2thZ2UgaXMgaW5zdGFsbGVkLCBpdCB3aWxsIGJlIGxvYWRlZA0KI2lmIGFueSBhcmUgbm90LCB0aGUgbWlzc2luZyBwYWNrYWdlKHMpIHdpbGwgYmUgaW5zdGFsbGVkIGFuZCBsb2FkZWQNCnBhY2thZ2UuY2hlY2sgPC0gbGFwcGx5KHBhY2thZ2VzLCBGVU4gPSBmdW5jdGlvbih4KSB7DQogIGlmICghcmVxdWlyZSh4LCBjaGFyYWN0ZXIub25seSA9IFRSVUUpKSB7DQogICAgaW5zdGFsbC5wYWNrYWdlcyh4LCBkZXBlbmRlbmNpZXMgPSBUUlVFKQ0KICAgIGxpYnJhcnkoeCwgY2hhcmFjdGVyLm9ubHkgPSBUUlVFKQ0KICB9DQp9KQ0KDQojdmVyaWZ5IHRoZXkgYXJlIGxvYWRlZA0Kc2VhcmNoKCkNCg0KYGBgDQoNCg0KDQojIEludHJvZHVjY2nDs24geSBtb3RpdmFjacOzbg0KDQpFbiBsb3Mgw7psdGltb3MgYcOxb3MsIGVsIHBsYWdpbyB5IHN1IGRldGVjY2nDs24gc2UgaGFuIHBvc2ljaW9uYWRvIGNvbW8gdW4gcHJvYmxlbWEgZW4gY3JlY2ltaWVudG8gZGVudHJvIGRlbCDDoW1iaXRvIGFjYWTDqW1pY28uIFNlZ8O6biBsYSBSZWFsIEFjYWRlbWlhIEVzcGHDsW9sYSAoUkFFKSwgcGxhZ2lhciBlcyBsYSBhY2Npw7NuIGRlIGNvcGlhciBlbiBsbyBzdXN0YW5jaWFsIG9icmFzIGFqZW5hcyBkw6FuZG9sYXMgY29tbyBwcm9waWFzLiBFbiBlbCBjYXNvIGRlIHRleHRvcywgcGxhZ2lhciBlcyBsYSBhY2Npw7NuIGRlIGF0cmlidWlyc2UgbGEgYXV0b3LDrWEgZGUgZnJhZ21lbnRvcyBvIGlkZWFzIGN1eWEgcHJvcGllZGFkIGludGVsZWN0dWFsIG5vIGxlIGNvcnJlc3BvbmRlIGFsIGF1dG9yLiANCg0KRGViaWRvIGFsIGF1bWVudG8gZGUgZXN0ZSBwcm9ibGVtYSwgaGFuIHN1cmdpZG8gZGlmZXJlbnRlcyBzb2x1Y2lvbmVzIHBvciBwYXJ0ZSBkZWwgbXVuZG8gZW1wcmVzYXJpYWwgeSBkZWwgZWR1Y2F0aXZvLiBFbnRyZSBvdHJhcywgZWwgZGVzYXJyb2xsbyBkZSBzaXN0ZW1hcyBwYXJhIGF5dWRhciBhIGxvcyBlZHVjYWRvcmVzIGVuIGxhIGFyZHVhIHRhcmVhIGRlIGlkZW50aWZpY2FyIHRyYWJham9zIHBsYWdpYWRvcywgbyBpbmNsdXNvIGVsIGRlc2Fycm9sbG8gZGUgc2lzdGVtYXMgcXVlIHNvbiBjYXBhY2VzIGRlIGRldGVjdGFyIGVsIHBsYWdpbyBhdXRvbcOhdGljYW1lbnRlLg0KDQpVbm8gZGUgbG9zIHByb2JsZW1hcyBkZSBlc3RvcyBzaXN0ZW1hcyBkZSBkZXRlY2Npw7NuIGRlIHBsYWdpbyBlcyBsYSBmYWx0YSBkZSBkYXRvcyBwYXJhIGVudHJlbmFyIGxvcyBtb2RlbG9zIHlhIHF1ZSBubyBoYXkgbXVjaG9zIHJlcG9zaXRvcmlvcyBjb24gdGV4dG9zIHF1ZSBlc3TDqW4gYWRtaXRpZG9zIGNvbW8gcGxhZ2lhZG9zLiBFc3RvIHNlIGRlYmUgYSBsYSBpbGVnYWxpZGFkIGRlIHB1YmxpY2FyIHRleHRvcyBwbGFnaWFkb3MuIFBvciBlc28sIGVuIGVzdGUgdHJhYmFqbyB1c2FyZW1vcyB1biBjb3JwdXMgZGUgdGV4dG8gcGxhZ2lhZG8gcXVlIGhhIHNpZG8gc2ltdWxhZG8gcG9yIGxvcyBpbnZlc3RpZ2Fkb3JlcyB5IHF1ZSBlc3TDoSBkaXNwb25pYmxlIHBhcmEgdXNvIGxpYnJlLiANCg0KIyBUZW9yaWENCg0KDQpFbiBlc3RlIGFwYXJ0YWRvIGRlc2Fycm9sbGFtb3MgbG9zIGNvbmNlcHRvcyB5IHTDqWNuaWNhcyBuZWNlc2FyaWFzIHBhcmEgbGxldmFyIGEgY2FibyBtw6FzIHRhcmRlIGVuIHVuIGNhc28gcHLDoWN0aWNvIGxhIGNvbnN0cnVjY2nDs24gZGUgdW4gZGV0ZWN0b3IgZGUgcGxhZ2lvLiBFc3RhIHRlb3LDrWEgZXN0YSBzdWJkaXZpZGlkYSBlbiBsb3MgZGlmZXJlbnRlcyBwYXNvcyBhIHNlZ3VpciBlbiBsYSBwcsOhY3RpY2EgcGFyYSBlbCB0cmF0YWRvIGRlIGxvcyBkYXRvcyB5IGxhIGNvbnN0cnVjY2nDs24geSBldmFsdWFjacOzbiBkZWwgbW9kZWxvLg0KDQpBdGVuZGllbmRvIGxvcyBkaWZlcmVudGVzIHRpcG9zIGRlIHBsYWdpbyB5IGxvcyBtw6l0b2RvcyB1c2Fkb3MgcGFyYSBzdSBkZXRlY2Npw7NuLCBwb2RlbW9zIHJlYWxpemFyIGRpc3RpbnRhcyBjbGFzaWZpY2FjaW9uZXMuIEEgY29udGludWFjacOzbiBzZSBleHBvbmVuIGFsZ3VuYXMgZGUgZWxsYXMgW0B0aXBvczFdIFtAdGlwb3MyXS4gDQpfTm90YTogVGFtYmnDqW4gZXMgbXV5IGltcG9ydGFudGUgZGlmZXJlbmNpYXIgcmVzcGVjdG8gYWwgb2JqZXRvIGRlIGVzdHVkaW8uIFNpIHNlIGVzdMOhIGFuYWxpemFuZG8gcGxhZ2lvIGVuIGPDs2RpZ28sIGxhcyB0w6ljbmljYXMgbm8gc2Vyw6FzIGlndWFsIHF1ZSBzaSBlc3RhbW9zIHRyYXRhbmRvIGNvbiB1biBhcnTDrWN1bG8gY2llbnTDrWZpY28uIE51ZXN0cm8gaW50ZXLDqXMgc2UgY2VudHJhIGVuIHRleHRvIHBsYW5vIHkgbm8gZW4gY8OzZGlnby5fDQoNCkxhIGNsYXNpZmljYWNpw7NuIG1lbm9zIHJlbGV2YW50ZSBwYXJhIG51ZXN0cm8gdHJhYmFqbyBzZXLDrWEgbGEgcXVlIGF0aWVuZGUgYSBsYSBpbnRlbmNpw7NuIGRlIHF1aWVuIGVzY3JpYmUgZWwgdGV4dG8uIERpY2hhIGNsYXNpZmljYWNpw7NuIHBvZHLDrWEgc2VyIFtAaW50ZW5jaW9uXToNCg0KIVtDbGFzaWZpY2FjacOzbiBkZWwgcHJhZ2lvIHNlZ8O6biBsYSBpbnRlbmNpw7NuIGRlbCBlc2NyaXRvcl0oZGlhZ3JhbWFfcGxhZ2lvX2ludGVuY2lvbi5wbmcpDQoNClVuYSBkZSBsYXMgY2xhc2lmaWNhY2lvbmVzIHF1ZSBzZSBoYWNlbiBtw6FzIGNvbcO6bm1lbnRlIHNvYnJlIGVsIHBsYWdpbywgc29uIGxhcyBkZSBwbGFnaW8gaW50csOtbnNlY28geSBlbCBwbGFnaW8gZXh0csOtbnNlY28uDQoqIFBsYWdpbyBpbnRyw61uc2VjbzogU2UgaGFibGEgZGUgcGxhZ2lvIGludHLDrW5zZWNvIGN1YW5kbyBlbiB1biBtaXNtbyBkb2N1bWVudG8gbyB0ZXh0bywgY2FtYmlhIGVsIGVzdGlsbyBkZSByZWRhY2Npw7NuIHkgc2UgZGlzdGluZ3VlbiBkaWZlcmVudGVzIGF1dG9yZXMuIFN1IG5vbWJyZSB2aWVuZSBkZWwgaGVjaG8gcXVlIG5vIHNlIHJlcXVpZXJlbiBkZSBiYXNlcyBkZSBkYXRvcyBuaSBmdWVudGVzIGV4dGVybmFzIHBhcmEgbGEgY29tcGFyYWNpw7NuLiBDb21vIGNvbnNlY3VlbmNpYSwgZGUgZGljaG8gbcOpdG9kbyBzb2xvIHBvZHJlbW9zIHNhY2FyIHVuYSBzb3NwZWNoYSBkZWwgcGxhZ2lvLCBwdWVzIG5vIGNvbnRhcmVtb3MgY29uIGxhIGZ1ZW50ZSBkZWwgcGxhZ2lvLg0KDQoqIFBsYWdpbyBleHRyw61uc2VjbzogRG9uZGUgbG9zIHJlY3Vyc29zIG9yaWdpbmFsZXMgbyBmdWVudGUgZGVsIHBsYWdpbyBzb24gdXNhZG9zIHkgY29tcGFyYWRvcyBjb24gZWwgdGV4dG8gYSBhbmFsaXphci4gRWwgcGxhZ2lvIG5vIHNvbG8gcHVlZGUgdmVuaXIgZGUgdW5hIGNvcGlhIHRleHR1YWwgZW50cmUgZG9jdW1lbnRvIG9yaWdpbmFsIHkgZWplbXBsYXIgKGxsYW1hZG8gX3BsYWdpbyBsaXRlcmFsXyBvIF9jb3BpYS1wZWdhXyksIHNpIHNlIHVzYW4gc2luw7NuaW1vcyB5IHNlIHJlZXNjcmliZW4gbGFzIG9yYWNpb25lcyBkZSBtYW5lcmEgc2ltaWxhciwgZGViZXJlbW9zIGF0ZW5kZXIgYSBsYSBzaW1pbGl0dWQgZW50cmUgYW1ib3MgZG9jdW1lbnRvcyAoZXN0ZSB0aXBvIGRlIHBsYWdpbyBlcyBsbGFtYWRvIF9wbGFnaW8gaW50ZWxpZ2VudGVfKS4gRXN0YSBzaW1pbGl0dWQgcHVlZGUgc2VwYXJhcnNlIGVuOg0KICAqIFNpbWlsaXR1ZCBTZW3DoW50aWNhOiBDb25zaWRlcmFuZG8gZWwgdXNvIGRlIHNpbsOzbmltb3MsIHNlIGJ1c2NhIGxhIHNpbWlsaXR1ZCBkZSBsYSBlc3RydWN0dXJhIHBvciAqKnBhbGFicmFzKiouDQogICogU2ltaWxpdHVkIFNpbnTDoWN0aWNhOiBTZSBidXNjYSB1bmEgc2ltaWxpdHVkIHBvciBvcmFjaW9uZXMsIGRvbmRlIHNlIGxlIGRhIHJlbGV2YW5jaWEgYSBzaW1pbGl0dWQgZW50cmUgb3JhY2lvbmVzIGEgbml2ZWwgc2ludMOhY3RpY28uDQogDQohW0NsYXNpZmljYWNpw7NuIGRlbCBwbGFnaW9dKHBsYWdpb190YXgucG5nKQ0KIA0KIyMgQ29ycHVzDQpVbiBjb3JwdXMgbGluZ8O8w61zdGljbyBlcyB1biBjb25qdW50byBkZSB0ZXh0b3MgdmFyaWFkb3MgcXVlIGN1bXBsZSBsYSBmdW5jacOzbiBkZSByZXByZXNlbnRhciBlbCB1c28gcmVhbCBkZSBsYSBsZW5ndWEgcXVlIHNlIHF1aWVyZSBleGFtaW5hci4gRW4gZWwgY2FzbyBkZWwgcHJvY2VzYW1pZW50byBkZSB0ZXh0byBlcyBuZWNlc2FyaW8gY29udGFyIGNvbiB1biBjb3JwdXMgYWRlY3VhZG8gYWwgY2FtcG8geSBjYXJhY3RlcsOtc3RpY2FzIHF1ZSBzZSBidXNjYW4gZXh0cmFlci4gRW4gbnVlc3RybyBjYXNvIGhhY2Vtb3MgdXNvIGRlbCBjb3JwdXMgcHJvcG9yY2lvbmFkbyBwb3IgbWllbWJyb3MgZGUgbGEgdW5pdmVyc2lkYWQgZGUgU2hlZmZpZWxkIFtAY29ycHVzX3NvdXJjZV0uIENvbnN0YSBkZSB1biBzZXQgZGUgNSBjdWVzdGlvbmVzIHF1ZSB2ZWludGUgYWx1bW5vcyB0dXZpZXJvbiBxdWUgcmVzcG9uZGVyLiBMb3MgYWx1bW5vcyBzZSBkaXZpZGllcm9uIGVuIGN1YXRybyBncnVwb3MgcXVlIHBvZMOtYW4gcmVzcG9uZGVyIGxhcyBwcmVndW50YXMgY29uIG1hdGVyaWFsZXMgZXh0ZXJub3MgbyBjb25zdWx0YW5kbyBww6FnaW5hcyBkZSBXaWtpcGVkaWEsIHBlcm1pdGnDqW5kb3NlIG5pdmVsZXMgZGlmZXJlbnRlcyBkZSBjb3BpYSB5IHJlZmVyZW5jaWEuIERlIGVzdGEgZm9ybWEgdGVuZW1vcyBkaWZlcmVudGVzIGdyYWRvcyBkZSBwbGFnaW8gY29uIGVsIGRvY3VtZW50byBvcmlnaW5hbCwgcXVlIHZhbiBkZWwgY29ydGEgeSBwZWdhIGFsIG5vIHVzbyBkZSBsYSBmdWVudGUsIHkgdW5hIGbDoWNpbCBjb21wYXJhY2nDs24uIEFkZW3DoXMgZGUgdG9kYXMgZXN0YXMgcmVzcHVlc3RhcywgdGFtYmnDqW4gc2UgcHJvcG9yY2lvbmEgdW4gw61uZGljZSBjb24gZWwgYWx1bW5vIHF1ZSBlc2NyaWJpw7MgY2FkYSB0ZXh0bywgY3VhbCBlcmEgbGEgcHJlZ3VudGEgb3JpZ2luYWwgeSBlbCBuaXZlbCBkZSBjb3BpYSAobmVhciBjb3B5LCBsaWdodCByZXZpc2lvbiwgaGVhdnkgcmV2aXNpb24sIG5vbi0gcGxhZ2lhcmlzbSkgZGViaWRhbWVudGUgaW5kZXhhZG8uIEVOIHJlc3VtaWRhcyBjdWVudGFzLCBudWVzdHJvIGNvcnB1cyBjdWVudGEgY29uIDEwMCB0ZXh0b3MgYnJldmVzIChlbnRyZSAyMDAgeSAzMDAgcGFsYWJyYXMpIGNvbiBkaWZlcmVudGVzIG5pdmVsZXMgZGUgcGxhZ2lvLCBkZSBtYW5lcmEgcXVlIHNvbiBsbyBzdWZpY2llbnRlbWVudGUgbGFyZ29zIHBhcmEgcmVhbGl6YXIgdW4gYW7DoWxpc2lzIGRpZMOhY3RpY28gc2luIGNvbnZlcnRpcnNlIGVuIHVuYSBjYXJnYSBkaWbDrWNpbCBkZSBwcm9jZXNhciBjb24gbnVlc3Ryb3MgbWVkaW9zLg0KDQojIyBQcmVwcm9jZXNhZG8gICANCg0KRWwgcHJlcG9yY2VzYWRvIGVzIGVsIHByaW1lciBjb250YWN0byBjb24gbG9zIGRhdG9zLCBsYXMgcHJpbWVyYXMgdGFyZWFzIGRlIGxpbXBpZXphIHkgZm9ybWF0ZW8gZGUgbG9zIGRhdG9zIHN1ZWxlbiBzZXIgcG9yIGxvIGdlbmVyYWwgc2VuY2lsbGFzIHkgZW4gc3UgbWF5b3LDrWEgZ2Vuw6lyaWNhcy4gDQoNClByaW1lcm8gZXhwb25lbW9zIGxhcyBmdW5jaW9uZXMgYsOhc2ljYXMgcGFyYSBlc3RlIHByb3DDs3NpdG86DQpgYGB7cn0NCiNsaWJyZXLDrWFzIHBhcmEgY2FyZ2FyIGxvcyBkYXRvcyB5IHByZXByb2Nlc2FybG9zIA0KDQp0ZXh0bzwtIiDCoSBFc3RvIGVzICAgIHVuIHRleHRvIGRlIHBydWViYSBwYXJhIGRlbW9zdHJhciBsYSAgIGxpbXBpZXphIGluY2ljaWFsIGRlIGxvcyB0ZXh0b3MhICjDocOhw6nDp8OxKSINCiNwYXNhcmxvIGEgbWluw7pzY3VsYXMNCnRleHRvPC10b2xvd2VyKHRleHRvKSANCnRleHRvDQpgYGANCg0KDQpgYGB7cn0NCiNlbGltaW5hciBzaWdub3MgZGUgcHVudHVhY2nDs24sIGNhcsOhY3RlcmVzIHByb2JsZW3DoXRpY29zDQp0ZXh0bzwtZ3N1YigiW1s6cHVuY3Q6XV0iLCIgIix0ZXh0bykNCnRleHRvPC1nc3ViKCIoXFxuKXwoXFx0KSIsIiAiLHRleHRvKQ0KdGV4dG8NCg0KI3kgY29tcHJvYmFyIHF1ZSBubyBoYXkgZXNwYWNpb3MgcmVwZXRpZG9zIHkgYWwgcHJpbmNpcGlvIHkgZmluYWwgZGVsIHN0cmluZyAoc3RyX3RyaW0oKSkNCnRleHRvPC1nc3ViKCJcXHMrIiwiICIsc3RyX3RyaW0odGV4dG8pKQ0KI2xhIGZ1bmNpw7NuIHN0cl9zcXVpc2goKSBkZSBsYSBsaWJyZXLDrWEgc3RyaW5nciBoYWNlIGV4YWN0YW1lbnRlIA0KI2VsaW1pbGFyIGxvcyBlc3BhY2lvcyBlbiBibGFuY28gYWwgY29taWVuem8geSBhbCBmaW5hbCBkZWwgc3RyaW5nDQp0ZXh0bw0KYGBgDQoNCkVuIGVsIGNhc28gZGUgcXVlIGVsIGlkaW9tYSB0ZW5nYSBhY2VudHVhY2nDs24sIHNlcsOtYSByZWNvbWVuZGFibGUgZWxpbWluYXJsYSBwYXJhIG5vIHRlbmVyIHByb2JsZW1hcyBkZSBmb3JtYXRvIG5pIGNvbnNpZGVyYXIgZGlmZXJlbnRlcyBwYWxhYnJhcyBpZ3VhbGVzIHNvbG8gcG9yIGVzdGFyIGFjZW50dWFkYSBvIG5vLiBFc3RvIGxvIHBvZGVtb3MgaGFjZXIgZGUgZm9ybWEgc2VuY2lsbGEgdXNhbmRvIHN0cmluZ2k6OnN0cmlfdHJhbnNfZ2VuZXJhbCgpDQpgYGB7cn0NCg0KdGV4dG88LXN0cmluZ2k6OnN0cmlfdHJhbnNfZ2VuZXJhbCh0ZXh0bywiTGF0aW4tQVNDSUkiKSANCiMgZXN0ZSBwcm9jZXNvIHNlcsOhIG11eSBkZXBlbmRpZW50ZSBkZWwgaWRpb21hIGRlbCB0ZXh0bw0KdGV4dG8NCmBgYA0KDQoNClVuYSBsaW1waWV6YSBtw6FzIGludGVuc2l2YSBwb2Ryw61hIHNlZ3VpciBjb24gbGEgZWxpbWluYWNpw7NuIGRlIHBhbGFicmFzIHVuacOzbiBgc3RvcC13b3Jkc2AgeSBoYXN0YSBsYSByZWR1Y2Npw7NuIGRlIHBhbGFicmFzIGhhc3RhIHN1IHJhw616ICh0w6lybWlubyBfc3RlbW1pbmdfIGVuIGluZ2zDqXMpLlBhcmEgZXN0ZSBwcm9ww7NzaXRvIGhheSBsaWJyZXLDrWFzIHF1ZSBpbXBsZW1lbnRhbiBmdW5jaW9uZXMgcGFyYSBkaWNobyBwcm9wb3NpdG8gZW4gUiwgYWxndW5hcyBkZSBlbGxhcyBzb24gYHRtYCB5IGB0aWR5dGV4dGAuICANClNpbiBlbWJhcmdvIGVzdGEgc29icmUtc2ltcGxpZmljYWNpw7NuIHB1ZWRlIG5vIHNlciBsbyBtw6FzIHJlY29tZW5kYWRvIGVuIHRvZG9zIGxvcyBjYXNvcy4gTGEgZWxlY2Npw7NuIHNlcsOhIG11eSBkZXBlbmRpZW50ZSBkZWwgYWxnb3JpdG1vIHkgZGVsIHRpcG8gZGUgcGxhZ2lvIGJ1c2NhZG8gKHlhIGhlbW9zIGNvbWVudGFkbyBwcmV2aWFtZW50ZSBxdWUgaGF5IGRpZmVyZW50ZXMgdGlwb3MgZGUgcGxhZ2lvIGEgZGV0ZWN0YXIpLg0KDQpgYGB7cn0NCiNlbGltaW5hciBzdG9wLXdvcmRzIGNvbiBsYSBsaWJyZXLDrWEgdG0NCg0KdGV4dG8yPC1yZW1vdmVXb3Jkcyh0ZXh0byxzdG9wd29yZHMoImVzIikpDQp0ZXh0bzI8LXN0cl9zcXVpc2godGV4dG8yKQ0KdGV4dG8yDQoNCg0KYGBgDQoNCmBgYHtyfQ0KI3N0ZW1taW5nIHBhbGFicmFzIGNvbiBsYSBsaWJyZXJpYSB0bQ0KdGV4dG8yPC11bmxpc3Qoc3RyX3NwbGl0KHRleHRvMiwiICIpKQ0KdGV4dG8zPC1zdGVtRG9jdW1lbnQodGV4dG8yLGxhbmd1YWdlID0gImVzIikNCnRleHRvMw0KYGBgDQoNCg0KIyMgVG9rZW5pemFkbw0KDQpVbmEgdmV6IGltcG9ydGFkbyBlbCBjb3JwdXMgeSBkZXB1cmFkbyBsb3MgZGF0b3MsIGVsIHNpZ3VpZW50ZSBwYXNvIGVzIHRva2VtaXphcmxvcy4gRXN0byBjb25zaXN0ZSBlbiBzZXBhcmFyIGVuIGVsZW1lbnRvcyBtw6FzIHBlcXVlw7FvcyBlbCBjb25qdW50byBkZSBkYXRvcyBxdWUgdGVuZW1vcy4gSGFibGFuZG8gZGUgZGF0b3MgZGUgdGlwbyB0ZXhvLCB0b2tlbWl6YXIgcG9kcsOtYSBzZXIgc2VwYXJhciBlbiBwYWxhYnJhcyBvIGVuIGZyYXNlcyBwb3IgZWplbXBsby4gTGEgZnVuY2nDs24gYHN0cnNwbGl0KClgIG5vcyBwZXJtaXRpcsOtYSBoYWNlciB2YXJpYW5kbyBlbCBhcmd1bWVudG8gZGUgc2VwYXJhY2nDs24sIGBzcGxpdGAgIHNlZ8O6biBub3MgY29udmVuZ2EuDQoNCiAgLSBgc3BsaXQ9IiAiYCB1biBlc3BhY2lvIHBvZHLDrWEgc2VydmlyIHBhcmEgc2VwYXJhciBlbiBwYWxhYnJhcy4gDQogIC0gYHNwbGl0PSIuImAgdW4gcHVudG8gcG9kcsOtYSBzZXJ2aXIgcGFyYSBzZXBhcmFyIGVuIGZyYXNlcy4gDQoNClNpbiBlbWJhcmdvLCBlbiBlc3RlIGNhc28gbm9zIHZhIGEgaW50ZXJlc2FyIHVzYXIgTi1ncmFtYXMsIHF1ZSBzb24gc3ViY29uanVudG9zIGRlbCBjb25qdW50byBvcmlnaW5hbCBxdWUgdmFuIGNvZ2llbmRvIGNvbWJpbmFjaW9uZXMgZGUgbiBlbGVtZW50b3MgeSBkw7NuZGUgZWwgZWxlbWVudG8gc2lndWllbnRlIGVzdMOhIGZvcm1hZG8gcG9yIGxvcyAobi0xKSBlbGVtZW50b3MgZGVsIGFudGVyaW9yIG3DoXMgdW4gZWxlbWVudG8gbnVldm8uIFBvciBlamVtcGxvLCBzaSB0ZW5lbW9zIGVsIHRleHRvOiAiSG95IGhlbW9zIHF1ZWRhZG8gZW4gbGEgcnVzYSBhIGxhcyBzZWlzIi4gU3UgZGl2aXNpw7NuIGVuIDMtZ3JhbWFzIChvIHRyaWdyYW1hcykgc2Vyw61hOiANCg0KW0hveSBoZW1vcyBxdWVkYWRvXSwgW2hlbW9zIHF1ZWRhZG8gZW5dLCBbcXVlZGFkbyBlbiBsYV0sIFtlbiBsYSBydXNhXSxbbGEgcnVzYSBhXSwgW3J1c2EgYSBsYXNdLFthIGxhcyBzZWlzXS4gDQoNCkVuIFIsIHNlIHB1ZWRlIGhhY2VyIGbDoWNpbG1lbnRlIHVzYW5kbyBlbCBwYXF1ZXRlIGBuZ3JhbWAsIHkgbcOhcyBjb25jcmV0YW1lbnRlIGxhIGZ1bmNpw7NuIGB1bm5lc3RfdG9rZW5zKClgLCBxdWUgdG9tYSBjb21vIGFyZ3VtZW50b3MgcHJpbmNpcGFsZXMgbG9zIHNpZ3VpZW50ZXM6IA0KDQogIC0gYHRibGAgdW4gZGF0YSBmcmFtZQ0KICAtIGBvdXRwdXRgIGVsIG5vbWJyZSBkZSBsYSBjb2x1bW5hIHF1ZSBzZSB2YSAgY3JlYXINCiAgLSBgaW5wdXRgIGxvcyBkYXRvcyBxdWUgcXVlcmVtb3MgdXNhci4NCiAgLSBgdG9rZW5gIGEgcXXDqSB0b2tlbiBxdWVyZW1vcyBjb252ZXJ0aXJsb3MgKGVuIGVzdGUgY2FzbyBlbCBxdWUgbcOhcyBub3MgaW50ZXJlc2EgZXMgIm5ncmFtcyIpDQogIC0gYG5gIGFyZ3VtZW50byBhZGljaW9uYWwgcGFyYSBuLWdyYW1hcyBwYXJhIGluZGljYXIgZWwgdGFtYcOxbyBkZSDDqXN0b3MuDQogIA0KTG9zIE4tZ3JhbWFzIHNvbiBtdXkgdXRpbGl6YWRvcyBlbiBhcGxpY2FjaW9uZXMgZGUgYW7DoWxpc2lzIGRlIHRleHRvLCBjb21vIHBvciBlamVtcGxvIGVuIGxhIGRldGVjY2nDs24gZGUgcGxhZ2lvLiBMYSBzaW1pbGl0dWQgZW50cmUgZG9zIHRleHRvcyBzZSBwdWVkZSBjYWxjdWxhciBjb250YW5kbyBlbCBuw7ptZXJvIGRlIE4tZ3JhbWFzIHF1ZSB0aWVuZW4gZW4gY29tw7puLg0KDQoNCiMjIE1ldHJpY2FzIHkgbW9kZWxvcw0KDQpMYXMgbcOpdHJpY2FzIHkgbW9kZWxvcyB1dGlsaXphZG9zIGEgbGEgaG9yYSBkZSBldmFsdWFyIGxvcyBuaXZlbGVzIGRlIHBsYWdpbyBlbnRyZSB0ZXh0b3MsIHlhIHNlYW4gZGVsIG1pc21vIGF1dG9yLCBvIGRlIHZhcmlvcywgc2UgZGViZW4gZGl2aWRpciBlbiB2YXJpb3MgZ3J1cG9zLCBkZXBlbmRpZW5kbyBkZSBsYSBwcmV2aWEgdG9rZW5pemFjacOzbiBvIGFncnVwYWNpw7NuIGRlIGxhcyBwYWxhYnJhcyBvIGxldHJhcyBkZWwgY29ycHVzLg0KDQojIyMgTcOpdHJpY2FzIGJhc2FkYXMgZW4gc2ltaWxpdHVkZXMgZGUgdG9rZW5lcw0KDQpVbmEgZGUgbGFzIGZvcm1hcyBtw6FzIHV0aWxpemFkYXMgYSBsYSBob3JhIGRlIGNvbXBhcmFyIGJsb3F1ZXMgZGUgdGV4dG8gZXMgbGEgZGUgY29tcGFyYWNpb25lcyB2ZWN0b3JpYWxlcywgZW4gbGFzIHF1ZSwgcG9yIGVqZW1wbG8sIGNhZGEgcGFsYWJyYSBzZSBjb252aWVydGUgZW4gZWwgw61uZGljZSBkZSB1biB2ZWN0b3IsIHkgbGEgY2FudGlkYWQgZGUgdmVjZXMgcXVlIGFwYXJlY2UgZXMgc3UgZXNjYWxhci4gVXRpbGl6YW5kbyBlc3RlIHByb2Nlc28sIHRhbWJpZW4gc2UgcHVlZGUgZXh0cmFwb2xhciBkZSBtYW5lcmEgcXVlIGVuIHZleiBkZSBjb250YXIgbGFzIHBhbGFicmFzLCBzZSBjdWVudGUgbG9zIHBhcmVzIG8gdHJpb3MgZGUgcGFsYWJyYXMsIGxvIHF1ZSB2ZW5kcmlhIGEgc2VyIHNlcGFyYWNpb24gZW4gbi1ncmFtYXMoYmlncmFtYXMgbyB0cmlncmFtYXMpLg0KDQpTb24gYmFzdGFudGUgZWZpY2llbnRlcyBlbiBnZW5lcmFsLCB5IGZ1bmNpb25hbiBiaWVuIHBhcmEgdGV4dG9zIGxhcmdvcyBhIGRpZmVyZW5jaWEgZGUgbG9zIG90cm9zIHRpcG9zIGRlIG3DqXRyaWNhcy4NCg0KUGFyYSBlc3RhIHRva2VuaXphY2nDs24sIGxhcyBtw6l0cmljYXMgbWFzIHV0aWxpemFkYXMsIHNvbiBsYXMgZGUgbWVkaWRhIGRlIGRpc3RhbmNpYSBlbnRyZSB1biB0ZXh0byB5IG90cm8sIGNvbXBhcmFuZG8gdmVjdG9yIGEgdmVjdG9yLg0KDQotVW5hIHByaW1lcmEgbcOpdHJpY2Egc2VyaWEgbGEgZGUgKipKYWNjYXJkKiouDQoNCkVzdGEgbcOpdHJpY2EgdHJhdGEgZGUgbWVkaXIgbG9zIGVsZW1lbnRvcyBxdWUgY29tcGFydGVuIGFtYm9zIHZlY3RvcmVzLg0KDQokRF97amFjY2FyZH1cbGVmdCAoIFgsWSBccmlnaHQgKT0xLSBcZnJhY3tcc3VtIHhfe2l9XGNhcCB5X3tpfSB9e1xzdW0geF97aX1cY3VwIHlfe2p9fSQNCg0KRW4gdMOpcm1pbm9zIGdlbmVyYWxlcywgY29uc2lzdGUgZW4gZGl2aWRpciBlbCB2b2NhYnVsYXJpbyBjb23Dum4gZGUgbG9zIGRvY3VtZW50b3MgZW50cmUgZWwgdm9jYWJ1bGFyaW8gZGUgbGEgdW5pw7NuIGRlIGFtYm9zIGRvY3VtZW50b3MsIHkgbGUgcmVzdGFtb3MgMSBwYXJhIHF1ZSBkb3MgdmVjdG9yZXMgc2VhbiBwYXJlY2lkb3MgY3VhbmRvIGVzdGFuIGNlcmNhIGRlIDAgeSBsZWphbm9zIGN1YW5kbyBzZSBhY2VyY2FuIGEgMS4NCg0KLUxhIHNpZ3VpZW50ZSBtw6l0cmljYSBlcyBtdXkgcGFyZWNpZGEgeWEgcXVlIGVzIGxhICoqSmFjY2FyZCBwZXNhZGEqKi4NCkVuIGVzdGEgbcOpdHJpY2Egc2UgdGllbmUgZW4gY3VlbnRhIHRhbWJpZW4gbGEgY2FudGlkYWQgcG90ZW5jaWFsIGRlIGNvaW5jaWRlbmNpYXMuDQoNCiREX3t3amFjY2FyZH1cbGVmdCggWCxZXHJpZ2h0ICk9MS0gXGZyYWN7XHN1bSBtaW5cbGVmdCAoIHgseSBccmlnaHQgKSB9e1xzdW0gbWF4KHgseSl9JA0KDQotTcOpdHJpY2EgZGUgKipTb3JlbnNlbioqLg0KDQokRF97c29yZW5zZW59XGxlZnQgKCBYLFkgXHJpZ2h0ICk9MS0gXGZyYWN7Mipcc3VtIHhfe2l9XGNhcCB5X3tpfSB9e1xzdW0geF97an1cY2FwIDErXHN1bSB5X3trfVxjYXAgMX0kDQoNCkNvbnNpc3RlIGVuIGRpdmlkaXIgbGEgY2FudGlkYWQgZGUgcGFsYWJyYXMgY29tdW5lcyBzZSBtdWx0aXBsaWNhbiBwb3IgMiwgeWEgcXVlIHNlIHJlcGl0ZW4gYWwgc2VyIGNvbXVuZXMuIEVzdG8gc2UgZGl2aWRlIHBvciB0b2RhcyBsYXMgcGFsYWJyYXMgdGlwbyBkZSBhbWJvcyBkb2N1bWVudG9zLg0KDQotQ29uICoqSmFjY2FyZCoqIHkgKipTb3JlbnNlbioqLCBhcGFyZWNlIHVuIHByb2JsZW1hIHkgZXMgcXVlIHNpIHVubyBkZSBsb3MgZG9zIHRleHRvcyBhIGNvbXBhcmFyIHRpZW5lIHVuIHRhbWHDsW8gbXVjaG8gbWF5b3IgcXVlIG90cm8sIGxhcyBkaXN0YW5jaWFzIHRpZW5kZW4gc2llbXByZSBhIGNlcm8sIHBvciBlbGxvICoqT3ZlcmxhcCoqIHRyYXRhIGRlIGRpc21pbnVpciBlc3RlIHByb2JsZW1hIGFsIGRpdmlkaXJsbyBwb3IgZWwgdm9jYWJ1bGFyaW8gbcOhcyBwZXF1ZcOxbyBxdWUgY29ycmVzcG9uZGUgYSB1bm8gZGUgbG9zIGRvY3VtZW50b3MuDQoNCiREX3tvdmVybGFwfVxsZWZ0ICggWCxZIFxyaWdodCApPTEtIFxmcmFjezIqXHN1bSB4X3tpfVxjYXAgeV97aX0gfXttaW4oXHN1bSB4X3tqfVxjYXAgMStcc3VtIHlfe2t9XGNhcCAxKX0kDQoNCi1MYSBtw6l0cmljYSAqTWFzaSogKE1lYXN1cmluZyBBZ3JlZW1lbnQgb24gU2V0LXZhbHVlZCBJdGVtcykgc2lndWUgdW4gcHJvY2VzbyBwYXJlY2lkbyBhbCBvdmVybGFwIHBlcm8gdGllbmUgZW4gY3VlbnRhIGVsIG1heW9yLg0KDQokRF97bWFzaX1cbGVmdCAoIFgsWSBccmlnaHQgKT0xLSBcZnJhY3syKlxzdW0geF97aX1cY2FwIHlfe2l9IH17bWF4KFxzdW0geF97an1cY2FwIDErXHN1bSB5X3trfVxjYXAgMSl9JA0KDQoNCiMjIyBNw6l0cmljYXMgYmFzYWRhcyBlbiBzaW1pbGl0dWRlcyBkZSBlZGljacOzbg0KDQpFc3RhcyBtw6l0cmljYXMgZW4gZ2VuZXJhbCBzZSBiYXNhbiBlbiBjb21wYXJhciBwYWxhYnJhcyBtZWRpYW50ZSBlbCBuw7ptZXJvIGRlIHRyYXNuZm9ybWFjaW9uZXMgcXVlIGhheSBxdWUgcmVhbGl6YXIgcGFyYSBsbGVnYXIgZGUgdW5hIGEgb3RyYS4NClNlIHN1ZWxlbiB1dGlsaXphciBwYXJhIGNvbXBhcmFyIGRpc3RhbmNpYXMgbGluZ3Vpc3RpY2FzIGVudHJlIGRpZmVyZW50ZXMgaWRpb21hcywgZHVwbGljYWNpw7NuIGRlIHRleHRvcyB5IGNvcnJlY2nDs24gZGUgbMOpeGljby4gTGFzIG3DoXMgdXRsaXphZGFzIHNvbjoNCg0KLURpc3RhbmNpYSBkZSAqKkhhbW1pbmcqKi4NCg0KQ29tcGFyYSBjYWRhIGxldHJhIGRlIGRvcyBwYWxhYnJhcyBiYXNhZGFzIHBvciBzdSBwb3NpY2nDs24sIHN1IHZlbnRhamEgZXMgc3UgdmVsb2NpZGFkIHkgc2ltcGxpY2lkYWQsIHBvciBlbCBjb250cmFyaW8sIGVzIG11eSBlc3RyaWN0bywgeWEgcXVlIG5lY2VzaXRhIHF1ZSBhbWJhcyBwYWxhYnJhcyBzZWFuIGRlbCBtaXNtbyB0YW1hw7FvLg0KDQotRGlzdGFuY2lhIGRlICoqTGV2ZW5zaHRlaW4qKi4NCg0KQ29tcGFyYSBlbCBuw7ptZXJvIGRlIHRyYW5zZm9ybWFjaW9uZXMgbmVjZXNhcmlhcyBwYXJhIHRyYW5zZm9ybWFyIHVuYSBwYWxhYnJhIGVuIG90cmEsIGVzdGFzIHRyYW5zZm9ybWFjaW9uZXMgc2UgcmVzdW1lbiBlbjoNCiAgICANCiAgICAtSW5zZXJ0YXIgY2FyYWN0ZXJlcy4NCiAgICAtRWxpbWluYXIgY2FyYWN0ZXJlcy4NCiAgICAtU3VzdGl0dWlyIHVuIGNhcmFjdGVyIHBvciBvdHJvLg0KICAgIA0KRXN0YSBkaXN0YW5jaWEgZW4gZ2VuZXJhbCBlcyBtw6FzIGNvbXBsZXRhIHkgY29tcGxlamEgcXVlIGxhIGRpc3RhbmNpYSBkZSBIYW1taW5nLg0KDQojIyMgTcOpdHJpY2FzIGJhc2FkYXMgZW4gbGFzIHNlY3VlbmNpYXMgZGUgcGFsYWJyYXMNCg0KQ29tcGFyYSBsYXMgZGlmZXJlbmNpYXMgZW50cmUgZG9zIHBhbGFicmFzLCB0ZW5pZW5kbyBlbiBjdWVudGEgbGFzIHN1Yi1wYWxhYnJhcyBvIHN1Yi1jYWRlbmFzIGRlIHBhbGFicmFzIG3DoXMgbGFyZ2FzIGRlbnRybyBkZSBsYXMgcGFsYWJyYXMgaW5jaWFsZXMuIChMb25nZXN0IGNvbW1vbiBzdWJzZXF1ZW5jZSBhbmQgbG9uZ2VzdCBjb21tb24gc3Vic3RyaW5nKQ0KDQpMYSBkaWZlcmVuY2lhIGLDoXNpY2EgZW50cmUgbGFzIHN1Yi1wYWxhYnJhcyB5IHN1Yi1jYWRlbmFzIGRlIHBhbGFicmFzLCByZXNpZGUgZW4gcXVlIGVsIHByaW1lcm8gdXRpbGl6YSBsZXRyYXMgZGVudHJvIGRlIGxhIHBhbGFicmEgc2luIG5lY2VzaWRhZCBkZSBxdWUgc2VhbiBjb250aWd1YXMsIHkgZWwgc2VndW5kbyBzaSBpbXBsaWNhIHF1ZSBlc3TDqSB1bmEgY29udGlndWEgYSBsYSBvdHJhLg0KUG9yIGVqZW1wbG8gZW50cmUgbGFzIHBhbGFicmFzICdwYWxhYnJhcycgeSAncGFyYWxlbGFzJywgY29uIGVsIG3DqXRvZG8gZGUgc3ViLXBhbGFicmFzIHRlbmRyw61hbiBlbiBjb23Dum4gOiAncGFsYXMnLCBtaWVudHJhcyBxdWUgY29uIGVsIG3DqXRvZG8gZGUgc3ViLWNhZGVuYXMgc2Vyw61hOiAncGFsYScgeSAncGFyYScuDQoNCiMjIEV2YWx1YWNpw7NuIGRlIGxvcyByZXN1bHRhZG9zDQoNCiMgQ2FzbyBwcsOhdGljbw0KDQpFbiBlc3RlIGFwYXJ0YWRvLCB2YW1vcyBhIGFwbGljYXIgbG9zIHByb2NlZGltaWVudG9zIGRlc2NyaXRvcyBhIGxvIGxhcmdvIGRlbCB0cmFiYWpvIGEgdW4gY2FzbyByZWFsIGRlIGRldGVjY2nDs24gZGUgcGxhZ2lvLCBkZXNjcml0byBlbiBlbCBhcGFydGFkbyBkZWwgQ29ycHVzLiAgDQoNClBhcmEgZWwgYW7DoWxzaXMsIG5vc290cm9zIGxvIHF1ZSB0ZW5kcmVtb3Mgc2Vyw6EgdW4gZGF0YSBmcmFtZSBjb24gMTAwIG11ZXN0cmFzLGRlIGxhcyBjdWFsZXMgbGFzIDk1IHByaW1lcmFzIG11ZXN0cmFzIHNvbiBsYSByZXNwdWVzdGEgYSB1bmEgZGUgbGFzIHByZWd1bnRhcyBwb3IgcGFydGUgZGUgbG9zIGFsdW1ub3MsIHkgbGFzIDUgw7psdGltYXMgbXVlc3RyYXMgc29uIGxvcyBlbmxhY2VzIGEgV2lraXBlZGlhIHF1ZSBwb2TDrWFuIGNvbnN1bHRhciBwYXJhIHJlc3BvbmRlciBhIGNhZGEgdW5hIGRlIGxhcyA1IHByZWd1bnRhcy4gQWxndW5hcyBkZSBsYXMgY29sdW1uYXMgZGUgZXNlIGRhdGEgZnJhbWUgc2Vyw6FuIGVsIHRpcG8gZGUgcGxhZ2lvIGNvbiBlbCBxdWUgc2UgcGVkw61hIHJlc3BvbmRlciwgbGEgcHJlZ3VudGEgcXVlIHNlIHJlc3BvbmRlLCBlbCBpbmRpdmlkdW8gcXVlIGxhIHJlc3BvbmRlIHkgbGEgcmVzcHVlc3RhIGVuIGZvcm1hIGRlIGNhZGVuYSBkZSB0ZXh0by4NCg0KVW5hIHZleiBxdWUgdGVuZW1vcyBlbCBkYXRhIGZyYW1lLCBsbyBwcmltZXJvIHF1ZSBoYWNlbW9zIGVzIGRlcHVyYXIgbGEgY29sdW1uYSBxdWUgY29udGllbmUgbGEgcmVzcHVlc3RhIHRhbCB5IGNvbW8gc2UgaGEgZXhwbGljYWRvIGVuIGVsIGFwYXJ0YWRvIGRlIHByZXByb2Nlc2FkbyBkZSBsb3MgZGF0b3MuIEVuIHBhcnRpY3VsYXIsIGRlc2Fycm9sbGFtb3MgdW5hIGZ1bmNpw7NuIGBhZ3J1cGEoKWAgcXVlIGVsaW1pbmEgbG9zIHNpZ25vcyBkZSBwdW50dWFjacOzbiB5IGNvbnZpZXJ0ZSB0b2RvIGEgbWluw7pzY3VsYXMuIFVuYSB2ZXogaGVjaG8gZXN0bywgZGl2aWRpbW9zIGNhZGEgdW5hIGRlIGxhcyByZXNwdWVzdGFzIGVuIE4tZ3JhbWFzLCBlbiBjb25jcmV0byBoZW1vcyBjb2dpZG8gTj0zIHlhIHF1ZSBzZSBoYSB2aXN0byBxdWUgZXMgbGEgcXVlIG1lam9yIGZ1bmNpb25hLg0KDQpgYGB7cixpbmNsdWRlPUZBTFNFfQ0KI01pZ3VlbCBIb3J0ZWxhbm8gMi8xMS8yMDIxIw0KbGlicmFyeSh0aWR5cikNCmxpYnJhcnkodGlkeXRleHQpDQpsaWJyYXJ5KGRwbHlyKQ0KbGlicmFyeShuZ3JhbSkNCg0KIyMjIyBmdW5jaW9uZXMgIyMjIw0KIyBEZXZ1ZWx2ZSBlbCB0ZXh0byBpbnRyb2R1Y2lkbyBlbiBmb3JtYSBkZSBsaXN0YSBkZSBOZ3JhbWFzIA0KYWdydXBhIDwtZnVuY3Rpb24odGV4dCl7DQogIHRlbXAgPC11bmxpc3QodGV4dCkNCiAgdGVtcHRlbXAgPC0gdGVtcFt0ZW1wICE9ICIiXQ0KICANCiAgdGVtcDIgPC0gdGVtcFsxXQ0KICBmb3IoaSBpbiAyOmxlbmd0aCh0ZW1wKSl7DQogICAgdGVtcDIgPC0gcGFzdGUodGVtcDIsdGVtcFtpXSkNCiAgfQ0KICB0ZW1wMiA8LSBnc3ViKCdbWzpwdW5jdDpdIF0rJywnICcsdGVtcDIpJT4lDQogICAgdG9sb3dlcigpDQogICMlPiUNCiAgIyAgc3Ryc3BsaXQoc3BsaXQ9IlxccyIpDQogIA0KICB0ZW1wIDwtIHRpYmJsZSh3b3JkID0gdW5saXN0KHRlbXAyKSklPiUNCiAgICB1bm5lc3RfdG9rZW5zKHRyaWdyYW0sIHdvcmQsdG9rZW4gPSAibmdyYW1zIixuPTMpI2NhbWJpYXIgZWwgbsO6bWVybyBwYXJhIHRhbWHDsW8gZGVsIG5ncmFtYQ0KfQ0KYGBgDQoNCg0KVW5hIHZleiB0ZW5lbW9zIGNhZGEgcmVzcHVlc3RhIGRpdmlkaWRhIGVuIHRyaWdyYW1hcywgcG9kZW1vcyBwcm9jZWRlciBhIGFwbGljYXIgYWxndW5hcyBkZSBsYXMgbcOpdHJpY2FzIGRlc2NyaXRhcyBlbiBlbCB0cmFiYWpvIHBhcmEgdmVyIHNpIGhhY2VuIGJ1ZW5hcyBwcmVkaWNjaW9uZXMuIEhlbW9zIGVzY29naWRvIGxhcyBtw6l0cmljYXMgZGUgSmFjY2FyZCwgZGlzdGFuY2lhIExDUyB5IGxhIG3DqXRyaWNhIGRlICdjb250YWlubWVudCcuIEEgY29udGludWFjacOzbiwgbW9zdHJhbW9zIHVuYSBwZXF1ZcOxYSB0YWJsYSBkw7NuZGUgcG9kZW1vcyB2ZXIgZWwgcG9yY2VudGFqZSBkZSBwbGFnaW8gcXVlIG5vcyBicmluZGEgY2FkYSB1bmEgZGUgbGFzIG3DqXRyaWNhcywgY3VhbmRvIGNvbXByb2JhbW9zIGVsIG5pdmVsIGRlIHBsYWdpbyBlbnRyZSB0b2RhcyBsYXMgcmVzcHVlc3RhcyBkZSBsYSBwcmltZXJhIHByZWd1bnRhIChpbmNsdWlkYSBsYSBmdWVudGUgZGUgV2lraXBlZGlhKS4gUG9yIGxvIHRhbnRvLCB0ZW5kcmVtb3MgdW5hIHBvcmNlbnRhamUgZGUgcGxhZ2lvIHBhcmEgbGFzIDIwIHJlc3B1ZXN0YXMgZGVsIGRhdGEgZnJhbWUuDQoNCg0KYGBge3IsaW5jbHVkZT1GQUxTRX0NCg0KDQojIEN1ZW50YSBsYXMgdmVjZXMgcXVlIHNlIHJlcGl0ZW4gbGFzIHBhbGFicmFzIGVudHJlIGRvcyB0ZXh0b3MNCmNvbXBhcmEgPC0gZnVuY3Rpb24ob3JpZ2luYWwsIHNvc3BlY2hvc28pew0KICANCiAgY29pbmMgPC0gYygpDQogIGNvaW5jMiA8LSBjKCkNCiAgDQogIG9yIDwtIHRpYmJsZSh0cmlncmFtID0gdW5saXN0KG9yaWdpbmFsKSkNCiAgc3VzIDwtIHRpYmJsZSh0cmlncmFtID0gdW5saXN0KHVubGlzdChzb3NwZWNob3NvKSkpDQogIA0KICBzdXMgPC0gc2VwYXJhdGUoc3VzLHRyaWdyYW0sYygiUDEiLCJQMiIsIlAzIiksc2VwPSIgIikNCiAgb3IgPC0gc2VwYXJhdGUob3IsdHJpZ3JhbSxjKCJQMSIsIlAyIiwiUDMiKSxzZXA9IiAiKQ0KICANCiAgZm9yKCBpIGluIDE6bGVuZ3RoKHN1cyRQMSkpeyNhcXXDrSBlcyBkb25kZSBoYXkgcXVlIHBvbmVyIHVuIG1vZGVsbyBkZSB2ZXJkYWQsIHVuYSBkaXN0YW5jaWEgbyBtw6l0cmljYQ0KICAgIGNvdW50IDwtIDA7DQogICAgY291bnQyIDwtIDANCiAgICBzdXNfc3RyIDwtIGMoc3VzW2ksXSRQMSxzdXNbaSxdJFAyLHN1c1tpLF0kUDMpDQogICAgDQogICAgZm9yKCBqIGluIDE6bGVuZ3RoKG9yJFAxKSl7DQogICAgICBvcl9zdHIgPC0gYyhvcltqLF0kUDEsb3JbaixdJFAyLG9yW2osXSRQMykNCiAgICAgIGNvdW50IDwtIGNvdW50ICsgYWxsKG9yX3N0ciA9PSBzdXNfc3RyKQ0KICAgICAgY291bnQyIDwtIGNvdW50MiArIHN1bShvcl9zdHI9PXN1c19zdHIpDQogICAgfQ0KICAgIA0KICAgIGNvaW5jIDwtIGFwcGVuZChjb2luYyxjb3VudCkgICAgICNzaW1wbGVtZW50ZSB1biB2ZWN0b3IgY29uIGVsIG7Dum1lcm8gZGUgdmVjZXMgcXVlIHNlIGhhIHJlcGV0aWRvIGNhZGEgcGFsYWJyYSBkZWwgbmdyYW1hDQogICAgY29pbmMyIDwtIGFwcGVuZChjb2luYzIsY291bnQyKQ0KICAgIHNjb3JlIDwtIHN1bShjb2luYykvbGVuZ3RoKHN1cyRQMSkNCiAgICBzYWxpZGEgPC0gYyhzY29yZSxjb2luYzIpDQogIH0NCiAgcmV0dXJuKHNhbGlkYSkNCn0NCg0KY29tcGFyYV9kamFiYmFyZCA8LSBmdW5jdGlvbihvcmlnaW5hbCwgc29zcGVjaG9zbyl7DQogIA0KICBvciA8LSB1bmxpc3Qob3JpZ2luYWwpDQogIHN1cyA8LSB1bmxpc3Qoc29zcGVjaG9zbykNCiAgDQogIGRqIDwtIDEgLSBsZW5ndGgoaW50ZXJzZWN0KHN1cyxvcikpL2xlbmd0aCh1bmlvbihzdXMsb3IpKQ0KICByZXR1cm4oZGopDQp9DQoNCiMjIyMgTGltcGllemEgZGUgbG9zIGRhdG9zICMjIyMNCmxvYWQoZmlsZSA9ICJjb3JwdXNfYXNfZGF0YWZyYW1lLlJkYSIpDQpUYSA8LSBjb3JwdXNbY29ycHVzJFRhc2s9PSJhIixdDQoNCg0KI0xhIGNvbHVtbmEgdGV4dG8gYWhvcmEgZXN0w6EgZW4gZm9ybWEgZGUgbmdyYW1hcw0KVGEkVGV4dCA8LSBsYXBwbHkoVGEkVGV4dCxGVU4gPSBhZ3J1cGEpDQoNCg0KIyMjIyBjb21wYXJhbW9zIE5ncmFtcyAjIyMNCk9yaWdpbmFsIDwtIHVubGlzdChUYSRUZXh0WzIwXSkNClRhJGRqYWNjYXJkIDwtIGxhcHBseShUYSRUZXh0LEZVTiA9IGNvbXBhcmFfZGphYmJhcmQsb3JpZ2luYWwgPSBPcmlnaW5hbCkgI29idGVuZW1vcyBsaXN0YSBjb24gbsO6bWVybyBkZSBjb25jaWRlbmNhIHBvciBuZ3JhbWENCmF1eDIgPC0gbGFwcGx5KFRhJFRleHQsRlVOID0gY29tcGFyYSxvcmlnaW5hbCA9IE9yaWdpbmFsKQ0KZm9yIChpIGluIDE6bGVuZ3RoKFRhJFRleHQpKXsNCiAgVGEkY29udGFpbm1lbnRbaV0gPC0gYXV4MltbaV1dWzFdDQogIFRhJENvaW5jaWRlbmNpYXNfY29udGFpbm1lbnRbaV0gPC0gbGlzdChhdXgyW1tpXV1bMjogbGVuZ3RoKGF1eDJbW2ldXSldKQ0KfQ0KVmlldyhUYSkNCg0KIyB2aXN1YWxpemFtb3MgcGFzYW5kbyBsb3MgdmVjdG9yZXMgZGUgY29pbmNpZGVuY2lhcyBhIG1hdHJpY2VzIHkgaGFjaWVuZG8gaGVhdG1hcA0KVGEkQ2NtYXRyaXggPC0gbGFwcGx5KFRhJENvaW5jaWRlbmNpYXNfY29udGFpbm1lbnQsIG1hdHJpeCwgbmNvbCA9IDIwKQ0KDQpzYXBwbHkoVGEkQ2NtYXRyaXgsaGVhdG1hcCkNCmhlYXRtYXAoVGEkQ2NtYXRyaXhbWzJdXSxSb3d2PU5BLENvbHY9TkEpDQpgYGANCg0KDQogDQoNCmBgYHtyLGluY2x1ZGU9RkFMU0V9DQoNCiMgUFJFR1VOVEEgQiANCg0KVGIgPC0gY29ycHVzW2NvcnB1cyRUYXNrPT0iYiIsXQ0KI0xhIGNvbHVtbmEgdGV4dG8gYWhvcmEgZXN0w6EgZW4gZm9ybWEgZGUgbmdyYW1hcw0KVGIkVGV4dCA8LSBsYXBwbHkoVGIkVGV4dCxGVU4gPSBhZ3J1cGEpDQojIyMjIGNvbXBhcmFtb3MgTmdyYW1zICMjIw0KT3JpZ2luYWwgPC0gdW5saXN0KFRiJFRleHRbMjBdKQ0KVGIkZGphY2NhcmQgPC0gbGFwcGx5KFRiJFRleHQsRlVOID0gY29tcGFyYV9kamFiYmFyZCxvcmlnaW5hbCA9IE9yaWdpbmFsKSAjb2J0ZW5lbW9zIGxpc1RiIGNvbiBuw7ptZXJvIGRlIGNvbmNpZGVuY2EgcG9yIG5ncmFtYQ0KYXV4MiA8LSBsYXBwbHkoVGIkVGV4dCxGVU4gPSBjb21wYXJhLG9yaWdpbmFsID0gT3JpZ2luYWwpDQpmb3IgKGkgaW4gMTpsZW5ndGgoVGIkVGV4dCkpew0KICBUYiRjb250YWlubWVudFtpXSA8LSBhdXgyW1tpXV1bMV0NCiAgVGIkQ29pbmNpZGVuY2lhc19jb250YWlubWVudFtpXSA8LSBsaXN0KGF1eDJbW2ldXVsyOiBsZW5ndGgoYXV4MltbaV1dKV0pDQp9DQpWaWV3KFRiKQ0KDQoNCnZhcmlhYmxlcyA8LSBjKCJUYXNrIiwiQ2F0ZWdvcnkiLCJkamFjY2FyZCIsImNvbnRhaW5tZW50IikNCnRhYmxhX3Jlc3VtZW5fYiA8LSBUYlssdmFyaWFibGVzXQ0Ka2FibGUodGFibGFfcmVzdW1lbl9iKQ0KYGBgDQoNCg0KDQpgYGB7cixpbmNsdWRlPUZBTFNFfQ0KDQojIFBSRUdVTlRBIEMNCg0KVGMgPC0gY29ycHVzW2NvcnB1cyRUYXNrPT0iYyIsXQ0KI0xhIGNvbHVtbmEgdGV4dG8gYWhvcmEgZXN0w6EgZW4gZm9ybWEgZGUgbmdyYW1hcw0KVGMkVGV4dCA8LSBsYXBwbHkoVGMkVGV4dCxGVU4gPSBhZ3J1cGEpDQojIyMjIGNvbXBhcmFtb3MgTmdyYW1zICMjIw0KT3JpZ2luYWwgPC0gdW5saXN0KFRjJFRleHRbMjBdKQ0KVGMkZGphY2NhcmQgPC0gbGFwcGx5KFRjJFRleHQsRlVOID0gY29tcGFyYV9kamFiYmFyZCxvcmlnaW5hbCA9IE9yaWdpbmFsKSAjb2J0ZW5lbW9zIGxpc1RiIGNvbiBuw7ptZXJvIGRlIGNvbmNpZGVuY2EgcG9yIG5ncmFtYQ0KYXV4MiA8LSBsYXBwbHkoVGMkVGV4dCxGVU4gPSBjb21wYXJhLG9yaWdpbmFsID0gT3JpZ2luYWwpDQpmb3IgKGkgaW4gMTpsZW5ndGgoVGMkVGV4dCkpew0KICBUYyRjb250YWlubWVudFtpXSA8LSBhdXgyW1tpXV1bMV0NCiAgVGMkQ29pbmNpZGVuY2lhc19jb250YWlubWVudFtpXSA8LSBsaXN0KGF1eDJbW2ldXVsyOiBsZW5ndGgoYXV4MltbaV1dKV0pDQp9DQpWaWV3KFRjKQ0KDQoNCnZhcmlhYmxlcyA8LSBjKCJUYXNrIiwiQ2F0ZWdvcnkiLCJkamFjY2FyZCIsImNvbnRhaW5tZW50IikNCnRhYmxhX3Jlc3VtZW5fYyA8LSBUY1ssdmFyaWFibGVzXQ0Ka2FibGUodGFibGFfcmVzdW1lbl9jKQ0KYGBgDQoNCg0KDQpgYGB7cixpbmNsdWRlPUZBTFNFfQ0KDQojIFBSRUdVTlRBIEQNCg0KVGQgPC0gY29ycHVzW2NvcnB1cyRUYXNrPT0iZCIsXQ0KI0xhIGNvbHVtbmEgdGV4dG8gYWhvcmEgZXN0w6EgZW4gZm9ybWEgZGUgbmdyYW1hcw0KVGQkVGV4dCA8LSBsYXBwbHkoVGQkVGV4dCxGVU4gPSBhZ3J1cGEpDQojIyMjIGNvbXBhcmFtb3MgTmdyYW1zICMjIw0KT3JpZ2luYWwgPC0gdW5saXN0KFRkJFRleHRbMjBdKQ0KVGQkZGphY2NhcmQgPC0gbGFwcGx5KFRkJFRleHQsRlVOID0gY29tcGFyYV9kamFiYmFyZCxvcmlnaW5hbCA9IE9yaWdpbmFsKSAjb2J0ZW5lbW9zIGxpc1RiIGNvbiBuw7ptZXJvIGRlIGNvbmNpZGVuY2EgcG9yIG5ncmFtYQ0KYXV4MiA8LSBsYXBwbHkoVGQkVGV4dCxGVU4gPSBjb21wYXJhLG9yaWdpbmFsID0gT3JpZ2luYWwpDQpmb3IgKGkgaW4gMTpsZW5ndGgoVGQkVGV4dCkpew0KICBUZCRjb250YWlubWVudFtpXSA8LSBhdXgyW1tpXV1bMV0NCiAgVGQkQ29pbmNpZGVuY2lhc19jb250YWlubWVudFtpXSA8LSBsaXN0KGF1eDJbW2ldXVsyOiBsZW5ndGgoYXV4MltbaV1dKV0pDQp9DQpWaWV3KFRkKQ0KDQoNCnZhcmlhYmxlcyA8LSBjKCJUYXNrIiwiQ2F0ZWdvcnkiLCJkamFjY2FyZCIsImNvbnRhaW5tZW50IikNCnRhYmxhX3Jlc3VtZW5fZCA8LSBUZFssdmFyaWFibGVzXQ0Ka2FibGUodGFibGFfcmVzdW1lbl9kKQ0KYGBgDQoNCg0KDQpgYGB7cixpbmNsdWRlPUZBTFNFfQ0KDQojIFBSRUdVTlRBIEUNCg0KVGUgPC0gY29ycHVzW2NvcnB1cyRUYXNrPT0iZSIsXQ0KI0xhIGNvbHVtbmEgdGV4dG8gYWhvcmEgZXN0w6EgZW4gZm9ybWEgZGUgbmdyYW1hcw0KVGUkVGV4dCA8LSBsYXBwbHkoVGUkVGV4dCxGVU4gPSBhZ3J1cGEpDQojIyMjIGNvbXBhcmFtb3MgTmdyYW1zICMjIw0KT3JpZ2luYWwgPC0gdW5saXN0KFRlJFRleHRbMjBdKQ0KVGUkZGphY2NhcmQgPC0gbGFwcGx5KFRlJFRleHQsRlVOID0gY29tcGFyYV9kamFiYmFyZCxvcmlnaW5hbCA9IE9yaWdpbmFsKSAjb2J0ZW5lbW9zIGxpc1RiIGNvbiBuw7ptZXJvIGRlIGNvbmNpZGVuY2EgcG9yIG5ncmFtYQ0KYXV4MiA8LSBsYXBwbHkoVGUkVGV4dCxGVU4gPSBjb21wYXJhLG9yaWdpbmFsID0gT3JpZ2luYWwpDQpmb3IgKGkgaW4gMTpsZW5ndGgoVGUkVGV4dCkpew0KICBUZSRjb250YWlubWVudFtpXSA8LSBhdXgyW1tpXV1bMV0NCiAgVGUkQ29pbmNpZGVuY2lhc19jb250YWlubWVudFtpXSA8LSBsaXN0KGF1eDJbW2ldXVsyOiBsZW5ndGgoYXV4MltbaV1dKV0pDQp9DQpWaWV3KFRlKQ0KDQoNCnZhcmlhYmxlcyA8LSBjKCJUYXNrIiwiQ2F0ZWdvcnkiLCJkamFjY2FyZCIsImNvbnRhaW5tZW50IikNCnRhYmxhX3Jlc3VtZW5fZSA8LSBUZVssdmFyaWFibGVzXQ0Ka2FibGUodGFibGFfcmVzdW1lbl9lKQ0KYGBgDQoNCg0KDQpgYGB7cn0NClRhIDwtIFRhWywtMTNdDQpWaWV3KFRhKQ0KVF9hYmNkZSA8LSByYmluZChUYSxUYixUYyxUZCxUZSkNClZpZXcoVF9hYmNkZSkNCg0Kc2F2ZShUX2FiY2RlLGZpbGU9Ii4vUHVudHVhY2lvbmVzLlJkYXRhIikNCmBgYA0KYGBge3J9DQojQ0FSR0FNT1MgTE9TIERBVE9TDQoNCmxvYWQoIlB1bnR1YWNpb25lcy5SZGF0YSIpDQpWaWV3KFRfYWJjZGUpDQpgYGANCg0KYGBge3J9DQojaW5zdGFsbC5wYWNrYWdlcygia2FibGVFeHRyYSIpDQpsaWJyYXJ5KCJrYWJsZUV4dHJhIikNCnZhcmlhYmxlcyA8LSBjKCJUYXNrIiwiQ2F0ZWdvcnkiLCJkamFjY2FyZCIsImNvbnRhaW5tZW50IikNCnRhYmxhX3Jlc3VtZW4gPC0gVF9hYmNkZVssdmFyaWFibGVzXQ0Ka2FibGUodGFibGFfcmVzdW1lbikNCmBgYA0KDQpFbiBsYSB0YWJsYSBwb2RlbW9zIHZlciBlbCBwb3JjZW50YWplIGRlIHBsYWdpbyBxdWUgc2UgbGUgYXNpZ25hIGEgbGEgcmVzcHVlc3RhIGVuIGZ1bmNpw7NuIGRlIGxhIG3DqXRyaWNhIHVzYWRhLiBFbiBnZW5lcmFsLCB2ZW1vcyBxdWUgdG9kYXMgZnVuY2lvbmFuIGJhc3RhbnRlIGJpZW4geSBub3MgZGFuIHVuIHJlc3VsdGFkbyBhY29yZGUgY29uIGxhIGNhdGVnb3LDrWEsIHF1ZSByZWNvcmRlbW9zIHF1ZSBoYWNlIHJlZmVyZW5jaWEgYWwgdGlwbyBkZSBwbGFnaW8gcXVlIGhhYsOtYW4gdXNhZG8gbG9zIGFsdW1ub3MgcGFyYSBjb250ZXN0YXIgYSBsYSBwcmVndW50YS4NCg0KVmVhbW9zIGxvIGJ1ZW5vIHF1ZSBlcyBudWVzdHJvIG1vZGVsbyBwYXJhIGxhIHRhcmVhICJhIi4gQWhvcmEgdmFtb3MgYSBzdXBvbmVyIHF1ZSB0ZW5lbW9zIHNvbG8gZG9zIGNhdGVnb3LDrWFzOiBQTEFHSUFETyB5IE5PIFBMQUdJQURPLiBEZW50cm8gZGUgbGEgY2F0ZWdvcsOtYSBQTEFHSUFETyBlc3TDoW4gaW5jbHVpZGFzIGxhcyBjYXRlZ29yw61hcyAiY3V0IiwgImxpZ2h0IiB5ICJoZWF2eSIuIFZhbW9zIGEgZXZhbHVhciBlbnRvbmNlcyBsb3MgYWNpZXJ0b3MgZGVsIG1vZGVsbyBlbiBmdW5jacOzbiBkZSBsYXMgZG9zIG3DqXRyaWNhcyB1dGlsaXphZGFzLiBQYXJhIGVsbG8sIGNvbnNpZGVyYW1vcyBxdWUgdW5hIG3DqXRyaWNhIGRlICJjb250YWlubWVudCIgc3VwZXJpb3IgYSAwLjUwIHNlIGNvbnNpZGVyYSBkZW50cm8gZGUgbGEgY2F0ZWdvcsOtYSBQTEFHSUFETywgeSBzaSBvYnRlbmVtb3MgdW4gImNvbnRhaW5tZW50IiBwb3IgZGViYWpvIGRlIDAuNSBjb25zaWRlcmFyZW1vcyBxdWUgZXN0YW1vcyBlbiBsYSBjYXRlZ29yw61hIE5PIFBMQUdJQURPLg0KDQpgYGB7cn0NCiNpbnN0YWxsLnBhY2thZ2VzKCJjYXJldCIpDQpsaWJyYXJ5KCJjYXJldCIpDQoNCmZvciAoaSBpbiAxOmRpbShUX2FiY2RlKVsxXSl7DQogIGlmIChUX2FiY2RlJGNvbnRhaW5tZW50W2ldPDAuNSl7DQogICAgVF9hYmNkZSRQcmVkaWNjaW9uW2ldIDwtICJOTyBQTEFHSU8iDQogIH0gZWxzZXsNCiAgICBUX2FiY2RlJFByZWRpY2Npb25baV0gPC0gIlBMQUdJTyINCiAgfQ0KfQ0KVmlldyhUX2FiY2RlKQ0KDQpmb3IgKGkgaW4gMTpkaW0oVF9hYmNkZSlbMV0pew0KICBpZihUX2FiY2RlJENhdGVnb3J5W2ldPT0ibm9uIil7DQogICAgVF9hYmNkZSRSZWFsaWRhZFtpXSA8LSAiTk8gUExBR0lPIg0KICB9IGVsc2V7DQogICAgVF9hYmNkZSRSZWFsaWRhZFtpXSA8LSAiUExBR0lPIg0KICB9DQp9DQoNClRfYWJjZGUkUmVhbGlkYWQgPC0gZmFjdG9yKFRfYWJjZGUkUmVhbGlkYWQpDQoNClRfYWJjZGUkUHJlZGljY2lvbiA8LSBmYWN0b3IoVF9hYmNkZSRQcmVkaWNjaW9uKQ0KDQpjb25mdXNpb25NYXRyaXgoVF9hYmNkZSRSZWFsaWRhZCxUX2FiY2RlJFByZWRpY2Npb24pDQpgYGANCg0KT2JzZXJ2YW1vcyBxdWUgdGllbmUgdW4gYWNjdXJhY3kgZGVsIDc4LjM2JSwgZXMgZGVjaXIsIHVuIGFjaWVydG8gZGUgY2FzaSBlbCA4MCUuIEVzIHVuYSBtZWRpZGEgYmFzdGFudGUgYWNlcHRhYmxlLCBwb3IgbG8gcXVlIHBvZGVtb3MgY29uY2x1aXIgcXVlIG51ZXN0cm8gcGVxdWXDsW8gbW9kZWxvIG5vcyBzaXJ2ZSBwYXJhIGRldGVjdGFyIHBsYWdpbyBjb24gY2llcnRhIGZpYWJpbGlkYWQgZW4gcmVzcHVlc3RhcyBjb3J0YXMgZXNjcml0YXMgcG9yIGFsdW1ub3MuIFRhbWLDqW4gcG9kZW1vcyB2aXN1YWxpemFyIGxhIHNpbWlsaXR1ZCBkZSBsb3MgdGV4dG9zIGdyw6FmaWNhbWVudGUgY29uIGVzdGEgbcOpdHJpY2EuIEVuIGxhIHNpZ3VpZW50ZSBmaWN1cmEgY2FkYSBjYXNpbGxhIGVzIHVuIG5ncmFtYSwgZGlzcHVlc3RvIGRlIGxhIG1hbmVyYSBlbiBxdWUgZXN0YXLDrWEgZW4gdW4gdGV4dG8gZXNjcml0bywgeSBlbCBjb2xvciByZXByZXNlbnRhIGxhIHNpbWlsaXR1ZCBlbmNvbnRyYWRhIGRlIGVzdGUgbmdyYW1hIGVuIGVsIHRleHRvIG9yaWdpbmFsLiBTZSBhcHJlY2lhbiBsYXMgZGlmZXJlbmNpYXMgZW50cmUgdW4gdGV4dG8gY29uIGZyYWdtZW50b3MgY29waWFkbyB5IHBlZ2FkbyAoaXpxdWllcmRhKSwgeSBvdHJvIHNpbiBwbGFnaW8gKGRlcmVjaGEpLg0KIVtpbWFnZV0oY29udnNzaW4uanBnKQ0KDQojIEJpYmxpb2ZyYWdpYQ0K</div>



</div>

<script>

// add bootstrap table styles to pandoc tables
function bootstrapStylePandocTables() {
  $('tr.odd').parent('tbody').parent('table').addClass('table table-condensed');
}
$(document).ready(function () {
  bootstrapStylePandocTables();
});

$(document).ready(function () {
  $('.knitsql-table').addClass('kable-table');
  var container = $('.kable-table');
  container.each(function() {

    // move the caption out of the table
    var table = $(this).children('table');
    var caption = table.children('caption').detach();
    caption.insertBefore($(this)).css('display', 'inherit');
  });
});

</script>

<!-- tabsets -->

<script>
$(document).ready(function () {
  window.buildTabsets("TOC");
});

$(document).ready(function () {
  $('.tabset-dropdown > .nav-tabs > li').click(function () {
    $(this).parent().toggleClass('nav-tabs-open');
  });
});
</script>

<!-- code folding -->
<script>
$(document).ready(function () {
  window.initializeSourceEmbed("informe_final_V1.Rmd");
  window.initializeCodeFolding("show" === "show");
});
</script>


<!-- dynamically load mathjax for compatibility with self-contained -->
<script>
  (function () {
    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src  = "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML";
    document.getElementsByTagName("head")[0].appendChild(script);
  })();
</script>

</body>
</html>
