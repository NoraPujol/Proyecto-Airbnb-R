


####CARGA DE PAQUETES####
if(!require("readxl")) {
  install.packages("readxl")
  library("readxl")
}

if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}
#readxl - paquete para leer de excel



#### IMPORTACIÓN DE DATOS####

getwd()
setwd(getwd())

dfLA<-read_excel(path="listings_LosAngeles.xlsx")



save(dfLA,file="LosAngeles.rda") #esto es para guardar una base de datos
load("LosAngeles.rda")
dfLA<-get(load("LosAngeles.rda"))




dfMadrid <- read.csv("listings_Madrid.csv", sep=",", dec=".", quote = "\"'",
                          header=TRUE, skip = 0, na.strings = "NA")

dfMadrid <- read.table("listings_Madrid.csv", sep=",", dec=".", quote = "\"'",
                         header=TRUE, skip = 0, na.strings = "NA")

save(dfMadird,file="Madrid.rda") #esto es para guardar una base de datos
load("Madrid.rda")
dfMadird<-get(load("Madrid.rda"))


#### ESTRUCTURA DE DATOS #### 
#empezar con str

dim(dfNYC) # Dimensiones
nrow(dfNYC) # Número de filas
ncol(dfNYC) # Número de columnas
str(dfNYC)


