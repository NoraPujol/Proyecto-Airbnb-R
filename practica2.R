
##=========================================================================##
##  R para Analytics y Business Intelligence                               ##
##  PRACTICA 2                                                             ##
##  ANÁLISIS DE DATOS DE ALOJAMIENTOS AIRBNB EN BELGICA                    ##
##                                                          Autoras:       ##
##                                                          Nora Pujol     ## 
##                                                          Elsa Olmedo    ##
##=========================================================================##

####__CARGA DE PAQUETES__####

if(!require("readxl")) {
  install.packages("readxl")
  library("readxl")
}

if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}
#readxl - paquete para leer de excel

if(!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")
}

if(!require("corrplot")) {
  install.packages("corrplot")
  library("corrplot")
}

if(!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library("RColorBrewer")
}


if(!require("ggthemes")) {
  install.packages("ggthemes")
  library("ggthemes")
}

if(!require("rworldmap")) {
  install.packages("rworldmap")
  library("rworldmap")
}

if(!require("GGally")) {
  install.packages("GGally")
  library("GGally")
}

if(!require("tm")) {
  install.packages("tm")
  library("tm")
}
if(!require("SnowballC")) {
  install.packages("SnowballC")
  library("SnowballC")
}
if(!require("wordcloud")) {
  install.packages("wordcloud")
  library("wordcloud")
}


#rm(list = ls()) #Para empezar de 0 si tenemos un environment muy cargado

####__IMPORTACIÓN DE DATOS__####

getwd()
setwd(getwd())

dfBel<-read_excel("airbnb_belgium.xlsx")



save(dfBel,file="Belgium.rda") #esto es para guardar una base de datos


####__ESTRUCTURA DE DATOS__#### 
str(dfBel)
dfBel %>% colnames() %>% length() #me da el numero de columnas
#ncol(dfBel) # Número de columnas forma alternativa

dim(dfBel) # Dimensiones
nrow(dfBel) # Número de filas

summary(dfBel)
is.na(dfBel) #indica si tengo espacios sin valores

#eliminar id de estancias duplicadas
table(duplicated(dfBel$room_id)) #no hay habitaciones duplicadas


####__ANÁLISIS__####

##Tipo de habitación####
table(dfBel$room_type) #tabla que me devuelve la frecuencia de cada tipo de alojamiento

#para saber solamente cuantos hay de un tipo usando diplyr
dfBel %>% filter(room_type=="Entire home/apt") %>% nrow() 

#para saber solamente cuantos hay de un tipo en una ciudad especifica: Para probar y verificar el uso de diplyr
dfBel %>% filter(borough=="Virton") %>% nrow()

#Alojamientos por barrio(neighbourhood) 
dfBel %>% select(neighborhood) %>% table()
#table(select(dfBel,neighborhood)) #lo mimso pero sin usar el paquete diplyr

#Representación del tipo de alojamiento en cada ciudad
ggplot(dfBel)+geom_bar(aes(x=borough,fill=room_type)) + coord_flip()+facet_grid(.~room_type) +
  xlab("Ciudades")+ 
  ylab("Frecuencia") + 
  ggtitle("Tipo de alojamiento por ciudad")


#tipo de habitación en funcion de cada ciudad en diagrama de barras 

    #con colores preestablecidos
ggplot(dfBel, aes(x=room_type, fill=borough)) + geom_bar(position="dodge") +
  xlab("Tipo de habitación")+ 
  ylab("Frecuencia") + 
  ggtitle("Tipo de alojamiento por ciudad")

    #no hemos conseguido que nos funcione con colores . Hemos intentado esta opcion de usar una paleta pero todas las paletas tenian
    #un maximo de 12 colores y nosotros necesitabamos 45
#ggplot(dfBel, aes(x=room_type, fill=borough)) + geom_bar(position="dodge") + 
  #scale_fill_gradientn(colours = colorRampPalette(brewer.pal(5,"Paired"), space="Lab")(100)) + theme_classic()

    #Se ha generando también con una paleta de colores propia
paleta <- colorRampPalette(c("red","violet","cyan", "green3", "magenta", "yellow", "pink", "brown"))
ggplot(dfBel, aes(x=room_type, fill=borough)) + geom_bar(position="dodge") + 
  scale_fill_manual(values = paleta(45)) + xlab("Tipo de habitación")+ 
  ylab("Frecuencia") + 
  ggtitle("Tipo de alojamiento por ciudad (paleta manual")



#Relacionar si el nº de huespedes influye en el tipo de apartamento a escoger. 
ggplot(dfBel, aes(x=room_type,y=rooms_per_host, fill=room_type)) +
  geom_jitter(height = 0,alpha = 0.1, shape = 21, size = 1) + 
  geom_boxplot(outlier.shape = NA,alpha=0.5,fill="gold",
               color="gold", width=0.5) + geom_line(col="grey") +  xlab("Tipo de estancia")+ 
  ylab("Número de huéspedes") + 
  ggtitle("Tipo de apartamento por nº de huéspedes alojados")

    #Se ve claramente que sipuesto que para mas de 25 huespedes casi siempre
    #se alquilan casas enteras, y las habitaciones compartidas
    #las alquilan los grupos de huespedes mas pequeños


##Satisfacción####

#diagrama de barras para la cantidad de reviews  según el tipo de habitación

ggplot(dfBel,aes(room_type,overall_satisfaction,fill=room_type)) + geom_col()+
  scale_fill_brewer(palette = "Blues")+
  theme(axis.text.x=element_text(angle=90,hjust=0.5))+
  xlab("Tipo de habitación")+ 
  ylab("suma de puntuación satisfacción") + 
  ggtitle("Satisfaccion según el tipo de habitación")

ggplot(dfBel,aes(room_type,reviews, fill=room_type)) + geom_col()+
  scale_fill_brewer(palette = "Blues")+
  xlab("Tipo de habitación")+ 
  ylab("nº evaluaciones") + 
  ggtitle("nº de evaluaciones según el tipo de habitación")
    #en el entire home esta mas calificado porque ademas hay mas reviews.


#diagrama de barras de frecuencia de calificaciones de segun tipo de apartamento 
display.brewer.all()#para ver las paletas disponibles del paquete brewer.
ggplot(dfBel, aes(x=room_type,  fill=as.factor(overall_satisfaction)))+geom_bar(position="fill") +
  theme() + scale_fill_brewer(palette="RdYlBu") +  
  theme(axis.text.x=element_text(angle=90,hjust=0.5))+
  labs(x="tipo de estancia", y="frecuencia",fill="puntuación")+
  ggtitle("Densidad de cada puntuación según tipo de alojamiento")
  

##Num habitaciones vs num de huespedes en casa completa####

#para comprobar que no hay 0 habitaciones o 0 huespedes se mira el mínimo de cada una
min(dfBel$rooms_per_host)
min(dfBel$accommodates)

#se grafica el numero de habitaciones por huesped con el numero de huespedes, para ver cuando se cruzan las variables. 
table(dfBel$rooms_per_host,dfBel$accommodates)
ggplot(dfBel, aes(x=rooms_per_host, y=accommodates)) + geom_point(alpha=0.1,fill="lightgreen",col="lightgreen") +
  geom_density2d(bins=35, col="darkblue") + xlab("habitaciones por huesped")+ ylab("huéspedes") + 
  ggtitle("Relacion del nº huéspedes y habitaciones por huesped")
    
    #se ponen tambien un geom density 2D pero de todas formas no se ve claro, hacemos un HEAT MAP

##Heat Map habitaciones por huesped#
dfBel %>% group_by(rooms_per_host,accommodates) %>% summarise(recuento=n()) %>%
  ggplot(aes(x=rooms_per_host,y=accommodates,fill=recuento))+
  geom_tile()+
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')(100))+
  xlab("habitaciones por huesped")+ 
  ylab("huéspedes") + 
  ggtitle("Relacion del nº huéspedes y habitaciones por huesped")
  #se observa como la mayor concentración se encuentra en pocas habitacions por persona y pocas personas, es decir, habitaciones individuales o dobles. 

#Relacion entre el numero de dormitorios y el precio del apartamento
ggplot(dfBel, aes(x=bedrooms,y=price))+ 
  geom_density2d(bins=75,alpha=0.2,color="darkblue")+
  geom_rug(alpha=0.1, color="magenta")+ 
  xlab("nº dormitorios")+ 
  ylab("Precio") + 
  ggtitle("Relacion entre el numero de dormitorios y el precio del apartamento")

#Para ir viendo que graficos podemos hacer con diferentes variables, se usa el ggpairs. Entrecruza variables y las muestra en mini gráficos.

ggpairs(dfBel[,c("price","rooms_per_host","room_type","accommodates")], axisLabels = "none") +
  theme_few() #matriz de cruce graficos posibles


##Precios####
#Análisis general
summary(dfBel$price)
max_price<-max(dfBel$price)
min_price<-min(dfBel$price)
  #para tener más datos de las estancias con el precio mayor
dfBel[dfBel$price==max_price,8:16]
dfBel[dfBel$price==min_price,8:16]

##histograma para ver densidad de precios (ayuda con geom_rug)
ggplot(dfBel, aes(x=price)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.1, boundary=0, fill="green",color="black", alpha=0.2)+
  geom_density(color="pink",size=1) + geom_rug(alpha=0.01) + xlab("Precio")+ 
  ylab("Densidad") + 
  ggtitle("Densidad de precios en Bélgica")


##Dividir los datos por precios
df_pa<-dfBel[dfBel$price>500,] #me coge todas las columnas de las observaciones con un precio mayor a 500
df_pb<-dfBel[dfBel$price<=500,] #me coge todas las columnas de las observaciones con un precio menor o igual a 500

#HISTOGRAMA PARA CADA UNO 
  #Precio alto
ggplot(df_pa, aes(x=price)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.1, boundary=0, fill="green",color="black", alpha=0.2)+
  geom_density(color="green4",size=1)+geom_rug(alpha=0.01) + xlab("Precio")+ 
  ylab("Densidad") + 
  ggtitle("Densidad de precios altos en Bélgica")

  #Precio bajo
ggplot(df_pb, aes(x=price)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.1, boundary=0, fill="green",color="black", alpha=0.2)+
  geom_density(color="green4",size=1)+geom_rug(alpha=0.01) + xlab("Precio")+ 
  ylab("Densidad") + 
  ggtitle("Densidad de bajos altos en Bélgica")


#Tipo habitación + Ciudad + Precios####

#Heat map de el precio segun la zona y el tipo de habitación 
display.brewer.all() #se aplica la paleta en "rev" porque así el rojo sale en los mayores precios, que creemos que es mas indicativo y el azul en precios más bajos (buenos)
  
dfBel %>% group_by(room_type,borough) %>% count(price)%>% 
  ggplot(aes(x=room_type,y=borough,fill=price))+
  geom_tile()+
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'RdYlBu')), space='Lab')(100)) +
  theme(axis.text.x=element_text(angle=90,hjust=0.5))+
  xlab("tipo de habitación")+ 
  ylab("ciudad") + 
  ggtitle("Heatmap relacion ciudad y tipo de estancia según precio")

#Heat map de el precio bajo segun la zona ya que el anterior no es claro para los precios bajos
df_pb %>% group_by(room_type,borough) %>% count(price)%>%
  ggplot(aes(x=room_type,y=borough,fill=price))+
  geom_tile()+
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'RdYlBu')), space='Lab')(100)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("tipo de habitación")+ 
  ylab("ciudad") + 
  ggtitle("HHeatmap relacion ciudad y tipo de estancia según precio BAJO")

##Mapa de localización de Bélgica####
#Análisi de las longitudes y latitudes máximas para introducir en la funcion
summary(dfBel$longitude)
summary(dfBel$latitude)

#Mapa de Bélgica y localizacion de apartamentos:
mapWorld <- getMap(resolution = 'low')
ggplot(mapWorld,aes(x=long,y=lat,group=group)) + geom_polygon(fill="white",col="grey") +
  geom_point(data=dfBel,inherit.aes = FALSE,aes(y=dfBel$latitude,x=dfBel$longitude),shape=16,
             col="#4343BB",alpha=0.01,size=5)+coord_equal(xlim=c(2.56,6.36),ylim=c(49.49,51.51))


#___ANÁLISIS TOP 10 CIUDADES(borough)___####

#hacemos una tabla de el top 10 de ciudades con mas apartamento
df_ciudades<-table(dfBel$borough)

df_ordenadas<-sort(df_ciudades, decreasing = TRUE)
top_ciudades <- head(df_ordenadas, n=10L)
top_ciudades <- as.data.frame(top_ciudades)

ciudades10<-top_ciudades$Var1

#generamos un data frame nuevo que contenga las dos variables que nos interesan:

#las 10 top ciudades con sus precios correspondientes
dfTop_1<-dfBel[,c(7,14)]
dfTop_2<-dfTop_1[which(dfTop_1$borough %in% ciudades10),]
unique(dfTop_2$borough)

  #precio medio de cada ciudad
dfTop_2%>%group_by(borough)%>%summarize(mprice=mean(price))

  #diagrama de barras
ggplot(dfTop_2, aes(x=borough)) + geom_bar(fill="#FFA500",col="grey")+  
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Ciudad")+ 
  ylab("Nº apartamentos") + 
  ggtitle("Cantidad de apartamentos por ciudad de las Top10")

#top 10 ciudades con tipo de habitacion + precio. 
Top_room1<-dfBel[,c(4,7,14)]
Top_room2<-Top_room1[which(Top_room1$borough %in% ciudades10),]
unique(Top_room2$room_type)

ggplot(Top_room2, aes(x=borough,y=price,colour=room_type)) + geom_point(alpha=0.5)+
  geom_jitter(width=0.1,height=0.1,shape=21) +  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Ciudad")+ 
  ylab("Precio") + 
  ggtitle("Precio en cada ciudad según el tipo de apartamentos")
#parecido a un gráfico de la subseccion "Tipo de habitación" pero solamente con las 10 ciudades con más alojamientos 



#diagrama de caja para las top 10 ciudades relacionando precio según ciudad
ggplot(dfTop_2, aes(x=borough,y=price,)) + 
  geom_jitter(height = 0,alpha = 0.1, shape = 21, size = 1, fill="lightgreen") +
  geom_boxplot(outlier.shape = NA,notch = TRUE,alpha=0.8,fill="lightblue",color="white", width=0.5) + 
  geom_line(col="grey")+  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Ciudad")+ 
  ylab("Precio") + 
  ggtitle("Precio medio de los apartamentos según ciudad")




##___BRUSELAS__####

#Seleccionar sólo los datos de la ciudad de Bruselas ya que es la que más alojamientos tiene
Bruselas<-dfBel[dfBel$borough=="Brussel",]

#Mapa de Bruselas 
summary(Bruselas$longitude)
summary(Bruselas$latitude)

mapWorld <- getMap(resolution = 'low')
ggplot(mapWorld,aes(x=long,y=lat,group=group)) + geom_polygon(fill="white",col="grey") +
  geom_point(data=Bruselas,inherit.aes = FALSE,aes(y=Bruselas$latitude,x=Bruselas$longitude),shape=16,
             col="#4343BB",alpha=0.01,size=3) + coord_equal(xlim=c(4.26,4.46),ylim=c(50.76,50.91))

#Gráfico en el que se relacione el nº de apartamentos en cada barrio dentro de Bruselas,
# ya que no habíamos utilizado todavía esa variable, con el tipo de apartamento.
ggplot(Bruselas, aes(x=neighborhood,fill=room_type)) + geom_bar() +
  scale_fill_brewer( palette="Set2")+  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Vecindario")+ 
  ylab("Nº apartamentos") + 
  ggtitle("Cantidad y tipo de apartamentos en Bruselas")

#Heat map de Bruselas
Bruselas %>% group_by(room_type,neighborhood) %>% count(overall_satisfaction)%>%
  ggplot(aes(x=room_type,y=neighborhood,fill=overall_satisfaction))+
  geom_tile()+
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'Paired')), space='Lab')(100)) +
  theme(axis.text.x=element_text(angle=90,hjust=0.5))+
  xlab("Tipo de habitación")+ 
  ylab("Barrio") + 
  ggtitle("Heatmap de satisfacción en Bruselas")

#Heat Map de precio y tipo de habitacion por barrios en Bruselas
Bruselas %>% group_by(room_type,neighborhood) %>% summarise(mprice=quantile(price,.9))%>% 
  ggplot(aes(x=room_type,y=neighborhood,fill=mprice))+
  geom_tile()+
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')(100)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Tipo de habitación")+ 
  ylab("Barrio") + 
  ggtitle("Heatmap de precios en función de tipo de apartamento y ubicación en Bruselas")



#___WORDCLOUD___####
    #los paquetes necesarios se instalan al inicio
# Calcular el numero medio de caracteres en la variable name

mean(nchar(dfBel$name), na.rm=TRUE)

# Analizar la variable names (nombre/descripcion del apartamento EN INGLES)
vis_name <- Corpus(VectorSource(dfBel$name))

vis_name <- tm_map(vis_name, content_transformer(tolower))
vis_name <- tm_map(vis_name, removeNumbers)
vis_name <- tm_map(vis_name, removeWords, stopwords(kind = "en"))
vis_name <- tm_map(vis_name, removeWords, stopwords(kind = "french")) #sigo teniendo palabras en frances irrelevantes (dans, avec)
vis_name <- tm_map(vis_name, removePunctuation)
vis_name <- tm_map(vis_name, stripWhitespace)

dtm <- TermDocumentMatrix(vis_name)
matriz <- as.matrix(dtm)
orden_apa <- sort(rowSums(matriz),decreasing=TRUE)
df <- data.frame(word = names(orden_apa),freq=orden_apa)

#saca la nube de palabras más repetidas (mayores) en la descripción (name) del alojamiento
windows()
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=30, random.order=F, scale=c(5,0.5),
          colors=brewer.pal(9, "Paired"))

###_______________________________________________________________________________________________###