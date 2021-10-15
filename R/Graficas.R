
#--------------------------------------------------------------------------------
# Tema:       Graficas con variables discretas y continuas
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      Estados, INEGI.
#             CONACYT (casos diarios):
#             https://datos.covid-19.conacyt.mx/#DownZCSV
# Github:     
# Notas:       

# Contenido

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Cargar la base de datos de referencia
#       4. Cargar base de datos complementaria 
#           4.1 Numero de casos de covid
#           4.2 Formato
#           4.3 Dividir la muestra en terciles   
#           4.4 Rangos: Bajo, Medio y Alto
#           4.5 Filtrar la base
#           4.6 Merge
#           4.7 Sustituir la base
#       5. Gráficas
#           5.1 Variable discreta
#           5.2 Variable continua

#--------------------------------------------------------------------------------


#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              

#1. Cargar librerias
library(foreign); library(ggplot2); library(dplyr); library(tidyverse)


#2.  Directorio de trabajo 


#3.  Cargar la base de datos de referencia
base_mx=read.dbf("00ent.dbf")

base_mx=base_mx %>% select(CVEGEO, CVE_ENT, NOMGEO)


#-----------------------------------
# NOTA:
# Necesitamos identificar una variable 
# que sirva como LLave para unir dos
# tablas.
#-----------------------------------

#4. Cargar base de datos complementaria

      #4.1 Buscar la base de datos del numero de casos de covid

      covid=read.csv("Casos_Diarios_Estado_Nacional_Confirmados_20210809.csv")
      
            #4.1.1 Crear la variable SUMA

            # mutate ....
            # %>% me sirven para unir procesos

            covid=covid%>%mutate(SUMA=rowSums(.[4:542]))    
            
            #4.1.2 Quitar el total
            # filter ....
            
            covid=covid%>%filter(cve_ent>=1)
            
            #4.1.3 Seleccionar la variable de estado y SUMA
            covid=covid%>%select(cve_ent, SUMA)
            
      #4.2 Agregar un 0 para tener el mismo formato
      covid=covid%>%mutate(cve_ent=formatC(cve_ent, width = 2, format = "d", flag = "0"))
      covid=covid%>%data.frame()
      
      
      #4.3 Dividir la variable SUMA en terciles
      quantile(covid$SUMA, probs = c(.33,.66))

      
      #4.4 Rangos: Bajo, Medio y Alto
      # Crear una nueva variable
      #  i. Indicar el nombre de la base + $ + nombre de la variable
      #       a. Si queremos numero, debir ir =0
      #       b. Si queremos texto, debe ir =""
      #  ii. La nueva variable esta vacia.
      
      covid$rango=""
      covid$rango[covid$SUMA<=47755]<-"Bajo"
      covid$rango[covid$SUMA>47755 & covid$SUMA<=75497]<-"Medio"
      covid$rango[covid$SUMA>75497]<-"Alto"
      
      #4.5 Filtrar la base
      covid=covid %>% select("cve_ent", "rango", "SUMA")
      

      
      #4.6 Merge
      # Indicar la columna Llave en ambos casos
      base_mx=base_mx%>%left_join(covid,
                  by = c("CVE_ENT" = "cve_ent"))

      #4.7 Guardamos la base base_mx
      #-----------------------------------
      # NOTA:
      # Con este procedimiento se sustituye 
      # la base original.
      #-----------------------------------
      
      
      write.dbf(base_mx,"00ent.dbf")

#5. Gráficas
    
      #5.1 Variable discreta
      
      class(base_mx$rango)    
      ggplot(base_mx,aes(rango)) + geom_bar(fill="yellowgreen")
      
      # 1ra capa, definir la base y la variable
      # la segunda capa la agrego con el signo de +
      # 2da capa tiene el tipo de grafica
      
      # Comentario:
      ggplot(base_mx,aes(rango)) + geom_bar(fill="yellowgreen")+
        xlab("Rango")+
        ylab("Casos")+
        ggtitle("Casos de covid")

      
      #5.2 Variable continua
      
      # Class me dice el tipo de dato
      class(base_mx$SUMA)                            
      base_mx$SUMA<-as.numeric(base_mx$SUMA)
      class(base_mx$SUMA)
      
      
      # 1ra capa
      g4<-ggplot(base_mx, aes(SUMA))                 # Base de datos y variable
      
      
      # Agregamos diferentes tipos de graficas
      g4+geom_area(stat="bin")
      
      g4+geom_freqpoly()
      
      g4+geom_histogram()+
        ggtitle("Casos Covid-19")+              
        xlab("Estados")+
        ylab("Personas")
      

      