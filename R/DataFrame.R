
#--------------------------------------------------------------------------------
# Tema:       DataFrame
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      PDS 2020_T4.
#             https://www.colef.mx/emif/basescuestionarios.html
# Github:     

# Contenido             

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Exploracion inicial
#           4.1 Etiquetar variables
#           4.1 Nuevas variables etiquetadas
#       5. Recodificar variables
#           5.1 Validar tipo de dato
#           5.2 Cambiar formato
#           5.3 Crear nueva variable 
#           5.4 Generar rangos 
#       6. Subconjuntos de datos
#           6.1 Variables (columnas)
#           6.2 Casos (filas)
#           6.3 Casos (filas) y variables (columnas)
#       7. Tabulado
#           7.1 Tabulado ponderado
#           7.2 Exportar a Excel

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo

rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign)
library(questionr)
library(ggplot2)
library(dplyr)    
library(tidyverse)
library(survey)
library(writexl)

install.packages("survey")

#---------------------------------------
# NOTA:
# Si aparece un error, revisa en la
# pestana "packages" si el paquete 
# este instalado.
# 
# install.package()

#---------------------------------------


#2.  Directorio de trabajo

getwd()  # ¿Donde estoy ubicado
setwd("D:/OneDrive - El Colegio de México A.C/5. Proyectos/2021/46. SIGMA/3 R-intro/Intro-R/Datos")

#---------------------------------------
# NOTA:
# Si aparece un error, revisa el tipo
# de diagonal que tienes. 
# Se debe usar "/" o "//"
#---------------------------------------



#3.  Importar datos


    #3.1 Bases en .dbf
    emif<-read.dbf("PDS 2020_T4.dbf")%>%data.frame()

    #3.1 Datos de SPSS (.sav)
    emif.2<-read.spss("PDS 2020_T4.sav")%>%data.frame()
    
    #3.3 Datos de Excel (.csv)
    emif.3<-read.csv("PDS 2020_T4.csv")%>%data.frame()
    
    # Nota:
    rm("emif.2", "emif.3")                               # rm sirve para quitar elementos

    
    
#4.  Exploracion inicial

names(emif)  # Nota:  
head(emif,2) # Nota:

# SEX = Sexo
# Nota: 
wtd.table(emif$SEXO)



  #4.1  Etiquetar variables
  

    # Para etiquetar una variable se usa la funcion "factor" con
    # tres argumentos: 
    
    #  i.   La variable que se va etiquetar:  emif$SEXO
    #  ii.  Los valores (levels)              c(1,2)
    #  iii. Las etiquetas (labels)            c("Hombre","Mujer")
    
    
    emif$SEXO <- factor(emif$SEXO,levels = c(1,2),labels = c("Hombre","Mujer"))
    wtd.table(emif$SEX)
     
    
    #-----------------------------------
    # NOTA:
    # Al etiquetar una variable, se 
    # sustituye su contenido original
    # Si lo vuelvo a correr, pierdo el contenido
    #-----------------------------------



  # 4.2 Etiquetar y crear nuevas variables
    
    # CLASE2 = 1, Poblacion ocupada
    wtd.table(emif$REGION)
    
    # Codigos: 
    # 1=Matamoros
    # 2=Nuevo Laredo
    # 3=Piedras Negras
    # 4=Cd. Juárez
    # 5=Nogales
    # 6=Mexicali
    # 7=Tijuana
    
    
    emif$lugar <- factor(emif$REGION,levels = c(1,2,3,4,5,6,7),labels = c("Matamoros","Nuevo Laredo","Piedras Negras","Cd. Juárez","Nogales","Mexicali", "Tijuana"))
    
    wtd.table(emif$lugar, weights = emif$ponfin3)                      # Con "weights=" se ponderan los datos 
    
    wtd.table(emif$lugar, weights = emif$ponfin3)%>%prop.table()       # Frecuencias ponderadas 
    

#5. Recodificar variables

    #5.1 Validar tipo de dato
    
    # EDA = Edad
    class(emif$EDAD)
    # class 
    
    #-----------------------------------
    # NOTA:
    # El 80% de los errores ocurren 
    # por no considerar el tipo de 
    # variable que tenemos.
    #-----------------------------------
    

    #5.2 Cambiar formato
    
    emif$EDAD <-emif$EDAD%>%as.character()%>%as.numeric()
    
    #-------------------------------------------------
    # NOTA:
    # El operador %>% encadena los procesos de 
    # izquierda a derecha:
    # i.    Toma la variable "edad" de la base "emif"
    # ii.   Luego conviertela en caracter
    # iii.  Despues pasala a numero
    
    #-------------------------------------------------
    
    class(emif$EDAD)
    
  
    #5.3 Crear nueva variable 
    
    length(emif)                             # numero de variables
    emif$edad_recod<-0                       # agregar una variable
    length(emif)
  
  
    #5.4 Generar rangos  
    
    # Operadores:
    
    # ==  comparación
    # &   Y
    # |   o
    
    emif$edad_recod[emif$EDAD >= 0 & emif$EDAD <=30] <- 1
    emif$edad_recod[emif$EDAD >= 31 & emif$EDAD <=59] <- 2
    emif$edad_recod[emif$EDAD >= 60] <- 3
  
    wtd.table(emif$edad_recod)

    emif$edad_recod<-factor(emif$edad_recod,levels = c(1,2,3),labels = c("Menos de 30","31-59","60 y +"))
    wtd.table(emif$edad_recod, weights = emif$FAC_TRI)
    wtd.table(emif$edad_recod, weights = emif$FAC_TRI)%>%prop.table()

    #-----------------------------------
    # NOTA:
    # Si se corre el proceso nuevamente 
    # se borra el etiquetado.
    #----------------------------------- 
        
       

#6. Subconjuntos 

    #6.1 Seleccionar VARIABLES (columnas)
    emif.1 <-emif %>%select("SEXO", "edad_recod","PONFIN3", "ESTRATO", "UPM")

  

    #6.2 Seleccionar CASOS (filas)
    #    Solo vamos a considerar mujeres 

    emif.2 <-emif %>%filter(SEXO=="Mujer")       # (Ver el punto 4.1)
    

    #6.3 Seleccionar CASOS (filas) y VARIABLES (columnas)
    
    # Indentacion: 
    emif.3 <- emif%>%
      select("SEXO", "edad_recod","PONFIN3", "ESTRATO", "UPM")%>%
      filter(SEXO=="Mujer")
    
    rm("emif.1","emif.2")          #Quitamos las bases que no usaremos
    
    
#7. Tabulado 
    
    #7.1 Tabulado ponderado
    tabulado1=wtd.table(emif.3$edad_recod, weights = emif.3$PONFIN3) %>% data.frame()
    names(tabulado1)=c("Rango de edad","Frecuencia")


    #7.2 Exportar a Excel
    write_xlsx(tabulado1,"tabulado1.xlsx")

    
    