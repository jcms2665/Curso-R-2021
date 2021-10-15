
#--------------------------------------------------------------------------------
# Tema:       Analisis de conglomerados: k-meadias
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      Encuesta Nacional de Ocupacion y Empleo, INEGI
#             https://www.inegi.org.mx/programas/enoe/15ymas/#Microdatos
# Github:     
# Notas:       

# Contenido

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Analisis de conglomerados
#           4.1 Normalizar datos
#           4.2 K-medias
#       5. Interpretacion

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign)                 
library(questionr)              
library(survey) 
library(car)
library(tidyverse)
library(stats)


#2.  Directorio de trabajo


#3.  Importar datos
sdemt<-data.frame(read.csv("SDEMT120.csv", header = TRUE))


#4. Analisis de conglomerados

# El objetivo de esta tecnica es hacer grupos homogeneos al interior
# y heterogeneos al exterior

ocupados<-sdemt %>% filter(clase2==1)


ac.base<-filter(ocupados, clase2==1)%>%
  select(anios_esc,hrsocup, ingocup)


# Validar tipo de datos
# class valida el tipo de dato
class(ac.base$anios_esc)
class(ac.base$hrsocup)
class(ac.base$ingocup)


# Cambiar a numerico
ac.base$anios_esc<-as.numeric(ac.base$anios_esc)
ac.base$hrsocup<-as.numeric(ac.base$hrsocup)
ac.base$ingocup<-as.numeric(ac.base$ingocup)


    #4.1 Normalizar datos
    ac.base<-mutate(ac.base, es.nor=(anios_esc-min(anios_esc))/(max(anios_esc)-min(anios_esc)))
    ac.base<-mutate(ac.base, hr.nor=(anios_esc-min(hrsocup))/(max(hrsocup)-min(hrsocup)))
    ac.base<-mutate(ac.base, ing.nor=(anios_esc-min(ingocup))/(max(ingocup)-min(ingocup)))
    
    
    # Generar un subconjunto de datos
    ac.base.nor<-ac.base %>% select(es.nor, hr.nor,ing.nor)
    
    
    #4.2 K-medias
    # 1er la base de datos
    # 2do numero de grupos
    
    # Cuando corro el modelo lo guardo
    # Fijar centroides set.seed
    set.seed(123)    
    fit <- kmeans(ac.base.nor, 3)
    
    
    # Ahora se pega la variable de cluster a la base
    ocupados <- data.frame(ocupados, fit$cluster)
    

    table(ocupados$fit.cluster)
    

#5. Interpretacion

    # Para caracterizar a los grupos, tabulamos
    # contra las unidades de analisis

ocupados$sex <- factor(ocupados$sex,levels = c(1,2),labels = c("Hombre","Mujer"))
ocupados$fit.cluster <- factor(ocupados$fit.cluster,levels = c(1,2,3),labels = c("G-1","G-2","G-3"))

resultado=table(ocupados$sex, ocupados$fit.cluster) %>% 
  prop.table(2)

resultado=resultado %>% data.frame()
write_xlsx(resultado,"resultado.xlsx")





