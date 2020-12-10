library(readr)
library(readxl)
library(stargazer)
library(tidyverse)

#<------------------------Ejemplo_1----------------------------->
## Importando la base de datos 
dt1<-read.csv("ejemplo_1.csv",sep = ";")
View(dt1) # Ver la base de datos importada

#Estimar el modelo 
## Creando las variables
salario<-dt1$salario
educ<-dt1$educacion

mod1<-lm(salario~educ)
summary(mod1)
stargazer(mod1,type = "text")

# T critico:
qt(0.05/2,933) # Valor absoluto 
# NOTA: R siempre dara el T critico negativo.

#<------------------------Ejemplo_2----------------------------->
## Importando la base de datos 
dt2<-read.csv("ejemplo_2.csv",sep = ";")
View(dt2) # Ver la base de datos importada

#Estimar el modelo 
## Creando las variables
peso<-dt2$peso_al_nacer
cigs<-dt2$cigs_cons_sem 

mod2<-lm(peso~cigs)
stargazer(mod2,type = "text")
