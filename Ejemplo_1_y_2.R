library(readr)
library(readxl)
library(stargazer)
library(tidyverse)

# Ejemplo 1 
dt1<-read.csv("ejemplo_1.csv",sep = ";")
View(dt1)

salario<-dt1$salario
educ<-dt1$educacion

mod1<-lm(salario~educ)
stargazer(mod1,type = "text")

# Ejemplo 2
dt2<-read.csv("ejemplo_2.csv",sep = ";")
View(dt2)

peso<-dt2$peso_al_nacer
cigs<-dt2$cigs_cons_sem
mod2<-lm(peso~cigs)


stargazer(mod2,type = "text")


