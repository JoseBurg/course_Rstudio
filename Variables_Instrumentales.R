# Ejemplo de Variables Instrumentales

# Cargando librerias e Importando los datos
library(wooldridge)
library(AER)
library(stargazer)

data("fertil2")
attach(fertil2)


a) Estimando el modelo por MCO

eq_mco_1 = lm(children ~ educ + age + I(age^2))
stargazer(eq_mco_1,type="text")

b) Análisis de relevancia de 
eq_fr = lm(EDUC ~ FRSTHALF + AGE + I(AGE^2))

eq_fr=lm(educ~frsthalf+age+agesq)
stargazer(eq_fr,type ="text")

c) Implementando paso a paso el estimador de MC2E
##Etapa 1:

eq_fr = lm(educ ~ frsthalf + age + agesq)
educ_hat = eq_fr$fitted.values

##Etapa 2:estimacion de la ecuacion estructural 
eq_mco_2 = lm(children ~ educ_hat + age + agesq)


#Continuacion de la etapa 2
##Usando la funcion "ivreg" del paquete AER
library(AER)

eq_vi_1 = ivreg(children ~ educ + age + agesq | frsthalf + age  + agesq)

## Comparando modelos
#reportando los resultados 

stargazer(eq_mco_1,eq_mco_2,eq_vi_1, type="text", title="Comparando MCO y MC2E")


# d)

##Modelo estimado por MCO
eq_mco_3 = lm(children ~ educ + age + agesq + electric + tv + bicycle)

##Modelo estimado por VI
eq_vi_2 = ivreg(children ~ educ + age + agesq) + electric + tv + bicycle | FRSTHALF + AGE + I(AGE^2)+ ELECTRIC + TV + BICYCLE)

##Comparando coeficientes de distintos modelos
stargazer(eq_mco_3,eq_vi_2, type = "text")


#prueba de hausman para constrastar endogeneidad
#paso #1: extraer residuos ecuacion 
res_eq_fr<-eq_fr$residuals

#paso #2: estimar la ecuacion estructural original coniderando los residuos de la forma reducida

hausman<-lm(children~educ+age+agesq+res_eq_fr)
