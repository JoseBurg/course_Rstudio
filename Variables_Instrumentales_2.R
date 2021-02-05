
####################
# Ejemplo Estimacion Modelos de Eleccion Binaria:
# a) Modelo de Probabilidad Lineal, b) Modelo Probit
# c) Modelo Logit


# Importacion de datos
rm(list=ls())
library(wooldridge)
data(mroz)
attach(mroz)

Y = inlf
X = cbind(educ, nwifeinc,kidsge6,kidslt6)

View(mroz)
#Estad?sticos Descriptivos

summary(inlf)
summary(X)

table(Y)
table(Y)/sum(table(Y))


# Estimacion de Modelos de Probabildiad

## Modelo de Probabilidad Lineal

mpl = lm(inlf ~ educ+nwifeinc+kidsge6+kidslt6)

## Modelo Probit

mprobit = glm(inlf~educ+nwifeinc+kidsge6+kidslt6,family=binomial (link="probit"))

## Modelo Logit

mlogit = glm(inlf~educ+nwifeinc+kidsge6+kidslt6,family=binomial (link="logit"))

## Resumiendo Resultados

library(stargazer)


stargazer(mpl,mprobit,mlogit,type="text")


# Probabilidades predichas

pred_mpl = predict(mpl)
pred_mlogit = predict(mlogit,type="response")
pred_mprobit = predict(mprobit,type="response")

summary(pred_mpl)
summary(pred_mprobit)
summary(pred_mlogit)

# Porcentaje correctamente predicho

table(true = inlf, pred = round(fitted(mpl)))
table(true = inlf, pred = round(fitted(mprobit)))
table(true = inlf, pred = round(fitted(mlogit))) 

# Pseudo R cuadrado de MacFadden

mprobit0 = update(mprobit, formula= Y ~ 1)
McFadden_probit = 1-as.vector(logLik(mprobit)/logLik(mprobit0))
McFadden_probit

mlogit0 = update(mlogit, formula= Y ~ 1)
McFadden_logit = 1-as.vector(logLik(mlogit)/logLik(mlogit0))
McFadden_logit

# Efectos marginales

##MPL
coef(mpl)

##Probit
gprobit = mean(dnorm(predict(mprobit, type = "link")))
gprobit * coef(mprobit)

##Logit
glogit = mean(dnorm(predict(mlogit, type = "link")))
glogit * coef(mlogit)

# Efecto de la educaci?n sobre la participaci?n

plot(pred_mlogit,educ)


# Evaluaci?n de Ho: Hijos no tienen un efecto significativo sobre la participacion

library(aod)
l =cbind(0,0,0,1,0)
wald.test(b=coef(mprobit), Sigma=vcov(mprobit), L=l)


# Simulando el impacto de cambios en la educacion sobre la participacion

nuevos_datos = data.frame(educ = seq(0,20,1),nwifeinc=rep(mean(nwifeinc),each=21)
                          ,kidsge6=rep(mean(kidsge6),each=21),kidslt6=rep(mean(kidslt6),each=21))

head(nuevos_datos)

nuevos_datos[,c("probit")] = predict(mprobit, nuevos_datos,type="response")
nuevos_datos[,c("logit")] = predict(mlogit, nuevos_datos,type="response")
nuevos_datos[,c("mpl")] = predict(mpl, nuevos_datos,type="response")


library(ggplot2)
ggplot(data=nuevos_datos, aes(educ)) + geom_line(aes(y=probit,colour="Probit")) +
  geom_line(aes(y=logit,colour="Logit"))+geom_line(aes(y=mpl,colour="MPL"))
