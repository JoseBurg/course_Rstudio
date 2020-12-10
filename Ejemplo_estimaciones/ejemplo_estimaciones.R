# Instalando y cargando el paquete de wooldrige 
install.packages("wooldridge") # instalando 
library("wooldridge")  # cargando 

# paquete adicional para representar los resultados mas organizados 
install.packages("stargazer")
library("stargazer")

# <-------------------------Ejemplo_01-------------------------->

# De la base de datos contenida en el paquete de Wooldridge llamada SLEEP75.RAW
# estime el modelo: sleep = \beta_0 + \beta_1 totwrk + u, donde sleep 
# es la cantidad de minutos de sueño por semana durante la noche y totwrk corresponde
# al total de minutos de trabajo por semana. 
# 1) Interprete los coeficientes 
# 2) Que indica el intercepto 

datos01<-sleep75
View(datos01)

mod01<-lm(datos01$sleep~datos01$totwrk)
summary(mod01) # Estimación forma 1
stargazer(mod01,type = "text")  # Estimación forma 2

# Convertiendo los minutos en hora 
# mod01.1<-lm(I(datos01$sleep/60)~I(datos01$totwrk/60))
# stargazer(mod01,mod01.1,type = "text") # modelo en mts y hrs


# <-------------------------Ejemplo_02-------------------------->
# Estime el modelo sleep = \beta_0 + \beta_1 age + e, age es la edad
# que tiene el individuo en años, utilice la base de datos del Ejemplo_01
# 1) Interprete los coeficientes
# 2) Que relacion se estima que tiene la edad con la cantidad de minutos 
# dormidos a la semana 



# <-------------------------Ejemplo_03-------------------------->
# Utilice la base de datos hprice1 y estime el modelo: 
# price = \beta_0 + \beta_1 sqrft + e, donde price es el precio 
# de casas dado en miles de dolares, sqrft es la superficie 
# en pies cuadrado
# 1) Cual es el incremento en precio estimado para 
# una casa con un pies cuadrado de superficie adicional 
# 2) Para una casa con mil pies cuadrados que precio se estima 
# en promedio

