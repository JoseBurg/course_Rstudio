# Prueba de heterocedasticidad

## Análisis gráfico 

```{r}
# Heterocedasticidad


# modelo 1
yhat1<-mod1$fitted.values # valores ajustado
residuos1<-mod1$residuals

# modelo 1
yhat2<-mod2$fitted.values  # valores ajustado
residuos2<-mod2$residuals


ggplot()+
  geom_point(aes(
    yhat1,
    residuos1
  ),color = "blue")+
  theme_apa()

ggplot()+
  geom_point(aes(
    yhat2,
    residuos2
  ),color = "blue")+
  theme_apa()


```



## Contraste de Breusch-Pagan

##### Residuos son homocedastico
$$H_0:Homocedasticidad$$
  
  ##### Residuos son heterocedasticos 
  $$H_1:Heterocedasticidad$$
  
  
  ```{r}
bptest(mod1)
bptest(mod2)
```

En este modelo hay heterocedasticidad, debido a que el p-valor es muy inferior al 10%. Por ende existe evidencia empirica para rechazar la hipotesis nula. 


## Corregiendo la heterocedasticidad 

### Corrigiendo los errores estandar

```{r}
coeftest(mod1,vcov. = hccm(mod1))
summary(mod1)
```

## Error de especificacion en la forma funcional 

$$H_0:No \ hay\ error\ de\ especificacion\ en\ la\ forma\ funcional$$
  
  
  $$H_1:Hay error de especificacion en la forma funcional$$
  
  
  
  ```{r}
mod1.1<-lm(I(log(ingresos))~sector+experiencia+educacion,data = prov)
stargazer(mod1.1,type = "text")
resettest(mod1.1,power = 2:3,type = c("fitted"))
```