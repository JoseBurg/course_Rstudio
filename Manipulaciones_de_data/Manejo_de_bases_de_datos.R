# setwd("C:/Users/econo/Google Drive/Universidad/UASD/Econometria/Clases")

library(readxl)

miembros <- read_xlsx("ENFT_Abril_2011.xlsx", sheet = "Miembros")
ocupacion <-read_xlsx("ENFT_Abril_2011.xlsx", sheet = "Ocupación")

library("readr")
excel_sheet("ENFT_Abril_2011.xlsx")
datos <- merge(miembros, ocupacion, by=c("EFT_VIVIENDA", "EFT_HOGAR","EFT_MIEMBRO"))


#Creando

sexo <-datos$EFT_SEXO
log_edad <- log(datos$EFT_EDAD)

# Creando a variable educacion

educ <- rep(NA, 23900)
for(i in 1:23900){
  if(datos$EFT_ULT_NIVEL_ALCANZADO[i]==1) educ[i] = 2  else
    if(datos$EFT_ULT_NIVEL_ALCANZADO[i]==2) educ[i] = 2 + datos$EFT_ULT_ANO_APROBADO[i] else
      if(datos$EFT_ULT_NIVEL_ALCANZADO[i]==3) educ[i] = 2+8+datos$EFT_ULT_ANO_APROBADO[i] else
        if(datos$EFT_ULT_NIVEL_ALCANZADO[i]==4) educ[i] = 2+10+datos$EFT_ULT_ANO_APROBADO[i] else
          if(datos$EFT_ULT_NIVEL_ALCANZADO[i]==5) educ[i] = 2+12+datos$EFT_ULT_ANO_APROBADO[i] else
            if(datos$EFT_ULT_NIVEL_ALCANZADO[i]==6) educ[i] = 2+16+datos$EFT_ULT_ANO_APROBADO[i] else
              if(datos$EFT_ULT_NIVEL_ALCANZADO[i]==7) educ[i] = 0
}


hist(educ)
