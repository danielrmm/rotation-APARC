# APLICATIVO RODIZIO APARC
# Daniel Rodrigo de M. Magalhaes
# Versao 1.06 
# 17 de setemrbo de 2019

## Projeto no GIT


## PACOTES ## 
install.packages("stringr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("sugrrants")
install.packages("tidyr")
install.packages("tsibble")
install.packages("readr")

#Library
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(sugrrants)
library(tidyr)
library(tsibble)
library(readr)



## ENTRADA DE DADOS ##
r = 60
c = 15
i=1
j=1

x = matrix (0, nrow = r, ncol = c)
x = as.data.frame(x)

x[1,] = c(0,1,1,1,1,1,0,0,0,0,0,0,0,0,0)

## Codigo dos Operadores
op = c(29:41,52)

# Dias de Mare morta
mm = dmy("07/09/2019", "08/09/2019", "23/09/2019","24/09/2019" )

## Definir Periodo

inicio = dmy("01/9/2019")
periodo = seq(inicio, inicio + days (r), by = "days")


## CODE ##

# Adicionar data
x[,1]=dmy(x[,1])

for (i in 1:r){
  x[i,1]= periodo[i]
}



# Fazer rodizio

for (i in 2:r){
  for (j in 2:c){
   if(any(x[i,1] == mm)){
     x[i,j] =  x[i-1,j]
   }else{
     if (j<=6){
        x[i,j]= x[i-1, j+9]
    }else{
      x[i,j]=x [i-1,j-5]
     }
    }
  }
}

# Inserir os numeros dos operadores
for (i in 1:r){
  for (j in 2:c){
    if(any(x[i,1] == mm)){
      x[i,j]=NA
      }else{
        if (x[i,j] == 1){
          x[i,j]= op[j-1]
        }else{
          x[i,j]= "x"
        }
      }
  }
}


# Organizar tabela para exportar

y = 
  x%>% 
  mutate (class = ifelse (is.na(V2), "parado", "mergulho"))%>%
  mutate (Subject = ifelse (class == "parado", "Sem Mergulho", 
                            paste(V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15, sep="-")
  ))%>%
  mutate (Subject = str_remove_all(Subject, "-x"))%>%
  mutate (Subject = str_remove_all(Subject, "x-"))%>%
  mutate(Subject=replace(Subject, V1 == mm | V1 == mm[1] , "Sem Mergulho"))%>%  # Gambiarra (Subject, V1 == mm | V1 == mm[1])
  mutate("Start date" = V1)%>%
  select("Start date", Subject)

# Exportar arquivo para adicionar ao google calendar

write.csv(
  y,
  file="Rodizio_set",
  quote=FALSE,
  row.names=FALSE
)


### PROXIMOS PASSOS:
### Usar o google sheet para sincronizar automatico com o google calendar 

# R to Google Sheets
## https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/

## Google sheets to Google Calendar (N?o ? no R)
#https://automate.io/integration/google-calendar/google-sheets

## Shini app
