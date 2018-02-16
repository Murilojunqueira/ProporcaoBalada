# Script para Aprender a usar o API do Facebook no R

# Criado por Murilo Junqueira

# Data criação: 2018-01-31
# Ultima modificação: 2018-02-01


################## Prepara área de trabalho ##################

#clean memory
rm(list=ls(all=TRUE))
gc()

# install.packages("Rfacebook")
# install.packages("genderBR")

library(Rfacebook)

library(jsonlite)
library(genderBR)
library(tidyverse)
library(data.table)
library(stringr)
library(scales)
library(httr)
library(lubridate)


InputFolder <- "C:/Users/mjunqueira/Dropbox/Ideias, Ensaios e Projetos/2018-02 Facebook API/"
#InputFolder <- "E:/Users/Murilo/Dropbox/Ideias, Ensaios e Projetos/2018-02 Facebook API/"

source(paste0(InputFolder, "ProporcaoBaladaFuncoes.R"))


############################ Extração de dados de eventos #########################

# Site do API pessoal (PEGAR O TOKEN AQUI!):
# https://developers.facebook.com/tools/explorer/145634995501895/

token <- "EAACEdEose0cBAPgZBfZABZBrO1nT2ZAb7ZCDkMhWnopVhifIQo1ZBWXXIqqgLhnimR7SyQZAFfXoqUB3MnsYd6u8WT9wx3xvA7KetTTwjZAbdY7PaZBaAuw2e2OaWE9zj9zrZCuD4b1WKEBNs0vb4AlVtBZBCSjh6zqCbLoc57P35RqPpcinpR6PctNm3dFKEhYx6rw6lCNECFmbAZDZD"


# Variáveis gerais
# Limit <- 3000
# FaceAPIHost <- "https://graph.facebook.com/"


Baladas.df <- fread(paste0(InputFolder, "Blocos.csv"), 
                  sep = ";")

Baladas.df <- Baladas.df %>% 
  filter(!is.na(Evento.URL) & !is.na(N)) %>% 
  filter(Feito == 0) %>% 
  mutate(Evento.ID =  gsub("[^0-9]", "", Evento.URL) ) %>% 
  mutate(Evento.ID = as.numeric(Evento.ID))


Resumo.Eventos <- data.frame()

# Fez apenas dois eventos
for(i in seq_len(nrow(Baladas.df))) {
  
  # i <- 2
  Evento.ID <- Baladas.df$Evento.ID[i]
  Evento.Data <- AnaliseEvento(Evento.ID = Evento.ID, token = token)
  Evento.Data <- as.data.frame(Evento.Data, stringsAsFactors = FALSE)
  print("empilhando BD")
  Resumo.Eventos <- rbind(Resumo.Eventos, Evento.Data, stringsAsFactors = TRUE)
}

rm(i, Evento.Data)

# Consertar 
FullPath <- paste0(InputFolder, "ResumoEventos.csv")

write.table(Resumo.Eventos, file = FullPath, sep = ";", dec = ",", 
            row.names=FALSE, append = TRUE)


rm(Resumo.Eventos)


################## Estudos ##################

# Documentação do API do FACEBOOK:
# https://developers.facebook.com/docs/graph-api/reference/event

# Inspiração: https://www.youtube.com/watch?v=itdPcvWql9g
# http://thinktostart.com/analyzing-facebook-with-r/
# https://www.rdocumentation.org/packages/Rfacebook/versions/0.6.15


# Pacote para predizer o gênero no Brasil
# http://fmeireles.com/blog/rstats/genderbr-predizer-sexo


# Site do API pessoal (PEGAR O TOKEN AQUI!):
# https://developers.facebook.com/tools/explorer/145634995501895/


# End