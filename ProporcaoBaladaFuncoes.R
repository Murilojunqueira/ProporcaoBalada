

# Script para analisar a balada do dia

# Criado por Murilo Junqueira

# Data criação: 2018-01-31
# Ultima modificação: 2018-02-01


################## Prepara área de trabalho ##################

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




####################################################################################
############################## Funções ############################################


AnaliseEvento <- function(Evento.ID, token, Limit = 3000, InputFolder = getwd(),
                          FaceAPIHost = "https://graph.facebook.com/") {
  
  
  # Limit = 3000
  # FaceAPIHost = "https://graph.facebook.com/"
  
  Nomes.file <- paste0(InputFolder, "ListaNomes.csv")
  
  ############### Dados gerais do evento
  
  FaceAPIVersion <- "v2.12"
  
  FaceAPIURL.General <- paste0(FaceAPIHost, FaceAPIVersion, "/", Evento.ID,  "/" , "?access_token=", token)
  
  Event.df <- BaixaEventoFace(FaceAPIURL.General, as.df = FALSE)
  
  Evento <- list()
  #Evento$Descricao <- as.character(Event.df$description)
  Evento$Descricao <- ""
  Evento$nome <- as.character(Event.df$name)
  
  print(paste("Analisando o evento:", Evento$nome))
  
  #Evento$horainicio <- with_tz(ymd_hms(as.character(Event.df$start_time)), "America/Sao_Paulo")
  #Evento$horafim <- with_tz(ymd_hms(as.character(Event.df$end_time), tz = "America/Sao_Paulo"), "America/Sao_Paulo")
  Evento$horainicio <- as.character(Event.df$start_time)
  Evento$horafim <- as.character(Event.df$end_time)
  Evento$horafim <- ifelse(length(Evento$horafim) == 0, NA, Evento$horafim)
  Evento$NomeLocal <- as.character(Event.df$place$name)
  Evento$Cidade <- as.character(Event.df$place$location$city)
  Evento$Pais <- as.character(Event.df$place$location$country)
  Evento$Endereco <- as.character(Event.df$place$location$street)
  Evento$Latitude <- as.numeric(Event.df$place$location$latitude)
  Evento$Longitude <- as.numeric(Event.df$place$location$longitude)
  
  ############### Lotação
  
  print("Analisando lotação")
  
  FaceAPIVersion <- "v2.12"
  variaveis <- c("fields=attending_count", "fields=interested_count")
  
  
  FaceAPIURL.counts <- paste0(FaceAPIHost, Evento.ID, "?" , variaveis, "&access_token=", token)
  comparecerao <- BaixaEventoFace(FaceAPIURL.counts[1], "attending_count")
  Interessados <- BaixaEventoFace(FaceAPIURL.counts[2], "interested_count")
  
  Evento$comparecerao <- as.integer(comparecerao)
  Evento$interessados <- as.integer(Interessados)
  
  ############### Proporção de homens e mulheres
  
  print("Analisando Proporção Homens e Mulheres")
  
  FaceAPIVersion <- "v2.5"
  prefixo <-  paste0(FaceAPIHost, FaceAPIVersion, "/", Evento.ID, "/")
  posfixo <- paste0("?limit=", Limit, "&access_token=", token)
  
  # Dados dos interessados e dos que comparecerão
  Propocao.Comparecerao <- ProporcaoBalada(prefixo, posfixo, variavel = "attending", 
                                           sample = 250, Nomes.file = Nomes.file)
  Propocao.Interessados <- ProporcaoBalada(prefixo, posfixo, variavel = "interested", 
                                           sample = 250, Nomes.file = Nomes.file)
  
  # Deixa os nomes mais intuitivos
  names(Propocao.Comparecerao) <- paste("Confirmados", names(Propocao.Comparecerao))
  names(Propocao.Interessados) <- paste("Interessados", names(Propocao.Interessados))
  Evento <- c(Evento, as.list(Propocao.Comparecerao), as.list(Propocao.Interessados))
  
  Evento$Evento.URL <- paste0("https://www.facebook.com/events/", Evento.ID)
  
  ############### Procedimentos Finais
  
  print("Formantando BD")
  
  
  insere.NA <- function(x) {
    ifelse(length(x) == 0, NA, x)
  }
  
  Evento <- map(Evento, insere.NA)
  
  
  Evento.df <- as_tibble(Evento) %>% 
    select(nome, horainicio, horafim, NomeLocal, comparecerao, interessados, ends_with("Female"), 
           ends_with("Male"), everything())
  
  # Evento.Data <- Evento.df
  
  return(Evento.df)
  
}





# Baixa dados no API do Facebook
BaixaEventoFace <- function(url, data_path = 1, as.df = TRUE) {
  # Baixa os dados do evento
  raw.result <- GET(url = url)
  
  output <- raw.result %>% 
    # Transforma o resultado da busca de Unicode para texto
    content(as = "text", encoding = "utf-8") %>% 
    # Usa o parse do JSON
    jsonlite::fromJSON()
  
  if(as.df) {
    output <- output %>% 
      .[[data_path]] %>% 
      as_tibble()
  }
  
  return(output)
}




# Descobre o gênero das pessoas
GeneroEvento <- function(.df, NomeVar, sample = 150, Nomes.file = "ListaNomes.csv") {
  
  # .df <- Event.df
  # NomeVar <- "name"
  
  # Cria uma variável temporária para não alterar a variável original de nome
  .df$TempNameName <- .df[[NomeVar]]
  
  
  
  Event.df.Sample <- .df %>% 
    sample_n(ifelse(sample > nrow(.df), nrow(.df), sample)) %>% 
    mutate(first_name = word(TempNameName)) %>% 
    select(-TempNameName)
  
  
  if (file.exists(Nomes.file)) {
    
    Nomes.df <- fread(Nomes.file, sep = ";", dec = ",",
                      stringsAsFactors = FALSE)
    
    Nomes.df <- Nomes.df %>%
      mutate(buscado = 1)
    
    Event.df.Sample <- Event.df.Sample %>% 
      left_join(Nomes.df, by = "first_name") %>% 
      mutate(buscado = ifelse(is.na(buscado), 0, buscado))
    
  } else {
    Event.df.Sample$gender <- NA
    Event.df.Sample$buscado <- 0
  }
  
  nBuscar <- which(Event.df.Sample$buscado == 0)
  contador <- 1
  
  for(i in nBuscar) {
    
    Event.df.Sample$gender[[i]] <- get_gender(Event.df.Sample$first_name[[i]])
    print(paste0(contador, "/", length(nBuscar), " - " ,Event.df.Sample$name[[i]], " (", Event.df.Sample$gender[[i]], ")"))
    contador <- contador + 1
  }
  
  
  if (file.exists(Nomes.file)) { 
    
    Nomes.df <- fread(Nomes.file, sep = ";", dec = ",",
                      stringsAsFactors = FALSE)
    
    Nomes.Novos <- Event.df.Sample %>% 
      select(first_name, gender) %>% 
      rbind(Nomes.df) %>% 
      distinct(first_name, .keep_all = TRUE)
    
  } else {
    
    Nomes.Novos <- Event.df.Sample %>% 
      select(first_name, gender)
    
  }
  
  write.table(Nomes.Novos, file = Nomes.file, sep = ";", dec = ",", 
              row.names=FALSE, append = FALSE)
  
  Event.df.Sample <- Event.df.Sample %>% 
    select(-buscado)
  
  return(Event.df.Sample)
  
}




ProporcaoBalada <- function(prefixo, posfixo, variavel, sample = 250, Nomes.file = "ListaNomes.csv") {
  
  URLBusca <- paste0(prefixo, variavel, posfixo)
  
  Event.df <- BaixaEventoFace(URLBusca, "data")
  Event.df.Sample <- GeneroEvento(Event.df, "name", sample = sample, Nomes.file = Nomes.file)
  
  Event.df.Sample.Sum <- Event.df.Sample %>% 
    filter(!is.na(gender)) %>% 
    group_by(gender) %>% 
    summarise(total.gender = n()) %>% 
    mutate(proporcao = total.gender / sum( total.gender)) %>% 
    mutate(proporcao = percent(proporcao)) %>% 
    select(gender, proporcao) %>% 
    spread(gender, proporcao)
  
  return(Event.df.Sample.Sum)
  
}

# End
