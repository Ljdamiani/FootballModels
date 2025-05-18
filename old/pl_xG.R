

####### Dados com Gols Concedidos ###############

library(dplyr)
library(lubridate)

# Carrega as rodadas
load("dados/seasons.RData")

# Função concedidos
esperados <- function(seasons_df, d){
  
  # Separa jogos antes da Atual temporada
  indice = which(seasons_df$Date == d)[1]
  dados_ant_date = seasons_df[1:(indice-1), ] # antes da data
  dados <- seasons_df[indice:nrow(seasons_df), ] # a partir da data
  
  
  ### Calcularemos as medias dos gols concedidos dos times dentro e fora de casa
  
  teams_home = unique(seasons_df$Home)
  teams_away = unique(seasons_df$Away)
  
  ## Valores Iniciais
  
  dados$HxG = NA; dados$AxG = NA
  dados$HomexGTotal = NA; dados$AwayxGTotal = NA 
  dados$HomeGames = NA; dados$AwayGames = NA
  
  # Mandantes
  for (i in teams_home){
    
    # indices
    indice = which(dados_ant_date$Home == i)
    teamH = dados_ant_date[indice, ] # jogos do time em casa
    
    # dados dos jogos
    xGtotalH = sum(teamH$Home_xG)
    jogosH = nrow(teamH)
    xGH = xGtotalH/jogosH
    
    # Adicionando nos dados a partir da data de referencia
    indice2 = which(dados$Home == i)[1]
    dados$HxG[indice2] = xGH
    dados$HomexGTotal[indice2] = xGtotalH
    dados$HomeGames[indice2] = jogosH
    
  }
  
  # Visitantes
  for (i in teams_away){
    
    # indices
    indice = which(dados_ant_date$Away == i)
    teamA = dados_ant_date[indice, ] # jogos do time em casa
    
    # dados dos jogos
    xGtotalA = sum(teamA$Away_xG)
    jogosA = nrow(teamA)
    xGA = xGtotalA/jogosA
    
    # Adicionando nos dados a partir da data de referencia
    indice2 = which(dados$Away == i)[1]
    dados$AxG[indice2] = xGA
    dados$AwayxGTotal[indice2] = xGtotalA
    dados$AwayGames[indice2] = jogosA
    
  }
  
  #NaN
  for (i in 1:nrow(dados)){
    
    # Valores NaN
    if (is.nan(dados$HxG[i])){
      
      dados$HxG[i] = 0
      dados$HomexGTotal[i] = 0
      dados$HomeGames[i] = 0
    } 
    if (is.nan(dados$AxG[i])){
      
      dados$AxG[i] = 0
      dados$AwayxGTotal[i] = 0
      dados$AwayGames[i] = 0
    }
  }
  
  # Atualizando a partir dos jogos que vao acontecendo
  na_1st = which(is.na(dados$HxG) == TRUE)[1]
  
  for (i in na_1st:nrow(dados)){
    
    # Data
    data = dados$Date[i]
    
    # Times da partida
    teamH = dados$Home[i]
    teamA = dados$Away[i]
    
    # dados dos jogos
    indice = tail(which(dados$Home[1:i-1] == teamH), 1)
    xGtotalH = dados$HomexGTotal[indice] + dados$Home_xG[indice]
    jogosH = dados$HomeGames[indice] + 1
    xGH = xGtotalH/jogosH
    
    indice = tail(which(dados$Away[1:i-1] == teamA), 1)
    xGtotalA = dados$AwayxGTotal[indice] + dados$Away_xG[indice]
    jogosA = dados$AwayGames[indice] + 1
    xGA = xGtotalA/jogosA
    
    # Atualiza os nao preenchidos 
    if(is.na(dados$HxG[i])){
      dados$HxG[i] = xGH
      dados$HomexGTotal[i] = xGtotalH
      dados$HomeGames[i] = jogosH
    }
    if(is.na(dados$AxG[i])){
      dados$AxG[i] = xGA
      dados$AwayxGTotal[i] = xGtotalA
      dados$AwayGames[i] = jogosA
    }
    
  }
  
  return(dados)
}


# DAdos gols concedidos
# dados <- concedidos(seasons_df, d = "2022-08-05", conc_2nd = c(2, 2))

# ATUALIZAR FUNCAO PARA OLHAR PARA OS REBAIXADOS
# Atualizando a partir dos jogos que vao
  
  
  