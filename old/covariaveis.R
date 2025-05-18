

####### Manipula Dados ###############

library(dplyr)
library(lubridate)
library(TTR)

# load("dados/seasons.RData")

# Função média concedidos últimos jogos (Considerando efeito de decaimento)
concedidosX <- function(seasons_df, rodada, conc_2nd){
  
  dados = seasons_df
  date_today = max(rodada$Date)
  
  # Progresso
  print('Gols Concedidos (Média Ponderada)')
  pb <- txtProgressBar(min = 0, max = length(na.omit(dados$HomeGoals)), 
                       style = 3)  # Criando a barra de progresso
  
  ### Calcularemos as medias dos gols concedidos dos times dentro e fora de casa
  teams_home = unique(seasons_df$Home)
  teams_away = unique(seasons_df$Away)
  
  ## Valores Iniciais
  dados$HomeConc = NA; dados$AwayConc = NA
  
  # Atualizando a partir dos jogos que vao acontecendo
  
  for (i in 1:nrow(dados)){
    
    # Times da partida
    teamH = dados$Home[i]
    teamA = dados$Away[i]
    
    if (dados$Date[i] > date_today){break}
    
    # Mandante
    indice = which(dados$Home[1:i-1] == teamH)
    
    if ((length(indice) == 0)){
    
      # Veio da Segunda Divisão
      if (i > 380){dados$HomeConc[i] = conc_2nd[1]}
      
    } else if (length(indice) == 1){
      
      goalsH = dados$AwayGoals[indice]
      dados$HomeConc[i] = goalsH
      
    } else{
    
    goalsH = dados$AwayGoals[indice]
    
    if (NA %in% goalsH) {break}
    
    concH =  tail(WMA(goalsH, n = length(goalsH)), 1)
    dados$HomeConc[i] = concH
    
    }
    
    # Visitante
    indice = which(dados$Away[1:i-1] == teamA)
    
    if ((length(indice) == 0)){
      
      # Veio da Segunda Divisão
      if (i > 380) {dados$AwayConc[i] = conc_2nd[2]}
      
    } else if (length(indice) == 1){
      
      goalsA = dados$HomeGoals[indice]
      dados$AwayConc[i] = goalsA
      
    } else {
    
    goalsA = dados$HomeGoals[indice]
    
    if (NA %in% goalsH) {break}
    
    concA = tail(WMA(goalsA, n = length(goalsA)), 1)
    dados$AwayConc[i] = concA
    
    }
    
    setTxtProgressBar(pb, i)  # Atualizando o progresso
    
  }
  
  close(pb)  # Fechando a barra de progresso quando o looping terminar
  dados = dados[dados$Date > '2010-05-10', ]
  return(dados)
}


######################################################

# Insere Dummys

######################################################

dummys <- function(dados, minimo, n_temps, all_dummys = FALSE){
  
  ### Times
  teams = unique(dados$Home)
  
  # Dummy Geral
  dados$BIG6_H = 0
  dados$BIG6_A = 0
  #dados$meio_H = 0
  #dados$meio_A = 0
  #dados$reb_H = 0
  #dados$reb_A = 0
  
  # Progresso
  print('Dummys')
  pb <- txtProgressBar(min = 0, max = length(teams), style = 3)
  big6 = c('Liverpool', 'Tottenham', 'Chelsea',
           'Manchester City', 'Arsenal', 'Manchester Utd')
  
  for (i in 1:length(teams)){
    
    ### Número de Partida por Times
    team = teams[i]
    n = nrow(dados[dados$Home == team, ])
    
    # Dummy Big 6
    if (team %in% big6){
      dados['BIG6_H'] = ifelse(dados$Home == team, 1, dados$BIG6_H)
      dados['BIG6_A'] = ifelse(dados$Away == team, 1, dados$BIG6_A)
    }
    
    # Dummys somente para quem jogou um minimo de jogos
    #if (all_dummys == FALSE){
    
      # Dummy Própria do Time
      #if (n == 19*n_temps){
        
        #dados['BIG_H'] = ifelse(dados$Home == team, 1, dados$BIG_H)
        #dados['BIG_A'] = ifelse(dados$Away == team, 1, dados$BIG_A)
      
      #} else 
      #if (n > minimo){
        
        #dados[paste0(team, '_H')] = ifelse(dados$Home == team, 1, 0)
        #dados[paste0(team, '_A')] = ifelse(dados$Away == team, 1, 0)
        #dados['meio_H'] = ifelse(dados$Home == team, 1, dados$meio_H)
        #dados['meio_A'] = ifelse(dados$Away == team, 1, dados$meio_A)
        
      #} else {
      
      # Dummy Outros
      #dados['reb_H'] = ifelse(dados$Home == team, 1, dados$reb_H)
      #dados['reb_A'] = ifelse(dados$Away == team, 1, dados$reb_A)
        
      #}
    #} else{
      
      # Dummys para todos
      #dados[paste0(team, '_H')] = ifelse(dados$Home == team, 1, 0)
      #dados[paste0(team, '_A')] = ifelse(dados$Away == team, 1, 0)
      
    #}
    setTxtProgressBar(pb, i)  # Atualizando o progresso
    
  }
  close(pb)  # Fechando a barra de progresso quando o looping terminar

  return(dados)
  
}
