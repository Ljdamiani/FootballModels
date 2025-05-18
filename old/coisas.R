
for (i in 1:nrow(rodada)){
  
  # i = 2
  
  # Mandante e Visitante
  teamM = rodada$Home[i]
  teamV = rodada$Away[i]
  
  # Mostra o Jogo que ta percorrendo
  message(paste('      ', teamM, 'x', teamV, '    Wk', rodada$Wk[i], '    Jogo', i))
  
  # Dados dos Mandantes e Visitantes
  iH = which(dados$Home == teamM)
  iV = which(dados$Away == teamV)
  cols_H = colnames(dados)[grep("H$", colnames(dados))]
  cols_A = colnames(dados)[grep("A$", colnames(dados))]
  teamM_dados = dados[iH, c("Date", "Away", "HomeGoals", "AwayConc", cols_A)] # jogando em casa
  teamV_dados = dados[iV, c("Date", "Home", "AwayGoals", "HomeConc", cols_H)] # jogando fora
  
  # Treino e Teste
  teamM_treino = teamM_dados[teamM_dados$Date < rodada$Date[i], ]
  teamM_teste = teamM_dados[teamM_dados$Date == rodada$Date[i], ]
  teamV_treino = teamV_dados[teamV_dados$Date < rodada$Date[i], ]
  teamV_teste = teamV_dados[teamV_dados$Date == rodada$Date[i], ]
  
  # Series
  # Treino
  teamM_ts = ts(teamM_treino$HomeGoals)
  teamV_ts = ts(teamV_treino$AwayGoals)
  # Teste
  teamM_ts_teste <- ts(teamM_teste$HomeGoals)
  teamV_ts_teste <- ts(teamV_teste$AwayGoals)
  
  # Covariáveis
  if (length(iH) > 0){
    x_teamM <- teamM_treino[, !(names(teamM_treino) %in% c('Date', 'Away', 'HomeGoals', 
                                                           paste0(teamM, '_A')))]
    
    x_teamM_teste <- teamM_teste[, !(names(teamM_teste) %in% c('Date', 'Away', 'HomeGoals', 
                                                               paste0(teamM, '_A')))]
  } else{
    x_teamM <- teamM_treino$AwayConc
    x_teamM_teste <- teamM_teste$AwayConc
  }
  
  if (length(iV) > 0){
    x_teamV <- teamV_treino[, !(names(teamV_treino) %in% c('Date', 'Home', 'AwayGoals', 
                                                           paste0(teamV, '_H')))]
    
    x_teamV_teste <- teamV_teste[, !(names(teamV_teste) %in% c('Date', 'Home', 'AwayGoals', 
                                                               paste0(teamV, '_H')))]
  } else{
    x_teamV <- teamV_treino$HomeConc
    x_teamV_teste <- teamV_teste$HomeConc
  }
  
  
  ####### MODELO PARA MANDANTE ###################
  
  # Verificando os parametros p e q
  
  # Tabela com os AICs
  
  if (x_null) {x_teamM = NULL}
  
  aics = matrix(c(0, parx_aic(3, 3, teamM_ts, x_teamM, link = 'log')), 
                nrow = 4, # p
                ncol = 4, # q
                byrow = T)
  colnames(aics) = 0:3
  rownames(aics) = 0:3
  
  # P e q
  pq <- best_aic(aics)
  p <- pq[1]; q <- pq[2]
  aicM <- pq[3]
  
  # Modelo com as variaveis das medias concedidas - Gols do Mandante
  mod_parxM <- parx(dados = teamM_ts, x = x_teamM, 
                    p, q, distr = 'poisson', link = 'log')
  
  # Coeficientes
  coM = mod_parxM$coefficients
  
  # Predicao
  if (x_null){
    lM = predict(mod_parxM, n.ahead=1)$pred
  } else{
    lM = predict(mod_parxM, n.ahead=1, newxreg=x_teamM_teste)$pred
  }
  
  # Poisson P(lM)
  pM = rep(NA, 7)
  names(pM) = 0:6
  
  for (i in 0:6){
    pM[i+1] = dpois(x = i, lambda = lM)
  }
  
  
  ####### MODELO PARA VISITANTE ###################
  
  # Verificando os parametros p e q
  
  # Tabela com os AICs
  
  if (x_null) {x_teamV = NULL}
  
  aics = matrix(c(0, parx_aic(3, 3, teamV_ts, x_teamV, 'log')), 
                nrow = 4, # p
                ncol = 4, # q
                byrow = T)
  colnames(aics) = 0:3
  rownames(aics) = 0:3
  
  # P e q
  pq <- best_aic(aics)
  p <- pq[1]; q <- pq[2]
  aicV <- pq[3]
  
  # Modelo com as variaveis das medias concedidas - Gols do Mandante
  mod_parxA <- parx(dados = teamV_ts, x = x_teamV, 
                    p, q, distr = 'poisson', link = 'log')
  
  # Coeficientes
  coA = mod_parxA$coefficients
  
  # Predicao
  if (x_null){
    lA = predict(mod_parxA, n.ahead=1)$pred
  } else{
    lA = predict(mod_parxA, n.ahead=1, newxreg=x_teamV_teste)$pred
  }
  
  # Poisson P(lM)
  pA = rep(NA, 7)
  names(pA) = 0:6
  
  for (i in 0:6){
    pA[i+1] = dpois(x = i, lambda = lA)
  }
  
  ############################### TABELAS #############################################
  
  # Placares
  resultado = pM %*% t(pA)
  
  # Probabilidades
  pVM = sum(resultado[lower.tri(resultado)])
  pE = sum(diag(resultado))
  pVV = sum(resultado[upper.tri(resultado)])
  
  # Tabela Final
  probs_resultado = data.frame(M = teamM, GM = teamM_teste$HomeGoals,
                               GV = teamV_teste$AwayGoals, V = teamV, 
                               pVM = round(pVM, 4), pE = round(pE, 4), pVV = round(pVV, 4), 
                               aicM = round(aicM, 4), aicV = round(aicV, 4))
  print(probs_resultado)
  message(" ")
  
  # Guarda na Lista
  retorno[[paste0(teamM," x ", teamV)]][["resultado"]] <- resultado
  retorno[[paste0(teamM," x ", teamV)]][["probs"]] <- probs_resultado
  
}
