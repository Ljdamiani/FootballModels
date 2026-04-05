

##################################################################################


# ________________________ Função Auxiliares ___________________ #


##################################################################################

# EMA
EMA_lag <- function(x, alpha) {
  out <- numeric(length(x))
  out[1] <- NA_real_
  
  for (i in 2:length(x)) {
    if (is.na(out[i-1])) {
      out[i] <- x[i-1]
    } else {
      out[i] <- alpha * x[i-1] + (1 - alpha) * out[i-1]
    }
  }
  
  return(out)
}

################################################################################


###############################################################



# ________________________ Models __________________________ #



##############################################################

#### INGARCH / PARX ###############

parx <- function(dados, x, p, q, distr, link){
  
  if (p == 0 & q == 0){
    
    model <- 'Erro'
    
  } else if (p == 0){
    
    model <- tsglm(dados,
                   xreg = x,
                   distr = distr, link = link,
                   model = list(past_obs = seq(1, q))) # q - pastobs
    
  } else if (q == 0) {
    
    model <- tsglm(dados,
                   xreg = x,
                   distr = distr, link = link,
                   model = list(past_mean = seq(1, p))) # p - pastmean
    
  } else{
    
    model <- tsglm(dados,
                   xreg = x,
                   distr = distr, link = link,
                   model = list(past_obs = seq(1, q), # q - pastobs
                                past_mean = seq(1, p))) # p - pastmean 
  }
  
  return(model)
}

#### Seleciona os AICs pelos modelos ###############

parx_aic <- function(pmax, qmax, serie, x, link, distr){
  
  # pmax = 3; qmax = 3; serie = teamM_ts; x = x_teamM; link = 'identity'; distr = 'poisson'
  # pmax = 3; qmax = 3; serie = teamV_ts; x = x_teamV; link = 'log'; distr = 'poisson'
  
  # Verificando os parametros p e q
  nd_pars <- expand.grid(pastobs = 0:qmax, pastmean = 0:pmax)
  nd_pars = nd_pars[-1, ] # sem 0 e 0
  nd_aic <- rep(0, nrow(nd_pars))
  
  # Progresso
  pb <- txtProgressBar(min = 0, max = length(nd_aic), 
                       style = 3)  # Criando a barra de progresso
  
  # Modelos
  models <- list()
  
  # Calculando os AICs para cada modelo
  for(i in seq(nd_aic)){
    
    # Parametros
    q = nd_pars[i, 1]  # q - pastobs
    p = nd_pars[i, 2]  # p - pastmean
    
    # Salva Modelo
    mod <- try(parx(serie, x, p, q, distr, link), silent = T)
    
    if (class(mod) == "try-error") {
      mensagem <- paste("Erro em parâmetros: p =", p, 'e q =', q)
      message('         ')
      message(mensagem)
      message('         ')
      models[[paste0('mod_',p,q)]] <- mod
      # AIC
      nd_aic[i] <- 99999
      
    } else {
      models[[paste0('mod_',p,q)]] <- mod
      # AIC
      nd_aic[i] <- AIC(mod)
    }
    
    setTxtProgressBar(pb, i)  # Atualizando o progresso
  }
  
  close(pb)
  
  # Procura o melhor
  aics = matrix(c(0, nd_aic), 
                nrow = pmax+1, # p linhas
                ncol = qmax+1, # q colunas
                byrow = T)
  colnames(aics) = 0:pmax
  rownames(aics) = 0:qmax
  
  message('         ')
  message('     AICS:  q (coluna)   p (linha)  ')
  message('         ')
  print(aics)
  message('         ')
  
  # P e q
  pq <- best_aic(aics)
  p <- pq[1]; q <- pq[2]
  aic <- pq[3]
  
  # Melhor Modelo
  model <- models[[paste0('mod_',p,q)]]
  
  return(model)
  
}


########################################################################

# Funcao Gera Modelos - Carro Chefe

gera_modelos <- function(rodada, dados, link, distr, 
                         x_null = FALSE, all_teams = FALSE, identificador = ''){
  
  # Times
  if (all_teams){
    teams1 = unique(dados$Home)
    teams2 = unique(dados$Away)
  } else{
    teams1 = unique(rodada$Home)
    teams2 = unique(rodada$Away)
  }
  
  # Models
  models <- list()
  
  # ACFs
  acfs <- list()
  
  # Modelos Mandantes
  for (i in 1:length(teams1)){
    
    # Mandante
    teamM = teams1[i]
    message(' ')
    message(identificador)
    message(paste(teamM, '    Mandante', '    ', i))
    
    # Dados dos Mandantes
    iH = which(dados$Home == teamM)
    teamM_dados = dados[iH, c("Date", "Away", "HomeGoals", "AwayConc")]
    #teamM_dados = dados[iH, c("Date", "Away", "HomeGoals", "AwayConc", "BIG6_A")]
    
    # Treino
    teamM_treino = teamM_dados[teamM_dados$Date < min(rodada$Date), ]
    teamM_ts = ts(teamM_treino$HomeGoals)
    x_teamM <- teamM_dados$AwayConc
    #x_teamM <- as.matrix(teamM_dados[,c('AwayConc', 'BIG6_A')])
    
    # ACFS
    acfs[[paste0(teamM,'_M')]] <- acf(teamM_ts, 
                                      lag.max = length(teamM_ts),
                                      main = paste0(teamM,'_M'))
    
    # Modelo (via AIC para parametros)
    if (x_null) {x_teamM = NULL}
    mod_M <- parx_aic(3, 3, teamM_ts, x_teamM, link, distr)
    
    # Salva Modelo
    models[[paste0(teamM,'_M')]] <- mod_M
    
  }
  
  # Modelos Visitantes
  for (i in 1:length(teams2)){
    
    # Visitante
    teamV = teams2[i]
    message(' ')
    message(identificador)
    message(paste(teamV, '    Visitante', '    ', i))
    
    # Dados dos Visitantes
    iV = which(dados$Away == teamV)
    teamV_dados = dados[iV, c("Date", "Home", "AwayGoals", "HomeConc")] 
    
    # Treino
    teamV_treino = teamV_dados[teamV_dados$Date < min(rodada$Date), ]
    teamV_ts = ts(teamV_treino$AwayGoals)
    x_teamV <- teamV_treino$HomeConc
    
    # ACFS
    acfs[[paste0(teamV,'_V')]] <- acf(teamV_ts, 
                                      lag.max = length(teamV_ts),
                                      main = paste0(teamV,'_V'))
    
    # Modelo (via AIC para parametros)
    if (x_null) {x_teamV = NULL}
    mod_V <- parx_aic(3, 3, teamV_ts, x_teamV, link, distr)
    
    # Salva Modelo
    models[[paste0(teamV,'_V')]] <- mod_V
    
  }
  
  return(models)
  
}



##################################################################################



# ________________________ Rodada Independente __________________________ #



##################################################################################


# Funcao Rodada Independente

rodada_ind <- function(rodada, dados, models, x_null = FALSE){
  
  retorno <- list()
  
  for (i in 1:nrow(rodada)){
    
    # i = 1
    
    # Mandante e Visitante
    teamM = rodada$Home[i]
    teamV = rodada$Away[i]
    
    # Mostra o Jogo que ta percorrendo
    message(paste('      ', teamM, 'x', teamV, '    Wk', rodada$Wk[i], '    Jogo', i))
    
    # Dados dos Mandantes e Visitantes
    iH = which(dados$Home == teamM)
    teamM_dados = dados[iH, c("Date", "Away", "HomeGoals", "AwayConc")]
    iV = which(dados$Away == teamV)
    teamV_dados = dados[iV, c("Date", "Home", "AwayGoals", "HomeConc")] 
    
    # Teste
    teamM_teste = teamM_dados[teamM_dados$Date == rodada$Date[i], ]
    teamV_teste = teamV_dados[teamV_dados$Date == rodada$Date[i], ]
    
    # Series Teste
    teamM_ts_teste <- ts(teamM_teste$HomeGoals)
    teamV_ts_teste <- ts(teamV_teste$AwayGoals)
    
    # Covariáveis
    x_teamM_teste <- teamM_teste$AwayConc
    x_teamV_teste <- teamV_teste$HomeConc
    
    
    ####### MODELO PARA MANDANTE ###################
    mod_M <- models[[paste0(teamM,'_M')]]
    
    # AIC
    aicM <- AIC(mod_M)
    
    # Predicao
    if (x_null){
      lM = predict(mod_M, n.ahead=1)$pred
    } else{
      lM = predict(mod_M, n.ahead=1, newxreg=x_teamM_teste)$pred
    }
    
    # Poisson P(lM)
    pM = rep(NA, 7)
    names(pM) = 0:6
    for (i in 0:6){
      pM[i+1] = dpois(x = i, lambda = lM)
    }
    
    ####### MODELO PARA VISITANTE ###################
    mod_V <- models[[paste0(teamV,'_V')]]
    
    # Predicao
    if (x_null){
      lV = predict(mod_V, n.ahead=1)$pred
    } else{
      lV = predict(mod_V, n.ahead=1, newxreg=x_teamV_teste)$pred
    }
    
    # AIC
    aicV <- AIC(mod_V)
    
    # Poisson P(lM)
    pV = rep(NA, 7)
    names(pV) = 0:6
    for (i in 0:6){
      pV[i+1] = dpois(x = i, lambda = lV)
    }
    
    ############################### TABELAS #############################################
    
    # Placares
    resultado = pM %*% t(pV)
    
    # Probabilidades
    pVM = sum(resultado[lower.tri(resultado)])
    pE = sum(diag(resultado))
    pVV = sum(resultado[upper.tri(resultado)])
    
    # Padroniza para soma 1
    total = pVM + pE + pVV
    pVM = pVM/total; pE = pE/total; pVV = pVV/total
    
    # Extras Probs
    BTTS = sum(resultado[-1, -1])
    Mais_de_1meio = (sum(resultado[2:7, 1]) + sum(resultado[1, 2:7]) + sum(resultado[-1, -1]))
    
    # Tabela Final
    probs_resultado = data.frame(M = teamM, GM = teamM_teste$HomeGoals,
                                 GV = teamV_teste$AwayGoals, V = teamV, 
                                 pVM = round(pVM, 4), pE = round(pE, 4), pVV = round(pVV, 4), 
                                 BTTS = round(BTTS, 4), 'p1.5' = round(Mais_de_1meio, 4),
                                 aicM = round(aicM, 4), aicV = round(aicV, 4))
    print(probs_resultado)
    message(" ")
    
    # Guarda na Lista
    retorno[[paste0(teamM," x ", teamV)]][["resultado"]] <- resultado
    retorno[[paste0(teamM," x ", teamV)]][["probs"]] <- probs_resultado
    
  }
  
  return(retorno)
  
}



##################################################################################



# ________________________ Funções para Cópulas __________________________ #



##################################################################################

est_copula <- function(rodada, dados, models){
  
  # Times
  teams1 = unique(dados$Home)
  teams2 = unique(dados$Away)
  
  # Dados Cópula
  dados_copula <- dados[dados$Date < min(rodada$Date), ]
  
  # Coluna de Resíduos
  dados_copula$ResH = NA
  dados_copula$ResA = NA
  
  # ACFs dos Resíduos
  acfs_res <- list()
  
  # Modelos Mandantes
  for (i in 1:length(teams1)){
    
    # Mandante
    teamM = teams1[i]
    print(paste(teamM, '    Mandante', '    ', i))
    
    # Dados dos Mandantes
    iH = which(dados_copula$Home == teamM)
    teamM_dados = dados_copula[iH, c("Date", "Away", "HomeGoals", "AwayConc")] # jogando em casa
    
    # Modelo
    mod_M <- models[[paste0(teamM,'_M')]]
    
    # Substitui os resíduos
    dados_copula[iH, 'ResH'] = dados_copula[iH,]$HomeGoals - mod_M$fitted.values
    
    # ACFS
    acfs_res[[paste0(teamM,'_M')]] <- acf(dados_copula[iH, 'ResH'], 
                                          lag.max = length(dados_copula[iH, 'ResH']),
                                          main = paste0(teamM,'_M'))
  }
  
  # Modelos Visitantes
  for (i in 1:length(teams2)){
    
    # Visitante
    teamV = teams2[i]
    print(paste(teamV, '    Visitante', '    ', i))
    
    # Dados dos Mandantes
    iV = which(dados_copula$Away == teamV)
    teamV_dados = dados_copula[iV, c("Date", "Home", "AwayGoals", "HomeConc")] 
    
    # Modelo
    mod_V <- models[[paste0(teamV,'_V')]]
    
    # Substitui os resíduos
    dados_copula[iV, 'ResA'] = dados_copula[iV,]$AwayGoals - mod_V$fitted.values
    
    # ACFS
    acfs_res[[paste0(teamV,'_V')]] <- acf(dados_copula[iV, 'ResA'], 
                                          lag.max = length(dados_copula[iV, 'ResA']),
                                          main = paste0(teamV,'_V'))
  }
  
  # ---------------------------------- #
  # Estimação da Cópula pelos Resíduos #
  # ---------------------------------- #
  
  # Acumuladas Empíricas
  
  # dados_copula$F_ResH <- sapply(dados_copula$ResH, function(x) ecdf(dados_copula$ResH)(x))
  # dados_copula$F_ResA <- sapply(dados_copula$ResA, function(x) ecdf(dados_copula$ResA)(x))
  
  matriz_dados_cop <- pobs(as.matrix(dados_copula[, c("ResH", "ResA")]))
  
  var_a <- matriz_dados_cop[,1]
  var_b <- matriz_dados_cop[,2]
  
  # Correlação
  # cor(matriz_dados_cop, method = "kendall")
  # hist(dados_copula$ResH)
  # hist(dados_copula$ResA)
  
  # https://www.r-bloggers.com/2016/03/how-to-fit-a-copula-model-in-r-heavily-revised-part-2-fitting-the-copula/
  
  # selectedCopula <- BiCopSelect(var_a, var_b, familyset = seq(1, 6))
  # print(selectedCopula)
  
  # Estimação -> ML
  gau <- fitCopula(normalCopula(), data = matriz_dados_cop, method = 'ml')
  aic_gau <- AIC(gau)
  
  frank <- fitCopula(frankCopula(), data = matriz_dados_cop, method = 'ml')
  aic_frank <- AIC(frank)
  
  clay <- fitCopula(claytonCopula(), data = matriz_dados_cop, method = 'ml')
  aic_clay <- AIC(clay)
  
  aics = c(aic_gau, aic_frank, aic_clay)
  
  # Retorno
  if (min(aics) == aic_gau){
    selectedCopula = list('Normal', gau)
  } else if (min(aics) == aic_frank){
    selectedCopula = list('Frank', frank)
  } else if (min(aics) == aic_clay){
    selectedCopula = list('Clayton', clay)
  }
  
  # Retorna a copula estimada
  return(selectedCopula)
}



##################################################################################


rodada_biv <- function(rodada, models, x_null = FALSE, name_copula, copula_model){
  
  retorno <- list()
  
  # Generate grid
  gols1 = 0:7; gols2 = 7:0
  grid = as.matrix(expand.grid(gols1, gols2))
  
  # Rodada
  for (i in 1:nrow(rodada)){
    
    #i = 1
    
    # Mandante e Visitante
    teamM = rodada$Home[i]
    teamV = rodada$Away[i]
    
    # Mostra o Jogo que ta percorrendo
    message(paste(teamM, 'x', teamV, '    Wk', rodada$Wk[i], '    Jogo', i))
    
    # Dados dos Mandantes e Visitantes
    iH = which(dados$Home == teamM)
    iV = which(dados$Away == teamV)
    teamM_dados = dados[iH, c("Date", "Away", "HomeGoals", "AwayConc")] # jogando em casa
    teamV_dados = dados[iV, c("Date", "Home", "AwayGoals", "HomeConc")] # jogando fora
    
    # Teste
    teamM_teste = teamM_dados[teamM_dados$Date == rodada$Date[i], ]
    teamV_teste = teamV_dados[teamV_dados$Date == rodada$Date[i], ]
    x_teamM_teste <- teamM_teste$AwayConc
    x_teamV_teste <- teamV_teste$HomeConc
    
    # Modelos
    mod_M = models[[paste0(teamM, '_M')]]
    mod_V = models[[paste0(teamV, '_V')]]
    
    # ACIS
    aicM = AIC(mod_M)
    aicV = AIC(mod_V)
    
    # Predicao
    if (x_null){
      lM = predict(mod_M, n.ahead=1)$pred
      lV = predict(mod_V, n.ahead=1)$pred
    } else{
      lM = predict(mod_M, n.ahead=1, newxreg=x_teamM_teste)$pred
      lV = predict(mod_V, n.ahead=1, newxreg=x_teamV_teste)$pred
    }
    
    
    ###################################################
    ### __________ Prediction - Bivariate _________ ###
    ###################################################
    # https://www.r-bloggers.com/2015/10/modelling-dependence-with-copulas-in-r/
    
    # Parâmetros copula
    rho <- copula_model@estimate
    
    # Copula Selecionada
    if (name_copula == 'Normal'){
      biFit = normalCopula(param = rho, dim = 2)
    } else if (name_copula == 'Frank'){
      biFit = frankCopula(param = rho, dim = 2)
    } else if (name_copula == 'Clayton'){
      biFit = claytonCopula(param = rho, dim = 2)
    }
    
    # Build the bivariate distribution
    biv <- mvdc(biFit, margins = c("pois","pois"), 
                paramMargins = list(list(lambda = lM), 
                                    list(lambda = lV)))
    
    # Compute the density
    pm <- dMvdc(grid, biv)
    
    # Resultados
    rslts = cbind(grid, pm)
    
    # Matriz - Placares
    resultado = matrix(NA, 8, 8)
    
    for (i in 1:nrow(rslts)){
      # Guarda a probabilidade do placar (i -> linha Home / j -> coluna Away)
      resultado[rslts[i, 1] + 1, rslts[i, 2] + 1] = rslts[i, 3]
    }
    row.names(resultado) = 0:7; colnames(resultado) = 0:7
    
    ############################### TABELAS #############################################
    
    # Probabilidades
    pVM = sum(resultado[lower.tri(resultado)])
    pE = sum(diag(resultado))
    pVV = sum(resultado[upper.tri(resultado)])
    
    # Padroniza para soma 1
    total = pVM + pE + pVV
    pVM = pVM/total; pE = pE/total; pVV = pVV/total
    
    # Extras Probs
    BTTS = sum(resultado[-1, -1])
    Mais_de_1meio = (sum(resultado[2:7, 1]) + sum(resultado[1, 2:7]) + sum(resultado[-1, -1]))
    
    # Tabela Final
    probs_resultado = data.frame(M = teamM, GM = teamM_teste$HomeGoals,
                                 GV = teamV_teste$AwayGoals, V = teamV, 
                                 pVM = round(pVM, 4), pE = round(pE, 4), pVV = round(pVV, 4), 
                                 BTTS = round(BTTS, 4), 'p1.5' = round(Mais_de_1meio, 4),
                                 aicM = round(aicM, 4), aicV = round(aicV, 4))
    
    print(probs_resultado)
    message(" ")
    
    # Guarda na Lista
    retorno[[paste0(teamM," x ", teamV)]][["resultado"]] <- resultado
    retorno[[paste0(teamM," x ", teamV)]][["probs"]] <- probs_resultado
    
  }
  
  return(retorno)
  
}

##################################################################################

#### DIXON COLES ###############

fit_dc_model <- function(data_train) {
  my_weights = weights_dc(data_train$Match_Date, xi=0.0019)
  goalmodel(
    goals1 = data_train$home_goals,
    goals2 = data_train$away_goals,
    team1  = data_train$home_team,
    team2  = data_train$away_team,
    weights = my_weights,
    dc  = T
  )
}

predict_dc_match <- function(model, home_team, away_team) {
  prob_matrix <- predict_result(
    model,
    team1 = home_team,
    team2 = away_team,
    return_df = T
  )
  return(prob_matrix)
}

backtest_dc <- function(data, season_test) {
  
  data <- data %>% arrange(Match_Date)
  results <- data.frame()
  test_idx <- which(data$Season == season_test)
  
  for(i in test_idx){
    
    # Train Data
    data_train <- data[1:(i-1), ]
    data_test <- data[i, ]
    model_dc <- fit_dc_model(data_train)
    
    # Predict
    P <- predict_dc_match(
      model_dc,
      home_team = data_test$home_team,
      away_team = data_test$away_team
    )
    P$Match_Date = data$Match_Date[i]
    results <- bind_rows(results, P)
    cat("Match", i-test_idx[1]+1, " - ", data$home_team[i], " x ",  data$away_team[i], "\n")
  }
  return(results)
}

