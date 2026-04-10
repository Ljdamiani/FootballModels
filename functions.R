

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


###############################################################



# ________________________ Models __________________________ #



##############################################################

#### PARX (INGARCH) ###############

# parx <- function(dados, x, p, q, distr, link){
#   
#   if (p == 0 & q == 0){
#     
#     model <- 'Erro'
#     
#   } else if (p == 0){
#     
#     model <- tsglm(dados,
#                    xreg = x,
#                    distr = distr, link = link,
#                    model = list(past_obs = seq(1, q))) # q - pastobs
#     
#   } else if (q == 0) {
#     
#     model <- tsglm(dados,
#                    xreg = x,
#                    distr = distr, link = link,
#                    model = list(past_mean = seq(1, p))) # p - pastmean
#     
#   } else{
#     
#     model <- tsglm(dados,
#                    xreg = x,
#                    distr = distr, link = link,
#                    model = list(past_obs = seq(1, q), # q - pastobs
#                                 past_mean = seq(1, p))) # p - pastmean 
#   }
#   
#   return(model)
# }

fit_parx_team <- function(
    y,
    x = NULL,
    p_max = 2,
    q_max = 2,
    links = c("log","identity"),
    distr = "poisson",
    max_vars = 10
){
  
  # caso sem covariáveis
  if(is.null(x)){
    x_list <- list(NULL)
  } else{
    
    x <- as.matrix(x)
    
    # começa com nenhuma variável
    x_list <- list(NULL)
    
    # seleção forward
    selected <- c()
    remaining <- 1:ncol(x)
    
    best_aic <- Inf
    
    repeat{
      
      aic_try <- rep(Inf, length(remaining))
      
      for(i in seq_along(remaining)){
        
        vars <- c(selected, remaining[i])
        
        fit <- try(
          tsglm(
            y,
            xreg = x[,vars,drop=FALSE],
            model=list(past_obs=1,past_mean=1)
          ),
          silent=TRUE
        )
        
        if(!inherits(fit,"try-error")){
          aic_try[i] <- AIC(fit)
        }
      }
      
      if(min(aic_try) < best_aic){
        
        best <- which.min(aic_try)
        
        selected <- c(selected, remaining[best])
        remaining <- remaining[-best]
        
        best_aic <- min(aic_try)
        
      } else break
      
      if(length(selected) >= max_vars) break
    }
    
    x_list <- list(
      NULL,
      x[,selected,drop=FALSE]
    )
  }
  
  grid <- expand.grid(
    p = 0:p_max,
    q = 0:q_max,
    link = links
  )
  
  best_model <- NULL
  best_aic <- Inf
  best_spec <- NULL
  
  for(g in 1:nrow(grid)){
    
    p <- grid$p[g]
    q <- grid$q[g]
    link <- grid$link[g]
    
    model_list <- list()
    if(q>0) model_list$past_obs <- 1:q
    if(p>0) model_list$past_mean <- 1:p
    
    for(xreg in x_list){
      
      fit <- try(
        tsglm(
          y,
          xreg = xreg,
          distr = distr,
          link = link,
          model = model_list
        ),
        silent = TRUE
      )
      
      if(!inherits(fit,"try-error")){
        
        aic <- AIC(fit)
        
        if(aic < best_aic){
          
          best_aic <- aic
          best_model <- fit
          
          best_spec <- list(
            p = p,
            q = q,
            link = link,
            vars = if(is.null(xreg)) NULL else colnames(xreg),
            AIC = aic
          )
        }
      }
    }
  }
  
  attr(best_model,"best_spec") <- best_spec
  
  return(best_model)
}

fit_parx_team <- function(
    y,
    x = NULL,
    p_max = 3,
    q_max = 3,
    links = c("log", "identity"),
    distr = "poisson"
){
  
  grid <- expand.grid(
    p = 0:p_max,
    q = 0:q_max,
    link = links
  )
  
  if(!is.null(x)){
    mu <- colMeans(x, na.rm=TRUE)
    sd <- apply(x,2,sd,na.rm=TRUE)
    
    x <- scale(x, center = mu, scale = sd)
  } else{
    mu <- NULL
    sd <- NULL
  }
  
  best_aic <- Inf
  best_model <- NULL
  best_spec <- NULL
  
  for(i in 1:nrow(grid)){
    
    p <- grid$p[i]
    q <- grid$q[i]
    link <- as.character(grid$link[i])
    
    model_list <- list()
    
    if(q > 0) model_list$past_obs  <- 1:q
    if(p > 0) model_list$past_mean <- 1:p
    
    fit <- try(
      tsglm(
        y,
        xreg = x,
        distr = distr,
        link = link,
        model = model_list
      ),
      silent = TRUE
    )
    
    if(!inherits(fit,"try-error")){
      
      aic <- AIC(fit)
      
      if(is.finite(aic) && aic < best_aic){
        
        best_aic <- aic
        best_model <- fit
        
        best_spec <- list(
          p = p,
          q = q,
          link = link,
          AIC = aic
        )
      }
    }
  }
  
  attr(best_model,"best_spec") <- best_spec
  
  return(best_model)
}

fit_parx_league <- function(
    data,
    y_col,
    x_col,
    p_max = 3,
    q_max = 3
){
  
  data = df_parx
  y_col = "Score"
  x_col = c(paste0("MA_", vars_all), "newly_promoted_team", "newly_promoted_opp")
  p_max = 3
  q_max = 3
  teams <- unique(data$Team)
  models <- list()
  
  for(team in teams){
    
    # HOME
    dH <- data[(data$Team == team)&(data$Home_Away == "Home"), ]
    y <- dH[[y_col]]
    x <- if(!is.null(x_col)) as.matrix(dH[, x_col]) else NULL
    
    models[[paste0(team,"_H")]]
    teste <- fit_parx_team(y, x, p_max, q_max)
    
    # AWAY
    dA <- data[data$Away == team, ]
    y <- dA[[y_away]]
    x <- if(!is.null(x_col)) as.matrix(dA[, x_col]) else NULL
    
    models[[paste0(team,"_A")]] <- 
      fit_parx_team(y, x, p_max, q_max)
  }
  
  return(models)
}

backtest_parx <- function(data, season_test){
  
  data <- data[order(data$Date), ]
  
  test_idx <- which(data$Season == season_test)
  
  results <- list()
  
  for(i in test_idx){
    
    cat("Match", i-test_idx[1]+1,"\n")
    
    # treino até t-1
    train <- data[1:(i-1), ]
    
    # 1) FIT TODOS TIMES
    models <- fit_parx_league(train)
    
    # 2) RESÍDUOS
    train_res <- compute_parx_residuals(train, models)
    
    # 3) CÓPULA
    cop <- estimate_parx_copula(train_res)
    
    # 4) PREVER JOGO
    game <- data[i,]
    
    lambda <- predict_parx_lambda(
      game$Home,
      game$Away,
      models
    )
    
    # 5) BIVARIADA
    prob <- predict_parx_copula(
      lambda$lambda_H,
      lambda$lambda_A,
      cop$model
    )
    
    results[[i]] <- list(
      match = game,
      prob = prob,
      copula = cop$name
    )
  }
  
  return(results)
}

backtest_parx_fast <- function(data, season_test){
  
  data <- data[order(data$Date), ]
  test_idx <- which(data$Season == season_test)
  
  # inicializa modelos com treino inicial
  train0 <- data[1:(test_idx[1]-1), ]
  models <- fit_parx_league(train0)
  
  results <- list()
  
  for(i in test_idx){
    
    game <- data[i,]
    
    # 1 previsão com modelos atuais
    lambda <- predict_parx_lambda(
      game$Home,
      game$Away,
      models
    )
    
    # 2 resíduos + cópula
    train <- data[1:(i-1), ]
    
    train_res <- compute_parx_residuals(train, models)
    
    cop <- estimate_parx_copula(train_res)
    
    # 3 previsão bivariada
    prob <- predict_parx_copula(
      lambda$lambda_H,
      lambda$lambda_A,
      cop$model
    )
    
    results[[i]] <- prob
    
    # 4 atualiza modelos APENAS times afetados
    models <- update_parx_models(
      models,
      data[1:i,],
      game$Home,
      game$Away
    )
  }
  
  return(results)
}

update_parx_models <- function(
    models,
    data,
    home,
    away,
    x_home = NULL,
    x_away = NULL
){
  
  # HOME team update
  dH <- data[data$Home == home, ]
  
  y <- dH$HomeGoals
  x <- if(!is.null(x_home)) as.matrix(dH[,x_home]) else NULL
  
  models[[paste0(home,"_H")]] <- 
    fit_parx_team(y, x)
  
  
  # AWAY team update
  dA <- data[data$Away == away, ]
  
  y <- dA$AwayGoals
  x <- if(!is.null(x_away)) as.matrix(dA[,x_away]) else NULL
  
  models[[paste0(away,"_A")]] <- 
    fit_parx_team(y, x)
  
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

compute_parx_residuals <- function(data, models){
  
  data$ResH <- NA
  data$ResA <- NA
  
  teams <- unique(c(data$Home, data$Away))
  
  for(team in teams){
    
    # HOME
    idxH <- data$Home == team
    
    modH <- models[[paste0(team,"_H")]]
    
    data$ResH[idxH] <- 
      data$HomeGoals[idxH] - modH$fitted.values
    
    
    # AWAY
    idxA <- data$Away == team
    
    modA <- models[[paste0(team,"_A")]]
    
    data$ResA[idxA] <- 
      data$AwayGoals[idxA] - modA$fitted.values
  }
  
  return(data)
}

estimate_parx_copula <- function(data){
  
  U <- pobs(as.matrix(data[,c("ResH","ResA")]))
  
  fit_gau   <- fitCopula(normalCopula(),  U, method="ml")
  fit_frank <- fitCopula(frankCopula(),   U, method="ml")
  fit_clay  <- fitCopula(claytonCopula(), U, method="ml")
  
  aics <- c(
    AIC(fit_gau),
    AIC(fit_frank),
    AIC(fit_clay)
  )
  
  best <- which.min(aics)
  
  list(
    name = c("Normal","Frank","Clayton")[best],
    model = list(fit_gau,fit_frank,fit_clay)[[best]]
  )
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


predict_parx_lambda <- function(
    home,
    away,
    models,
    x_home = NULL,
    x_away = NULL
){
  
  mod_H <- models[[paste0(home,"_H")]]
  mod_A <- models[[paste0(away,"_A")]]
  
  lH <- predict(mod_H, n.ahead = 1, newxreg = x_home)$pred
  lA <- predict(mod_A, n.ahead = 1, newxreg = x_away)$pred
  
  return(list(lambda_H = lH, lambda_A = lA))
}

predict_parx_independent <- function(lambda_H, lambda_A, max_goals = 7){
  
  pH <- dpois(0:max_goals, lambda_H)
  pA <- dpois(0:max_goals, lambda_A)
  
  outer(pH, pA)
}

predict_parx_copula <- function(lambda_H, lambda_A, copula){
  
  biv <- mvdc(
    copula,
    margins = c("pois","pois"),
    paramMargins = list(
      list(lambda = lambda_H),
      list(lambda = lambda_A)
    )
  )
  
  grid <- expand.grid(0:7,0:7)
  pm <- dMvdc(as.matrix(grid), biv)
  
  matrix(pm, 8, 8)
}


##################################################################################

#### DIXON COLES ###############

fit_dc_model <- function(data_train, x1 = NULL, x2 = NULL, xi = 0.0019) {
  
  my_weights <- weights_dc(data_train$Match_Date, xi = xi)
  
  # Sem covariáveis
  if (is.null(x1) && is.null(x2)) {
    
    model <- goalmodel(
      goals1 = data_train$home_goals,
      goals2 = data_train$away_goals,
      team1  = data_train$home_team,
      team2  = data_train$away_team,
      weights = my_weights,
      dc = TRUE
    )
    
  } else {
    
    X1 <- as.matrix(x1)
    X2 <- as.matrix(x2)
    X1 <- scale(X1)
    X2 <- scale(X2)
    
    model <- goalmodel(
      goals1 = data_train$home_goals,
      goals2 = data_train$away_goals,
      team1  = data_train$home_team,
      team2  = data_train$away_team,
      x1 = X1,
      x2 = X2,
      weights = my_weights,
      dc = TRUE
    )
    
  }
  return(model)
}

predict_dc_match <- function(model, home_team, away_team, x1 = NULL, x2 = NULL) {
  
  if (is.null(x1) && is.null(x2)) {
    prob_matrix <- predict_result(
      model,
      team1 = home_team,
      team2 = away_team,
      return_df = TRUE
    )
  } else {
    prob_matrix <- predict_result(
      model,
      team1 = home_team,
      team2 = away_team,
      x1 = as.matrix(x1),
      x2 = as.matrix(x2),
      return_df = TRUE
    )
  }
  return(prob_matrix)
}

backtest_dc <- function(data, season_test, cols_x1 = NULL, cols_x2 = NULL) {
  
  data <- data %>% arrange(Match_Date)
  results <- data.frame()
  test_idx <- which(data$Season == season_test)
  
  for(i in test_idx){
    
    data_train <- data[1:(i-1), ]
    data_test  <- data[i, ]
    
    # Extrair covariáveis se existirem
    if (!is.null(cols_x1)) {
      X1_train <- data_train[, cols_x1, drop = FALSE]
      X2_train <- data_train[, cols_x2, drop = FALSE]
      
      X1_test  <- data_test[, cols_x1, drop = FALSE]
      X2_test  <- data_test[, cols_x2, drop = FALSE]
      
    } else {
      X1_train <- X2_train <- NULL
      X1_test  <- X2_test  <- NULL
    }
    
    model_dc <- fit_dc_model(
      data_train,
      x1 = X1_train,
      x2 = X2_train
    )
    
    P <- predict_dc_match(
      model_dc,
      home_team = data_test$home_team,
      away_team = data_test$away_team,
      x1 = X1_test,
      x2 = X2_test
    )
    
    P$Match_Date <- data$Match_Date[i]
    results <- bind_rows(results, P)

    cat(
      "Match", i - test_idx[1] + 1, " - ",
      data$home_team[i], " x ", data$away_team[i], "\n"
    )
  }
  colnames(results) <- c("Home", "Away", "PH", "PD", "PA", "Date")
  results <- results %>% select(Date, everything())
  return(results)
}


##################################################################################

#### POISSON BIVARIADA ###############


fit_bp_footbayes <- function(data_train) {
  fit <- mle_foot(
    data  = data_train,
    model = "biv_pois",
    predict = 1,
    interval = "Wald"
  )
  return(fit)
}

predict_bp_footbayes <- function(model, data_train, home_team, away_team) {
  pred <- foot_prob_modified(
    object = model,
    data = data_train,
    home_team = home_team,
    away_team = away_team
  )
  return(pred)
}

backtest_bp_footbayes <- function(data, season_test, datas) {

  # data = df_bivpois
  # season_test = 2024
  # datas = datas_bivpois
  results <- data.frame()
  test_idx <- which(data$periods == season_test)
  
  for(i in test_idx){
    
    # if(i == 1) next
    data_train <- data[1:i, ] # prediction row together
    
    model <- fit_bp_footbayes(data_train)
    pred <- predict_bp_footbayes(model, data_train)
    results <- bind_rows(results, pred$prob_table)
    results$Match_Date = datas[i]
    
    cat("Match", i - min(test_idx) + 1, " - ",
        data$home_team[i], " x ", data$away_team[i], "\n")
  }
  colnames(results) <- c("Home", "Away", "PH", "PD", "PA", "Date")
  results <- results %>% select(Date, everything())  
  return(results)
}

foot_prob_modified <- function (object, data, home_team, away_team) {
  required_cols <- c("periods", "home_team", "away_team", 
                     "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:", paste(missing_cols, 
                                                          collapse = ", ")))
  }
  teams <- unique(c(data$home_team, data$away_team))
  if (inherits(object, "list")) {
    predict <- object$predict
  }
  else {
    stop("Provide one among these four model fit classes: 'stanfit', 'CmdStanFit', 'stanFoot' or 'list'.")
  }
  if (is.null(predict) || predict == 0) {
    stop("foot_prob cannot be used if the 'predict' argument is set to zero.")
  }
  data_prev <- data[(dim(data)[1] - predict + 1):(dim(data)[1]), 
  ]
  if (missing(home_team) & missing(away_team)) {
    home_team <- data_prev$home_team
    away_team <- data_prev$away_team
  }
  if (length(home_team) != length(away_team)) {
    stop("Please, include the same number for home and away teams.")
  }
  find_match <- c()
  for (i in 1:length(home_team)) {
    find_match[i] <- which(data_prev$home_team %in% home_team[i] & 
                             data_prev$away_team %in% away_team[i])
  }
  true_goal_home <- data_prev$home_goals[find_match]
  true_goal_away <- data_prev$away_goals[find_match]
  if (length(find_match) == 0) {
    stop(paste("There is not any out-of-sample match:", 
               home_team, "-", away_team, sep = ""))
  }
  if (inherits(object, "list")) {
    model <- object$model
    predict <- object$predict
    n.iter <- 200
    team1_prev <- object$team1_prev
    team2_prev <- object$team2_prev
    N_prev <- predict
    prediction_routine <- function(team1_prev, team2_prev, 
                                   att, def, home, corr, ability, model, predict, n.iter) {
      mean_home <- exp(home[1, 2] + att[team1_prev, 2] + 
                         def[team2_prev, 2])
      mean_away <- exp(att[team2_prev, 2] + def[team1_prev, 
                                                2])
      if (model == "double_pois") {
        x <- y <- matrix(NA, n.iter, predict)
        for (n in 1:N_prev) {
          x[, n] <- rpois(n.iter, mean_home[n])
          y[, n] <- rpois(n.iter, mean_away[n])
        }
      }
      else if (model == "biv_pois") {
        couple <- array(NA, c(n.iter, predict, 2))
        for (n in 1:N_prev) {
          couple[, n, ] <- rbvpois(n.iter, a = mean_home[n], 
                                   b = mean_away[n], c = corr[1, 2])
        }
        x <- couple[, , 1]
        y <- couple[, , 2]
      }
      else if (model == "skellam") {
        diff_y <- matrix(NA, n.iter, predict)
        for (n in 1:N_prev) {
          diff_y[, n] <- rskellam(n.iter, mu1 = mean_home[n], 
                                  mu2 = mean_away[n])
        }
        x <- diff_y
        y <- matrix(0, n.iter, predict)
      }
      else if (model == "student_t") {
        sigma_y <- object$sigma_y
        diff_y <- matrix(NA, n.iter, predict)
        for (n in 1:N_prev) {
          diff_y[, n] <- rt.scaled(n.iter, df = 7, mean = home[1, 
                                                               2] + ability[team1_prev[n], 2] - ability[team2_prev[n], 
                                                                                                        2], sd = sigma_y)
        }
        x <- round(diff_y)
        y <- matrix(0, n.iter, predict)
      }
      prob_func <- function(mat_x, mat_y) {
        if(is.vector(mat_x)){
          res <- mat_x - mat_y
          prob_h <- sum(res > 0)/n.iter
          prob_d <- sum(res == 0)/n.iter
          prob_a <- sum(res < 0)/n.iter
        } else{
          res <- mat_x - mat_y
          prob_h <- apply(res, 2, function(x) sum(x > 0))/n.iter
          prob_d <- apply(res, 2, function(x) sum(x == 0))/n.iter
          prob_a <- apply(res, 2, function(x) sum(x < 0))/n.iter
        }
        return(list(prob_h = prob_h, prob_d = prob_d, prob_a = prob_a))
      }
      conf <- prob_func(x, y)
      tbl <- data.frame(home_team = teams[team1_prev[find_match]], 
                        away_team = teams[team2_prev[find_match]], prob_h = conf$prob_h[find_match], 
                        prob_d = conf$prob_d[find_match], prob_a = conf$prob_a[find_match])
      return(tbl)
    }
    if (predict != 0) {
      prob_matrix <- prediction_routine(team1_prev, team2_prev, 
                                        object$att, object$def, object$home, object$corr, 
                                        object$abilities, model, predict, n.iter)
    }
    return(list(prob_table = prob_matrix))
  }
}





