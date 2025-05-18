
# ______________ MIX MODEL _______________ #


# Packages
library(dplyr)
library(tscount)
library(copula)

season = dados[dados$Date >= '2022-08-05', ]
rownames(season) <- NULL

season$Wk[177] = 19 # era 7
season$Wk[188] = 20 # era 7
season$Wk[189] = 20 # era 7
season$Wk[210] = 23 # era 8
season$Wk[221] = 23.5 # era 12
season$Wk[240] = 25 # era 7
season$Wk[241] = 25.5 # era 7
season$Wk[262] = 27.5 # era 7
season$Wk[263] = 27 # era 8
season$Wk[281:286] = 29.5 # era 7, 7, 7, 8, 25, 7
season$Wk[335:337] = 34.5 # eram 28
season$Wk[358] = 36.5 # era 25
season$Wk[369:370] = 37.5 # eram 32

wks = c(24, 25, 26, 27, 28, 29, 
        30, 31, 32, 33, 34, 35, 36, 37, 38)
teams = unique(dados$Home)
aux = c('_M', '_V')
identificador = c()

for (k in 1:length(wks)){ # rodadas
  
  models <- list()
  wk = wks[k]
  
  for (t in 1:length(teams)){ # teams
    for (o in 1:2){ # mandante ou visitante
    
      team =  paste0(teams[t], aux[o])
  
      # Compara Modelos Via AIC
      mod1 = models_rodada[[paste('rodada', wk)]][[1]][[team]]
      mod2 = models_rodada[[paste('rodada', wk)]][[2]][[team]]
      mod3 = models_rodada[[paste('rodada', wk)]][[3]][[team]]
      # mod4 = models_rodada[[paste('rodada', wk)]][[4]][[team]]
      
      aics = c(AIC(mod1), AIC(mod2), AIC(mod3)) #, AIC(mod4))
      indice = which(aics == min(aics))
      
      if (indice == 1){mod = mod1; identificador = append(identificador, 'I sem cov')}
      if (indice == 2){mod = mod2; identificador = append(identificador, 'I')}
      if (indice == 3){mod = mod3; identificador = append(identificador, 'Log sem cov')}
      # if (indice == 4){mod = mod4; identificador = append(identificador, 'Log')}
      
      # Salva Modelo
      models[[team]] <- mod  
  
    }
  }
  models_rodada[[paste('rodada', wk)]][[5]] = models
}

table(identificador)


# ______________ PROBS MIX MODEL _______________ #


#########################
#_____ INDEPENDENTE ____#
#########################

for (k in 1:length(wks)){ # rodadas
  
  wk = wks[k]
  # rodada = dados[dados$Wk == wk & dados$Date > '2022-08-05', ]
  rodada = season[season$Wk == wk, ]
  retorno = list()
  models <- models_rodada[[paste('rodada', wk)]][[5]]
  
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
    if ("eta" %in% substr(names(coef(mod_M)), 1, 3)){
      lM = predict(mod_M, n.ahead=1, newxreg=x_teamM_teste)$pred
    } else{
      lM = predict(mod_M, n.ahead=1)$pred
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
    if ("eta" %in% substr(names(coef(mod_V)), 1, 3)){
      lV = predict(mod_V, n.ahead=1, newxreg=x_teamV_teste)$pred
    } else{
      lV = predict(mod_V, n.ahead=1)$pred
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
  
  probs_mix = c(); for(i in 1:nrow(rodada)){probs_mix = rbind(probs_mix, retorno[[i]][[2]])}
  probs_rodada[[paste('rodada', wk)]][['Mix']][[1]] <- probs_mix
  
}


#########################
#_____ Copula ____#
#########################


for (k in 1:length(wks)){ # rodadas
  
  wk = wks[k]
  # rodada = dados[dados$Wk == wk & dados$Date > '2022-08-05', ]
  rodada = season[season$Wk == wk, ]
  models <- models_rodada[[paste('rodada', wk)]][[5]]
  
  # Copula
  selectedCopula <- est_copula(rodada, dados, models) # Seleciona a Copula
  name_copula <- selectedCopula[[1]]
  copula_model <- selectedCopula[[2]]
  
  # Probabilidades
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
    if ("eta" %in% substr(names(coef(mod_M)), 1, 3)){
      lM = predict(mod_M, n.ahead=1, newxreg=x_teamM_teste)$pred
    } else{
      lM = predict(mod_M, n.ahead=1)$pred
    }
    
    # Predicao
    if ("eta" %in% substr(names(coef(mod_V)), 1, 3)){
      lV = predict(mod_V, n.ahead=1, newxreg=x_teamV_teste)$pred
    } else{
      lV = predict(mod_V, n.ahead=1)$pred
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
  
  probs_mix_cop = c(); for(i in 1:nrow(rodada)){probs_mix_cop = rbind(probs_mix_cop, retorno[[i]][[2]])}
  probs_rodada[[paste('rodada', wk)]][['Mix']][[2]] <- probs_mix_cop

}







