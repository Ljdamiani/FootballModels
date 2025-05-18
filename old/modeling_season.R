


########################################################################

# ____________ # Modeling using PARX with Copulas  # ________________ #

# _________________ Author: Leonardo Damiani ________________________ # 

#######################################################################

# Season 2022/23

# Directory
setwd("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models")

# Packages
library(dplyr)
library(tscount)
library(copula)
library(VineCopula)
library(lubridate)
library(measures)

# source("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models/get_data.R")
source("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models/covariaveis.R")
source("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models/functions.R")

# Data
load("dados/seasons.RData")
seasons_df = seasons_df[seasons_df$Date < '2023-08-10', ]

# Rodada
rodada_max = seasons_df[seasons_df$Wk == 38 & seasons_df$Date > '2022-08-05', ]

# Dados desde de 2010
# Calcula as médias de gols concedidos (Parâmetro de Defesa)
dados <- concedidosX(seasons_df, rodada_max, c(2, 2))

# Covariável 


# Campeonato a ser Predito
season = dados[dados$Date >= '2022-08-05', ]
rownames(season) <- NULL

# Correção de jogos fora da data correta da rodada (entrarão em outras rodadas)
i_jogos = c(177, 188, 189, 210, 221, 240, 241, 262, 263, 281:286, 335:337, 358, 369:370)
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

wks = unique(season$Wk)
wks

wks = c(33, 34, 35, 36, 37, 38)
wks = c(24)

###########################################################################

# models_rodada = list()
# probs_rodada = list()
# copulas_rodada = list()

for (k in 1:length(wks)){
  
  wk = wks[k]
  rodada = season[season$Wk == wk, ]
  
  # --------------------------------------------------------------- #
  ####### ______________ PARX / INGARCH (p, q) ____________  ########
  # --------------------------------------------------------------- #
  
  #########################
  #_______ MODELOS _______#
  #########################
  
  
  # Poisson + Identity + Sem cov
  message('    ')
  message('Identidade Sem Cov')
  message('    ')
  models_I_semx <- gera_modelos(rodada, dados, link = 'identity', distr = 'poisson', 
                                x_null = T, all_teams = T, 
                                identificador = paste('Identidade Sem Cov', wk))
  
  # Poisson + Identity (ALL)
  message('    ')
  message('Identidade')
  message('    ')
  models_I <- gera_modelos(rodada, dados, link = 'identity', distr = 'poisson', 
                           x_null = FALSE, all_teams = T, 
                           identificador = paste('Identidade', wk))
  
  # Poisson + Log + Sem cov
  message('    ')
  message('Log Sem Cov')
  message('    ')
  models_log_semx <- gera_modelos(rodada, dados, link = 'log', distr = 'poisson',
                                 x_null = T, all_teams = T,
                                 identificador = paste('Log Sem Cov', wk))

  # Poisson + Log (ALL)
  message('    ')
  message('Log')
  message('    ')
  models_log <- gera_modelos(rodada, dados, link = 'log', distr = 'poisson',
                            x_null = FALSE, all_teams = T,
                            identificador = paste('Log', wk))


  
  
  #########################
  #_____ INDEPENDENTE ____#
  #########################
  
  
  # Poisson + Identity + Sem cov
  rslts_I_semx <- rodada_ind(rodada, dados, models_I_semx, x_null = TRUE)
  probs_I_semx = c(); for(i in 1:length(rslts_I_semx)){
    probs_I_semx = rbind(probs_I_semx, rslts_I_semx[[i]][[2]])}
  probs_I_semx
  
  # Poisson + Identity (ALL)
  rslts_I <- rodada_ind(rodada, dados, models_I)
  probs_I = c(); for(i in 1:length(rslts_I)){
    probs_I = rbind(probs_I, rslts_I[[i]][[2]])}
  probs_I
  
  # Poisson + Log + Sem cov
  rslts_log_semx <- rodada_ind(rodada, dados, models_log_semx, x_null = T)
  probs_log_semx = c(); for(i in 1:length(rslts_log_semx)){
    probs_log_semx = rbind(probs_log_semx, rslts_log_semx[[i]][[2]])}
  probs_log_semx
  
  # Poisson + Log (ALL)
  rslts_log <- rodada_ind(rodada, dados, models_log)
  probs_log = c(); for(i in 1:length(rslts_log)){
    probs_log = rbind(probs_log, rslts_log[[i]][[2]])}
  probs_log
  
  
  
  #########################
  #_____ Copula ____#
  #########################
  
  # Poisson + Identity + Sem cov
  selectedCopula <- est_copula(rodada, dados, models_I_semx) # Seleciona a Copula
  name_copula_I_semx <- selectedCopula[[1]]
  name_copula_I_semx
  copula_model_I_semx <- selectedCopula[[2]]
  
  # Probabilidades
  rslts_I_semx_cop <- rodada_biv(rodada, models_I_semx, x_null = T,
                                 name_copula_I_semx, copula_model_I_semx)
  probs_I_semx_cop = c(); for(i in 1:length(rslts_I_semx_cop)){
    probs_I_semx_cop = rbind(probs_I_semx_cop, rslts_I_semx_cop[[i]][[2]])}
  probs_I_semx_cop
  
  
  # Poisson + Identity (ALL)
  selectedCopula <- est_copula(rodada, dados, models_I) # Seleciona a Copula
  name_copula_I <- selectedCopula[[1]]
  name_copula_I
  copula_model_I <- selectedCopula[[2]]
  
  # Probabilidades
  rslts_I_cop <- rodada_biv(rodada, models_I, x_null = F,
                            name_copula_I, copula_model_I)
  probs_I_cop = c(); for(i in 1:length(rslts_I_cop)){
    probs_I_cop = rbind(probs_I_cop, rslts_I_cop[[i]][[2]])}
  probs_I_cop
  
  
  # Poisson + Log + Sem cov
  selectedCopula <- est_copula(rodada, dados, models_log_semx) # Seleciona a Copula
  name_copula_log_semx <- selectedCopula[[1]]
  name_copula_log_semx
  copula_model_log_semx <- selectedCopula[[2]]

  # Probabilidades
  rslts_log_semx_cop <- rodada_biv(rodada, models_log_semx, x_null = T,
                                  name_copula_log_semx, copula_model_log_semx)
  probs_log_semx_cop = c(); for(i in 1:length(rslts_log_semx_cop)){
    probs_log_semx_cop = rbind(probs_log_semx_cop, rslts_log_semx_cop[[i]][[2]])}
  probs_log_semx_cop
  
  
  # Poisson + Log (ALL)
  selectedCopula <- est_copula(rodada, dados, models_log) # Seleciona a Copula
  name_copula_log <- selectedCopula[[1]]
  name_copula_log
  copula_model_log <- selectedCopula[[2]]
  
  # Probabilidades
  rslts_log_cop <- rodada_biv(rodada, models_log, x_null = F,
                             name_copula_log, copula_model_log)
  probs_log_cop = c(); for(i in 1:length(rslts_log_cop)){
    probs_log_cop = rbind(probs_log_cop, rslts_log_cop[[i]][[2]])}
  probs_log_cop
  
  
  
  # Salvas as probs das rodadas
  probs <- list()
  probs[['Identidade Sem Cov']] = list(probs_I_semx, probs_I_semx_cop)
  probs[['Identidade']] = list(probs_I, probs_I_cop)
  probs[['Log Sem Cov']] = list(probs_log_semx, probs_log_semx_cop)
  probs[['Log']] = list(probs_log, probs_log_cop)
  
  models_rodada[[paste('rodada', wk)]] <- list(models_I_semx, models_I, 
                                               models_log_semx, models_log)
  probs_rodada[[paste('rodada', wk)]] <- probs
  copulas_rodada[[paste('rodada', wk)]] <- c(name_copula_I_semx, name_copula_I,
                                             name_copula_log, name_copula_log_semx)
  
}


