


########################################################################

# ____________ # Modeling using PARX with Copulas  # ________________ #

# _________________ Author: Leonardo Damiani ________________________ # 

#######################################################################


# Directory
setwd("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models")

# Packages
library(dplyr)
library(tscount)
library(copula)
library(VineCopula)
library(lubridate)
library(measures)

source("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models/get_data.R")
source("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models/covariaveis.R")
source("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models/functions.R")

# Data
load("dados/seasons.RData")

# Rodada
# rodada_38 = dados[dados$Wk == 38 & dados$Date > '2022-08-05', ]
rodada = seasons_df[seasons_df$Wk == 20 & seasons_df$Date > '2023-08-05', ]

# Dados desde de 2010
# Calcula as médias de gols concedidos (Parâmetro de Defesa)
seasons_df = seasons_df[-(5493),]
dados <- concedidosX(seasons_df, rodada, c(2, 2))
# dados <- seasons[seasons$Date > '2017-08-05',]

# Dummys dos Times
# Minimo 11 temporadas de 14
# dados <- dummys(dados, 200, n_temps = 14, all_dummys = FALSE)


# Variáveis Extras
# ...

###########################################################################

# --------------------------------------------------------------- #
####### ______________ PARX / INGARCH (p, q) ____________  ########
# --------------------------------------------------------------- #

rodada = dados[dados$Wk == 20 & dados$Date > '2023-08-05', ]

probs_rodada = list()

#########################
#_______ MODELOS _______#
#########################

# Poisson + Identity + Sem cov
models_I_semx <- gera_modelos(rodada, dados, link = 'identity', distr = 'poisson', 
                         x_null = T, all_teams = T)

# Poisson + Identity (ALL)
models_I <- gera_modelos(rodada, dados, link = 'identity', distr = 'poisson', 
                         x_null = FALSE, all_teams = T)

# Poisson + Log + Sem cov
models_log_semx <- gera_modelos(rodada, dados, link = 'log', distr = 'poisson', 
                           x_null = T, all_teams = T)

# Poisson + Log (ALL)
models_log <- gera_modelos(rodada, dados, link = 'log', distr = 'poisson', 
                           x_null = FALSE, all_teams = T)

# Log
# for negative covariate effects





#########################
#_____ INDEPENDENTE ____#
#########################

# Poisson + Identity + Sem cov
rslts_I_semx <- rodada_ind(rodada, dados, models_I_semx, x_null = TRUE)
probs_I_semx = c(); for(i in 1:10){probs_I_semx = rbind(probs_I_semx, rslts_I_semx[[i]][[2]])}
probs_I_semx

# Poisson + Identity (ALL)
rslts_I <- rodada_ind(rodada, dados, models_I)
probs_I = c(); for(i in 1:10){probs_I = rbind(probs_I, rslts_I[[i]][[2]])}
probs_I

# Poisson + Log + Sem cov
rslts_log_semx <- rodada_ind(rodada, dados, models_log_semx, x_null = T)
probs_log_semx = c(); for(i in 1:10){probs_log_semx = rbind(probs_log_semx, rslts_log_semx[[i]][[2]])}
probs_log_semx

# Poisson + Log (ALL)
rslts_log <- rodada_ind(rodada, dados, models_log)
probs_log = c(); for(i in 1:10){probs_log = rbind(probs_log, rslts_log[[i]][[2]])}
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
probs_I_semx_cop = c(); for(i in 1:10){probs_I_semx_cop = rbind(probs_I_semx_cop, 
                                                                rslts_I_semx_cop[[i]][[2]])}
probs_I_semx_cop


# Poisson + Identity (ALL)
selectedCopula <- est_copula(rodada, dados, models_I) # Seleciona a Copula
name_copula_I <- selectedCopula[[1]]
name_copula_I
copula_model_I <- selectedCopula[[2]]

# Probabilidades
rslts_I_cop <- rodada_biv(rodada, models_I, x_null = F,
                          name_copula_I, copula_model_I)
probs_I_cop = c(); for(i in 1:10){probs_I_cop = rbind(probs_I_cop, 
                                                      rslts_I_cop[[i]][[2]])}
probs_I_cop


# Poisson + Log + Sem cov
selectedCopula <- est_copula(rodada, dados, models_log_semx) # Seleciona a Copula
name_copula_log_semx <- selectedCopula[[1]]
name_copula_log_semx
copula_model_log_semx <- selectedCopula[[2]]

# Probabilidades
rslts_log_semx_cop <- rodada_biv(rodada, models_log_semx, x_null = T,
                               name_copula_log_semx, copula_model_log_semx)
probs_log_semx_cop = c(); for(i in 1:10){probs_log_semx_cop = rbind(probs_log_semx_cop, 
                                                                rslts_log_semx_cop[[i]][[2]])}
probs_log_semx_cop


# Poisson + Log (ALL)
selectedCopula <- est_copula(rodada, dados, models_log) # Seleciona a Copula
name_copula_log <- selectedCopula[[1]]
name_copula_log
copula_model_log <- selectedCopula[[2]]

# Probabilidades
rslts_log_cop <- rodada_biv(rodada, models_log, x_null = F,
                               name_copula_log, copula_model_log)
probs_log_cop = c(); for(i in 1:10){probs_log_cop = rbind(probs_log_cop, 
                                                                rslts_log_cop[[i]][[2]])}
probs_log_cop



# Salvas as probs das rodadas
probs <- list()
probs[['Identidade Sem Cov']] = list(probs_I_semx, probs_I_semx_cop)
probs[['Identidade']] = list(probs_I, probs_I_cop)
probs[['Log Sem Cov']] = list(probs_log_semx, probs_log_semx_cop)
probs[['Log']] = list(probs_log, probs_log_cop)

probs_rodada[[paste('rodada', '20')]] <- probs
#csave.image("dados/models_probs.RData")

# rm(list=setdiff(ls(),c('rslts_38', 'rslts_38_sem_x')))



