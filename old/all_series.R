



########################################################################

# ____________ # ALL SERIES  # ________________ #

# _________________ Author: Leonardo Damiani ________________________ # 

#######################################################################

# Directory
setwd("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models")

# Packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(extrafont)
library(gridExtra)

# source("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models/get_data.R")
source("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models/covariaveis.R")
source("C:/Users/Games/Documents/Bolsa PROBIC/Football_Models/functions.R")

# Data
load("dados/seasons.RData")
dados <- seasons_df[seasons_df$Date > '2010-05-09', ]

###################################################################################

# Treino até Wk 33
treino = dados[dados$Date < '2023-04-29', ]
rownames(treino) <- NULL
# treino = treino[4181:nrow(treino),]
treino = treino[1:nrow(treino),]
times = unique(treino$Home)

# Gráfico das Séries Mandante
series = list()

for (i in 1:length(times)){
  
  time = times[i]
  
  home <- treino[treino$Home == time, ]
  
  away <- treino[treino$Away == time, ]
  
  series[[time]] = list(home, away)

}

ggplot(series[['Liverpool']][[1]], 
       aes(x = Date, y = HomeGoals)) +
  labs(y = 'Goals', x = '', title = 'Home Liverpool') +
  geom_line(col = 'black') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 8))

ggplot(series[['Liverpool']][[2]], 
       aes(x = Date, y = HomeGoals)) +
  labs(y = 'Goals', x = '', title = 'Away Liverpool') +
  geom_line(col = 'black') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 8))

ggplot(series[['Brighton']][[1]], 
       aes(x = Date, y = HomeGoals)) +
  labs(y = 'Goals', x = '', title = 'Home Brighton') +
  geom_line(col = 'black') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 8))

ggplot(series[['Brighton']][[2]], 
       aes(x = Date, y = HomeGoals)) +
  labs(y = 'Goals', x = '', title = 'Away Brighton') +
  geom_line(col = 'black') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 8))








# Teste ADF

library(urca)
aux <- series[['Liverpool']][[1]]['HomeGoals']
summary(ur.df(aux$HomeGoals, type='drift', lags=5))




