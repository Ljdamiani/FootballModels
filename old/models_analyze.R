

library(dplyr)
library(tscount)
library(copula)
library(ggplot2)
library(dplyr)
library(extrafont)
library(gridExtra)
library(ggtext)


wks = c(24, 25, 26, 27, 28, 29, 
        30, 31, 32, 33, 34, 35, 36, 37, 38)
teams = unique(dados$Home)

# Parâmetros Por Modelo
pq_I_semx = data.frame()
pq_I = data.frame()
pq_log_semx = data.frame()
pq_log = data.frame()
aux = c('_M', '_V')

for (k in 1:length(wks)){ # rodadas
  wk = wks[i]
  
  for (i in 1:4){ # modelos
    for (t in 1:length(teams)){ # teams
      for (o in 1:2){ # mandante ou visitante
        
        team = teams[t]
        cofs = coef(models_rodada[[k]][[i]][[paste0(team, aux[o])]])
        
        if (i == 1){
          p = sum("beta" == substr(names(cofs[-1]), 1, 4))
          q = sum("alpha" == substr(names(cofs[-1]), 1, 5))
          pq_I_semx = rbind(pq_I_semx, data.frame(pq = paste0(p,',',q), MV = aux[o]))
          
        } else if (i == 2){
          p = sum("beta" == substr(names(cofs[-c(1, length(cofs))]), 1, 4))
          q = sum("alpha" == substr(names(cofs[-c(1, length(cofs))]), 1, 5))
          pq_I = rbind(pq_I, data.frame(pq = paste0(p,',',q), MV = aux[o]))
          
        } else if (i == 3){
          p = sum("beta" == substr(names(cofs[-1]), 1, 4))
          q = sum("alpha" == substr(names(cofs[-1]), 1, 5))
          pq_log_semx = rbind(pq_log_semx, data.frame(pq = paste0(p,',',q), MV = aux[o]))
          
        } else {
          p = sum("beta" == substr(names(cofs[-c(1, length(cofs))]), 1, 4))
          q = sum("alpha" == substr(names(cofs[-c(1, length(cofs))]), 1, 5))
          pq_log = rbind(pq_log, data.frame(pq = paste0(p,',',q), MV = aux[o]))
          
        }
      }
    }
  }
}

loadfonts(device = "win")
library(latex2exp)

table(pq_I_semx)

# Mandantes
g1 <- pq_I_semx[pq_I_semx$MV == '_M', ] %>%
  group_by(pq) %>%
  count(pq) %>% 
  ggplot(aes(x = pq, y = n)) +
  labs(x = '', y = 'Count', title = TeX("$PARX_{I}^{*} - Home$")) +
  geom_col(fill = 'lightgreen', col = 'white') + 
  theme_bw(base_family = "Times")

# visitantes
g2 <- pq_I_semx[pq_I_semx$MV == '_V', ] %>%
  group_by(pq) %>%
  count(pq) %>% 
  ggplot(aes(x = pq, y = n)) +
  labs(x = '', y = '', title = TeX("$PARX_{I}^{*} - Away$")) +
  geom_col(fill = 'lightgreen', col = 'white') + 
  theme_bw(base_family = "Times")

table(pq_I)

# Mandantes
g3 <- pq_I[pq_I$MV == '_M', ] %>%
  group_by(pq) %>%
  count(pq) %>% 
  ggplot(aes(x = pq, y = n)) +
  labs(x = '', y = 'Count', title = TeX("$PARX_{I} - Home$")) +
  geom_col(fill = 'lightgreen', col = 'white') + 
  theme_bw(base_family = "Times")

# visitantes
g4 <- pq_I[pq_I$MV == '_V', ] %>%
  group_by(pq) %>%
  count(pq) %>% 
  ggplot(aes(x = pq, y = n)) +
  labs(x = '', y = '', title = TeX("$PARX_{I} - Away$")) +
  geom_col(fill = 'lightgreen', col = 'white') + 
  theme_bw(base_family = "Times")

table(pq_log_semx)

# Mandantes
g5 <- pq_log_semx[pq_log_semx$MV == '_M', ] %>%
  group_by(pq) %>%
  count(pq) %>% 
  ggplot(aes(x = pq, y = n)) +
  labs(x = '', y = 'Count', title = TeX("$PARX_{L}^{*} - Home$")) +
  geom_col(fill = 'lightgreen', col = 'white') + 
  theme_bw(base_family = "Times")

# visitantes
g6 <- pq_log_semx[pq_log_semx$MV == '_V', ] %>%
  group_by(pq) %>%
  count(pq) %>% 
  ggplot(aes(x = pq, y = n)) +
  labs(x = '', y = '', title = TeX("$PARX_{L}^{*} - Away$")) +
  geom_col(fill = 'lightgreen', col = 'white') + 
  theme_bw(base_family = "Times")

table(pq_log)

# Mandantes
g7 <- pq_log[pq_log$MV == '_M', ] %>%
  group_by(pq) %>%
  count(pq) %>% 
  ggplot(aes(x = pq, y = n)) +
  labs(x = 'Number of Parameters (p, q)', y = 'Count', title = TeX("$PARX_{L} - Home$")) +
  geom_col(fill = 'lightgreen', col = 'white') + 
  theme_bw(base_family = "Times")

# visitantes
g8 <- pq_log[pq_log$MV == '_V', ] %>%
  group_by(pq) %>%
  count(pq) %>% 
  ggplot(aes(x = pq, y = n)) +
  labs(x = 'Number of Parameters (p, q)', y = '', title = TeX("$PARX_{L} - Away$")) +
  geom_col(fill = 'lightgreen', col = 'white') + 
  theme_bw(base_family = "Times")


grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8,
             ncol = 2)

# grid.arrange(g1, g2, g3, g4,
#              ncol = 2)
# 
# grid.arrange(g5, g6, g7, g8,
#              ncol = 2)

# -------------------------------------------------------------------------------- #


# Partida Exemplo Arsenal e Chelsea

# Campeonato a ser Predito
season = dados[dados$Date >= '2022-08-05', ]
rownames(season) <- NULL

partida <- season[season$Home == 'Arsenal' & 
                    season$Away == 'Chelsea', ]


mod1 <- models_rodada$`rodada 34`[[3]]$Arsenal_M
mod2 <- models_rodada$`rodada 34`[[3]]$Chelsea_V

lM = predict(mod1, n.ahead=1)$pred
lV = predict(mod2, n.ahead=1)$pred

# Poisson P(lM)
pM = rep(NA, 7)
names(pM) = 0:6
for (i in 0:6){
  pM[i+1] = dpois(x = i, lambda = lM)
}

# Poisson P(lM)
pV = rep(NA, 7)
names(pV) = 0:6
for (i in 0:6){
  pV[i+1] = dpois(x = i, lambda = lV)
}


# Placares
resultado = pM %*% t(pV)
round(resultado, 5)

# Probabilidades
pVM = sum(resultado[lower.tri(resultado)])
pE = sum(diag(resultado))
pVV = sum(resultado[upper.tri(resultado)])

sum(c(pVM, pE, pVV))

# -------------------------------------------------------------------------------- #

# Calibragem dos AJustes

par(mfrow=c(4,4))

pit(models_rodada$`rodada 34`[[1]][['Everton_M']], main=TeX("$PARX_{I}^* (Home)$"))
pit(models_rodada$`rodada 34`[[2]][['Everton_M']], 
    main=TeX("$PARX_{I} (Home)$"), ylab='')
pit(models_rodada$`rodada 34`[[3]][['Everton_M']], 
    main=TeX("$PARX_{L}^* (Home)$"), ylab='')
pit(models_rodada$`rodada 34`[[4]][['Everton_M']], 
    main=TeX("$PARX_{L} (Home)$"), ylab='')

marcal(models_rodada$`rodada 34`[[1]][['Everton_M']], main = 'Marginal Calibration', 
       xlab = 'Goals', ylim = c(-0.1, 0.1))
marcal(models_rodada$`rodada 34`[[2]][['Everton_M']], main = 'Marginal Calibration', 
       xlab = 'Goals', ylab='', ylim = c(-0.1, 0.1))
marcal(models_rodada$`rodada 34`[[3]][['Everton_M']], main = 'Marginal Calibration', 
       xlab = 'Goals', ylab='', ylim = c(-0.1, 0.1))
marcal(models_rodada$`rodada 34`[[4]][['Everton_M']], main = 'Marginal Calibration', 
       xlab = 'Goals', ylab='', ylim = c(-0.1, 0.1))


pit(models_rodada$`rodada 34`[[1]][['Everton_V']], main=TeX("$PARX_{I}^* (Away)$"))
pit(models_rodada$`rodada 34`[[2]][['Everton_V']], 
    main=TeX("$PARX_{I} (Away)$"), ylab='')
pit(models_rodada$`rodada 34`[[3]][['Everton_V']], 
    main=TeX("$PARX_{L}^* (Away)$"), ylab='')
pit(models_rodada$`rodada 34`[[4]][['Everton_V']], 
    main=TeX("$PARX_{L} (Away)$"), ylab='')

marcal(models_rodada$`rodada 34`[[1]][['Everton_V']], main = 'Marginal Calibration', 
       xlab = 'Goals', ylim = c(-0.1, 0.1))
marcal(models_rodada$`rodada 34`[[2]][['Everton_V']], main = 'Marginal Calibration', 
       xlab = 'Goals', ylab='', ylim = c(-0.1, 0.1))
marcal(models_rodada$`rodada 34`[[3]][['Everton_V']], main = 'Marginal Calibration', 
       xlab = 'Goals', ylab='', ylim = c(-0.1, 0.1))
marcal(models_rodada$`rodada 34`[[4]][['Everton_V']], main = 'Marginal Calibration', 
       xlab = 'Goals', ylab='', ylim = c(-0.1, 0.1))

