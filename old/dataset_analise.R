



########################################################################

# ____________ # DATA SET E ANALISE  # ________________ #

# _________________ Author: Leonardo Damiani ________________________ # 

#######################################################################

# Season 2022/23

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
seasons_df = seasons_df[seasons_df$Date < '2023-08-10', ]

# Rodada
rodada_max = seasons_df[seasons_df$Wk == 38 & seasons_df$Date > '2022-08-05', ]

# Dados desde de 2010
# Calcula as médias de gols concedidos (Parâmetro de Defesa)
dados <- concedidosX(seasons_df, rodada_max, c(2, 2))

# Campeonato a ser Predito
season = dados[dados$Date >= '2022-08-05', ]
rownames(season) <- NULL

###################################################################################

# Distribuição de Partidas por Times
# Mesma Distr para jogos Away

loadfonts(device = "win")

# Supondo que 'Home' seja a variável categórica que você quer ordenar
dados$Home <- factor(dados$Home, 
                     levels = unique(dados$Home[order(dados$Home, decreasing = T)]))

dados %>%
  group_by(Home) %>%
  count(Home) %>%
  ggplot(., aes(x = n, y = reorder(Home, n))) +
  geom_col(fill = 'tomato', color = 'white', alpha = 0.6) +
  labs(x = 'Number of Games', y = '', title = 'Distribution of Home Games') +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(face = 'bold', hjust = 1),
        axis.title.x = element_text(hjust = 0.5, vjust = -1),
        plot.title = element_text(size = 15, face = 'italic',  
                                  hjust = 0.5, vjust = 1.5))


###################################################################################

# Treino até Wk 33
treino = dados[dados$Date < '2023-04-29', ]

# Hist

# Mandante
g1 <- treino[treino$Home == 'Arsenal', ] %>%
  group_by(HomeGoals) %>%
  count(HomeGoals) %>%
  ggplot(aes(x = HomeGoals, y = n)) +
  labs(x = 'Home Goals', y = '', title = 'Arsenal') +
  geom_col(fill = 'red', col = 'white') + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme_bw(base_family = "Times")

round(mean(treino[treino$Home == 'Arsenal', ]$HomeGoals), 2)

g2 <- treino[treino$Home == 'Chelsea', ] %>%
  group_by(HomeGoals) %>%
  count(HomeGoals) %>%
  ggplot(aes(x = HomeGoals, y = n)) +
  labs(x = 'Home Goals', y = '', title = 'Chelsea') +
  geom_col(fill = 'darkblue', col = 'white') + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme_bw(base_family = "Times")

round(mean(treino[treino$Home == 'Chelsea', ]$HomeGoals), 2)

g3 <- treino[treino$Home == 'Liverpool', ] %>%
  group_by(HomeGoals) %>%
  count(HomeGoals) %>%
  ggplot(aes(x = HomeGoals, y = n)) +
  labs(x = 'Home Goals', y = '', title = 'Liverpool') +
  geom_col(fill = 'red', col = 'white') + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme_bw(base_family = "Times")

round(mean(treino[treino$Home == 'Liverpool', ]$HomeGoals), 2)

g4 <- treino[treino$Home == 'Manchester Utd', ] %>%
  group_by(HomeGoals) %>%
  count(HomeGoals) %>%
  ggplot(aes(x = HomeGoals, y = n)) +
  labs(x = 'Home Goals', y = '', title = 'Manchester Utd') +
  geom_col(fill = 'darkred', col = 'white') + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme_bw(base_family = "Times")

round(mean(treino[treino$Home == 'Manchester Utd', ]$HomeGoals), 2)

g5 <- treino[treino$Home == 'Manchester City', ] %>%
  group_by(HomeGoals) %>%
  count(HomeGoals) %>%
  ggplot(aes(x = HomeGoals, y = n)) +
  labs(x = 'Home Goals', y = '', title = 'Manchester City') +
  geom_col(fill = 'skyblue', col = 'white') + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme_bw(base_family = "Times")

round(mean(treino[treino$Home == 'Manchester City', ]$HomeGoals), 2)

g6 <- treino[treino$Home == 'Tottenham', ] %>%
  group_by(HomeGoals) %>%
  count(HomeGoals) %>%
  ggplot(aes(x = HomeGoals, y = n)) +
  labs(x = 'Home Goals', y = '', title = 'Tottenham') +
  geom_col(fill = '#3F3F9D', col = 'white') + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme_bw(base_family = "Times")

round(mean(treino[treino$Home == 'Tottenham', ]$HomeGoals), 2)


grid.arrange(g1, g2, g3, g4, g5, g6,
             nrow = 2)



# ------------------------------------------------------------ #

rownames(treino) <- NULL
# treino = treino[4181:nrow(treino),]
treino = treino[1:nrow(treino),]

# Gráfico das Séries Mandante
gA1 <- ggplot(treino[treino$Home == 'Arsenal', ], 
       aes(x = Date, y = HomeGoals)) +
  labs(y = 'Goals', x = '', title = 'Home Arsenal') +
  geom_line(col = 'red') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gA2 <- ggplot(treino[treino$Away == 'Arsenal', ], 
             aes(x = Date, y = AwayGoals)) +
  labs(y = '', x = '', title = 'Away Arsenal') +
  geom_line(col = 'red') + 
  theme_bw(base_family = "Times")  +
  lims(y = c(0, 5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# Gráfico das Séries Mandante
gB1 <- ggplot(treino[treino$Home == 'Brighton', ], 
              aes(x = Date, y = HomeGoals)) +
  labs(y = 'Goals', x = '', title = 'Home Brighton') +
  geom_line(col = 'blue') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 9)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gB2 <- ggplot(treino[treino$Away == 'Brighton', ], 
              aes(x = Date, y = AwayGoals)) +
  labs(y = '', x = '', title = 'Away Brighton') +
  geom_line(col = 'blue') + 
  theme_bw(base_family = "Times")  +
  lims(y = c(0, 9)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gC1 <- ggplot(treino[treino$Home == 'Chelsea', ], 
             aes(x = Date, y = HomeGoals)) +
  labs(y = '', x = '', title = 'Home Chelsea') +
  geom_line(col = 'darkblue') + 
  theme_bw(base_family = "Times")  +
  lims(y = c(0, 7)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gC2 <- ggplot(treino[treino$Away == 'Chelsea', ], 
             aes(x = Date, y = AwayGoals)) +
  labs(y = '', x = '', title = 'Away Chelsea') +
  geom_line(col = 'darkblue') + 
  theme_bw(base_family = "Times")  +
  lims(y = c(0, 7)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gL1 <- ggplot(treino[treino$Home == 'Liverpool', ], 
             aes(x = Date, y = HomeGoals)) +
  labs(y = 'Goals', x = '', title = 'Home Liverpool') +
  geom_line(col = 'red') + 
  theme_bw(base_family = "Times")  +
  lims(y = c(0, 9)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gL2 <- ggplot(treino[treino$Away == 'Liverpool', ], 
             aes(x = Date, y = AwayGoals)) +
  labs(y = '', x = '', title = 'Away Liverpool') +
  geom_line(col = 'red') + 
  theme_bw(base_family = "Times")  +
  lims(y = c(0, 9)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gMu1 <- ggplot(treino[treino$Home == 'Manchester Utd', ], 
             aes(x = Date, y = HomeGoals)) +
  labs(y = '', x = '', title = 'Home Manchester Utd') +
  geom_line(col = 'darkred') + 
  theme_bw(base_family = "Times")  +
  lims(y = c(0, 5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gMu2 <- ggplot(treino[treino$Away == 'Manchester Utd', ], 
             aes(x = Date, y = AwayGoals)) +
  labs(y = '', x = '', title = 'Away Manchester Utd') +
  geom_line(col = 'darkred') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gMc1 <- ggplot(treino[treino$Home == 'Manchester City', ], 
             aes(x = Date, y = HomeGoals)) +
  labs(y = 'Goals', x = '', title = 'Home Manchester City') +
  geom_line(col = 'skyblue') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 7)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gMc2 <- ggplot(treino[treino$Away == 'Manchester City', ], 
             aes(x = Date, y = AwayGoals)) +
  labs(y = '', x = '', title = 'Away Manchester City') +
  geom_line(col = 'skyblue') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 7)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gT1 <- ggplot(treino[treino$Home == 'Tottenham', ], 
             aes(x = Date, y = HomeGoals)) +
  labs(y = '', x = '', title = 'Home Tottenham') +
  geom_line(col = '#3F3F9D') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 6)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

gT2 <- ggplot(treino[treino$Away == 'Tottenham', ], 
             aes(x = Date, y = AwayGoals)) +
  labs(y = '', x = '', title = 'Away Tottenham') +
  geom_line(col = '#3F3F9D') + 
  theme_bw(base_family = "Times") +
  lims(y = c(0, 6)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")


grid.arrange(gA1, gA2,
             gC1, gC2,
             gL1, gL2,
             gMu1, gMu2, 
             gMc1, gMc2, 
             gT1, gT2,
             nrow = 3)

grid.arrange(gL1, gL2, gB1, gB2, nrow = 2)


# ------------------------------------------------------------ #


# Treino até Wk 33
treino = dados[dados$Date < '2023-04-29', ]

# Gráfico das ACF 
# Home Arsenal
dados_acf <- treino[treino$Home == 'Arsenal', ]$HomeGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gA1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3)  +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "ACF", title = 'Home Arsenal') +
  theme_bw() +
  theme(text = element_text(family = "Times"))  

# Away Arsenal
dados_acf <- treino[treino$Away == 'Arsenal', ]$AwayGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gA2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "", title = 'Away Arsenal') +
  theme_bw() +
  theme(text = element_text(family = "Times")) 

# Home Chelsea
dados_acf <- treino[treino$Home == 'Chelsea', ]$HomeGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gC1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "", title = 'Home Chelsea') +
  theme_bw() +
  theme(text = element_text(family = "Times"))  

# Away Chelsea
dados_acf <- treino[treino$Away == 'Chelsea', ]$AwayGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gC2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "", title = 'Away Chelsea') +
  theme_bw() +
  theme(text = element_text(family = "Times")) 

# Home Liverpool
dados_acf <- treino[treino$Home == 'Liverpool', ]$HomeGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gL1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "ACF", title = 'Home Liverpool') +
  theme_bw() +
  theme(text = element_text(family = "Times"))  

# Away Liverpool
dados_acf <- treino[treino$Away == 'Liverpool', ]$AwayGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gL2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "", title = 'Away Liverpool') +
  theme_bw() +
  theme(text = element_text(family = "Times")) 

# Home Manchester Utd
dados_acf <- treino[treino$Home == 'Manchester Utd', ]$HomeGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, length(dados_acf))
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gMu1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "", title = 'Home Manchester Utd') +
  theme_bw() +
  theme(text = element_text(family = "Times"))  

# Away Manchester Utd
dados_acf <- treino[treino$Away == 'Manchester Utd', ]$AwayGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, length(dados_acf))
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gMu2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "", title = 'Away Manchester Utd') +
  theme_bw() +
  theme(text = element_text(family = "Times")) 

# Home Manchester City
dados_acf <- treino[treino$Home == 'Manchester City', ]$HomeGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gMc1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "Lag", y = "ACF", title = 'Home Manchester City') +
  theme_bw() +
  theme(text = element_text(family = "Times"))  

# Away Manchester City
dados_acf <- treino[treino$Away == 'Manchester City', ]$AwayGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gMc2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "Lag", y = "", title = 'Away Manchester City') +
  theme_bw() +
  theme(text = element_text(family = "Times")) 

# Home Tottenham
dados_acf <- treino[treino$Home == 'Tottenham', ]$HomeGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, length(dados_acf))
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gT1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "Lag", y = "", title = 'Home Tottenham') +
  theme_bw() +
  theme(text = element_text(family = "Times"))  

# Away Tottenham
dados_acf <- treino[treino$Away == 'Tottenham', ]$AwayGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, length(dados_acf))
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gT2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "Lag", y = "", title = 'Away Tottenham') +
  theme_bw() +
  theme(text = element_text(family = "Times")) 


grid.arrange(gA1, gA2,
             gC1, gC2,
             gL1, gL2,
             gMu1, gMu2, 
             gMc1, gMc2, 
             gT1, gT2,
             nrow = 3)





# Gráfico das ACF 

# Home Liverpool
dados_acf <- treino[treino$Home == 'Liverpool', ]$HomeGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gL1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "ACF", title = 'Home Liverpool') +
  theme_bw() +
  theme(text = element_text(family = "Times"))  

# Away Liverpool
dados_acf <- treino[treino$Away == 'Liverpool', ]$AwayGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gL2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "", title = 'Away Liverpool') +
  theme_bw() +
  theme(text = element_text(family = "Times")) 

# Home Brighton
dados_acf <- treino[treino$Home == 'Brighton', ]$HomeGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gB1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3)  +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "ACF", title = 'Home Brighton') +
  theme_bw() +
  theme(text = element_text(family = "Times"))  

# Away Brighton
dados_acf <- treino[treino$Away == 'Brighton', ]$AwayGoals
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 19)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gB2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "", title = 'Away Brighton') +
  theme_bw() +
  theme(text = element_text(family = "Times"))



grid.arrange(gL1, gL2,
             gB1, gB2,
             nrow = 2)










