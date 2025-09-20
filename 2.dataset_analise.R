
########################################################################

# ____________ # DATA SET & ANALISE  # ________________ #

# ___________ Author: Leonardo Damiani _______________ # 

#######################################################################

# Packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(extrafont)
library(gridExtra)
library(stringr)
library(robts)
library(forecast)

# source("functions.R")

loadfonts(device = "win")

# Get Data
countries <- c("ENG")
Matches_Bakcup <- data.frame()
for (country in countries){
  for (ano in 2020:2025){
    load(paste0("dados/matches_", country, "_", ano, ".RData"))
    assign(paste0("matches_", country, "_", ano), matches)
    Matches_Bakcup <- bind_rows(Matches_Bakcup, matches)
  }
}

# Since 2019 (Season 19/20)
Matches <- Matches_Bakcup %>%
  mutate(
    Wk = str_extract(Matchweek, "(?<=Matchweek )\\d+"),
    Score = case_when(
      Home_Away == "Home" ~ Home_Score,
      Home_Away == "Away" ~ Away_Score
    ),
    Formation = case_when(
      Home_Away == "Home" ~ Home_Formation,
      Home_Away == "Away" ~ Away_Formation
    ),
    Match_Date = as.Date(Match_Date)
  ) %>%
  select(-Matchweek, -Game_URL, -Player_Href, -Min,
         -Home_Team, -Home_Formation, -Home_Score, -Home_xG, -Home_Goals, -Home_Yellow_Cards, -Home_Red_Cards,
         -Away_Team, -Away_Formation, -Away_Score, -Away_xG, -Away_Goals, -Away_Yellow_Cards, -Away_Red_Cards) %>%
  select(Match_Date, League, Wk, Team, Score, everything())

# Calcula as médias de gols concedidos (Parâmetro de Defesa)
# dados <- concedidosX(seasons_df, rodada_max, c(2, 2))

# Last Season
# season = dados[dados$Date >= '2022-08-05', ]
# rownames(season) <- NULL

###################################################################################

# Distribution Teams

# Factor - Order
Matches$Team <- factor(Matches$Team, levels = unique(Matches$Team[order(Matches$Team, decreasing = T)]))

Matches %>%
  group_by(Team, Home_Away) %>%
  summarise(n = n()) %>%
  filter(Home_Away == "Home") %>%
  ggplot(., aes(x = n, y = reorder(Team, n))) +
  geom_col(fill = 'tomato', color = 'white', alpha = 0.6) +
  labs(x = 'Number of Games', y = '', title = 'Distribution of Home Games') +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(face = 'bold', hjust = 1),
        axis.title.x = element_text(hjust = 0.5, vjust = -1),
        plot.title = element_text(size = 15, face = 'italic',  
                                  hjust = 0.5, vjust = 1.5))


###################################################################################

# Data Train -> 19/20 -> 23/24
treino = Matches[Matches$Match_Date < '2024-08-16', ]

# Mandante
g1 <- treino %>%
  filter(Team == "Arsenal") %>%
  group_by(Score) %>%
  count(Score) %>%
  ggplot(aes(x = Score, y = n)) +
  labs(x = 'Home Goals', y = '', title = 'Arsenal') +
  geom_col(fill = 'red', col = 'white') + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme_bw(base_family = "Times")

# round(mean(treino[treino$Home == 'Arsenal', ]$HomeGoals), 2)
# 
# g2 <- treino[treino$Home == 'Chelsea', ] %>%
#   group_by(HomeGoals) %>%
#   count(HomeGoals) %>%
#   ggplot(aes(x = HomeGoals, y = n)) +
#   labs(x = 'Home Goals', y = '', title = 'Chelsea') +
#   geom_col(fill = 'darkblue', col = 'white') + 
#   scale_x_continuous(breaks = seq(0, 8, by = 1)) +
#   theme_bw(base_family = "Times")
# 
# round(mean(treino[treino$Home == 'Chelsea', ]$HomeGoals), 2)
# 
# g3 <- treino[treino$Home == 'Liverpool', ] %>%
#   group_by(HomeGoals) %>%
#   count(HomeGoals) %>%
#   ggplot(aes(x = HomeGoals, y = n)) +
#   labs(x = 'Home Goals', y = '', title = 'Liverpool') +
#   geom_col(fill = 'red', col = 'white') + 
#   scale_x_continuous(breaks = seq(0, 8, by = 1)) +
#   theme_bw(base_family = "Times")
# 
# round(mean(treino[treino$Home == 'Liverpool', ]$HomeGoals), 2)
# 
# g4 <- treino[treino$Home == 'Manchester Utd', ] %>%
#   group_by(HomeGoals) %>%
#   count(HomeGoals) %>%
#   ggplot(aes(x = HomeGoals, y = n)) +
#   labs(x = 'Home Goals', y = '', title = 'Manchester Utd') +
#   geom_col(fill = 'darkred', col = 'white') + 
#   scale_x_continuous(breaks = seq(0, 8, by = 1)) +
#   theme_bw(base_family = "Times")
# 
# round(mean(treino[treino$Home == 'Manchester Utd', ]$HomeGoals), 2)
# 
# g5 <- treino[treino$Home == 'Manchester City', ] %>%
#   group_by(HomeGoals) %>%
#   count(HomeGoals) %>%
#   ggplot(aes(x = HomeGoals, y = n)) +
#   labs(x = 'Home Goals', y = '', title = 'Manchester City') +
#   geom_col(fill = 'skyblue', col = 'white') + 
#   scale_x_continuous(breaks = seq(0, 8, by = 1)) +
#   theme_bw(base_family = "Times")
# 
# round(mean(treino[treino$Home == 'Manchester City', ]$HomeGoals), 2)
# 
# g6 <- treino[treino$Home == 'Tottenham', ] %>%
#   group_by(HomeGoals) %>%
#   count(HomeGoals) %>%
#   ggplot(aes(x = HomeGoals, y = n)) +
#   labs(x = 'Home Goals', y = '', title = 'Tottenham') +
#   geom_col(fill = '#3F3F9D', col = 'white') + 
#   scale_x_continuous(breaks = seq(0, 8, by = 1)) +
#   theme_bw(base_family = "Times")
# 
# round(mean(treino[treino$Home == 'Tottenham', ]$HomeGoals), 2)
# 
# 
# grid.arrange(g1, g2, g3, g4, g5, g6,
#              nrow = 2)



# ------------------------------------------------------------ #

rownames(treino) <- NULL
# treino = treino[4181:nrow(treino),]
treino = treino[1:nrow(treino),]

# Gráfico das Séries Mandante
aux <- subset(treino, Team == "Arsenal" & Home_Away == "Home")
media_home <- mean(aux$Score)
gA1 <- ggplot(subset(treino, Team == "Arsenal" & Home_Away == "Home"), 
              aes(x = Match_Date, y = Score)) +
  labs(y = 'Goals', x = '', title = 'Home Arsenal') +
  geom_line(col = 'red', alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, col = 'darkred') +
  annotate("text",
           x = min(treino$Match_Date, na.rm = TRUE),
           y = 4.8,
           label = paste("Mean:", round(media_home, 2)),
           hjust = 0,
           size = 4,
           color = "red") +
  theme_bw(base_family = "Times") +
  lims(y = c(0, 5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# Gráfico para jogos fora de casa
aux <- subset(treino, Team == "Arsenal" & Home_Away == "Away")
media_away <- mean(aux$Score)
gA2 <- ggplot(subset(treino, Team == "Arsenal" & Home_Away == "Away"), 
              aes(x = Match_Date, y = Score)) +
  labs(y = 'Goals', x = '', title = 'Away Arsenal') +
  geom_line(col = 'red', alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, col = 'darkred') +
  annotate("text",
           x = min(treino$Match_Date, na.rm = TRUE),
           y = 4.8,
           label = paste("Mean:", round(media_away, 2)),
           hjust = 0,
           size = 4,
           color = "blue") +
  theme_bw(base_family = "Times") +
  lims(y = c(0, 5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# # Gráfico das Séries Mandante
#               aes(x = Date, y = HomeGoals)) +
#   labs(y = 'Goals', x = '', title = 'Home Brighton') +
#   geom_line(col = 'blue') + 
#   theme_bw(base_family = "Times") +
#   lims(y = c(0, 9)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gB2 <- ggplot(treino[treino$Away == 'Brighton', ], 
#               aes(x = Date, y = AwayGoals)) +
#   labs(y = '', x = '', title = 'Away Brighton') +
#   geom_line(col = 'blue') + 
#   theme_bw(base_family = "Times")  +
#   lims(y = c(0, 9)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gC1 <- ggplot(treino[treino$Home == 'Chelsea', ], 
#              aes(x = Date, y = HomeGoals)) +
#   labs(y = '', x = '', title = 'Home Chelsea') +
#   geom_line(col = 'darkblue') + 
#   theme_bw(base_family = "Times")  +
#   lims(y = c(0, 7)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gC2 <- ggplot(treino[treino$Away == 'Chelsea', ], 
#              aes(x = Date, y = AwayGoals)) +
#   labs(y = '', x = '', title = 'Away Chelsea') +
#   geom_line(col = 'darkblue') + 
#   theme_bw(base_family = "Times")  +
#   lims(y = c(0, 7)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gL1 <- ggplot(treino[treino$Home == 'Liverpool', ], 
#              aes(x = Date, y = HomeGoals)) +
#   labs(y = 'Goals', x = '', title = 'Home Liverpool') +
#   geom_line(col = 'red') + 
#   theme_bw(base_family = "Times")  +
#   lims(y = c(0, 9)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gL2 <- ggplot(treino[treino$Away == 'Liverpool', ], 
#              aes(x = Date, y = AwayGoals)) +
#   labs(y = '', x = '', title = 'Away Liverpool') +
#   geom_line(col = 'red') + 
#   theme_bw(base_family = "Times")  +
#   lims(y = c(0, 9)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gMu1 <- ggplot(treino[treino$Home == 'Manchester Utd', ], 
#              aes(x = Date, y = HomeGoals)) +
#   labs(y = '', x = '', title = 'Home Manchester Utd') +
#   geom_line(col = 'darkred') + 
#   theme_bw(base_family = "Times")  +
#   lims(y = c(0, 5)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gMu2 <- ggplot(treino[treino$Away == 'Manchester Utd', ], 
#              aes(x = Date, y = AwayGoals)) +
#   labs(y = '', x = '', title = 'Away Manchester Utd') +
#   geom_line(col = 'darkred') + 
#   theme_bw(base_family = "Times") +
#   lims(y = c(0, 5)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gMc1 <- ggplot(treino[treino$Home == 'Manchester City', ], 
#              aes(x = Date, y = HomeGoals)) +
#   labs(y = 'Goals', x = '', title = 'Home Manchester City') +
#   geom_line(col = 'skyblue') + 
#   theme_bw(base_family = "Times") +
#   lims(y = c(0, 7)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gMc2 <- ggplot(treino[treino$Away == 'Manchester City', ], 
#              aes(x = Date, y = AwayGoals)) +
#   labs(y = '', x = '', title = 'Away Manchester City') +
#   geom_line(col = 'skyblue') + 
#   theme_bw(base_family = "Times") +
#   lims(y = c(0, 7)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gT1 <- ggplot(treino[treino$Home == 'Tottenham', ], 
#              aes(x = Date, y = HomeGoals)) +
#   labs(y = '', x = '', title = 'Home Tottenham') +
#   geom_line(col = '#3F3F9D') + 
#   theme_bw(base_family = "Times") +
#   lims(y = c(0, 6)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# gT2 <- ggplot(treino[treino$Away == 'Tottenham', ], 
#              aes(x = Date, y = AwayGoals)) +
#   labs(y = '', x = '', title = 'Away Tottenham') +
#   geom_line(col = '#3F3F9D') + 
#   theme_bw(base_family = "Times") +
#   lims(y = c(0, 6)) +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y")
# 
# 
# grid.arrange(gA1, gA2,
#              gC1, gC2,
#              gL1, gL2,
#              gMu1, gMu2, 
#              gMc1, gMc2, 
#              gT1, gT2,
#              nrow = 3)
# 
# grid.arrange(gL1, gL2, gB1, gB2, nrow = 2)


# ------------------------------------------------------------ #

# Gráfico das ACF 

acf_spearman_bootstrap <- function(x, max_lag = 80, n_boot = 1000, conf_level = 0.95, seed = 123) {
  set.seed(seed)
  n <- length(x)
  acf_vals <- numeric(max_lag + 1)
  lower_ci <- numeric(max_lag + 1)
  upper_ci <- numeric(max_lag + 1)
  
  acf_vals[1] <- 1
  lower_ci[1] <- 1
  upper_ci[1] <- 1
  
  for (lag in 1:max_lag) {
    obs_cor <- cor(x[1:(n - lag)], x[(lag + 1):n], method = "spearman")
    acf_vals[lag + 1] <- obs_cor
    
    # Bootstrap
    boot_vals <- replicate(n_boot, {
      idx <- sample(1:(n - lag), size = n - lag, replace = TRUE)
      cor(x[idx], x[idx + lag], method = "spearman")
    })
    
    alpha <- (1 - conf_level) / 2
    lower_ci[lag + 1] <- quantile(boot_vals, probs = alpha)
    upper_ci[lag + 1] <- quantile(boot_vals, probs = 1 - alpha)
  }
  
  acf_df <- data.frame(
    lag = 0:max_lag,
    acf = acf_vals,
    limite_inferior = lower_ci,
    limite_superior = upper_ci
  )
  
  # Gráfico estilo ggplot personalizado
  g <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
                fill = "grey", alpha = 0.3) +
    geom_segment(aes(xend = lag, yend = 0), 
                 color = "black", linewidth = 0.6) +
    labs(x = "", y = "", title = 'Spearman ACF com IC bootstrap') +
    theme_bw() +
    theme(text = element_text(family = "Times"))
  
  return(g)
}

# Home Arsenal
dados_acf <- subset(treino, Team == "Arsenal" & Home_Away == "Home")$Score
teste <- acf_spearman_bootstrap(dados_acf)
Acf()

# Home Arsenal
dados_acf <- subset(treino, Team == "Arsenal" & Home_Away == "Home")$Score
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 90)
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
dados_acf <- subset(treino, Team == "Arsenal" & Home_Away == "Away")$Score
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 90)
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

# # Home Chelsea
# dados_acf <- treino[treino$Home == 'Chelsea', ]$HomeGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, 19)
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gC1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "", y = "", title = 'Home Chelsea') +
#   theme_bw() +
#   theme(text = element_text(family = "Times"))  
# 
# # Away Chelsea
# dados_acf <- treino[treino$Away == 'Chelsea', ]$AwayGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, 19)
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gC2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "", y = "", title = 'Away Chelsea') +
#   theme_bw() +
#   theme(text = element_text(family = "Times")) 
# 
# # Home Liverpool
# dados_acf <- treino[treino$Home == 'Liverpool', ]$HomeGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, 19)
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gL1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "", y = "ACF", title = 'Home Liverpool') +
#   theme_bw() +
#   theme(text = element_text(family = "Times"))  
# 
# # Away Liverpool
# dados_acf <- treino[treino$Away == 'Liverpool', ]$AwayGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, 19)
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gL2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "", y = "", title = 'Away Liverpool') +
#   theme_bw() +
#   theme(text = element_text(family = "Times")) 
# 
# # Home Manchester Utd
# dados_acf <- treino[treino$Home == 'Manchester Utd', ]$HomeGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, length(dados_acf))
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gMu1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "", y = "", title = 'Home Manchester Utd') +
#   theme_bw() +
#   theme(text = element_text(family = "Times"))  
# 
# # Away Manchester Utd
# dados_acf <- treino[treino$Away == 'Manchester Utd', ]$AwayGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, length(dados_acf))
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gMu2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "", y = "", title = 'Away Manchester Utd') +
#   theme_bw() +
#   theme(text = element_text(family = "Times")) 
# 
# # Home Manchester City
# dados_acf <- treino[treino$Home == 'Manchester City', ]$HomeGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, 19)
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gMc1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "Lag", y = "ACF", title = 'Home Manchester City') +
#   theme_bw() +
#   theme(text = element_text(family = "Times"))  
# 
# # Away Manchester City
# dados_acf <- treino[treino$Away == 'Manchester City', ]$AwayGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, 19)
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gMc2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "Lag", y = "", title = 'Away Manchester City') +
#   theme_bw() +
#   theme(text = element_text(family = "Times")) 
# 
# # Home Tottenham
# dados_acf <- treino[treino$Home == 'Tottenham', ]$HomeGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, length(dados_acf))
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gT1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "Lag", y = "", title = 'Home Tottenham') +
#   theme_bw() +
#   theme(text = element_text(family = "Times"))  
# 
# # Away Tottenham
# dados_acf <- treino[treino$Away == 'Tottenham', ]$AwayGoals
# limite_superior <- 1.96 / sqrt(length(dados_acf))
# limite_inferior <- -limite_superior
# 
# acf_result <- acf(dados_acf, length(dados_acf))
# acf_df <- data.frame(lag = acf_result$lag, 
#                      acf = acf_result$acf)
# 
# gT2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
#   geom_hline(yintercept = 0) +
#   geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#               fill = "grey", alpha = 0.3) +
#   geom_segment(aes(xend = lag, yend = 0), 
#                color = "black", linewidth = 0.6) +
#   labs(x = "Lag", y = "", title = 'Away Tottenham') +
#   theme_bw() +
#   theme(text = element_text(family = "Times")) 
# 
# 
# grid.arrange(gA1, gA2,
#              gC1, gC2,
#              gL1, gL2,
#              gMu1, gMu2, 
#              gMc1, gMc2, 
#              gT1, gT2,
#              nrow = 3)





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










