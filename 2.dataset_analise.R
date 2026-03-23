
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
library(zoo)

# Functions
# source("functions.R")

# Fonts
loadfonts(device = "win")

# Get Data
countries <- c("ENG")
Matches_Bakcup <- data.frame()
for (country in countries){
  for (ano in 2020:2025){
    load(paste0("dados/matches_", country, "_", ano, ".RData"))
    assign(paste0("matches_", country, "_", ano), matches)
    matches$Season = paste0(ano-1, "/", ano)
    Matches_Bakcup <- bind_rows(Matches_Bakcup, matches)
  }
}

# Since 2019 (Season 19/20)
Matches <- Matches_Bakcup %>%
  mutate(
    Wk = str_extract(Matchweek, "(?<=Matchweek )\\d+"),
    Team = factor(Team, levels = unique(Team[order(Team, decreasing = T)])),
    Score = case_when(
      Home_Away == "Home" ~ Home_Score,
      Home_Away == "Away" ~ Away_Score
    ),
    ScoreA = case_when(
      Home_Away == "Home" ~ Away_Score,
      Home_Away == "Away" ~ Home_Score
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
  select(Match_Date, League, Wk, Team, Score, ScoreA, everything())

# Calcula as médias de gols concedidos (Parâmetro de Defesa)
# dados <- concedidosX(seasons_df, rodada_max, c(2, 2))

# Last Season
# season = dados[dados$Date >= '2022-08-05', ]
# rownames(season) <- NULL

###################################################################################

# Distribution Teams
Matches %>%
  group_by(Team, Home_Away) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(Home_Away == "Home") %>%
  ggplot(aes(x = n, y = reorder(Team, n))) +
  geom_col(fill = "#ff6347", color = "white", alpha = 0.8) +
  labs(
    x = "Número de Jogos",
    y = "",
    title = "Distribuição de Jogos em Casa"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(hjust = 0.5, vjust = -1),
    plot.title = element_text(size = 16, face = "italic", hjust = 0.5, vjust = 2),
    plot.margin = margin(10, 30, 10, 10) # espaço pro label fora da barra
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))


###################################################################################

# Relatório de Análise dos Gols
treino = Matches
team = "Manchester City"

# Mandante
g1 <- treino %>%
  filter(Team == team, Home_Away == "Home") %>%
  group_by(Score) %>% count(Score) %>%
  ggplot(aes(x = Score, y = n)) +
  labs(x = 'Home Goals', y = '', title = paste('Home', team)) +
  geom_col(fill = 'red', col = 'white') + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme_bw(base_family = "Times")

# Away
g2 <- treino %>%
  filter(Team == team, Home_Away == "Away") %>%
  group_by(Score) %>%
  count(Score) %>%
  ggplot(aes(x = Score, y = n)) +
  labs(x = 'Away Goals', y = '', title = paste('Away', team)) +
  geom_col(fill = 'red', col = 'white') + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme_bw(base_family = "Times")

# ------------------------------------------------------------ #

# Gráfico das Séries Mandante
aux <- treino %>% 
  filter(Team == team & Home_Away == "Home") %>%
  mutate(
    i = row_number(),
    mov_avg = rollmean(Score, k = 5, fill = NA, align = "right"),
    change_season = ifelse(Season != lag(Season), TRUE, FALSE)
  ) %>%
  select(i, Match_Date, Season, Team, Score, mov_avg, change_season)

media_home <- mean(aux$Score)
season_labels <- aux %>%
  group_by(Season) %>%
  summarise(
    x = mean(range(i)),   # centro da temporada no eixo x
    .groups = "drop"
  )

gL1 <- ggplot(aux, aes(x = i, y = Score)) +
  labs(y = 'Goals', x = 'Number of Matches', title = paste('Home', team)) +
  # Linhas verticais de separação
  geom_vline(xintercept = which(aux$change_season == TRUE) - 0.5, 
             linetype = "dashed", color = "gold", size = 1.1) +
  # Linha dos gols
  geom_line(color = "red", alpha = 0.6, size = 1) +
  # Linha da média móvel
  geom_line(aes(y = mov_avg), color = "darkred", size = 1.5) +
  # Média geral
  annotate("text", x = min(aux$i), y = 6.5,
           label = paste("Mean:", round(media_home, 2)),
           hjust = 0, size = 5, color = "darkred") +
  # Labels centralizados para cada temporada
  geom_text(data = season_labels, aes(x = x, y = 8.5, label = Season),
            inherit.aes = FALSE, color = "#DAA520", size = 5) +
  theme_bw(base_family = "Times") +
  lims(y = c(0, 9))

# Gráfico para jogos fora de casa
aux <- treino %>% 
  filter(Team == team & Home_Away == "Away") %>%
  mutate(
    i = row_number(),
    mov_avg = rollmean(Score, k = 5, fill = NA, align = "right"),
    change_season = ifelse(Season != lag(Season), TRUE, FALSE)
  ) %>%
  select(i, Match_Date, Season, Team, Score, mov_avg, change_season)

media_home <- mean(aux$Score)
season_labels <- aux %>%
  group_by(Season) %>%
  summarise(
    x = mean(range(i)),   # centro da temporada no eixo x
    .groups = "drop"
  )

gL2 <- ggplot(aux, aes(x = i, y = Score)) +
  labs(y = 'Goals', x = 'Number of Matches', title = paste('Away', team)) +
  # Linhas verticais de separação
  geom_vline(xintercept = which(aux$change_season == TRUE) - 0.5, 
             linetype = "dashed", color = "gold", size = 1.1) +
  # Linha dos gols
  geom_line(color = "red", alpha = 0.6, size = 1) +
  # Linha da média móvel
  geom_line(aes(y = mov_avg), color = "darkred", size = 1.5) +
  # Média geral
  annotate("text", x = min(aux$i), y = 6.5,
           label = paste("Mean:", round(media_home, 2)),
           hjust = 0, size = 5, color = "darkred") +
  # Labels centralizados para cada temporada
  geom_text(data = season_labels, aes(x = x, y = 8.5, label = Season),
            inherit.aes = FALSE, color = "#DAA520", size = 5) +
  theme_bw(base_family = "Times") +
  lims(y = c(0, 9))

# ------------------------------------------------------------ #

 # GOLS CONCEDIDOS

# Gráfico das Séries Mandante
aux <- treino %>% 
  filter(Team == team & Home_Away == "Home") %>%
  mutate(
    i = row_number(),
    mov_avg = rollmean(ScoreA, k = 5, fill = NA, align = "right"),
    change_season = ifelse(Season != lag(Season), TRUE, FALSE)
  ) %>%
  select(i, Match_Date, Season, Team, ScoreA, mov_avg, change_season)

media_home <- mean(aux$ScoreA)
season_labels <- aux %>%
  group_by(Season) %>%
  summarise(
    x = mean(range(i)),   # centro da temporada no eixo x
    .groups = "drop"
  )

gLc1 <- ggplot(aux, aes(x = i, y = ScoreA)) +
  labs(y = 'Goals', x = 'Number of Matches', title = paste('Home', team)) +
  # Linhas verticais de separação
  geom_vline(xintercept = which(aux$change_season == TRUE) - 0.5, 
             linetype = "dashed", color = "gold", size = 1.1) +
  # Linha dos gols
  geom_line(color = "red", alpha = 0.6, size = 1) +
  # Linha da média móvel
  geom_line(aes(y = mov_avg), color = "darkred", size = 1.5) +
  # Média geral
  annotate("text", x = min(aux$i), y = 6.5,
           label = paste("Mean C.:", round(media_home, 2)),
           hjust = 0, size = 5, color = "darkred") +
  # Labels centralizados para cada temporada
  geom_text(data = season_labels, aes(x = x, y = 8.5, label = Season),
            inherit.aes = FALSE, color = "#DAA520", size = 5) +
  theme_bw(base_family = "Times") +
  lims(y = c(0, 9))

# Gráfico para jogos fora de casa
aux <- treino %>% 
  filter(Team == team & Home_Away == "Away") %>%
  mutate(
    i = row_number(),
    mov_avg = rollmean(ScoreA, k = 5, fill = NA, align = "right"),
    change_season = ifelse(Season != lag(Season), TRUE, FALSE)
  ) %>%
  select(i, Match_Date, Season, Team, ScoreA, mov_avg, change_season)

media_home <- mean(aux$ScoreA)
season_labels <- aux %>%
  group_by(Season) %>%
  summarise(
    x = mean(range(i)),   # centro da temporada no eixo x
    .groups = "drop"
  )

gLc2 <- ggplot(aux, aes(x = i, y = ScoreA)) +
  labs(y = 'Goals', x = 'Number of Matches', title = paste('Away', team)) +
  # Linhas verticais de separação
  geom_vline(xintercept = which(aux$change_season == TRUE) - 0.5, 
             linetype = "dashed", color = "gold", size = 1.1) +
  # Linha dos gols
  geom_line(color = "red", alpha = 0.6, size = 1) +
  # Linha da média móvel
  geom_line(aes(y = mov_avg), color = "darkred", size = 1.5) +
  # Média geral
  annotate("text", x = min(aux$i), y = 6.5,
           label = paste("Mean C.:", round(media_home, 2)),
           hjust = 0, size = 5, color = "darkred") +
  # Labels centralizados para cada temporada
  geom_text(data = season_labels, aes(x = x, y = 8.5, label = Season),
            inherit.aes = FALSE, color = "#DAA520", size = 5) +
  theme_bw(base_family = "Times") +
  lims(y = c(0, 9))

# ------------------------------------------------------------ #

# Gráfico das ACF 
# acf_spearman_bootstrap <- function(x, max_lag = 80, n_boot = 1000, conf_level = 0.95, seed = 123) {
#   set.seed(seed)
#   n <- length(x)
#   acf_vals <- numeric(max_lag + 1)
#   lower_ci <- numeric(max_lag + 1)
#   upper_ci <- numeric(max_lag + 1)
#   
#   acf_vals[1] <- 1
#   lower_ci[1] <- 1
#   upper_ci[1] <- 1
#   
#   for (lag in 1:max_lag) {
#     obs_cor <- cor(x[1:(n - lag)], x[(lag + 1):n], method = "spearman")
#     acf_vals[lag + 1] <- obs_cor
#     
#     # Bootstrap
#     boot_vals <- replicate(n_boot, {
#       idx <- sample(1:(n - lag), size = n - lag, replace = TRUE)
#       cor(x[idx], x[idx + lag], method = "spearman")
#     })
#     
#     alpha <- (1 - conf_level) / 2
#     lower_ci[lag + 1] <- quantile(boot_vals, probs = alpha)
#     upper_ci[lag + 1] <- quantile(boot_vals, probs = 1 - alpha)
#   }
#   
#   acf_df <- data.frame(
#     lag = 0:max_lag,
#     acf = acf_vals,
#     limite_inferior = lower_ci,
#     limite_superior = upper_ci
#   )
#   
#   # Gráfico estilo ggplot personalizado
#   g <- ggplot(acf_df, aes(x = lag, y = acf)) +
#     geom_hline(yintercept = 0) +
#     geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
#                 fill = "grey", alpha = 0.3) +
#     geom_segment(aes(xend = lag, yend = 0), 
#                  color = "black", linewidth = 0.6) +
#     labs(x = "", y = "", title = 'Spearman ACF com IC bootstrap') +
#     theme_bw() +
#     theme(text = element_text(family = "Times"))
#   
#   return(g)
# }

# Home Arsenal
# dados_acf <- subset(treino, Team == team & Home_Away == "Home")$Score
# teste <- acf_spearman_bootstrap(dados_acf)
# Acf()

# Home Arsenal
dados_acf <- subset(treino, Team == team & Home_Away == "Home")$Score
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 90)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gLacf1 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3)  +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "ACF", title = paste('Home', team)) +
  theme_bw() +
  theme(text = element_text(family = "Times"))  

# Away Arsenal
dados_acf <- subset(treino, Team == team & Home_Away == "Away")$Score
limite_superior <- 1.96 / sqrt(length(dados_acf))
limite_inferior <- -limite_superior

acf_result <- acf(dados_acf, 90)
acf_df <- data.frame(lag = acf_result$lag, 
                     acf = acf_result$acf)

gLacf2 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), 
              fill = "grey", alpha = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), 
               color = "black", linewidth = 0.6) +
  labs(x = "", y = "", title = paste('Away', team)) +
  theme_bw() +
  theme(text = element_text(family = "Times")) 


# GRID
grid.arrange(
  gL1, gL2,
  gLacf1, gLacf2,
  gLc1, gLc2
)



