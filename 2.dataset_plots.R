
########################################################################

# ____________ # DATA SET & ANALISIS  # ________________ #

# ___________ Author: Leonardo Damiani _______________ # 

#######################################################################

# Packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(extrafont)
library(stringr)
# library(robts)
library(forecast)
library(zoo)

# Functions
# source("functions.R")

# Fonts
library(extrafont)
# font_import()   # demora um pouco, roda só uma vez
loadfonts(device = "win")

# Get Data
countries <- c(
  "ENG",
  "ESP",
  "ITA"
)
Matches_Bakcup <- data.frame()
for (country in countries){
  for (ano in 2021:2025){
    load(paste0("dados/matches_", country, "_", ano, ".RData"))
    assign(paste0("matches_", country, "_", ano), matches)
    matches$Season = paste0(ano-1, "/", ano)
    Matches_Bakcup <- bind_rows(Matches_Bakcup, matches)
  }
}

# Since 2020 (Season 20/21)
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
  select(Match_Date, League, Wk, Season, Home_Team, Away_Team, Home_Score, Away_Score, 
         Team, Home_Away, Score, ScoreA) %>%
  group_by(Team) %>%
  mutate(
    n = n(),
    cluster = case_when(
      n > 160 ~ "No Relegation",
      n > 130 ~ "4 Seasons",
      n > 80 ~ "3 Seasons",
      n > 50 ~ "2 Seasons",
      T ~ "1 Season"
    )
  )

Matches_Unique <- Matches %>%
  group_by(Match_Date, League, Wk, Home_Team, Away_Team, Home_Score, Away_Score) %>%
  slice(1) %>% ungroup()

cat(
  paste0(
    "Jogos ENG: ", nrow(filter(Matches_Unique, League == "Premier League")), "\n",
    "Jogos ESP: ", nrow(filter(Matches_Unique, League == "La Liga")), "\n",
    "Jogos ITA: ", nrow(filter(Matches_Unique, League == "Serie A"))
  )
)

###################################################################################

# Distribution Teams

plot_df <- Matches %>% ungroup() %>%
  mutate(n_total = n_distinct(Team)) %>%
  group_by(cluster) %>%
  summarise(
    n_matches = mean(n),
    n_teams   = n_distinct(Team),   # ajuste se o nome da variável for outro
    n_teams_perc = n_teams/first(n_total),
    .groups = "drop"
  ) %>%
  mutate(
    txt_color = ifelse(n_matches > mean(n_matches), "white", "black"),
    txt_size = ifelse(n_matches > 50, 5, 3.5)
  )
plot_df$n_matches = c(38, 76, 114, 152, 190)

ggplot(plot_df, aes(x = n_matches, y = reorder(cluster, n_matches))) +
  geom_col(
    aes(fill = n_matches),
    color = "black",
    width = 0.8
  ) +
  geom_text(
    aes(
      x = n_matches / 2,
      label = paste0(n_teams, " Teams", " (", round(n_teams_perc, 2)*100, "%)" ),
      color = txt_color,
      size = txt_size
    ),
    family = "Times New Roman",
  ) +
  scale_fill_gradient(
    low  = "grey95",
    high = "grey20",
    guide = "none"
  ) +
  labs(
    x = "Number of Matches",
    y = NULL,
    title = "Distribution of Matches"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(face = "italic", hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  scale_color_identity() +
  scale_size_identity() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))


###################################################################################

# Gols Histograns
teams_plot = c("Liverpool", "Manchester City", "Milan", "Internazionale", "Barcelona", "Real Madrid")
aux_teams = Matches %>%
  filter(Team %in% teams_plot) %>%
  mutate(
    Home_Away = factor(as.character(Home_Away), levels = c("Home", "Away")),
    Team = factor(as.character(Team), levels = teams_plot)
  ) %>%
  count(Team, Home_Away, Score) %>% ungroup() %>%
  complete(Team, Home_Away, Score = 0:9, #max(Score), 
           fill = list(n = 0))

fills_colors <- c(
  "Liverpool"       = "#d00027",
  "Manchester City" = "#6caddf",
  "Real Madrid"     = "white",
  "Barcelona"       = "#a50044",
  "Milan"           = "#fb090b",
  "Internazionale"  = "#0267ab"
)

colors_colors <- c(
  "Liverpool"       = "black",
  "Manchester City" = "black",
  "Real Madrid"     = "black",
  "Barcelona"       = "black",
  "Milan"           = "black",
  "Internazionale"  = "black"
)

ggplot(aux_teams, aes(x = Score, y = n, fill = Team, color = Team)) +
  geom_col() +
  
  facet_grid(
    Team ~ Home_Away,
    switch = "y",
    labeller = labeller(
      Home_Away = c(Home = "Home", Away = "Away")
    )
  ) +
  
  scale_fill_manual(values = fills_colors) +
  scale_color_manual(values = colors_colors) +
  
  labs(x = "Goals", y = "") +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  scale_y_continuous(position = "right") +
  
  theme_bw(base_family = "Times New Roman") +
  theme(
    strip.placement = "outside",
    legend.position = "none"
  )

# Means
aux_means = Matches %>%
  ungroup() %>%
  filter(Team %in% teams_plot) %>%
  mutate(
    Home_Away = factor(as.character(Home_Away), levels = c("Home", "Away")),
    Team = factor(as.character(Team), levels = teams_plot)
  ) %>%
  group_by(Team, Home_Away) %>% 
  summarise(lambda_est = mean(Score), .groups = "drop")

aux_means

# ------------------------------------------------------------ #

# Series
teams_plot = c("Manchester City", "Brentford", "Bournemouth")
aux_teams = Matches %>%
  filter(Team %in% teams_plot) %>%
  mutate(
    Home_Away = factor(as.character(Home_Away), levels = c("Home", "Away")),
    Team = factor(as.character(Team), levels = teams_plot)
  )

fills_colors <- c(
  "Manchester City" = "#6caddf",
  "Brentford"       = "#e30713",
  "Bournemouth"     = "#fb090b"
)

ggplot(aux_teams, aes(x = Match_Date, y = Score, color = Team)) +
  
  # linha gols
  geom_line(aes(color = Team), alpha = 0.7, linewidth = 0.8) +
  
  facet_grid(
    Team ~ Home_Away,
    switch = "y",
    labeller = labeller(
      Home_Away = c(Home = "Home", Away = "Away")
    )
  ) +
  
  scale_color_manual(values = fills_colors) +
  labs(x = "", y = "Goals") +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 2), position = "right") +
  
  theme_bw(base_family = "Times New Roman") +
  theme(
    axis.title.y = element_text(margin = margin(r = 50)),
    strip.placement = "outside",
    legend.position = "none"
  )


# ------------------------------------------------------------ #
                       
# ACF
teams_plot = c("Manchester City", "Brentford", "Bournemouth")
acf_teams = Matches %>%
  filter(Team %in% teams_plot) %>%
  mutate(
    Home_Away = factor(Home_Away, levels = c("Home", "Away")),
    Team = factor(Team, levels = teams_plot)
  ) %>%
  group_by(Team, Home_Away) %>%
  group_modify(~{
    
    acf_obj = acf(.x$Score, plot = FALSE, lag.max = 30)
    
    tibble(
      lag = as.numeric(acf_obj$lag),
      acf = as.numeric(acf_obj$acf),
      limite_superior = 1.96 / sqrt(length(.x$Score))
    )
  }) %>%
  ungroup()

fills_colors <- c(
  "Manchester City" = "#6caddf",
  "Brentford"       = "#e30713",
  "Bournemouth"     = "#fb090b"
)

ggplot(acf_teams, aes(x = lag, y = acf, color = Team)) +
  
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.8, alpha = 0.8) +
  geom_hline(aes(yintercept = limite_superior), linetype = "dashed") +
  geom_hline(aes(yintercept = -limite_superior), linetype = "dashed") +
  
  facet_grid(
    Team ~ Home_Away,
    switch = "y",
    labeller = labeller(
      Home_Away = c(Home = "Home", Away = "Away")
    )
  ) +
  
  scale_color_manual(values = fills_colors) +
  labs(x = "Lag", y = "") +
  scale_y_continuous(position = "right") +
  
  theme_bw(base_family = "Times New Roman") +
  theme(
    axis.title.y = element_text(margin = margin(l = 50)),
    strip.placement = "outside",
    legend.position = "none"
  )
