

########################################################################

# ____________ # Modeling using PARX with Copulas  # ________________ #

# _________________ Author: Leonardo Damiani ________________________ # 

#######################################################################

# Packages
library(dplyr)
library(tidyr)
library(tscount)
library(copula)
library(VineCopula)
library(lubridate)
library(measures)
library(stringr)
library(forecast)
library(zoo)
library(slider)
library(TTR)

# Packages Football
# pkgbuild::check_build_tools(debug = TRUE)
# pak::pak("jalapic/engsoccerdata")
# pak::pak("opisthokonta/goalmodel")
library(goalmodel)

# Functions
source("functions.R")

###################################################################################

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
    Season = factor(Season, levels = unique(Season[order(Team, decreasing = T)])),
    Team = factor(Team, levels = unique(Team[order(Team, decreasing = T)])),
    Score = case_when(
      Home_Away == "Home" ~ Home_Score,
      Home_Away == "Away" ~ Away_Score
    ),
    Score_A = case_when(
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
         -Home_Formation, -Home_Score, -Home_xG, -Home_Goals, -Home_Yellow_Cards, -Home_Red_Cards,
         -Away_Formation, -Away_Score, -Away_xG, -Away_Goals, -Away_Yellow_Cards, -Away_Red_Cards) %>%
  select(Match_Date, Season, League, Wk, Team, Score, Score_A, everything())

###################################################################################

# VARIABLES
vars_ref <- c(
  
  # ===> Offensives
  "xG_Expected",     # Expected Goals
  "Sh",              # Shots
  "SoT",             # Shots On Target

  "SCA_SCA",         # Shot - Creating Actions (Passes, Take-ons and Drawing Fouls)
  "GCA_SCA",         # Goal - Creating Actions
  
  "xAG",             # Expected Goals after the Assist
  "xA",              # Expected Assists
  "KP",              # Key Passes
  "Final_Third",     # Passes in Final Third
  "PPA",             # Passes into Penalty Area
  "CrsPA",           # Crosses into Penalty Area
  "PrgP",            # Passes Progressivos
  
  "Cmp_Total",       # Comp. Passes
  "Att_Total",       # Attemptes
  "Cmp_percent_Total",
  "TotDist_Total",
  "PrgDist_Total",
  "Cmp_Short",
  "Att_Short",
  "Cmp_percent_Short",
  "Cmp_Medium",
  "Att_Medium",
  "Cmp_percent_Medium",
  "Cmp_Long",
  "Att_Long",
  "Cmp_percent_Long",
  
  "Live_Pass_Types",
  "Dead_Pass_Types",
  "FK_Pass_Types",
  "TB_Pass_Types",   # Through Balls
  "Sw_Pass_Types",   # Switch
  "Crs_Pass_Types",  # Crosses
  "TI_Pass_Types",
  "CK_Pass_Types",
  
  "Touches",         # Touches
  "Def Pen_Touches",
  "Def 3rd_Touches",
  "Mid 3rd_Touches",
  "Att 3rd_Touches",
  "Att Pen_Touches",
  
  "Att_Take_Ons",    # Dribbles
  "Succ_Take_Ons",
  "Succ_percent_Take_Ons",
  "Tkld_Take_Ons",
  "Tkld_percent_Take_Ons",
  
  "Carries_Carries", # Carries
  "PrgC_Carries",
  "CPA_Carries",
  "Mis_Carries",
  "Dis_Carries",
  "PrgDist_Carries",
  "Final_Third_Carries",
  "Fld",             # Fouls Drawn
  
  # ===> Defensives
  "CrdY",            # Yellow Card
  "CrdR",            # Red Card
  "Fls",             # Fouls Commited
  
  "Tkl",             # Tackle
  "TklW",
  "Def 3rd_Tackles",
  "Mid 3rd_Tackles",
  "Att 3rd_Tackles",
  
  "Tkl_Challenges",  # Tackle in Dribbling
  "Att_Challenges",
  "Tkl_percent_Challenges",

  "Blocks",          # Block
  "Sh_Blocks",
  "Pass_Blocks",
  
  "Int",             # Interception
  "Clr",             # Cleareance
  "Err",             # Mistake leading a Shot 
  "Won_percent_Aerial_Duels",
  "Recov"            # Ball recoveries

)

# Adjust Matches
df_opponents <- Matches %>% 
  select(Season, Match_Date, League, Wk, Home_Team, Away_Team, Team, Home_Away, Score, Score_A, all_of(vars_ref)) %>%
  group_by(Season, Match_Date, League, Wk, Home_Team, Away_Team) %>%
  mutate(
    
    # Beyond Score_A - Replicate Opponents Variables
    across(
      -c(Team, Home_Away, Score, Score_A),
      ~ rev(.x),
      .names = "{.col}_A"
    )
    
  ) %>%
  ungroup()

# Cols Variables
vars_all <- c("Score_A", vars_ref, paste0(vars_ref, "_A"))

# EMA
alpha <- 2 / (10 + 1)  # 0.18 => 10 observations
df_means <- df_opponents %>%
  arrange(Match_Date) %>% 
  group_by(Team, Home_Away) %>%
  
  mutate(
    across(
      all_of(vars_all),
      ~ EMA_lag(.x, alpha),
      .names = "MA_{.col}"
    )
  ) %>%
  ungroup() %>%
  
  # History Mean Until NA
  group_by(Home_Away) %>%
  mutate(
    across(
      all_of(vars_all),
      ~ {
        cs <- cumsum(ifelse(is.na(.x), 0, .x))
        n  <- cumsum(!is.na(.x))
        lag(cs / n)
      },
      .names = "HIST_{.col}"
    )
  ) %>%
  ungroup() %>%
  
  # Fallback conditional per season
  mutate(
    across(
      starts_with("MA_"),
      ~ {
        col_base <- sub("MA_", "", cur_column())
        hist_col <- paste0("HIST_", col_base)
        
        ifelse(
          is.na(.) & as.numeric(substr(as.character(Season), 1, 4)) > 2019,
          get(hist_col),
          .
        )
      }
    )
  ) %>%
  select(-all_of(paste0("HIST_",vars_all))) %>%
  mutate(season_start = as.numeric(substr(as.character(Season), 1, 4)))

###################################################################################

# Qualitative Variables

# Promoted Teams
teams_by_season <- df_means %>% distinct(Team, season_start)
teams_by_season <- teams_by_season %>%
  arrange(Team, season_start) %>%
  group_by(Team) %>%
  mutate(
    was_present_last_season = lag(season_start) == (season_start - 1),
    newly_promoted = ifelse(
      (is.na(was_present_last_season) | !was_present_last_season) & season_start != 2019,
      1,
      0
    )
  ) %>%
  ungroup() 

df_means <- df_means %>%
  left_join(
    teams_by_season %>%
      select(Team, season_start, newly_promoted),
    by = c("Team", "season_start")
  ) %>%
  rename(newly_promoted_team = newly_promoted) %>%
  group_by(Season, Match_Date, League, Wk, Home_Team, Away_Team) %>%
  mutate(newly_promoted_opp = rev(newly_promoted_team)) %>%
  ungroup()

##############################################################################

#################### ----------------------------------- #####################
#################### ______________ MODELOS ____________ #####################
#################### ------------------------------------#####################

# 5 Seasons
df_full <- df_means %>% filter(Season != "2019/2020")

##############################################################################

# DIXEN & COLES (1997)
# https://github.com/opisthokonta/goalmodel

df_dc <- df_full %>%
  select(
    Match_Date,
    Season,
    home_team = Home_Team,
    away_team = Away_Team,
    home_goals = Score,
    away_goals = Score_A
  ) %>%
  arrange(Match_Date) %>%
  group_by(Match_Date, Season, home_team, away_team) %>%
  slice(1) %>%
  ungroup()

# Example
model_dc <- fit_dc_model(df_dc)
pred <- predict_dc_match(
  model_dc,
  home_team = "Liverpool",
  away_team = "Chelsea"
)
pred

# Rue-Salvesen adjustment (2001)

results_dc <- backtest_dc(data = df_dc, season_test = "2022/2023")
results_dc[[1]]$match
results_dc[[1]]$prob_matrix

# Bivariada Poisson 

# INGARCH Independencia só com MA_SCORE_A de covariavel 

# INGARCH considerando Correlação por Cópulas

# INGARCH (modelagem por time) Independencia só com MA_SCORE_A de covariavel 

# INGARCH (modelagem por time) Correlação por Cópulas

#########################
#_______ MODELOS _______#
#########################













# Poisson + Identity + Sem cov
models_I_semx <- gera_modelos(rodada, dados, link = 'identity', distr = 'poisson', x_null = T, all_teams = T)

# Poisson + Identity (ALL)
models_I <- gera_modelos(rodada, dados, link = 'identity', distr = 'poisson', x_null = FALSE, all_teams = T)

# Poisson + Log + Sem cov
models_log_semx <- gera_modelos(rodada, dados, link = 'log', distr = 'poisson', x_null = T, all_teams = T)

# Poisson + Log (ALL)
models_log <- gera_modelos(rodada, dados, link = 'log', distr = 'poisson', x_null = FALSE, all_teams = T)

# OBS: Log for negative covariate effects


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


###################
#_____ Copula ____#
###################

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










###########################################################################

# Salvas as probs das rodadas
probs <- list()
probs[['Identidade Sem Cov']] = list(probs_I_semx, probs_I_semx_cop)
probs[['Identidade']] = list(probs_I, probs_I_cop)
probs[['Log Sem Cov']] = list(probs_log_semx, probs_log_semx_cop)
probs[['Log']] = list(probs_log, probs_log_cop)

probs_rodada[[paste('rodada', '20')]] <- probs
#csave.image("dados/models_probs.RData")

# rm(list=setdiff(ls(),c('rslts_38', 'rslts_38_sem_x')))


