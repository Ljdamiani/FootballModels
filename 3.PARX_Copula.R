

########################################################################

# ____________ # Modeling using PARX with Copulas  # ________________ #

# _________________ Author: Leonardo Damiani ________________________ # 

#######################################################################

# Packages
library(dplyr)
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

# Functions
source("functions.R")

###################################################################################

# Get Data
countries <- c("ENG")
Matches_Bakcup <- data.frame()
for (country in countries){
  for (ano in 2020:2024){
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
  select(Match_Date, Season, League, Wk, Team, Score, ScoreA, everything())

###################################################################################

# https://github.com/GuechtouliAnis/Football-Data-Scraping/blob/main/Features%20Explained.md

# VARIABLES
vars_ref <- c(
  
  # Offensives
  "xG_Expected",     # Expected Goals
  "xA",              # Expected Assists
  "Sh",              # Shots
  "SoT",             # Shots On Target
  "Touches",         # Touches
  "Touches_Touches",
  "Def Pen_Touches",
  "Def 3rd_Touches",
  "Mid 3rd_Touches",
  "Att 3rd_Touches",
  "Att Pen_Touches",
  "PrgP_Passes",
  "Carries_Carries",
  "PrgC_Carries",
  
  # Defensives
  "CrdY",            # Yellow Card
  "CrdR",            # Red Card
  "Tkl",             # Tackle
  "Tkl_Tackles",
  "TklW_Tackles",
  "TklW",
  "Def 3rd_Tackles",
  "Mid 3rd_Tackles",
  "Att 3rd_Tackles",
  "Tkl_Challenges",
  "Att_Challenges",
  "Tkl_percent_Challenges",
  "Lost_Challenges",
  "Int",             # Interception
  "Blocks",          # Block
  "Blocks_Outcomes",
  "Blocks_Blocks",
  "Sh_Blocks",
  "Pass_Blocks",
  "Tkl+Int",
  "Clr",
  "Err",
  "Won_percent_Aerial_Duels",
  "Recov",


  "SCA_SCA",
  "GCA_SCA",
  
  "Cmp_Passes",
  "Att_Passes",
  "Cmp_percent_Passes",
  
  
  "Att_Take_Ons",
  "Succ_Take_Ons",
  "Cmp_Total",
  "Att_Total",
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
  "KP",
  "Final_Third",
  "PPA",
  "CrsPA",
  "PrgP",
  "Att",
  "Live_Pass_Types",
  "Dead_Pass_Types",
  "FK_Pass_Types",
  "TB_Pass_Types",
  "Sw_Pass_Types",
  "Crs_Pass_Types",
  "TI_Pass_Types",
  "CK_Pass_Types",
  "In_Corner_Kicks",
  "Out_Corner_Kicks",
  "Str_Corner_Kicks",
  "Cmp_Outcomes",
  "Off_Outcomes",
  
  "Live_Touches",
    
  "Succ_percent_Take_Ons",
  "Tkld_Take_Ons",
  "Tkld_percent_Take_Ons",
  "TotDist_Carries",
  "PrgDist_Carries",
  "Final_Third_Carries",
  "CPA_Carries",
  "Mis_Carries",
  "Dis_Carries",
  "Rec_Receiving",
  "PrgR_Receiving",
  "Fls",
  "Fld",
  "Off",
  "Crs",
  "OG",
)

Attacking Stats
Gls – Goals scored
Ast – Assists provided
G+A – Goals + Assists
xG – Expected goals
xAG – Expected assists
npxG – Non-penalty expected goals
G-PK – Goals excluding penalties

Defensive Stats
Tkl – Total tackles
TklW – Tackles won
Blocks – Blocks made
Int – Interceptions
Tkl+Int – Combined tackles and interceptions
Clr – Clearances
Err – Errors leading to goals

Passing & Creativity Stats
PrgP – Progressive passes
PrgC – Progressive carries
KP – Key passes (passes leading to a shot)
Cmp%_stats_passing – Pass completion percentage
Ast_stats_passing – Assists
xA – Expected assists
PPA – Passes into the penalty area

Goalkeeping Stats
GA – Goals conceded
Saves – Saves made
Save% – Save percentage
CS – Clean sheets
CS% – Clean sheet percentage
PKA – Penalties faced
PKsv – Penalty saves

Possession & Ball Control
Touches – Total touches of the ball
Carries – Total ball carries
PrgR – Progressive runs (carries moving the ball forward significantly)
Mis – Miscontrols
Dis – Times dispossessed

Miscellaneous Stats
CrdY – Yellow cards
CrdR – Red cards

# First Filter
aux_matches <- Matches %>%
  select(Match_Date, League, Wk, Team, Score, ScoreA, all_of(vars_ref))

aux_matches <- aux_matches %>%
  group_by(Team, Home_Away) %>%
  mutate(
    
    across(
      all_of(vars_ref),
      ~ slide_dbl(
        .x = .x,
        .f = function(x){
          
          x <- x[-length(x)]  # remove jogo atual
          
          if (length(x) == 0){
            return(NA_real_)
          } else if (length(x) == 1){
            return(x)
          } else {
            n_window <- min(length(x), max_window)
            return(tail(WMA(x, n = n_window), 1))
          }
        },
        .before = Inf,
        .complete = FALSE
      ),
      .names = "MA_{.col}"
    ),
    
    # aplicar regra de default em todas as MA_
    across(
      starts_with("MA_"),
      ~ case_when(
        is.na(.) & row_number() > 380 ~ var_2nd,
        is.na(.) ~ 1,
        TRUE ~ .
      )
    )
    
  ) %>%
  ungroup()

###################################################################################

# Metade Final da Temporada 24/25 - Teste
test_data = filter(Matches, Match_Date > '2025-01-02') # Dois Jogos Atrasados 15º rodada
train_data = filter(Matches, Match_Date <= '2025-01-02')

###################################################################################

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





########################################################################

# ____________ # Modeling using PARX with Copulas  # ________________ #

# _________________ Author: Leonardo Damiani ________________________ # 

#######################################################################

# Packages
library(dplyr)
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

# Functions
source("functions.R")

###################################################################################

# Get Data
countries <- c("ENG")
Matches_Bakcup <- data.frame()
for (country in countries){
  for (ano in 2020:2024){
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

###################################################################################

# VARIÁVEIS
vars_ref <- c("xG", "shots", "passes")

# First Filter
aux_matches <- Matches 


aux_matches <- aux_matches %>%
  group_by(Team, Home_Away) %>%
  mutate(
    
    across(
      all_of(vars_ref),
      ~ slide_dbl(
        .x = .x,
        .f = function(x){
          
          x <- x[-length(x)]  # remove jogo atual
          
          if (length(x) == 0){
            return(NA_real_)
          } else if (length(x) == 1){
            return(x)
          } else {
            n_window <- min(length(x), max_window)
            return(tail(WMA(x, n = n_window), 1))
          }
        },
        .before = Inf,
        .complete = FALSE
      ),
      .names = "MA_{.col}"
    ),
    
    # aplicar regra de default em todas as MA_
    across(
      starts_with("MA_"),
      ~ case_when(
        is.na(.) & row_number() > 380 ~ var_2nd,
        is.na(.) ~ 1,
        TRUE ~ .
      )
    )
    
  ) %>%
  ungroup()

###################################################################################

# Metade Final da Temporada 24/25 - Teste
test_data = filter(Matches, Match_Date > '2025-01-02') # Dois Jogos Atrasados 15º rodada
train_data = filter(Matches, Match_Date <= '2025-01-02')

###################################################################################

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



