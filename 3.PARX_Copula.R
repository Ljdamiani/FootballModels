

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
library(caret)

# Packages Football
# pkgbuild::check_build_tools(debug = TRUE)
# pak::pak("jalapic/engsoccerdata")
# pak::pak("opisthokonta/goalmodel")
library(goalmodel)
library(footBayes)
library(extraDistr)

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

  # "SCA_SCA",         # Shot - Creating Actions (Passes, Take-ons and Drawing Fouls)
  "GCA_SCA",         # Goal - Creating Actions
  
  # "xAG",             # Expected Goals after the Assist
  "xA",              # Expected Assists
  # "KP",              # Key Passes
  "Final_Third",     # Passes in Final Third
  "PPA",             # Passes into Penalty Area
  "CrsPA",           # Crosses into Penalty Area
  "PrgP",            # Passes Progressivos
  
  # "Cmp_Total",       # Comp. Passes
  # "Att_Total",       # Attemptes
  "Cmp_percent_Total",
  # "TotDist_Total",
  "PrgDist_Total",
  # "Cmp_Short",
  # "Att_Short",
  # "Cmp_percent_Short",
  # "Cmp_Medium",
  # "Att_Medium",
  # "Cmp_percent_Medium",
  # "Cmp_Long",
  # "Att_Long",
  "Cmp_percent_Long",
  
  # "Live_Pass_Types",
  "Dead_Pass_Types",
  "FK_Pass_Types",
  "TB_Pass_Types",   # Through Balls
  "Sw_Pass_Types",   # Switch
  "Crs_Pass_Types",  # Crosses
  "TI_Pass_Types",
  "CK_Pass_Types",
  
  # "Touches",         # Touches
  "Def Pen_Touches",
  "Def 3rd_Touches",
  "Mid 3rd_Touches",
  "Att 3rd_Touches",
  "Att Pen_Touches",
  
  # "Att_Take_Ons",    # Dribbles
  # "Succ_Take_Ons",
  "Succ_percent_Take_Ons",
  # "Tkld_Take_Ons",
  # "Tkld_percent_Take_Ons",
  
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

# High correlation (somente vars_ref)
# X_matches <- Matches %>% select(all_of(vars_ref))
# corr <- cor(X_matches, use = "pairwise.complete.obs")
# highCorr <- findCorrelation(corr, cutoff = 0.85)
# print(colnames(X_matches)[highCorr])

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
# lambda = attack - defense + home + Xbeta
# https://github.com/opisthokonta/goalmodel

df_dc <- df_full %>%
  rename(
    home_team = Home_Team,
    away_team = Away_Team,
    home_goals = Score,
    away_goals = Score_A
  ) %>%
  arrange(Match_Date) %>%
  group_by(Match_Date, Season, home_team, away_team) %>%
  mutate(side = if_else(row_number() == 1, "home", "away")) %>%
  ungroup() %>%
  pivot_wider(
    names_from = side,
    values_from = -c(Match_Date, Season, home_team, away_team),
    names_glue = "{.value}_{side}"
  )  %>%
  rename(home_goals = home_goals_home, away_goals = away_goals_home) %>%
  select(Match_Date, Season, home_team, away_team, home_goals, away_goals,
         starts_with("MA"),
         newly_promoted_team_home, newly_promoted_opp_home,
         newly_promoted_team_away, newly_promoted_opp_away)
  

# Example
# model_dc <- fit_dc_model(df_dc)
# summary(model_dc)
# pred <- predict_dc_match(model_dc, home_team = "Liverpool", away_team = "Arsenal")
# pred

# Backtest
results_dc <- backtest_dc(data = df_dc, season_test = "2024/2025")

##############################################################################

# BIVARIATE POISSON (2003)
# https://cran.r-project.org/web/packages/footBayes/vignettes/footBayes_a_rapid_guide.html#goal-based-models-fit
# https://rss.onlinelibrary.wiley.com/doi/10.1111/1467-9884.00366
# Karlis and Ntzoufras (2003) (MLE through an EM algorithm) and Koopman and Lit (2015);

# Yh = X1 + X3
# Ya = X2 + X3

df_bivpois <- df_full %>%
  select(
    Match_Date,
    season_start,
    home_team = Home_Team,
    away_team = Away_Team,
    home_goals = Score,
    away_goals = Score_A
  ) %>%
  arrange(Match_Date) %>%
  group_by(Match_Date, season_start, home_team, away_team) %>%
  slice(1) %>% ungroup() %>%
  rename(periods = season_start) %>%
  select(periods, home_team, away_team, home_goals, away_goals, Match_Date) %>%
  mutate(periods   = factor(periods))

datas_bivpois = df_bivpois$Match_Date
df_bivpois <- df_bivpois %>% select(-Match_Date)
# df_bivpois <- df_bivpois[1:1520, ] # Test

# Example
# model_bivpois <- fit_bp_footbayes(df_bivpois)
# model_bivpois
# pred <- predict_bp_footbayes(model_bivpois, df_bivpois)
# pred

# Backtest
results_bivpois <- backtest_bp_footbayes(data = df_bivpois, season_test = 2024, datas_bivpois)
results_bivpois

##############################################################################

# PARX MODELS
df_parx <- df_full %>%
  arrange(Match_Date) 

# Exemplo


#########################
#_______ MODELOS _______#
#########################

# PARX Independencia só com MA_SCORE_A de covariavel (De Angelis)

# PARX Independencia com Todas Variáveis

# PARX Correlação por Cópulas só com MA_SCORE_A de covariavel

# PARX Correlação por Cópulas com Todas Variáveis















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


