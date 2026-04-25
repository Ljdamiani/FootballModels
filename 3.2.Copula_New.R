

########################################################################

# ____________ # Modeling using PARX with Copulas  # ________________ #

# _________________ Author: Leonardo Damiani ________________________ # 

#######################################################################

# Packages
library(dplyr)
library(tidyr)
library(purrr)
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
library(qs2)

source("functions.R")

######################### DATA

# Get Data
countries <- c("ESP")
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
  "CrsPA"           # Crosses into Penalty Area

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

# 5 Seasons
df_full <- df_means %>% filter(Season != "2019/2020")
df_parx <- df_full %>% arrange(Match_Date) 

# Matches
games <- df_parx %>%
  group_by(Match_Date, Home_Team, Away_Team) %>%
  slice(1) %>% 
  ungroup()

games$game_id <- seq_len(nrow(games))

df_parx <- df_parx %>% left_join(games[,c("Match_Date","Home_Team","Away_Team","game_id")])




######################### MODEL - NEW COPULA

files_matches <- list.files("models", pattern = paste0(countries, "_matches\\.qs2$"), full.names = TRUE)

# Test Data
matches_test <- map_dfr(files_matches, function(f){
  country <- str_extract(basename(f), "^[A-Z]+")
  qs_read(f) %>%
    mutate(League = country)
})

# Models
files_list <- list.files("models", pattern = paste0("\\",countries), full.names = TRUE)
files_list_newcop <- files_list[str_detect(files_list, "NEWCOP")] %>% str_remove("\\_NEWCOP")
# files_list <- files_list[!str_detect(files_list, "matches|_dc|_bivpois|NEWCOP")]
files_list <- files_list[!str_detect(files_list, "matches|_dc|_bivpois|NEWCOP")]
files_list <- setdiff(files_list, files_list_newcop)
print(files_list)

# Loop 
for(f in files_list){
  
  file <- basename(f) |> str_remove("\\.qs2$")
  type <- str_remove(file, "^[A-Z]+_")
  cat("\nLoading:", file, "->", type, "\n")
  aux_models <- qs_read(f)
  
  # nrow(matches_test)
  results = list()
  for(i in seq_along(aux_models)){
    
    # if(i == 11){next} #ITA
    x <- aux_models[[i]]
    models <- x$models
    if(length(models) == 0){cat("\n", file, i, " - erro") 
      next}
  
    date   <- matches_test$Date[i]
    home   <- matches_test$Home[i]
    away   <- matches_test$Away[i]
    g      <- df_parx[(df_parx$Match_Date == date) & (df_parx$Home_Team == home) & (df_parx$Away_Team == away), ]$game_id[1]
    
    # cat("\n")
    # cat(i, home, "x", away)
  
    # -----------------------
    # Rows (fast)
    # -----------------------
    
    game_rows <- df_parx[df_parx$game_id == g, ]
    row_H <- game_rows[game_rows$Home_Away == "Home", ]
    row_A <- game_rows[game_rows$Home_Away == "Away", ]
    
    # -----------------------
    # Prediction
    # -----------------------
    
    mod1 = models[[paste0(home,"_H")]]
    x_col = colnames(mod1$xreg)
    if(is.null(x_col)){
      x_col <- map_vec(
        names(coef(mod1))[-1][!grepl("beta|alpha", names(coef(mod1))[-1])],
        ~ substr(.x, 2, nchar(.x))
      )
      if(all(is.na(x_col))){x_col <- NULL
      } else if (all(x_col == "")){x_col <- names(mod1$data$is_dummy)}
    }
    lambda_H <- predict_parx(
      mod1,
      row_H[,x_col,drop=FALSE]
    )
    
    mod2 = models[[paste0(away,"_A")]]
    x_col = colnames(mod2$xreg)
    if(is.null(x_col)){
      x_col <- map_vec(
        names(coef(mod2))[-1][!grepl("beta|alpha", names(coef(mod2))[-1])],
        ~ substr(.x, 2, nchar(.x))
      )
      if(all(is.na(x_col))){x_col <- NULL
      } else if (all(x_col == "")){x_col <- names(mod2$data$is_dummy)}
    }
    lambda_A <- predict_parx(
      mod2,
      row_A[,x_col,drop=FALSE]
    )
    
    # -----------------------
    # Independence
    # -----------------------
    
    prob_ind <- outer(dpois(0:7, lambda_H), dpois(0:7, lambda_A))
    
    PH <- sum(prob_ind[lower.tri(prob_ind)])
    PD <- sum(diag(prob_ind))
    PA <- sum(prob_ind[upper.tri(prob_ind)])
    
    prob_ind_df <- data.frame(
      Home = home,
      Away = away,
      PH = PH,
      PD = PD,
      PA = PA,
      Date = date
    )
    
    # -----------------------
    # Copula
    # -----------------------
    
    train <- df_parx %>%
      filter(game_id < g) %>%
      filter(Home_Team == home | Away_Team == away)
      
    aux_train <- train %>%
      group_by(Match_Date, Home_Team, Away_Team) %>%
      mutate(
        Home_Goals = first(Score),
        Away_Goals = last(Score)
      ) %>%
      select(Match_Date, Home_Team, Away_Team, Home_Goals, Away_Goals) %>%
      slice(1) %>% ungroup()
    
    aux_train$ResH <- NA
    aux_train$ResA <- NA
    teams <- unique(c(aux_train$Home_Team, aux_train$Away_Team))
    
    for(team in teams){
      
      # HOME
      gamesH <- df_parx %>% filter(game_id < g) %>%
        filter(Home_Team == team) %>% group_by(Match_Date, Home_Team, Away_Team) %>%
        slice(1) %>% ungroup()
      
      idxH1 <- aux_train$Home_Team == team
      idxH2 <- gamesH$Away_Team == away
      
      if(all(idxH1) == FALSE){
        modH <- models[[paste0(team,"_H")]]
        if (team == home){
          aux_train$ResH[idxH1] <- aux_train$Home_Goals[idxH1] - modH$fitted.values
        } else{
          aux_train$ResH[idxH1] <- aux_train$Home_Goals[idxH1] - modH$fitted.values[idxH2]
          if(length(aux_train$Home_Goals[idxH1]) != length(modH$fitted.values[idxH2])){print(team)}
        }
      }
        
      # AWAY
      gamesA <- df_parx %>% filter(game_id < g) %>%
        filter(Away_Team == team) %>% group_by(Match_Date, Home_Team, Away_Team) %>%
        slice(1) %>% ungroup()
      
      idxA1 <- aux_train$Away_Team == team
      idxA2 <- gamesA$Home_Team == home
      
      if(all(idxA1) == FALSE){
        modA <- models[[paste0(team,"_A")]]
        if (team == away){
          aux_train$ResA[idxA1] <- aux_train$Away_Goals[idxA1] - modA$fitted.value
        } else{
          aux_train$ResA[idxA1] <- aux_train$Away_Goals[idxA1] - modA$fitted.value[idxA2]
          if(length(aux_train$Away_Goals[idxA1]) != length(modA$fitted.value[idxA2])){print(team)}
        }
      }
    }
    
    res <- aux_train %>% filter(!is.na(ResH)) %>% filter(!is.na(ResA))

    U <- pobs(as.matrix(res[,c("ResH","ResA")]))
    
    fit_gau <- tryCatch(
      fitCopula(normalCopula(), U, method = "ml"),
      error = function(e) NULL
    )
    
    fit_frank <- tryCatch(
      fitCopula(frankCopula(), U, method = "ml"),
      error = function(e) NULL
    )
    
    fit_clay <- tryCatch(
      fitCopula(claytonCopula(), U, method = "ml"),
      error = function(e) NULL
    )
    
    aics <- c(
      ifelse(length(fit_gau)>0, AIC(fit_gau), NA),
      ifelse(length(fit_frank)>0, AIC(fit_frank), NA),
      ifelse(length(fit_clay)>0, AIC(fit_clay), NA)
    )
    
    best <- which.min(aics)
    
    cop <- list(
      name = c("Normal","Frank","Clayton")[best],
      model = list(fit_gau,fit_frank,fit_clay)[[best]]
    )
    
    # cop <- estimate_parx_copula(res)
    prob_cop <- predict_parx_copula(lambda_H, lambda_A, cop$model)
    
    PH_COP <- sum(prob_cop[lower.tri(prob_cop)])
    PD_COP <- sum(diag(prob_cop))
    PA_COP <- sum(prob_cop[upper.tri(prob_cop)])
    
    prob_cop_df <- data.frame(
      Home = home,
      Away = away,
      PH = PH_COP,
      PD = PD_COP,
      PA = PA_COP,
      Date = date
    )
    
    # -----------------------
    # Save
    # -----------------------
    
    results[[i]] <- list(
      models = models,
      date = date,
      home = home,
      away = away,
      lambda_H = lambda_H,
      lambda_A = lambda_A,
      prob_ind = prob_ind_df,
      prob_cop = prob_cop_df,
      copula = cop$name,
      copula_model = cop$model
    )
  }
  
  # Save
  qs_save(results, paste0("models//", file, "_NEWCOP.qs2"))
}


