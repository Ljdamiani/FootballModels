

##################################################################################


# ________________________ Função Auxiliares ___________________ #


##################################################################################

# EMA
EMA_lag <- function(x, alpha) {
  out <- numeric(length(x))
  out[1] <- NA_real_
  
  for (i in 2:length(x)) {
    if (is.na(out[i-1])) {
      out[i] <- x[i-1]
    } else {
      out[i] <- alpha * x[i-1] + (1 - alpha) * out[i-1]
    }
  }
  
  return(out)
}


###############################################################



# ________________________ Models __________________________ #



##############################################################

#### PARX (INGARCH) ###############

fit_parx_team <- function(
    y,
    x = NULL,
    p_max = 3,
    q_max = 3,
    links = c("identity", "log"),
    distr = "poisson",
    previous_model = NULL
){
  
  # -------------------------
  # SCALE X (MIN-MAX)
  # -------------------------
  
  if(!is.null(x)){
    
    x <- as.matrix(x)
    
    xmin <- apply(x, 2, min, na.rm = TRUE)
    xmax <- apply(x, 2, max, na.rm = TRUE)
    range <- xmax - xmin
    
    is_dummy <- apply(x, 2, function(v){
      vals <- unique(na.omit(v))
      all(vals %in% c(0,1))
    })
    
    range[range == 0] <- 1
    
    for(j in seq_len(ncol(x))){
      if(!is_dummy[j]){
        x[,j] <- (x[,j] - xmin[j]) / range[j]
      }
    }
    
  } else{
    xmin <- NULL
    xmax <- NULL
    is_dummy <- NULL
  }
  
  # -------------------------
  # GRID SEARCH
  # -------------------------
  
  grid <- expand.grid(
    p = 0:p_max,
    q = 0:q_max,
    link = links
  )
  grid <- grid[!(grid$p > 0 & grid$q == 0), ] # 
  
  best_model <- NULL
  best_aic <- Inf
  best_spec <- NULL
  
  for(g in 1:nrow(grid)){
    
    p <- grid$p[g]
    q <- grid$q[g]
    link <- as.character(grid$link[g])
    
    cat(paste("\n p =", p, " q =", q, " ", link))
    
    model_list <- list()
    if(q > 0) model_list$past_obs  <- 1:q
    if(p > 0) model_list$past_mean <- 1:p
    
    # -------------------------
    # INIT DO MODELO ANTERIOR
    # -------------------------
    
    start_vals <- NULL
    
    if(!is.null(previous_model)){
      start_vals <- make_start_from_previous(
        previous_model,
        model_list,
        x
      )
    }
    
    if(is.null(start_vals)){
      start_vals <- list(method="GLM")
    }
    
    if(p == 0 && q == 0){
      
      fit <- try({
        
        if(is.null(x)){
          
          glm(
            y ~ 1,
            family = poisson(link = link)
          )
          
        } else{
          
          glm(
            y ~ x,
            family = poisson(link = link)
          )
          
        }
        
      }, silent = TRUE)
      
      if(inherits(fit, "try-error")){
        fit <- NULL
      }
      
    } else{
      
      fit <- try(
        tsglm(
          y,
          xreg = x,
          distr = distr,
          link = link,
          model = model_list,
          
          info = "none",
          score = FALSE,
          
          start.control = list(
            method = if(is.null(start_vals)) "GLM" else "fixed"
          ),
          
          final.control = list(
            optim.method = "BFGS",
            optim.control = list(
              maxit = 40,
              reltol = 1e-5
            )
          )
        ),
        silent = TRUE
      )
      
      if(inherits(fit,"try-error")){
        fit <- NULL
      }
    }
    
    aic <- try(AIC(fit), silent = TRUE)
    
    if(!inherits(fit,"try-error") && is.finite(aic)){
      
      cat(paste("   ", round(aic, 3)))
      
      if(aic < best_aic){
        
        best_aic <- aic
        best_model <- fit
        
        best_spec <- list(
          p = p,
          q = q,
          link = link,
          vars = if(is.null(x)) NULL else colnames(x),
          AIC = aic
        )
      }
    }
  }
  
  cat(paste(
    "\n \n  BEST - p =", best_spec$p,
    " q =", best_spec$q,
    " ", best_spec$link,
    " ", best_spec$AIC, "\n"
  ))
  
  attr(best_model,"scaling") <- list(
    min = xmin,
    max = xmax
  )
  
  attr(best_model,"is_dummy") <- is_dummy
  attr(best_model,"best_spec") <- best_spec
  
  return(best_model)
}

##############################################################

#### BACKTEST PARX (INGARCH) ###############

backtest_parx <- function(data, season_test, x_col, model_name){
# 
#   data = df_parx
#   season_test = "2024/2025"
#   x_col = c("MA_xG_Expected", "MA_xG_Expected_A")
#   model_name = "PARX xG VARIABLES"
  
  data <- data[order(data$Match_Date), ]
  
  # Matches
  games <- data %>%
    group_by(Match_Date, Home_Team, Away_Team) %>%
    slice(1) %>% 
    ungroup()
  
  games$game_id <- seq_len(nrow(games))
  
  data <- data %>% 
    left_join(games[,c("Match_Date","Home_Team","Away_Team","game_id")])
  
  test_games <- which((games$Season == season_test) & (as.numeric(games$Wk) > 2))
  
  # -----------------------
  # First Train
  # -----------------------
  
  cat("\n First Train \n")
  first_date <- games$Match_Date[test_games[1]]
  train0 <- data[data$Match_Date < first_date, ]
  
  models <- fit_parx_league(train0, x_col)
  
  # pre-allocate
  results <- vector("list", length(test_games))
  
  # -----------------------
  # LOOP
  # -----------------------
  
  for(i in 1:length(test_games)){

    g <- test_games[i]
    
    date  <- games$Match_Date[g]
    home  <- games$Home_Team[g]
    away  <- games$Away_Team[g]
    
    cat("\n")
    cat(home, "x", away, "\n")
    cat("\n", model_name, "   ", i, " de ", length(test_games), "\n\n")
    
    # -----------------------
    # Rows (fast)
    # -----------------------
    
    idx <- data$game_id == g
    game_rows <- data[idx, ]
    
    row_H <- game_rows[game_rows$Home_Away == "Home", ]
    row_A <- game_rows[game_rows$Home_Away == "Away", ]
    
    # -----------------------
    # Prediction
    # -----------------------
    
    if(is.null(models[[paste0(home,"_H")]])){
      train_full <- data[data$game_id <= g, ]
      models <- update_parx_models(
        models,
        train_full,
        home,
        away,
        x_col
      )
      next
    } # Como
    
    lambda_H <- predict_parx(
      models[[paste0(home,"_H")]],
      row_H[,x_col,drop=FALSE]
    )
    
    lambda_A <- predict_parx(
      models[[paste0(away,"_A")]],
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
    
    train <- data[data$game_id < g, ]
    
    res <- compute_parx_residuals(train, models)
    
    cop <- estimate_parx_copula(res)
    
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
    
    # -----------------------
    # Update
    # -----------------------
    
    train_full <- data[data$game_id <= g, ]
    
    models <- update_parx_models(
      models,
      train_full,
      home,
      away,
      x_col
    )
  }
  
  return(results)
}

fit_parx_league <- function(
    data,
    x_col,
    p_max = 3,
    q_max = 3
){
  
  # data = train0
  # p_max = 3
  # q_max = 3
  
  y_col = "Score"
  teams <- unique(data$Team)
  models <- list()
  cat(paste0("\n"))
  
  for(team in teams){
    cat(paste0("\n", team, " ", which(team == teams), "/", length(teams), "\n"))
    
    # HOME
    cat(paste0("\n Home \n"))
    dH <- data[(data$Team == team)&(data$Home_Away == "Home"), ]
    y <- dH[[y_col]]
    x <- if(!is.null(x_col)) as.matrix(dH[, x_col]) else NULL
    
    if(nrow(dH) == 0){ # Como
      models[[paste0(team,"_H")]] <- NULL
    } else{
      models[[paste0(team,"_H")]] <- fit_parx_team(y, x, p_max, q_max)
    } 
    
    # AWAY
    cat(paste0("\n Away \n"))
    dA <- data[(data$Team == team)&(data$Home_Away == "Away"), ]
    y <- dA[[y_col]]
    x <- if(!is.null(x_col)) as.matrix(dA[, x_col]) else NULL
    
    models[[paste0(team,"_A")]] <- fit_parx_team(y, x, p_max, q_max)
  }
  
  return(models)
}

update_parx_models <- function(
    models,
    data,
    home,
    away,
    x_col = NULL,
    p_max = 3,
    q_max = 3
){
  
  # data = train_full
  
  cat("\n ===> UPDATE MODELS \n")
  
  y_col <- "Score"
  
  # =========================
  # HOME
  # =========================
  
  cat("\n Home - ", home, "\n")
  
  idxH <- (data$Team == home) & (data$Home_Away == "Home")
  dH <- data[idxH, ]
  
  y <- dH[[y_col]]
  x <- if(!is.null(x_col)) as.matrix(dH[,x_col]) else NULL
  
  model_name_H <- paste0(home,"_H")
  previous_model <- models[[model_name_H]]
  
  models[[model_name_H]] <- fit_parx_team(
    y,
    x,
    p_max,
    q_max,
    previous_model = previous_model
  )
  
  # =========================
  # AWAY
  # =========================
  
  cat("\n Away - ", away, "\n")
  
  idxA <- (data$Team == away) & (data$Home_Away == "Away")
  dA <- data[idxA, ]
  
  y <- dA[[y_col]]
  x <- if(!is.null(x_col)) as.matrix(dA[,x_col]) else NULL
  
  model_name_A <- paste0(away,"_A")
  
  previous_model <- models[[model_name_A]]
  
  models[[model_name_A]] <- fit_parx_team(
    y,
    x,
    p_max,
    q_max,
    previous_model = previous_model
  )
  
  return(models)
}

make_start_from_previous <- function(old_fit, new_model, xreg){
  
  cf <- coef(old_fit)
  
  k_past_obs  <- length(new_model$past_obs)
  k_past_mean <- length(new_model$past_mean)
  k_xreg      <- if(is.null(xreg)) 0 else ncol(xreg)
  
  start <- list(method = "fixed")
  
  i <- 1
  
  start$intercept <- cf[i]
  i <- i + 1
  
  if(k_past_obs > 0){
    start$past_obs <- cf[i:(i + k_past_obs - 1)]
    i <- i + k_past_obs
  }
  
  if(k_past_mean > 0){
    start$past_mean <- cf[i:(i + k_past_mean - 1)]
    i <- i + k_past_mean
  }
  
  if(k_xreg > 0){
    start$xreg <- cf[i:(i + k_xreg - 1)]
  }
  
  return(start)
}

##################################################################################

#### COPULA FUNCTIONS ###############

est_copula <- function(rodada, dados, models){
  
  # Times
  teams1 = unique(dados$Home)
  teams2 = unique(dados$Away)
  
  # Dados Cópula
  dados_copula <- dados[dados$Date < min(rodada$Date), ]
  
  # Coluna de Resíduos
  dados_copula$ResH = NA
  dados_copula$ResA = NA
  
  # ACFs dos Resíduos
  acfs_res <- list()
  
  # Modelos Mandantes
  for (i in 1:length(teams1)){
    
    # Mandante
    teamM = teams1[i]
    print(paste(teamM, '    Mandante', '    ', i))
    
    # Dados dos Mandantes
    iH = which(dados_copula$Home == teamM)
    teamM_dados = dados_copula[iH, c("Date", "Away", "HomeGoals", "AwayConc")] # jogando em casa
    
    # Modelo
    mod_M <- models[[paste0(teamM,'_M')]]
    
    # Substitui os resíduos
    dados_copula[iH, 'ResH'] = dados_copula[iH,]$HomeGoals - mod_M$fitted.values
    
    # ACFS
    acfs_res[[paste0(teamM,'_M')]] <- acf(dados_copula[iH, 'ResH'], 
                                          lag.max = length(dados_copula[iH, 'ResH']),
                                          main = paste0(teamM,'_M'))
  }
  
  # Modelos Visitantes
  for (i in 1:length(teams2)){
    
    # Visitante
    teamV = teams2[i]
    print(paste(teamV, '    Visitante', '    ', i))
    
    # Dados dos Mandantes
    iV = which(dados_copula$Away == teamV)
    teamV_dados = dados_copula[iV, c("Date", "Home", "AwayGoals", "HomeConc")] 
    
    # Modelo
    mod_V <- models[[paste0(teamV,'_V')]]
    
    # Substitui os resíduos
    dados_copula[iV, 'ResA'] = dados_copula[iV,]$AwayGoals - mod_V$fitted.values
    
    # ACFS
    acfs_res[[paste0(teamV,'_V')]] <- acf(dados_copula[iV, 'ResA'], 
                                          lag.max = length(dados_copula[iV, 'ResA']),
                                          main = paste0(teamV,'_V'))
  }
  
  # ---------------------------------- #
  # Estimação da Cópula pelos Resíduos #
  # ---------------------------------- #
  
  # Acumuladas Empíricas
  
  # dados_copula$F_ResH <- sapply(dados_copula$ResH, function(x) ecdf(dados_copula$ResH)(x))
  # dados_copula$F_ResA <- sapply(dados_copula$ResA, function(x) ecdf(dados_copula$ResA)(x))
  
  matriz_dados_cop <- pobs(as.matrix(dados_copula[, c("ResH", "ResA")]))
  
  var_a <- matriz_dados_cop[,1]
  var_b <- matriz_dados_cop[,2]
  
  # Correlação
  # cor(matriz_dados_cop, method = "kendall")
  # hist(dados_copula$ResH)
  # hist(dados_copula$ResA)
  
  # https://www.r-bloggers.com/2016/03/how-to-fit-a-copula-model-in-r-heavily-revised-part-2-fitting-the-copula/
  
  # selectedCopula <- BiCopSelect(var_a, var_b, familyset = seq(1, 6))
  # print(selectedCopula)
  
  # Estimação -> ML
  gau <- fitCopula(normalCopula(), data = matriz_dados_cop, method = 'ml')
  aic_gau <- AIC(gau)
  
  frank <- fitCopula(frankCopula(), data = matriz_dados_cop, method = 'ml')
  aic_frank <- AIC(frank)
  
  clay <- fitCopula(claytonCopula(), data = matriz_dados_cop, method = 'ml')
  aic_clay <- AIC(clay)
  
  aics = c(aic_gau, aic_frank, aic_clay)
  
  # Retorno
  if (min(aics) == aic_gau){
    selectedCopula = list('Normal', gau)
  } else if (min(aics) == aic_frank){
    selectedCopula = list('Frank', frank)
  } else if (min(aics) == aic_clay){
    selectedCopula = list('Clayton', clay)
  }
  
  # Retorna a copula estimada
  return(selectedCopula)
}

compute_parx_residuals <- function(data, models){
  
  data <- data %>%
    group_by(Match_Date, Home_Team, Away_Team) %>%
    mutate(
      Home_Goals = first(Score),
      Away_Goals = last(Score)
    ) %>%
    select(Match_Date, Home_Team, Away_Team, Home_Goals, Away_Goals) %>%
    slice(1) %>% ungroup()
  
  data$ResH <- NA
  data$ResA <- NA
  teams <- unique(c(data$Home_Team, data$Away_Team))
  
  for(team in teams){
    
    # HOME
    idxH <- data$Home_Team == team
    modH <- models[[paste0(team,"_H")]]
    data$ResH[idxH] <- data$Home_Goals[idxH] - modH$fitted.values
    
    # AWAY
    idxA <- data$Away_Team == team
    modA <- models[[paste0(team,"_A")]]
    data$ResA[idxA] <- data$Away_Goals[idxA] - modA$fitted.values
  }
  
  return(data)
}

estimate_parx_copula <- function(data){
  
  U <- pobs(as.matrix(data[,c("ResH","ResA")]))
  
  fit_gau   <- fitCopula(normalCopula(),  U, method="ml")
  fit_frank <- fitCopula(frankCopula(),   U, method="ml")
  fit_clay  <- fitCopula(claytonCopula(), U, method="ml")
  
  aics <- c(
    AIC(fit_gau),
    AIC(fit_frank),
    AIC(fit_clay)
  )
  
  best <- which.min(aics)
  
  list(
    name = c("Normal","Frank","Clayton")[best],
    model = list(fit_gau,fit_frank,fit_clay)[[best]]
  )
}

##################################################################################

#### PREDICTION ###############

predict_parx <- function(model, newx){
  
  scaling  <- attr(model, "scaling")
  is_dummy <- attr(model, "is_dummy")
  
  # -------------------------
  # SCALE NEWX
  # -------------------------
  
  if(!is.null(newx)){
    
    newx <- as.matrix(newx)
    
    for(j in seq_len(ncol(newx))){
      
      if(!is_dummy[j]){
        
        minj <- scaling$min[j]
        maxj <- scaling$max[j]
        range <- maxj - minj
        
        if(range == 0) range <- 1
        
        newx[,j] <- (newx[,j] - minj) / range
      }
    }
  }
  
  # -------------------------
  # GLM (1-step ahead)
  # -------------------------
  
  if(inherits(model, "glm")){
    
    if(is.null(newx)){
      return(as.numeric(predict(model, type="response")[1]))
    }
    
    newdata <- data.frame(x = I(as.matrix(newx)))
    
    return(
      as.numeric(
        predict(
          model,
          newdata = newdata,
          type = "response"
        )
      )
    )
  }
  
  # -------------------------
  # TSGLM (1-step ahead)
  # -------------------------
  
  predict(
    model,
    n.ahead = 1,
    newxreg = newx
  )$pred
}

predict_parx_copula <- function(lambda_H, lambda_A, copula_fit){
  
  cop <- copula_fit@copula
  cop <- setTheta(cop, copula_fit@estimate)
  
  biv <- mvdc(
    cop,
    margins = c("pois","pois"),
    paramMargins = list(
      list(lambda = lambda_H),
      list(lambda = lambda_A)
    )
  )
  
  grid <- expand.grid(0:7,0:7)
  pm <- dMvdc(as.matrix(grid), biv)
  matrix(pm, 8, 8)
}

##################################################################################

#### DIXON COLES ###############

fit_dc_model <- function(data_train, x1 = NULL, x2 = NULL, xi = 0.0019) {
  
  my_weights <- weights_dc(data_train$Match_Date, xi = xi)
  
  # Sem covariáveis
  if (is.null(x1) && is.null(x2)) {
    
    model <- goalmodel(
      goals1 = data_train$home_goals,
      goals2 = data_train$away_goals,
      team1  = data_train$home_team,
      team2  = data_train$away_team,
      weights = my_weights,
      dc = TRUE
    )
    
  } else {
    
    X1 <- as.matrix(x1)
    X2 <- as.matrix(x2)
    X1 <- scale(X1)
    X2 <- scale(X2)
    
    model <- goalmodel(
      goals1 = data_train$home_goals,
      goals2 = data_train$away_goals,
      team1  = data_train$home_team,
      team2  = data_train$away_team,
      x1 = X1,
      x2 = X2,
      weights = my_weights,
      dc = TRUE
    )
    
  }
  return(model)
}

predict_dc_match <- function(model, home_team, away_team, x1 = NULL, x2 = NULL) {
  
  if (is.null(x1) && is.null(x2)) {
    prob_matrix <- predict_result(
      model,
      team1 = home_team,
      team2 = away_team,
      return_df = TRUE
    )
  } else {
    prob_matrix <- predict_result(
      model,
      team1 = home_team,
      team2 = away_team,
      x1 = as.matrix(x1),
      x2 = as.matrix(x2),
      return_df = TRUE
    )
  }
  return(prob_matrix)
}

backtest_dc <- function(data, season_test, cols_x1 = NULL, cols_x2 = NULL) {
  
  data <- data %>% arrange(Match_Date)
  results <- data.frame()
  test_idx <- which((data$Season == season_test)&(data$Wk != 1))
  
  for(i in test_idx){
    
    data_train <- data[1:(i-1), ]
    data_test  <- data[i, ]
    
    # Extrair covariáveis se existirem
    if (!is.null(cols_x1)) {
      X1_train <- data_train[, cols_x1, drop = FALSE]
      X2_train <- data_train[, cols_x2, drop = FALSE]
      
      X1_test  <- data_test[, cols_x1, drop = FALSE]
      X2_test  <- data_test[, cols_x2, drop = FALSE]
      
    } else {
      X1_train <- X2_train <- NULL
      X1_test  <- X2_test  <- NULL
    }
    
    model_dc <- fit_dc_model(
      data_train,
      x1 = X1_train,
      x2 = X2_train
    )
    
    P <- predict_dc_match(
      model_dc,
      home_team = data_test$home_team,
      away_team = data_test$away_team,
      x1 = X1_test,
      x2 = X2_test
    )
    
    P$Match_Date <- data$Match_Date[i]
    results <- bind_rows(results, P)

    cat(
      "Match", i - test_idx[1] + 1, " - ",
      data$home_team[i], " x ", data$away_team[i], "\n"
    )
  }
  colnames(results) <- c("Home", "Away", "PH", "PD", "PA", "Date")
  results <- results %>% select(Date, everything())
  return(results)
}

##################################################################################

#### POISSON BIVARIADA ###############

fit_bp_footbayes <- function(data_train) {
  fit <- mle_foot(
    data  = data_train,
    model = "biv_pois",
    predict = 1,
    interval = "Wald"
  )
  return(fit)
}

predict_bp_footbayes <- function(model, data_train, home_team, away_team) {
  pred <- foot_prob_modified(
    object = model,
    data = data_train,
    home_team = home_team,
    away_team = away_team
  )
  return(pred)
}

backtest_bp_footbayes <- function(data, season_test, datas, weeks) {

  # data = df_bivpois
  # season_test = 2024
  # datas = datas_bivpois
  # weeks = wk_bovpois
  results <- data.frame()
  test_idx <- which((data$periods == season_test) & weeks)
  
  for(i in test_idx){
    
    cat("Match", i - min(test_idx) + 1, " - ",
        data$home_team[i], " x ", data$away_team[i], "\n")
    # if(i == 1) next
    data_train <- data[1:i, ] # prediction row together
    
    # Como
    if(("Como" %in% data_train$away_team) & !("Como" %in% data_train$home_team)){
      data_train <- data_train %>% filter(away_team != "Como")
    }
    
    model <- fit_bp_footbayes(data_train)
    pred <- predict_bp_footbayes(model, data_train)
    pred$prob_table$Match_Date = datas[i]
    results <- bind_rows(results, pred$prob_table)
    
  }
  colnames(results) <- c("Home", "Away", "PH", "PD", "PA", "Date")
  results <- results %>% select(Date, everything())  
  return(results)
}

foot_prob_modified <- function (object, data, home_team, away_team) {
  required_cols <- c("periods", "home_team", "away_team", 
                     "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:", paste(missing_cols, 
                                                          collapse = ", ")))
  }
  teams <- unique(c(data$home_team, data$away_team))
  if (inherits(object, "list")) {
    predict <- object$predict
  }
  else {
    stop("Provide one among these four model fit classes: 'stanfit', 'CmdStanFit', 'stanFoot' or 'list'.")
  }
  if (is.null(predict) || predict == 0) {
    stop("foot_prob cannot be used if the 'predict' argument is set to zero.")
  }
  data_prev <- data[(dim(data)[1] - predict + 1):(dim(data)[1]), 
  ]
  if (missing(home_team) & missing(away_team)) {
    home_team <- data_prev$home_team
    away_team <- data_prev$away_team
  }
  if (length(home_team) != length(away_team)) {
    stop("Please, include the same number for home and away teams.")
  }
  find_match <- c()
  for (i in 1:length(home_team)) {
    find_match[i] <- which(data_prev$home_team %in% home_team[i] & 
                             data_prev$away_team %in% away_team[i])
  }
  true_goal_home <- data_prev$home_goals[find_match]
  true_goal_away <- data_prev$away_goals[find_match]
  if (length(find_match) == 0) {
    stop(paste("There is not any out-of-sample match:", 
               home_team, "-", away_team, sep = ""))
  }
  if (inherits(object, "list")) {
    model <- object$model
    predict <- object$predict
    n.iter <- 200
    team1_prev <- object$team1_prev
    team2_prev <- object$team2_prev
    N_prev <- predict
    prediction_routine <- function(team1_prev, team2_prev, 
                                   att, def, home, corr, ability, model, predict, n.iter) {
      mean_home <- exp(home[1, 2] + att[team1_prev, 2] + 
                         def[team2_prev, 2])
      mean_away <- exp(att[team2_prev, 2] + def[team1_prev, 
                                                2])
      if (model == "double_pois") {
        x <- y <- matrix(NA, n.iter, predict)
        for (n in 1:N_prev) {
          x[, n] <- rpois(n.iter, mean_home[n])
          y[, n] <- rpois(n.iter, mean_away[n])
        }
      }
      else if (model == "biv_pois") {
        couple <- array(NA, c(n.iter, predict, 2))
        for (n in 1:N_prev) {
          couple[, n, ] <- rbvpois(n.iter, a = mean_home[n], 
                                   b = mean_away[n], c = corr[1, 2])
        }
        x <- couple[, , 1]
        y <- couple[, , 2]
      }
      else if (model == "skellam") {
        diff_y <- matrix(NA, n.iter, predict)
        for (n in 1:N_prev) {
          diff_y[, n] <- rskellam(n.iter, mu1 = mean_home[n], 
                                  mu2 = mean_away[n])
        }
        x <- diff_y
        y <- matrix(0, n.iter, predict)
      }
      else if (model == "student_t") {
        sigma_y <- object$sigma_y
        diff_y <- matrix(NA, n.iter, predict)
        for (n in 1:N_prev) {
          diff_y[, n] <- rt.scaled(n.iter, df = 7, mean = home[1, 
                                                               2] + ability[team1_prev[n], 2] - ability[team2_prev[n], 
                                                                                                        2], sd = sigma_y)
        }
        x <- round(diff_y)
        y <- matrix(0, n.iter, predict)
      }
      prob_func <- function(mat_x, mat_y) {
        if(is.vector(mat_x)){
          res <- mat_x - mat_y
          prob_h <- sum(res > 0)/n.iter
          prob_d <- sum(res == 0)/n.iter
          prob_a <- sum(res < 0)/n.iter
        } else{
          res <- mat_x - mat_y
          prob_h <- apply(res, 2, function(x) sum(x > 0))/n.iter
          prob_d <- apply(res, 2, function(x) sum(x == 0))/n.iter
          prob_a <- apply(res, 2, function(x) sum(x < 0))/n.iter
        }
        return(list(prob_h = prob_h, prob_d = prob_d, prob_a = prob_a))
      }
      conf <- prob_func(x, y)
      tbl <- data.frame(home_team = teams[team1_prev[find_match]], 
                        away_team = teams[team2_prev[find_match]], prob_h = conf$prob_h[find_match], 
                        prob_d = conf$prob_d[find_match], prob_a = conf$prob_a[find_match])
      return(tbl)
    }
    if (predict != 0) {
      prob_matrix <- prediction_routine(team1_prev, team2_prev, 
                                        object$att, object$def, object$home, object$corr, 
                                        object$abilities, model, predict, n.iter)
    }
    return(list(prob_table = prob_matrix))
  }
}





