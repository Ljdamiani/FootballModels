
####################################################

# ____________ # Probabilities  # ________________ #

# ___________ Author: Leonardo Damiani ___________ # 

####################################################

# Packages
library(qs2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(measures)
library(mlr3measures)

################################
#_______     MATCHES    _______#
################################

files_matches <- list.files("models", pattern = "matches\\.qs2$", full.names = TRUE)

# Test Data
matches_all <- map_dfr(files_matches, function(f){
  country <- str_extract(basename(f), "^[A-Z]+")
  qs_read(f) %>%
    mutate(League = country)
})

# Remove Como Match
matches_all <- matches_all %>%
  filter(!((Home == "Udinese") & (Away == "Como")))

df_probs <- matches_all
cols = c("Date","Home","Away")

##################################
#_______   DC & BIVPOISS  _______#
##################################

files_dc <- list.files("models", pattern = "_dc\\.qs2$", full.names = TRUE)
aux_probs = data.frame()
for(f in files_dc){
  name <- str_remove(basename(f), "\\.qs2$")
  cat("Loading:", name, "\n")
  tmp <- qs_read(f)
  tmp <- tmp %>%
    rename_with(~ paste0(.x, "_dc"), -all_of(cols))
  aux_probs = rbind(aux_probs, tmp)
  rm(tmp)
  gc()
}
df_probs <- df_probs %>% 
  left_join(aux_probs, by = cols) %>% 
  mutate(
    result_dc = case_when(
      PH_dc > pmax(PA_dc, PD_dc) ~ "H",
      PA_dc > pmax(PH_dc, PD_dc) ~ "A",
      PD_dc > pmax(PH_dc, PA_dc) ~ "D",
      T ~ NA
    )
  )

files_biv <- list.files("models", pattern = "_bivpois\\.qs2$", full.names = TRUE)
aux_probs = data.frame()
for(f in files_biv){
  name <- str_remove(basename(f), "\\.qs2$")
  cat("Loading:", name, "\n")
  tmp <- qs_read(f)
  tmp <- tmp %>%
    rename_with(~ paste0(.x, "_bp"), -all_of(cols))
  aux_probs = rbind(aux_probs, tmp)
  rm(tmp)
  gc()
}
df_probs <- df_probs %>%
  left_join(aux_probs, by = cols) %>%
  mutate(
    result_bp = case_when(
      PH_bp > pmax(PA_bp, PD_bp) ~ "H",
      PA_bp > pmax(PH_bp, PD_bp) ~ "A",
      PD_bp > pmax(PH_bp, PA_bp) ~ "D",
      T ~ NA
    )
  )

################################
#_______   JOIN MODELS  _______#
################################

files_list <- list.files(
  "models",
  pattern = "\\.qs2$",
  full.names = TRUE
)

# Remove used
files_list <- files_list[
  !str_detect(files_list, "matches|_dc|_bivpois")
]

aux_probs = data.frame()
aux_info = data.frame()
aux_copulas = data.frame()

for(f in files_list){
  
  file <- basename(f) |> str_remove("\\.qs2$")
  type <- str_remove(file, "^[A-Z]+_")
  cat("Loading:", file, "->", type, "\n")
  models <- qs_read(f)
  
  # IND
  tmp_ind <- map_dfr(models, ~ .x$prob_ind) %>%
    mutate(
      model = type,
      method = "ind"
    )
  
  # COP
  tmp_cop <- map_dfr(models, ~ .x$prob_cop) %>%
    mutate(
      model = type,
      method = "cop"
    )
  
  aux_probs <- rbind(aux_probs, tmp_ind, tmp_cop)
  
  # p + q + link
  tmp_list <- vector("list", length = 0)
  idx <- 1
  
  for(k in seq_along(models)){
    
    x <- models[[k]]
    # cat(paste0("\n", k))
    mods <- x$models
    
    for(i in seq_along(mods)){
      
      mod <- mods[[i]]
      cf  <- coef(mod)
      key <- paste0(
        paste(names(cf), collapse="|"),
        "::",
        paste(round(unname(cf), 8), collapse="|")
      )
      
      tmp_list[[idx]] <- list(
        model = type,
        key   = key,
        q     = max(mod$model$past_obs),
        p     = ifelse(length(mod$model$past_obs) == 0, 0, max(mod$model$past_mean)),
        link  = mod$link
      )
      
      idx <- idx + 1
    }
  }
  
  tmp_info <- bind_rows(tmp_list) %>% 
    distinct(key, .keep_all = TRUE) %>% 
    select(-key) %>% 
    mutate(
      across(
        where(is.numeric),
        ~ replace(.x, is.infinite(.x), 0)
      )
    )
  aux_info <- rbind(aux_info, tmp_info)
  
  # Copulas
  tmp_copulas <- map_dfr(models, function(x){
    tibble(
      Date  = x$prob_ind$Date[1],
      Home  = x$prob_ind$Home[1],
      Away  = x$prob_ind$Away[1],
      copula = x$copula,
      model  = type
    )
  })
  aux_copulas <- rbind(aux_copulas, tmp_copulas)
  
  rm(models, tmp_ind, tmp_cop, tmp_list, tmp_info, tmp_copulas)
  gc()
}

# Result
aux_probs_wide <- aux_probs %>%
  # mutate(result = c("H","D","A")[max.col(cbind(PH, PD, PA))])  %>%
  pivot_wider(
    names_from = c(model, method),
    values_from = c(PH, PD, PA),
    names_glue = "{.value}_{model}_{method}"
  )

df_probs <- df_probs %>% left_join(aux_probs_wide, by = cols)



############################################
#_________   PERFORMANCE METRICS   ________#
############################################

prob_cols <- grep("^PH_", names(df_probs), value = TRUE)
models <- prob_cols %>%
  sub("^PH_", "", .) %>%
  unique()

get_probs <- function(df, model){
  cbind(
    df[[paste0("PH_", model)]],
    df[[paste0("PD_", model)]],
    df[[paste0("PA_", model)]]
  )
}

df_performance <- df_probs %>%
  mutate(
    month_year = format(Date, "%Y-%m"),
    cluster = case_when(
      (cluster_home == "No Relegation") &
        (cluster_away == "No Relegation") ~ "No Relegation Teams",
    T ~ "At least 1 Relegation"
    )
  )

table(df_performance$League, df_performance$cluster)

#_______   CALC METRICS - LEAGUES  ______#

leagues <- c("ENG", "ESP", "ITA")
perf_list <- vector("list", length(models))
idx <- 1

for(lg in leagues){
  
  df_lg <- df_performance %>% filter(League == lg)
  
  for(i in seq_along(models)){
    
    m <- models[i]
    
    res <- tryCatch({
      
      probs <- get_probs(df_lg, m)
      row_sums <- rowSums(probs)
      row_sums[row_sums == 0] <- 1
      probs <- probs / row_sums
      
      ok <- complete.cases(probs, df_lg$result, 
                           df_lg$result_dc, df_lg$result_bp)
      
      probs <- probs[ok, ]
      obs   <- df_lg$result[ok]
      obs_factor <- factor(obs, levels = c("H","D","A"))
      
      pred <- c("H","D","A")[max.col(probs)]
      colnames(probs) = c("H","D","A")
      
      # -------------------------
      # McNemar vs DC
      # -------------------------
      
      acc_model <- pred == obs
      acc_dc    <- df_lg$result_dc[ok] == obs
      acc_bp    <- df_lg$result_bp[ok] == obs
      
      p_dc <- NA
      p_bp <- NA
      
      if(!str_detect(m, "dc")){
        tab_dc <- table(acc_model, acc_dc)
        if(all(dim(tab_dc) == c(2,2))){
          p_dc <- mcnemar.test(tab_dc)$p.value
        }
      }
      
      if(!str_detect(m, "bp")){
        tab_bp <- table(acc_model, acc_bp)
        if(all(dim(tab_bp) == c(2,2))){
          p_bp <- mcnemar.test(tab_bp)$p.value
        }
      }
      
      data.frame(
        league = lg,
        model = m,
        
        # metrics
        logloss = Logloss(probs, obs_factor),
        brier = mbrier(obs_factor, probs),
        match_result = mean(pred == obs),
        
        # McNemar
        p_dc = p_dc,
        p_bp = p_bp
      )
      
    }, error = function(e){
      NULL
    })
    
    if(!is.null(res)){
      perf_list[[idx]] <- res
      idx <- idx + 1
    }
  }
}

perf_df <- bind_rows(perf_list)

perf_df <- perf_df %>%
  separate(
    model,
    into = c("model", "method"),
    sep = "_(?=[^_]+$)"
  ) %>% 
  arrange(league, desc(match_result)) 

#_______   CALC METRICS - LEAGUES - GROUPS  ______#
leagues <- c("ENG", "ESP", "ITA")
clusters <- unique(df_performance$cluster)

perf_list <- list()
idx <- 1

for(lg in leagues){
  
  df_lg <- df_performance %>% filter(League == lg)
  
  for (clus in clusters){
    
    df_lg_clus <- df_lg %>% filter(cluster == clus)
    
    for(i in seq_along(models)){
      
      m <- models[i]
      
      res <- tryCatch({
        
        probs <- get_probs(df_lg_clus, m)
        row_sums <- rowSums(probs)
        row_sums[row_sums == 0] <- 1
        probs <- probs / row_sums
        
        ok <- complete.cases(
          probs,
          df_lg_clus$result,
          df_lg_clus$result_dc,
          df_lg_clus$result_bp
        )
        
        probs <- probs[ok, ]
        obs   <- df_lg_clus$result[ok]
        obs_factor <- factor(obs, levels = c("H","D","A"))
        
        pred <- c("H","D","A")[max.col(probs)]
        colnames(probs) = c("H","D","A")
        
        # -----------------------
        # McNemar preparation
        # -----------------------
        
        acc_model <- pred == obs
        acc_dc <- df_lg_clus$result_dc[ok] == obs
        acc_bp <- df_lg_clus$result_bp[ok] == obs
        
        p_dc <- NA
        p_bp <- NA
        
        # test vs DC
        if(!str_detect(m, "dc")){
          tab_dc <- table(acc_model, acc_dc)
          if(all(dim(tab_dc) == c(2,2))){
            p_dc <- mcnemar.test(tab_dc)$p.value
          }
        }
        
        # test vs BP
        if(!str_detect(m, "bp")){
          tab_bp <- table(acc_model, acc_bp)
          if(all(dim(tab_bp) == c(2,2))){
            p_bp <- mcnemar.test(tab_bp)$p.value
          }
        }
        
        data.frame(
          cluster = clus,
          league = lg,
          model = m,
          
          logloss = Logloss(probs, obs_factor),
          brier   = mbrier(obs_factor, probs),
          match_result = mean(acc_model),
          
          p_dc = p_dc,
          p_bp = p_bp
        )
        
      }, error = function(e){
        NULL
      })
      
      if(!is.null(res)){
        perf_list[[idx]] <- res
        idx <- idx + 1
      }
    }
  }
}

perf_df_cluster <- bind_rows(perf_list)

perf_df_cluster <- perf_df_cluster %>%
  separate(
    model,
    into = c("model", "method"),
    sep = "_(?=[^_]+$)"
  ) %>% 
  arrange(league, cluster, desc(match_result))

# Save Excel
library(writexl)
write_xlsx(
  list(
    Leagues = perf_df,
    Clusters = perf_df_cluster
  ),
  "results\\results.xlsx"
)


#_______   CALC METRICS - LEAGUES - MONTH  ______#

leagues <- c("ENG", "ESP", "ITA")
df_performance_filtered <- df_performance %>% filter(cluster == "No Relegation Teams")
table(df_performance_filtered$League, df_performance_filtered$month_year)

ms <- unique(df_performance_filtered$month_year)
perf_list <- vector("list", length(models))
for(lg in leagues){
  df_lg <- df_performance_filtered %>% filter(League == lg)
  for (month in ms){
    df_lg_m <- df_lg %>% filter(month_year == month)
    for(i in seq_along(models)){
      
      m <- models[i]
      res <- tryCatch({
        probs <- get_probs(df_lg_m, m)
        row_sums <- rowSums(probs)
        row_sums[row_sums == 0] <- 1
        probs <- probs / row_sums
        ok <- complete.cases(probs, df_lg_m$result)
        
        probs <- probs[ok, ]
        obs   <- df_lg_m$result[ok]
        obs_factor   <- factor(obs, levels = c("H","D","A"))
        pred <- c("H","D","A")[max.col(probs)]
        colnames(probs) = c("H","D","A")
        
        data.frame(
          month_year = month,
          league = lg,
          model = m,
          
          #__________ Log Loss _____________#
          logloss = Logloss(probs, obs_factor),
          
          #__________ Brier Score _____________#
          brier = mbrier(obs_factor, probs),
          
          #__________ Match Result Score _____________#
          match_result = mean(pred == obs)
        )
      }, error = function(e){
        NULL
      })
      if(!is.null(res)){
        perf_list[[idx]] <- res
        idx <- idx + 1
      }
    } 
  }
}

perf_df_m <- bind_rows(perf_list) %>%
  separate(
    model,
    into = c("model", "method"),
    sep = "_(?=[^_]+$)"
  ) %>% 
  arrange(league, month_year, desc(match_result))

# Plot
df_plot <- perf_df_m %>%
  mutate(month_year = as.Date(paste0(month_year, "-01"))) %>%
  pivot_longer(
    cols = c(logloss, brier, match_result),
    names_to = "metric",
    values_to = "value"
  )  %>%
  filter(
    model %in% c(
      "dc", "bp",
      "dummies", "dummies1", "dummies2",
      "xg", "xg1", "xg2", "cross", "cross2", "no_var", "ma_score"
    )
  ) %>%
  filter(metric == "match_result") %>%
  mutate(
    method2 = ifelse(is.na(method), "benchmark", method),
    is_benchmark = method2 == "benchmark"
  )

library(scales)

df_plot2 <- df_plot %>% 
  filter(
    (model == "dc") | (model == "bp") | 
      ((model == "dummies2") & (method == "cop")) |
      ((model == "dummies") & (method == "ind")) |
      # ((model == "cross") & (method == "ind")) |
      # ((model == "ma_score") & (method == "cop")) |
      ((model == "cross") & (method == "cop"))) %>%
  mutate(
    method2 = case_when(
      method2 == "benchmark" ~ "Benchmark",
      method == "cop" ~ "Copula",
      T ~ "Independence"
    ),
    model = case_when(
      model == "dummies" ~ "T2D + O2D",
      model == "dummies2" ~ "O2D",
      model == "cross" ~ "Crs + CrsC",
      model == "dc" ~ "Dixon-Coles",
      model == "bp" ~ "Biv. Poisson"
    ),
  )

ggplot(df_plot2, aes(
  x = month_year,
  y = value,
  color = model,
  linetype = method2,
  linewidth = is_benchmark,
  group = interaction(model, method2)
)) +
  geom_line() +
  facet_grid(league ~ metric,
             labeller = labeller(
               league = c(
                 ENG = "Premier League",
                 ESP = "La Liga",
                 ITA = "Serie A"
               ),
               metric = c(
                 logloss = "LogLoss",
                 brier = "Brier Score",
                 match_result = "MRS (No Relegation)"
               )
             )) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%Y-%m",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_linetype_manual(
    values = c(
      Benchmark = "solid",
      Independence = "dotted",
      Copula = "dotdash"
    ),
    name = "Method"
  ) +
  scale_linewidth_manual(
    values = c(`TRUE` = 1.3, `FALSE` = 0.8),
    guide = "none"
  ) +
  scale_color_discrete(name = "Model") +
  labs(
    x = "",
    y = ""
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    
    # legenda menor
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
  )

library(plotly)

p <- ggplot(df_plot, aes(
  x = month_year,
  y = value,
  color = model,
  group = interaction(model, method),
  text = paste(
    "Model:", model,
    "<br>Method:", method,
    "<br>League:", league,
    "<br>Metric:", metric,
    "<br>Value:", round(value,4)
  )
)) +
  geom_line() +
  facet_grid(league ~ metric, scales = "free_y") +
  theme_classic(base_family = "Times New Roman")

ggplotly(p, tooltip = "text")

#_______   CALC METRICS - LEAGUES - MONTH TOTAL  ______#

leagues <- c("ENG", "ESP", "ITA")
df_performance_filtered <- df_performance
table(df_performance_filtered$League, df_performance_filtered$month_year)

ms <- unique(df_performance_filtered$month_year)
perf_list <- vector("list", length(models))
for(lg in leagues){
  df_lg <- df_performance_filtered %>% filter(League == lg)
  for (month in ms){
    df_lg_m <- df_lg %>% filter(month_year == month)
    for(i in seq_along(models)){
      
      m <- models[i]
      res <- tryCatch({
        probs <- get_probs(df_lg_m, m)
        row_sums <- rowSums(probs)
        row_sums[row_sums == 0] <- 1
        probs <- probs / row_sums
        ok <- complete.cases(probs, df_lg_m$result)
        
        probs <- probs[ok, ]
        obs   <- df_lg_m$result[ok]
        obs_factor   <- factor(obs, levels = c("H","D","A"))
        pred <- c("H","D","A")[max.col(probs)]
        colnames(probs) = c("H","D","A")
        
        data.frame(
          month_year = month,
          league = lg,
          model = m,
          
          #__________ Log Loss _____________#
          logloss = Logloss(probs, obs_factor),
          
          #__________ Brier Score _____________#
          brier = mbrier(obs_factor, probs),
          
          #__________ Match Result Score _____________#
          match_result = mean(pred == obs)
        )
      }, error = function(e){
        NULL
      })
      if(!is.null(res)){
        perf_list[[idx]] <- res
        idx <- idx + 1
      }
    } 
  }
}

perf_df_m <- bind_rows(perf_list) %>%
  separate(
    model,
    into = c("model", "method"),
    sep = "_(?=[^_]+$)"
  ) %>% 
  arrange(league, month_year, desc(match_result))

# Plot
df_plot <- perf_df_m %>%
  mutate(month_year = as.Date(paste0(month_year, "-01"))) %>%
  pivot_longer(
    cols = c(logloss, brier, match_result),
    names_to = "metric",
    values_to = "value"
  )  %>%
  filter(
    model %in% c(
      "dc", "bp",
      "dummies", "dummies1", "dummies2",
      "xg", "xg1", "xg2", "cross", "cross2", "no_var", "ma_score"
    )
  ) %>%
  filter(metric == "match_result") %>%
  mutate(
    method2 = ifelse(is.na(method), "benchmark", method),
    is_benchmark = method2 == "benchmark"
  )

library(scales)

df_plot2 <- df_plot %>% 
  filter(
    (model == "dc") | (model == "bp") | 
      ((model == "dummies2") & (method == "cop")) |
      ((model == "dummies") & (method == "ind")) |
      # ((model == "cross") & (method == "ind")) |
      # ((model == "ma_score") & (method == "cop")) |
      ((model == "cross") & (method == "cop"))) %>%
  mutate(
    method2 = case_when(
      method2 == "benchmark" ~ "Benchmark",
      method == "cop" ~ "Copula",
      T ~ "Independence"
    ),
    model = case_when(
      model == "dummies" ~ "T2D + O2D",
      model == "dummies2" ~ "O2D",
      model == "cross" ~ "Crs + CrsC",
      model == "dc" ~ "Dixon-Coles",
      model == "bp" ~ "Biv. Poisson"
    ),
  )

ggplot(df_plot2, aes(
  x = month_year,
  y = value,
  color = model,
  linetype = method2,
  linewidth = is_benchmark,
  group = interaction(model, method2)
)) +
  geom_line() +
  facet_grid(league ~ metric,
             labeller = labeller(
               league = c(
                 ENG = "Premier League",
                 ESP = "La Liga",
                 ITA = "Serie A"
               ),
               metric = c(
                 logloss = "LogLoss",
                 brier = "Brier Score",
                 match_result = "MRS (No Relegation)"
               )
             )) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%Y-%m",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_linetype_manual(
    values = c(
      Benchmark = "solid",
      Independence = "dotted",
      Copula = "dotdash"
    ),
    name = "Method"
  ) +
  scale_linewidth_manual(
    values = c(`TRUE` = 1.3, `FALSE` = 0.8),
    guide = "none"
  ) +
  scale_color_discrete(name = "Model") +
  labs(
    x = "",
    y = ""
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    
    # legenda menor
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
  )

library(plotly)

p <- ggplot(df_plot, aes(
  x = month_year,
  y = value,
  color = model,
  group = interaction(model, method),
  text = paste(
    "Model:", model,
    "<br>Method:", method,
    "<br>League:", league,
    "<br>Metric:", metric,
    "<br>Value:", round(value,4)
  )
)) +
  geom_line() +
  facet_grid(league ~ metric, scales = "free_y") +
  theme_classic(base_family = "Times New Roman")

ggplotly(p, tooltip = "text")

# ----------------------------------------------------------------------------------------------

# Novo

#_______   CALC METRICS - LEAGUES  ______#

df_performance_cutted <- df_performance %>%
  filter(month_year != "2024-08")%>%
  filter(month_year != "2024-09")

leagues <- c("ENG", "ESP", "ITA")
perf_list <- vector("list", length(models))
idx <- 1

for(lg in leagues){
  
  df_lg <- df_performance_cutted %>% filter(League == lg)
  
  for(i in seq_along(models)){
    
    m <- models[i]
    
    res <- tryCatch({
      
      probs <- get_probs(df_lg, m)
      row_sums <- rowSums(probs)
      row_sums[row_sums == 0] <- 1
      probs <- probs / row_sums
      
      ok <- complete.cases(probs, df_lg$result, 
                           df_lg$result_dc, df_lg$result_bp)
      
      probs <- probs[ok, ]
      obs   <- df_lg$result[ok]
      obs_factor <- factor(obs, levels = c("H","D","A"))
      
      pred <- c("H","D","A")[max.col(probs)]
      colnames(probs) = c("H","D","A")
      
      # -------------------------
      # McNemar vs DC
      # -------------------------
      
      acc_model <- pred == obs
      acc_dc    <- df_lg$result_dc[ok] == obs
      acc_bp    <- df_lg$result_bp[ok] == obs
      
      p_dc <- NA
      p_bp <- NA
      
      if(!str_detect(m, "dc")){
        tab_dc <- table(acc_model, acc_dc)
        if(all(dim(tab_dc) == c(2,2))){
          p_dc <- mcnemar.test(tab_dc)$p.value
        }
      }
      
      if(!str_detect(m, "bp")){
        tab_bp <- table(acc_model, acc_bp)
        if(all(dim(tab_bp) == c(2,2))){
          p_bp <- mcnemar.test(tab_bp)$p.value
        }
      }
      
      data.frame(
        league = lg,
        model = m,
        
        # metrics
        logloss = Logloss(probs, obs_factor),
        brier = mbrier(obs_factor, probs),
        match_result = mean(pred == obs),
        
        # McNemar
        p_dc = p_dc,
        p_bp = p_bp
      )
      
    }, error = function(e){
      NULL
    })
    
    if(!is.null(res)){
      perf_list[[idx]] <- res
      idx <- idx + 1
    }
  }
}

perf_df2 <- bind_rows(perf_list)

perf_df2 <- perf_df2 %>%
  separate(
    model,
    into = c("model", "method"),
    sep = "_(?=[^_]+$)"
  ) %>% 
  arrange(league, desc(match_result)) 

#_______   CALC METRICS - LEAGUES - GROUPS  ______#
leagues <- c("ENG", "ESP", "ITA")
clusters <- unique(df_performance_cutted$cluster)

perf_list <- list()
idx <- 1

for(lg in leagues){
  
  df_lg <- df_performance_cutted %>% filter(League == lg)
  
  for (clus in clusters){
    
    df_lg_clus <- df_lg %>% filter(cluster == clus)
    
    for(i in seq_along(models)){
      
      m <- models[i]
      
      res <- tryCatch({
        
        probs <- get_probs(df_lg_clus, m)
        row_sums <- rowSums(probs)
        row_sums[row_sums == 0] <- 1
        probs <- probs / row_sums
        
        ok <- complete.cases(
          probs,
          df_lg_clus$result,
          df_lg_clus$result_dc,
          df_lg_clus$result_bp
        )
        
        probs <- probs[ok, ]
        obs   <- df_lg_clus$result[ok]
        obs_factor <- factor(obs, levels = c("H","D","A"))
        
        pred <- c("H","D","A")[max.col(probs)]
        colnames(probs) = c("H","D","A")
        
        # -----------------------
        # McNemar preparation
        # -----------------------
        
        acc_model <- pred == obs
        acc_dc <- df_lg_clus$result_dc[ok] == obs
        acc_bp <- df_lg_clus$result_bp[ok] == obs
        
        p_dc <- NA
        p_bp <- NA
        
        # test vs DC
        if(!str_detect(m, "dc")){
          tab_dc <- table(acc_model, acc_dc)
          if(all(dim(tab_dc) == c(2,2))){
            p_dc <- mcnemar.test(tab_dc)$p.value
          }
        }
        
        # test vs BP
        if(!str_detect(m, "bp")){
          tab_bp <- table(acc_model, acc_bp)
          if(all(dim(tab_bp) == c(2,2))){
            p_bp <- mcnemar.test(tab_bp)$p.value
          }
        }
        
        data.frame(
          cluster = clus,
          league = lg,
          model = m,
          
          logloss = Logloss(probs, obs_factor),
          brier   = mbrier(obs_factor, probs),
          match_result = mean(acc_model),
          
          p_dc = p_dc,
          p_bp = p_bp
        )
        
      }, error = function(e){
        NULL
      })
      
      if(!is.null(res)){
        perf_list[[idx]] <- res
        idx <- idx + 1
      }
    }
  }
}

perf_df_cluster2 <- bind_rows(perf_list)

perf_df_cluster2 <- perf_df_cluster %>%
  separate(
    model,
    into = c("model", "method"),
    sep = "_(?=[^_]+$)"
  ) %>% 
  arrange(league, cluster, desc(match_result))




###############################
#_______   PARAMETERS  _______#
###############################

# p e q

plot_df <- aux_info %>%
  filter(!str_detect(model, "mix|NEWCOP")) %>%
  count(link, p, q, name = "n") %>%
  filter(!is.na(link)) %>%
  group_by(link) %>%
  mutate(
    perc = n / sum(n),
    pair = paste0("(", p, ", ", q, ")")
  ) %>%
  ungroup() %>%
  arrange(p, q) %>%                # p = 0 - means
  mutate(pair = factor(pair, levels = unique(pair)))

# posição do texto
plot_df <- plot_df %>%
  mutate(
    label = paste0(n, " (", round(perc*100,1), "%)"),
    txt_out = n < max(n) * 0.35,
    x_text = ifelse(txt_out, n + max(n)*0.02, n/2),
    hjust = ifelse(txt_out, 0, 0.5)
  )

# distribuição global dos links
link_lab <- aux_info %>%
  filter(!is.na(link)) %>%
  count(link) %>%
  mutate(
    perc = round(100 * n / sum(n), 1),
    lab = paste0(link, ": ", perc, "%")
  ) %>%
  select(link, lab)

plot_df <- plot_df %>%
  left_join(link_lab, by = "link")

ggplot(plot_df, aes(x = n, y = pair)) +
  geom_col(
    aes(fill = n),
    color = "black",
    width = 0.8
  ) +
  geom_text(
    aes(
      x = x_text,
      label = label,
      hjust = hjust
    ),
    family = "Times New Roman"
  ) +
  facet_wrap(~link, scales = "free_y") +
  geom_label(
    data = ~ distinct(.x, link, lab),
    aes(
      x = Inf,
      y = -Inf,
      label = lab
    ),
    hjust = 1.1,
    vjust = -0.5,
    inherit.aes = FALSE,
    family = "Times New Roman"
  ) +
  scale_fill_gradient(
    low  = "grey95",
    high = "grey20",
    guide = "none"
  ) +
  labs(
    x = "Number of Models",
    y = "(p, q)",
    title = "Distribution of Orders by Link Function"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "italic", hjust = 0.5),
    plot.margin = margin(10, 60, 10, 10)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))


# Best Model
round(
  prop.table(
    table(filter(aux_info, model == "dummies")$link)
  ) * 100, 
  2
)

###############################
#_______     COPULA    _______#
###############################

copula_info <- aux_copulas %>%
  filter(!str_detect(model, "mix|NEWCOP"))

round(prop.table(table(copula_info$copula)) * 100, 2)

# Best Model
round(
  prop.table(
    table(filter(aux_copulas, model == "no_var")$copula)
  ) * 100, 
  2
)



# -------------------------------------------------------------------------------- #


# Example Arsenal e Chelsea
i <- which(matches_all$Home == 'Arsenal' & matches_all$Away == 'Chelsea')

x_models <- qs_read("models/ENG_no_var.qs2")
x <- x_models[[i]]
models <- x$models

date   <- matches_all$Date[i]
home   <- matches_all$Home[i]
away   <- matches_all$Away[i]

# g      <- df_parx[(df_parx$Match_Date == date) & (df_parx$Home_Team == home) & (df_parx$Away_Team == away), ]$game_id[1]
# 
# # -----------------------
# # Rows (fast)
# # -----------------------
# 
# game_rows <- df_parx[df_parx$game_id == g, ]
# row_H <- game_rows[game_rows$Home_Away == "Home", ]
# row_A <- game_rows[game_rows$Home_Away == "Away", ]

# -----------------------
# Prediction
# -----------------------

mod1 = models[[paste0(home,"_H")]]
x_col = colnames(mod1$xreg)
lambda_H <- predict_parx(
  mod1,
  x_col
)

mod2 = models[[paste0(away,"_A")]]
x_col = colnames(mod2$xreg)
lambda_A <- predict_parx(
  mod2,
  x_col
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
prob_ind_df


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

