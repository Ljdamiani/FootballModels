
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

# models_ENG_xg2 <- qs_read(paste0("models//ENG_xg2.qs2"))
# teste <- c(models_ENG_xg2[1:39], models_ENG_xg[40:length(models_ENG_xg)])
# qs_save(teste, paste0("models//ENG_xg.qs2"))


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

#_______   CALC METRICS  ______#

leagues <- c("ENG", "ESP", "ITA")
perf_list <- vector("list", length(models))
for(lg in leagues){
  df_lg <- df_probs %>% filter(League == lg)
  for(i in seq_along(models)){
    
    m <- models[i]
    res <- tryCatch({
      probs <- get_probs(df_lg, m)
      row_sums <- rowSums(probs)
      row_sums[row_sums == 0] <- 1
      probs <- probs / row_sums
      ok <- complete.cases(probs, df_lg$result)
      
      probs <- probs[ok, ]
      obs   <- df_lg$result[ok]
      obs_factor   <- factor(obs, levels = c("H","D","A"))
      pred <- c("H","D","A")[max.col(probs)]
      colnames(probs) = c("H","D","A")
      
      data.frame(
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

perf_df <- bind_rows(perf_list)
perf_df <- perf_df %>%
  separate(
    model,
    into = c("model", "method"),
    sep = "_(?=[^_]+$)"
  )

perf_df %>%
  arrange(league, match_result)


###############################
#_______   PARAMETERS  _______#
###############################

# p e q

plot_df <- aux_info %>%
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
    table(filter(aux_info, model == "no_var")$link)
  ) * 100, 
  2
)

###############################
#_______     COPULA    _______#
###############################

round(prop.table(table(aux_copulas$copula)) * 100, 2)

# Best Model
round(
  prop.table(
    table(filter(aux_copulas, model == "no_var")$copula)
  ) * 100, 
  2
)

