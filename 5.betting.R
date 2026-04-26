

####################################################

# _______________ # BETTING  # ________________ #

# ___________ Author: Leonardo Damiani ___________ # 

####################################################

# Packages
library(dplyr)
library(readr)
library(lubridate)

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
  filter(!((Home == "Udinese") & (Away == "Como"))) %>%
  mutate(
    cluster = case_when(
      (cluster_home == "No Relegation") &
        (cluster_away == "No Relegation") ~ "No Relegation Teams",
      T ~ "At least 1 Relegation"
    )
  )

# Teams Names
# setdiff(unique(odds$Home), unique(matches_all$Home))
# setdiff(unique(matches_all$Home), unique(odds$Home))
rename_teams <- c(
  "Man United"    = "Manchester United",
  "Man City"      = "Manchester City",
  "Newcastle"     = "Newcastle United",
  "Leicester"     = "Leicester City",
  "West Ham"      = "West Ham United",
  "Brighton"      = "Brighton & Hove Albion",
  "Tottenham"     = "Tottenham Hotspur",
  "Wolves"        = "Wolverhampton Wanderers",
  "Ipswich"       = "Ipswich Town",
  "Nott'm Forest" = "Nottingham Forest",
  
  "Ath Bilbao"    = "Athletic Club",
  "Ath Madrid"    = "Atlético Madrid",
  "Sociedad"      = "Real Sociedad",
  "Betis"         = "Real Betis",
  "Celta"         = "Celta Vigo",
  "Vallecano"     = "Rayo Vallecano",
  "Espanol"       = "Espanyol",
  "Leganes"       = "Leganés",
  "Alaves"        = "Alavés",
  
  "Inter"         = "Internazionale",
  "Verona"        = "Hellas Verona"
)

################################
#_______     ODDS    _______#
################################

# https://www.football-data.co.uk/italym.php

# ODDS
odds_ENG <- read_csv("dados\\odds\\ENG2425.csv") %>% mutate(League = "ENG")
odds_ESP <- read_csv("dados\\odds\\ESP2425.csv") %>% mutate(League = "ESP")
odds_ITA <- read_csv("dados\\odds\\ITA2425.csv") %>% mutate(League = "ITA")

odds <- bind_rows(odds_ENG, odds_ESP, odds_ITA) %>%
  select(
    League, Date,
    Home = HomeTeam,
    Away = AwayTeam,
    
    # 1XBet
    any_of(c("1XBH","1XBD","1XBA")),
    
    # Bet365
    any_of(c("B365H","B365D","B365A")),
    
    # Betfair
    any_of(c("BFH","BFD","BFA")),
    
    # Betfred
    any_of(c("BFDH","BFDD","BFDA")),
    
    # BetMGM
    any_of(c("BMGMH","BMGMD","BMGMA")),
    
    # BetVictor
    any_of(c("BVH","BVD","BVA")),
    
    # Blue Square
    any_of(c("BSH","BSD","BSA")),
    
    # Bet&Win
    any_of(c("BWH","BWD","BWA")),
    
    # Coral
    any_of(c("CLH","CLD","CLA")),
    
    # Gamebookers
    any_of(c("GBH","GBD","GBA")),
    
    # Interwetten
    any_of(c("IWH","IWD","IWA")),
    
    # Ladbrokes
    any_of(c("LBH","LBD","LBA")),
    
    # Pinnacle
    any_of(c("PSH","PSD","PSA","PH","PD","PA")),
    
    # Sporting Odds
    any_of(c("SOH","SOD","SOA")),
    
    # Sportingbet
    any_of(c("SBH","SBD","SBA")),
    
    # Stan James
    any_of(c("SJH","SJD","SJA")),
    
    # Stanleybet
    any_of(c("SYH","SYD","SYA")),
    
    # VC Bet
    any_of(c("VCH","VCD","VCA")),
    
    # William Hill
    any_of(c("WHH","WHD","WHA"))
  ) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Home = recode(Home, !!!rename_teams),
    Away = recode(Away, !!!rename_teams)
  )

odds_final <- odds %>%
  rowwise() %>%
  mutate(
    OddH = median(c_across(ends_with("H")), na.rm = TRUE),
    OddD = median(c_across(ends_with("D")), na.rm = TRUE),
    OddA = median(c_across(ends_with("A")), na.rm = TRUE),
    
    ProbH = 1 / OddH,
    ProbD = 1 / OddD,
    ProbA = 1 / OddA,
    Sum = ProbH + ProbD + ProbA
  ) %>%
  ungroup() %>%
  select(League, Date, Home, Away, OddH, OddD, OddA, ProbH, ProbD, ProbA, Sum) %>%
  left_join(select(matches_all, League, Date, Home, Away, cluster), 
            by = c("League", "Date", "Home", "Away")) %>%
  filter(!is.na(cluster))


##################################
#_______   PROBS  _______#
##################################

cols = c("Date","Home","Away")
df_probs <- matches_all

# Dixon-Coles
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

# Models Choosed
files_list <- list.files(
  "models",
  pattern = "\\.qs2$",
  full.names = TRUE
)
files_list <- files_list[str_detect(files_list, "dummies2_NEWCOP")]
# files_list <- files_list[!str_detect(files_list, "matches|_dc|_bivpois")]

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


##################################
#_______   BETS  _______#
##################################

bet <- merge(probs_mix_cop, odds, by.x = c('M', 'V'), by.y = c('HomeTeam', 'AwayTeam'))
bet <- bet %>%
  mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
  arrange(Date)

bet$Maior_Prob = NA
bet$Predito = NA
bet$aposta = NA
bet$odd = NA
threshold = 0

for (i in 1:nrow(bet)){
  
  aux = bet[i, ]
  probs = c('pVM' = aux$pVM, 'pE' = aux$pE, 'pVV' = aux$pVV)
  
  # Passo 1 - Resultado de Maior Prob
  prob = max(probs)
  bet$Maior_Prob[i] = prob
  
  # Passo 2 - Verifica se Vale a pena apostar
  nome = names(which(probs == prob))
  bet$Predito[i] = nome
  
  aposta = 0
  if (nome == 'pVM'){
    
    odd = aux$OddH
    aux2 = prob/aux$ProbH - 1
    if (aux2 > threshold){
      aposta = 1
    }
    
  } else if (nome == 'pE'){
    
    odd = aux$OddE
    aux2 = prob/aux$ProbE - 1
    if (aux2 > threshold){
      aposta = 1
    }
    
  } else{
    
    odd = aux$OddA
    aux2 = prob/aux$ProbA - 1
    if (aux2 > threshold){
      aposta = 1
    }
    
  }
  
  bet$aposta[i] = aposta
  bet$odd[i] = odd
  
}

bet$resultado = ifelse(bet$GM > bet$GV, 'pVM',
                ifelse(bet$GM == bet$GV, 'pE', 'pVV'))

table(bet$aposta)

# Somente os que devem ser apostados
bet1 = bet[bet$aposta == 1, ]
# sum(diag(table(bet1$Predito, bet1$resultado))) # acertos

nrow(bet1) # Número de apostas

qtd_apostada = 2 # Libra
qtd_apostada * nrow(bet1)

aux_bet = bet1 %>%
  mutate(odd_posicao = odd * qtd_apostada) %>%
  filter(Predito == resultado)

# Profit
sum(aux_bet$odd_posicao) - qtd_apostada * nrow(bet1)

# Return
(sum(aux_bet$odd_posicao) - (qtd_apostada * nrow(bet1)))/(qtd_apostada * nrow(bet1)) * 100


