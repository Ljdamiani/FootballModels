

########################################################################

## FBref Data - WorldFootball  ##

#######################################################################
# devtools::install_github("JaseZiv/worldfootballR") # nolint

library(worldfootballR)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(purrr)
library(stringr)
library(rvest)
library(progress)

# Funções (worldoffootball modificada)
source("dados/aux_get_data.R")
seasons = c(2021) # 2020:2025
campeonatos = c(
  # "ENG", 
  # "ESP", 
  "ITA"
)
df <- expand.grid(season = seasons, campeonato = campeonatos)
stats <- c("summary", "passing", "passing_types", "defense" , "possession", "misc", "keeper")

# Loop
for (i in 1:nrow(df)){
  
  # Infos
  # i = 1
  season <- df$season[i]
  comp <- as.character(df$campeonato[i])
  cat("\n\n")
  cat(paste0(i, " de ", nrow(df), " - ", comp, " ", season))
  cat("\n\n")
  urls <- match_urls(comp, season_end_year = as.character(season))
  pb <- progress::progress_bar$new(total = length(urls))
  
  matches <- data.frame()
  erros = c() 
  for (i_url in 361:length(urls)){
    
    # Temporadas
    url = urls[i_url]
    match <- get_data(match_url = url)
    if (is.character(match)){
      erros <- append(erros, match)
    } else{
      matches <- bind_rows(matches, match)
    }
  }
  
  # Guarda as temporadas
  if (nrow(matches)>0){df_final <- matches %>% arrange(Match_Date)}
  save(matches, file = paste0("dados/matches_", comp, "_", season, ".RData"))
  
}

