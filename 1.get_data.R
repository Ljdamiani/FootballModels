

########################################################################

## Dados do WorldFootball  ##

#######################################################################
# devtools::install_github("JaseZiv/worldfootballR") # nolint

library(worldfootballR)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)
library(rvest)
library(progress)

# Funções (worldoffootball modificada)
source("dados/aux_get_data.R")
seasons = 2020:2025
campeonatos = c("ENG", "ESP", "ITA", "FRA", "GER", "BRA")
df <- expand.grid(season = seasons, campeonato = campeonatos)
stats <- c("summary", "passing", "passing_types", "defense" , "possession", "misc", "keeper")


# TODO: OLHAR OS DE BAIXO
# - ENG 2023

# Loop
for (i in 17:nrow(df)){
  
  # Infos
  season <- df$season[i]
  comp <- df$campeonato[i]
  cat("\n\n")
  cat(paste0(i, " de ", nrow(df), " - ", comp, " ", season))
  cat("\n\n")
  urls <- fb_match_urls(comp, gender = "M", season_end_year = as.character(season), tier = "1st")
  pb <- progress::progress_bar$new(total = length(urls))
  
  matches <- data.frame()
  erros = c()
  for (url in urls){
    
    # Temporadas
    match <- get_data(match_url=url)
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





