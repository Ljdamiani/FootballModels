

########################################################################

## Dados do WorldFootball  ##

#######################################################################
# devtools::install_github("JaseZiv/worldfootballR")

library(worldfootballR)
library(dplyr)
library(lubridate)

# Temporadas da Premier League - Desde 2010
seasons = c()
last_season = year(today())

for (i in 2010:last_season){
  
  # Temporadas
  season <- fb_match_results("ENG", gender = "M", season_end_year = as.character(i))
  
  # Mais 
  # urls <- fb_match_urls("ENG", gender = "M", season_end_year = as.character(i), tier = "1st")
  # season <- fb_advanced_match_stats(match_url=urls,  stat_type="summary", team_or_player="team")
  
  # Limpeza dos dados
  cols = c("Wk", "Date", "Home", "HomeGoals", "Away", "AwayGoals")
  if (i >= 2018){cols = c("Wk", "Date", "Home", "HomeGoals", "Away", "AwayGoals")}
  season <- season[, cols]
  season <- season %>%
    mutate(Wk = as.numeric(Wk)) %>%
    arrange(Date)
  
  # Guarda as temporadas
  seasons[[paste0('PL', as.character(i))]] = season
  
}

# Uni todas as tabelas num unico data frame
seasons_df <- rbind(seasons$PL2010,
                    seasons$PL2011,
                    seasons$PL2012,
                    seasons$PL2013,
                    seasons$PL2014,
                    seasons$PL2015,
                    seasons$PL2016,
                    seasons$PL2017,
                    seasons$PL2018, # 17-18 primeira temp com xG no Fbref
                    seasons$PL2019, # 18-19
                    seasons$PL2020, # 19-20
                    seasons$PL2021, # 20-21
                    seasons$PL2022, # 21-22
                    seasons$PL2023, # 22-23
                    seasons$PL2024) # 23-24

# Salva os dados
rm(list=setdiff(ls(),c('seasons', 'seasons_df')))
save.image("dados/seasons.RData")



