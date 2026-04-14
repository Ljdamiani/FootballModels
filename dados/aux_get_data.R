
# --------------------------------------------------------------------------------------------------------------------------------------------------------------

#       FUNCTIONS

# --------------------------------------------------------------------------------------------------------------------------------------------------------------

# Function - Base (OLD)
# fb_match_urls()
# fb_advanced_match_stats()
# matches <- fb_match_results("ENG", gender = "M", season_end_year = as.character(i))

# Outra func
get_match_report_page <- function (match_page) 
{
  Game_URL <- match_page %>% rvest::html_nodes(".langs") %>% 
    rvest::html_node(".en") %>% rvest::html_attr("href")
  each_game_page <- tryCatch(match_page, error = function(e) NA)
  if (!is.na(each_game_page)) {
    game <- each_game_page %>% rvest::html_nodes("h1") %>% 
      rvest::html_text()
    tryCatch({
      League <- each_game_page %>% rvest::html_nodes("h1+ div a:nth-child(1)") %>% 
        rvest::html_text()
    }, error = function(e) {
      League <- NA
    })
    tryCatch({
      Match_Date <- each_game_page %>% rvest::html_nodes(".venuetime") %>% 
        rvest::html_attr("data-venue-date")
    }, error = function(e) {
      Match_Date <- NA
    })
    tryCatch({
      Matchweek <- each_game_page %>% rvest::html_nodes("h1+ div") %>% 
        rvest::html_text()
    }, error = function(e) {
      Matchweek <- NA
    })
    tryCatch({
      Home_Team <- each_game_page %>% rvest::html_nodes("div+ strong a") %>% 
        rvest::html_text() %>% .[1]
    }, error = function(e) {
      Home_Team <- NA
    })
    tryCatch({
      Home_Formation <- each_game_page %>% rvest::html_nodes(".lineup#a") %>% 
        rvest::html_nodes("th") %>% rvest::html_text() %>% 
        .[1] %>% gsub(".*\\(", "", .) %>% gsub("\\)", 
                                               "", .)
    }, error = function(e) {
      Home_Formation <- NA
    })
    tryCatch({
      Home_Score <- each_game_page %>% rvest::html_nodes(".scores") %>% 
        rvest::html_nodes(".score") %>% rvest::html_text() %>% 
        .[1]
    }, error = function(e) {
      Home_Score <- NA
    })
    tryCatch({
      Home_xG <- each_game_page %>% rvest::html_nodes(".scores") %>% 
        rvest::html_nodes(".score_xg") %>% rvest::html_text() %>% 
        .[1]
    }, error = function(e) {
      Home_xG <- NA
    })
    tryCatch({
      Home_Goals <- each_game_page %>% rvest::html_nodes("#a") %>% 
        .[[1]] %>% rvest::html_text() %>% stringr::str_squish()
    }, error = function(e) {
      Home_Goals <- NA
    })
    tryCatch({
      Home_Yellow_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% 
        .[1] %>% rvest::html_nodes("span.yellow_card, span.yellow_red_card") %>% 
        length()
    }, error = function(e) {
      Home_Yellow_Cards <- 0
    })
    tryCatch({
      Home_Red_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% 
        .[1] %>% rvest::html_nodes("span.red_card, span.yellow_red_card") %>% 
        length()
    }, error = function(e) {
      Home_Red_Cards <- 0
    })
    tryCatch({
      Away_Team <- each_game_page %>% rvest::html_nodes("div+ strong a") %>% 
        rvest::html_text() %>% .[2]
    }, error = function(e) {
      Away_Team <- NA
    })
    tryCatch({
      Away_Formation <- each_game_page %>% rvest::html_nodes(".lineup#b") %>% 
        rvest::html_nodes("th") %>% rvest::html_text() %>% 
        .[1] %>% gsub(".*\\(", "", .) %>% gsub("\\)", 
                                               "", .)
    }, error = function(e) {
      Away_Formation <- NA
    })
    tryCatch({
      Away_Score <- each_game_page %>% rvest::html_nodes(".scores") %>% 
        rvest::html_nodes(".score") %>% rvest::html_text() %>% 
        .[2]
    }, error = function(e) {
      Away_Score <- NA
    })
    tryCatch({
      Away_xG <- each_game_page %>% rvest::html_nodes(".scores") %>% 
        rvest::html_nodes(".score_xg") %>% rvest::html_text() %>% 
        .[2]
    }, error = function(e) {
      Away_xG <- NA
    })
    tryCatch({
      Away_Goals <- each_game_page %>% rvest::html_nodes("#b") %>% 
        .[[1]] %>% rvest::html_text() %>% stringr::str_squish()
    }, error = function(e) {
      Away_Goals <- NA
    })
    tryCatch({
      Away_Yellow_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% 
        .[2] %>% rvest::html_nodes("span.yellow_card, span.yellow_red_card") %>% 
        length()
    }, error = function(e) {
      Away_Yellow_Cards <- 0
    })
    tryCatch({
      Away_Red_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% 
        .[2] %>% rvest::html_nodes("span.red_card, span.yellow_red_card") %>% 
        length()
    }, error = function(e) {
      Away_Red_Cards <- 0
    })
    suppressWarnings(each_game <- cbind(League, Match_Date, 
                                        Matchweek, Home_Team, Home_Formation, Home_Score, 
                                        Home_xG, Home_Goals, Home_Yellow_Cards, Home_Red_Cards, 
                                        Away_Team, Away_Formation, Away_Score, Away_xG, 
                                        Away_Goals, Away_Yellow_Cards, Away_Red_Cards, Game_URL) %>% 
                       dplyr::as_tibble() %>% dplyr::mutate(Home_Score = as.numeric(.data[["Home_Score"]]), 
                                                            Home_xG = as.numeric(.data[["Home_xG"]]), Away_Score = as.numeric(.data[["Away_Score"]]), 
                                                            Away_xG = as.numeric(.data[["Away_xG"]])))
  }
  else {
    print(glue::glue("{Game_URL} is not available"))
    each_game <- data.frame()
  }
  return(each_game)
}

add_player_href <- function (df, parent_element, player_xpath) 
{
  player_elements <- xml2::xml_find_all(parent_element, player_xpath)
  player_hrefs <- xml2::xml_attr(player_elements, "href")
  n_diff <- nrow(df) - length(player_hrefs)
  res <- dplyr::mutate(df, Player_Href = c(player_hrefs, rep(NA_character_, 
                                                             n_diff)), .after = "Player")
  return(res)
}

extract_team_players <- function (match_page, xml_elements, team_idx, home_away) 
{
  team <- match_page %>% rvest::html_nodes("div+ strong a") %>% 
    rvest::html_text() %>% purrr::pluck(team_idx)
  team_stat <- xml_elements[team_idx] %>% rvest::html_table() %>% 
    data.frame() %>% clean_match_advanced_stats_data()
  team_stat <- add_player_href(team_stat, parent_element = xml_elements[team_idx], 
                                player_xpath = ".//tbody/tr/th/a")
  res <- cbind(list(Team = team, Home_Away = home_away), team_stat)
  return(res)
}

clean_match_advanced_stats_data <- function (df_in) 
{
  var_names <- df_in[1, ] %>% as.character()
  new_names <- paste(var_names, names(df_in), sep = "_")
  new_names <- new_names %>% gsub("\\..[0-9]", "", .) %>% 
    gsub("\\.[0-9]", "", .) %>% gsub("\\.", "_", .) %>% 
    gsub("_Var", "", .) %>% gsub("#", "Player_Num", .) %>% 
    gsub("%", "_percent", .) %>% gsub("_Performance", "", 
                                      .) %>% gsub("_Penalty", "", .) %>% gsub("1/3", "Final_Third", 
                                                                              .) %>% gsub("\\+/-", "Plus_Minus", .) %>% gsub("/", 
                                                                                                                             "_per_", .) %>% gsub("-", "_minus_", .) %>% gsub("90s", 
                                                                                                                                                                              "Mins_Per_90", .) %>% gsub("__", "_", .)
  names(df_in) <- new_names
  df_in <- df_in[-1, ]
  if (any(grepl("Nation", colnames(df_in)))) {
    df_in$Nation <- gsub(".*? ", "", df_in$Nation)
  }
  non_num_vars <- c("Player", "Nation", "Pos", "Age")
  cols_to_transform <- names(df_in)[!names(df_in) %in% non_num_vars]
  df_in <- df_in %>% dplyr::mutate_at(.vars = cols_to_transform, 
                                      .funs = function(x) {
                                        gsub(",", "", x)
                                      }) %>% dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {
                                        gsub("+", "", x)
                                      }) %>% dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric)
  return(df_in)
}


# Função auxiliar para carregar a página com header anônimo
load_page <- function(url, timeout_sec = 300) {
  library(httr)
  library(rvest)
  
  # Lista de User-Agents para simular diferentes navegadores
  user_agents <- c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/119.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:124.0) Gecko/20100101 Firefox/124.0"
  )
  
  start_time <- Sys.time()
  attempt <- 1
  
  repeat {
    if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > timeout_sec) {
      warning("Tempo máximo de tentativas excedido.")
      return(NA)
    }
    
    current_ua <- user_agents[(attempt - 1) %% length(user_agents) + 1]
    
    response <- httr::GET(
      url,
      httr::add_headers(`User-Agent` = current_ua)
    )
    
    status <- httr::status_code(response)
    
    if (status == 200) {
      # message("Página carregada com sucesso.")
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      return(rvest::read_html(content))
    } else if (status == 403) {
      message(sprintf("403 recebido. Tentando novamente com outro header... (Tentativa %d)", attempt))
      Sys.sleep(3 + runif(1, 0, 2))  # Delay aleatório entre 3-5s
      attempt <- attempt + 1
    } else {
      warning(sprintf("Erro inesperado (status %d).", status))
      Sys.sleep(2)
      attempt <- attempt + 1
    }
  }
}

# --------------------------------------------------------------------------------------------------------------------------------------------------------------

# Função Principal - Matchs
match_urls <- function (country, season_end_year, time_pause = 3) 
{
  
  # Variables
  main_url <- "https://web.archive.org/"
  country_abbr <- country
  season_end_year_num <- season_end_year

  # Seasons_Links
  seasons_links <- c(
    
    # ITA
    "ITA2020" = "",
    "ITA2021" = "https://web.archive.org/web/20230224175727/https://fbref.com/en/comps/11/2021-2022/schedule/2021-2022-Serie-A-Scores-and-Fixtures",
    "ITA2022" = "https://web.archive.org/web/20240917025509/https://fbref.com/en/comps/11/2022-2023/schedule/2022-2023-Serie-A-Scores-and-Fixtures",
    "ITA2023" = "",
    "ITA2024" = "",
    "ITA2025" = "https://web.archive.org/web/20251129170229/https://fbref.com/en/comps/11/2024-2025/schedule/2024-2025-Serie-A-Scores-and-Fixtures",
    
    # ESP
    "ESP2020" = "",
    "ESP2021" = "",
    "ESP2022" = "",
    "ESP2023" = "",
    "ESP2024" = "",
    "ESP2025" = "https://web.archive.org/web/20251001192418/https://fbref.com/en/comps/12/2024-2025/schedule/2024-2025-La-Liga-Scores-and-Fixtures",
    
    # ENG
    "ENG2020" = "https://web.archive.org/web/20240413092138/https://fbref.com/en/comps/9/2019-2020/schedule/2019-2020-Premier-League-Scores-and-Fixtures",
    "ENG2021" = "https://web.archive.org/web/20240530003044/https://fbref.com/en/comps/9/2020-2021/schedule/2020-2021-Premier-League-Scores-and-Fixtures",
    "ENG2022" = "https://web.archive.org/web/20241222043043/https://fbref.com/en/comps/9/2021-2022/schedule/2021-2022-Premier-League-Scores-and-Fixtures",
    "ENG2023" = "https://web.archive.org/web/20240502175524/https://fbref.com/en/comps/9/2022-2023/schedule/2022-2023-Premier-League-Scores-and-Fixtures",
    "ENG2024" = "https://web.archive.org/web/20251025004432/https://fbref.com/en/comps/9/2023-2024/schedule/2023-2024-Premier-League-Scores-and-Fixtures",
    "ENG2025" = "https://web.archive.org/web/20250808165023/https://fbref.com/en/comps/9/2024-2025/schedule/2024-2025-Premier-League-Scores-and-Fixtures"
  )
  fixtures_url <- seasons_links[paste0(country_abbr, season_end_year_num)]
  
  # Matchs
  time_wait <- time_pause
  get_each_seasons_urls <- function(fixture_url, time_pause = time_wait) {
    Sys.sleep(time_pause)
    match_report_urls <- load_page(fixture_url) %>% rvest::html_nodes("td.left~ .left+ .left a") %>% 
      rvest::html_attr("href") %>% paste0(main_url, .) %>% 
      unique()
    return(match_report_urls)
  }
  all_seasons_match_urls <- fixtures_url %>% purrr::map(get_each_seasons_urls) %>% unlist()
  history_index <- grep("-History", all_seasons_match_urls)
  if (length(history_index) != 0) {
    all_seasons_match_urls <- all_seasons_match_urls[-history_index]
  }
  return(all_seasons_match_urls)
}

# Função principal Get Data
get_data <- function (match_url, time_pause = 3) {
  
  # main_url <- "https://fbref.com"
  time_wait <- time_pause

  pb$tick()
  Sys.sleep(time_pause)
  
  # Load XML
  match_page <- tryCatch(load_page(match_url), error = function(e) NA)
  if (!is.na(match_page)) {
    
    # Content
    match_report <- tryCatch(get_match_report_page(match_page = match_page), error = function(e) NA)
    if (length(match_report)==1){
      while (is.na(match_report)){
        message(glue::glue("Tentando novamente..."))
        Sys.sleep(time_pause)
        match_page <- tryCatch(load_page(match_url), error = function(e) NA)
        match_report <- tryCatch(get_match_report_page(match_page = match_page), error = function(e) NA)
        
      }
    }
    all_tables <- match_page %>% rvest::html_nodes(".table_container")
    
    # Vetor com os padrões de id
    patterns <- c(
      "summary$", "passing$", "passing_types", 
      "defense$", "possession$", "misc$"
    )
    
    # Nomes desejados na lista
    names_vec <- c("summary", "passing", "passing_types", "defense", "possession", "misc")
    
    # Criação da lista filtrando por id e aplicando html_nodes("table")
    stat_df_list <- set_names(patterns, names_vec) %>%
      map(~ all_tables[str_detect(html_attr(all_tables, "id"), .x)]) %>%
      map(~ html_nodes(.x, "table"))
    
    stat_final = data.frame()
    if (length(stat_df_list) != 0) {
      
        for(j in 1:length(stat_df_list)){
          
          stat_df = stat_df_list[[j]]
          stat_type = names(stat_df_list)[[j]]
          
          home_stat <- extract_team_players(match_page, stat_df, 1, "Home")
          away_stat <- extract_team_players(match_page, stat_df, 2, "Away")
          stat_df_output <- dplyr::bind_rows(home_stat, away_stat)
          
          if (any(grepl("Nation", colnames(stat_df_output)))) {
            if (!stat_type %in% c("keeper", "shots")) {
                stat_df_output <- stat_df_output %>%
                dplyr::filter(stringr::str_detect(Player, " Players")) %>%
                dplyr::select(-Player, -Player_Num, -Nation, -Pos, -Age)
            }
          } else {
            if (!stat_type %in% c("keeper", "shots")) {
                stat_df_output <- stat_df_output %>%
                dplyr::filter(stringr::str_detect(Player, " Players")) %>%
                dplyr::select(-Player, -Player_Num, -Pos, -Age)
            }
          }
          
          stat_df_output <- dplyr::bind_cols(match_report, stat_df_output)
          
          if (nrow(stat_df_output) == 0) {
            message(glue::glue("NOTE: Stat Type '{stat_type}' is not found for this match. Check {match_url} to see if it exists."))
          }
          
          if (j == 1){stat_final <- stat_df_output
          } else{suppressMessages(stat_final <- stat_final %>% left_join(stat_df_output))}
        }

    } else {
      message(glue::glue("NOTE: Stat Type '{stat_type}' is not found for this match. Check {match_url} to see if it exists."))
      stat_final <- "Erro"
    }
  } else {
    message(glue::glue("Stats data not available for {match_url}"))
    stat_final <- "Erro"
  }
  
  return(stat_final)
}
