###############  PROGRAM HEADER #########################
#
# Program Name:  pgmExcitingGamesV2.R
# 
# Purpose:  Create a list of the best reg/post games to watch during quarantine for any team
#
# Instructions:
#   1- Load function by running code below
#   2- type function name fncGetTeamGames( and set the following variables:
#     2a- specify team = as the team you want to pull data for
#     2b- specify min_szn = as the earliest season you want to pull data for
#     2c- specify max_szn = as the latest season you want to pull data for
#   3- function will run and output results in the viewer pane, as well as saving a .csv of the created list to your
#     desktop
#
# 
# Program Change Log:
# 2020-04-27  Code created
#
############### END PROGRAM HEADER ##########################
options(stringsAsFactors = FALSE, warn = -1)

#### function to pull data by season and team ####

fncGetTeamGames <- function(team = team, min_szn = min_szn, max_szn = max_szn){

rm(game_data, szns, games, pbp, game_scores, team_smy, team_epa, opp_epa, game_epa, ovr_watchlist
   ,tm_nickname, desk_path)  
  
require(tidyverse)
require(nflfastR)
require(nflscrapR)
require(glue)
require(gt)
require(furrr)
  

team = toupper(team)
min_szn = as.numeric(min_szn)
max_szn = as.numeric(max_szn)
  
szns = seq(from = min_szn, to = max_szn, by = 1)  
  
game_data = lapply(szns, fast_scraper_schedules) %>% 
         do.call('rbind', .) %>% 
         filter(home_team == team | away_team == team) %>% 
         filter(season_type == 'REG' | season_type == 'POST')

games = game_data %>% pull(game_id)

pbp = fast_scraper(games, pp = TRUE)

# pbp = do.call('rbind', pbp)

game_scores = pbp %>% select(game_id, home_team, away_team, total_home_score, total_away_score) %>%
  group_by(game_id, home_team, away_team) %>% 
  summarise(home_score = max(total_home_score)
            ,away_score = max(total_away_score)) %>% ungroup()

game_data = game_data %>% 
  left_join(game_scores %>% select(game_id, home_team, away_team, home_score, away_score)
            , by = c('game_id', 'home_team', 'away_team')) %>% 
  mutate(team_score = ifelse(home_team == team, home_score, away_score)
         ,opp_score = ifelse(home_team != team, home_score, away_score)
         ,home_score = NULL
         ,away_score = NULL)

team_smy = pbp %>% 
  group_by(posteam, game_id) %>% 
  summarise(net_epa = sum(na.omit(epa))) %>% 
  ungroup()

team_epa = team_smy %>% filter(posteam == team) %>% rename('team_net_epa' = 'net_epa')

opp_epa = team_smy %>% filter(posteam != team & posteam != '') %>% rename('opp_tm' = 'posteam', 'opp_net_epa' = 'net_epa')

game_epa = team_epa %>% 
  left_join(opp_epa, by = 'game_id') %>% 
  mutate(watchability = team_net_epa - opp_net_epa)

percentile <- ecdf(game_epa$watchability)

ovr_watchlist = game_epa %>% 
  mutate(watch_index = percentile(watchability)
         ,season = as.numeric(substr(game_id, 1,4))
         ,rank = rank(-watch_index, ties.method = 'min')) %>% 
  left_join(game_data %>% select(game_id, week, team_score, opp_score), by = 'game_id') %>% 
  filter(watchability > 0) %>% 
  select(rank, posteam, opp_tm, season, week, team_score, opp_score,team_net_epa, opp_net_epa, watch_index) %>% 
  arrange(desc(watch_index))

tm_nickname = game_data %>% 
  select(home_team, home_nickname) %>% 
  filter(home_team == team) %>% 
  pull(home_nickname) %>% 
  unique()

table_out <- ovr_watchlist %>% 
  arrange(desc(watch_index)) %>% 
  gt() %>% 
  tab_header(title = glue('{tm_nickname} Quarantine Game Rewatch Ranking')
             ,subtitle = 'Best Games to Rewatch on NFL Gamepass #StayAtHome') %>% 
  tab_spanner(label = 'Final Score', columns = vars(team_score, opp_score)) %>%
  tab_spanner(label = 'Expected Points Added (EPA)', columns = vars(team_net_epa, opp_net_epa)) %>%
  fmt_number(columns = vars(watch_index, team_net_epa, opp_net_epa), decimals = 2) %>% 
  cols_label(rank = 'Rewatchability Rank'
             ,posteam = 'Good Guys'
             ,opp_tm = 'Bad Guys'
             ,season = 'Season'
             ,week = 'Week Nbr.'
             ,team_score = tm_nickname
             ,opp_score = 'Opp.'
             ,team_net_epa = tm_nickname
             ,opp_net_epa = 'Opp.'
             ,watch_index = 'Watchability Index') %>% 
  tab_source_note(source_note = 'data courtesy of @nflfastR') %>% 
  tab_source_note(source_note = 'analysis by @brownalytics')
  
  ovr_watchlist <<- ovr_watchlist
  
  desk_path = file.path(Sys.getenv('USERPROFILE'), 'Desktop') %>% 
    gsub("\\\\", "/", .)
  
  write.csv(ovr_watchlist, glue::glue('{desk_path}/{tolower(tm_nickname)}_rewatchability.csv'), row.names = FALSE)
  
  return(table_out)
  
}



  

