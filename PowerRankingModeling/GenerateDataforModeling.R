library(tidyverse)
library(nflfastR)
setwd("~/GitHub/PowerRankNFL")

#Load data
maindata <- plyr::ldply(list.files('WEPA/WEPAData/WEPAbyGame',full.names = T), read.csv, header=TRUE) %>% 
  mutate(
    week = stringr::str_split(game_id, "_")%>%  purrr::map_chr(c(2))  %>% as.integer()
  ) %>% 
  filter(
    season >= 2009
  )


for (
  szn in (min(maindata$season) + 1) : max(maindata$season)
  ){
  
  #For reference
  sub = maindata %>% filter(season == szn)
  prev_sub = maindata %>% filter(season == (szn-1))
  
  for (wk in 1:max(sub$week)){
    
    #Filter data
    data = maindata %>% filter(
      season == szn,
      week <= wk
    ) %>% mutate(
      game_week = week,
      prev_season_data = 0
      )
    
    # #Add prior data if needed
    # if (wk <= 7){
    # 
    #   prior_data = maindata %>% filter(
    #     season == (szn - 1),
    #     week > max(prev_sub$week) - (7 - wk)
    #   ) %>% 
    #     mutate(
    #       prev_season_data = 1,
    #       game_week = week)
    # 
    #   data = rbind(data,prior_data)
    #   
    # } else{
    #   
    #   data=data
    # }
    
    game = data %>% 
      mutate(
        WEPADiff = wepa -wepa_against
      ) %>% 
      select(
        game_id,
        team,
        opponent,
        game_week,
        WEPADiff,
        prev_season_data
      )
    
    
    #Add weights
    df = game %>%  
      mutate(season=szn,week=wk) %>% 
      arrange(game_id) %>% 
      mutate(
        week_diff = week - game_week
        # weight = case_when(
        #   week_diff == 0 ~ 1,
        #   week_diff == 1 ~ 1,
        #   week_diff == 2 ~ 1,
        #   week_diff == 3 ~ .9,
        #   week_diff == 4 ~ .8,
        #   week_diff == 5 ~ .7,
        #   week_diff == 6 ~ .6,
        #   week_diff == 7 ~ .5,
        #   week_diff == 8 ~ .4,
        #   week_diff == 9 ~ .3,
        #   week_diff > 9 ~ .3
        # ),
        # weight = if_else(prev_season_data == 1, .2,weight)
      )
    
    #file DF
    file_name = paste0(as.character(szn),'_',as.character(wk),'.csv')
    write.csv(df,paste0('PowerRankingModeling/OutputData/DataForModeling/',file_name),row.names = F)
    }
}
  
