library(tidyverse)
library(nflfastR)

#Load data
setwd("~/GitHub/PowerRankings2021/WEPAData/WEPAbyGame")
maindata <- plyr::ldply(list.files(), read.csv, header=TRUE) %>% 
  mutate(
    week = stringr::str_split(game_id, "_")%>%  purrr::map_chr(c(2))  %>% as.integer()
  ) %>% 
  filter(
    season >= 2009
  )

setwd("~/GitHub/PowerRankings2021")


lst = list()
lst0 = list()

for (
  szn in (min(maindata$season) + 1) : max(maindata$season)
  ){
  
  #Fore reference
  sub = maindata %>% filter(season == szn)
  prev_sub = maindata %>% filter(season == (szn-1))
  
  for (wk in 3:max(sub$week)){
    
    #Filter data
    data = maindata %>% filter(
      season == szn,
      week <= wk
    ) %>% mutate(
      weight = 1,
      weight = if_else((wk - week) >= 5,.80, weight),
      weight = if_else((wk - week) >= 10,.70, weight),
      game_week = week
      )
    
    #Add prior data if needed
    if (wk <= 4){
    
      prior_data = maindata %>% filter(
        season == (szn - 1),
        week > max(prev_sub$week) - (7 - wk)
      ) %>% 
        mutate(
          weight = .3,
          game_week = week)

      data = rbind(data,prior_data)
      
    }
    
    team = data %>% 
      select(
        game_id,
        team,
        wepa,
        weight,
        game_week
      )
    
    opponent = data %>%
      select(
        game_id,
        team,
        opponent,
        wepa_against
      )
    
    game = dplyr::left_join(team,opponent,by = c('game_id','team')) %>% 
      mutate(
        WEPADiff = wepa -wepa_against
      ) %>% 
      select(
        game_id,
        team,
        opponent,
        WEPADiff,
        weight,
        game_week
      )
    
    lst0[[paste(szn,wk)]] = game %>%  mutate(season=szn,week=wk)
    
    #Model
    model = game %>%
      lme4::lmer(
        formula=
          # Response variable
          (WEPADiff*weight) ~
          #adjust for opponent
          as.factor(opponent) +
          #this is the strength of each team
          (1|team)
      )
    
    #Extract random variable effects
    effects = broom.mixed::tidy(model,effects="ran_vals") %>%
      filter(group == 'team') %>% 
      select(level,estimate,std.error) %>% rename(team = level) %>% 
      arrange(
        by = estimate
      ) %>% 
      mutate(
        rank = 32:1,
        Team = paste(team,rank),
        season = szn,
        week=wk,
        
      )
    
    lst[[paste(szn,wk)]] = effects
    }
}
  
results_data = dplyr::bind_rows(lst0)
results = dplyr::bind_rows(lst) 
iters = results %>% filter(estimate == 0 | is.na(estimate)) %>% select(week) %>%  max()
for (i in 1:iters){
  results = results %>% 
    arrange(
      season,team,week
    ) %>% group_by(
      season,team
    ) %>%
    mutate(
      testimate = dplyr::if_else(estimate == 0 ,lag(estimate),estimate),
      std.error = dplyr::if_else(estimate == 0 ,lag(std.error),std.error), 
      rank = dplyr::if_else(estimate == 0 ,lag(rank),rank),
      Team = dplyr::if_else(estimate == 0 ,lag(Team),Team),
      rank_change = as.integer(rank) - lag(as.integer(rank)),
    ) %>% ungroup() %>% mutate(estimate = testimate) %>% 
    arrange(season,team,week) %>% 
    select(-testimate)
}


check = results %>% filter(
  estimate == 0 | is.na(estimate)
)
check$season %>% unique()
check$week %>% unique()


write.csv(results_data,'PowerRankings/results_alldata.csv',row.names = F)

#Plot
#Let's do 95% confidence interval
z <- 1.282
#Pretty plot


plot = results %>% 
  filter(season==2021,week==4) %>% 
  arrange(estimate)%>%
  ggplot(aes(x=factor(Team, level = Team),estimate)) + 
  
  geom_linerange(alpha = .7, color = 'gray40', linetype = 2,aes(ymin=estimate - z*std.error,
                                                                ymax=estimate + z*std.error))+
  
  geom_point(aes(fill = estimate),alpha = 1, color='gray20',pch=21,size=2.75) +
  
  geom_text(aes(label = round(estimate,1),y=estimate+.75),nudge_x = 0.4,size=2.7,alpha=0.5)  +
  
  coord_flip() + 
  theme_classic()  + 
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = 'none'
  ) + scale_x_discrete(expand = c(.04,.04))

plot



del = results %>% filter(season==2016)
