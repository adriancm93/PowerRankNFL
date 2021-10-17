library(tidyverse)
library(nflfastR)

#Load data

data = read.csv("C:/Users/adria/OneDrive/Documents/GitHub/PowerRankings2021/WEPAData/WEPAbyGame/WEPA_2021.csv")

#setwd("~/GitHub/PowerRankings2021/WEPAData/WEPAbyGame")
#data <- plyr::ldply(list.files(), read.csv, header=TRUE) %>% 
#  mutate(
#    week = stringr::str_split(game_id, "_")%>%  purrr::map_chr(c(2))  %>% as.integer()
#    )

setwd("~/GitHub/PowerRankings2021")

filt <- nflfastR::load_pbp(2021)

team = data %>% 
  select(
    game_id,
    team,
    wepa
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
    WEPADiff
  )

#Model
model = game %>%
  lme4::lmer(
    formula=
      # Response variable
      WEPADiff ~
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
    season = max(filt$season),
    week=max(filt$week)
    
  )

write.csv(effects,
          paste('C:/Users/adria/OneDrive/Documents/FirstShinnyApp/output',max(filt$season),max(filt$week),'.csv',sep='_'),
          row.names =  F
          )


#Plot
#Let's do 95% confidence interval
z <- 1.282
#Pretty plot
plot = effects %>%
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
  ) +
  labs(y = "Weighted EPA (WEPA) Difference",
       caption = "Data: nflfastR. WEPA model: Robby Greer @greerreNFL. Figure and analysis: @adriancm93",
       title = "Power Rankings",
       subtitle = paste0(
         unique(filt$season),' season, weeks ',min(filt$week),'-',max(filt$week), ', adjusted for opponent, 80% confidence intervals')
  ) + scale_x_discrete(expand = c(.04,.04))

plot

#Save it
ggsave(plot,dpi = 800, filename = 'ranksWEPA2020.png',  width = 11,height = 6)

