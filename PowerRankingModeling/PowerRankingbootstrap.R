library(tidyverse)
library(nflfastR)


szn = 2020
wk = 17

s2020 = results_data %>% filter(season==szn,week==wk)

s2020 = s2020 %>% 
  mutate(
    week_diff = week - game_week,
    
    weight2= case_when(
      week_diff == 0 ~ 1,
      week_diff == 1 ~ .9,
      week_diff == 2 ~ .8,
      week_diff == 3 ~ .7,
      week_diff == 4 ~ .6,
      week_diff == 5 ~ .5,
      week_diff == 6 ~ .4,
      week_diff == 7 ~ .3,
      week_diff == 8 ~ .2,
      week_diff == 9 ~ .1,
      week_diff > 9 ~ .1
    )
    
  )

preds = lm(WEPADiff ~ as.factor(opponent) , data = s2020, weights = weight2) %>% predict()
s2020$pred = preds
s2020$epaadj = s2020$WEPADiff - s2020$pred
res = s2020 %>% group_by(team) %>%  summarise(epaadj = mean(epaadj)) %>% arrange(desc(epaadj))


res %>% filter(team=='KC')$epaadj

# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data


effect <- function(data, indices,t) {
  
  d <- data[indices,] # allows boot to select sample
  
  preds = lm(WEPADiff ~ as.factor(opponent) , data = d, weights = weight2) %>% predict()
  d$pred = preds
  d$epaadj = d$WEPADiff - d$pred
  res = d %>% group_by(team) %>%  summarise(epaadj = mean(epaadj)) %>% arrange(desc(epaadj))
  
  stat = res %>% filter(team == t)
  return(stat$epaadj)
}

lst = list()
for (team in unique(s2020$team)){
  results <- boot(data=s2020, statistic=effect,
                  R=1000, t=team)
  df = data.frame('AdjWEPADiff' = results$t,'Team' = team)
  
  lst[[team]] = df
  
}

results = dplyr::bind_rows(lst)
colors = nflfastR::teams_colors_logos %>% 
  select(team_abbr,team_color) %>% 
  rename(
    Team = team_abbr 
  )

results = merge(results,colors, on='Team')


plot = results %>% 
  filter(
    Team %in% c('KC','JAX','SF')
  ) 

plotv = plot %>% ggplot(aes(x=AdjWEPADiff)) +
  geom_density(aes(fill=Team),alpha=.6)+
  scale_fill_manual(
    values = unique(plot$team_color)) +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(
    title = 'Power Rankings 2020',
  ) + ylim(c(0,.17))


ggsave(filename = 'pr_ci.png',plot=plotv,height = 5, width = 8)

summary_80 = results %>% 
  group_by(
    Team
  ) %>% 
  summarise( 
    UCI = quantile(AdjWEPADiff, .90) ,
    LCI = quantile(AdjWEPADiff, .10) ,
    AdjWEPADiff = mean(AdjWEPADiff),
  )


plotv = summary_80 %>% 
  arrange(AdjWEPADiff)%>%
  ggplot(aes(x=factor(Team, level = Team),AdjWEPADiff)) + 
  
  geom_linerange(alpha = .7, color = 'gray40', linetype = 2,aes(ymin= LCI,
                                                                ymax= UCI)) +
  
  geom_point(aes(fill = AdjWEPADiff),alpha = 1, color='gray20',pch=21,size=2.75) +
  
  geom_text(aes(label = round(AdjWEPADiff,1),y=AdjWEPADiff+.75),nudge_x = 0.4,size=2.7,alpha=0.5)  +
  
  coord_flip() + 
  theme_classic()  + 
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = 'none'
  ) + scale_x_discrete(expand = c(.04,.04))

plotv

