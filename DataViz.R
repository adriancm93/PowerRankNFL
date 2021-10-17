library(tidyverse)
library(ggplot2)
setwd("~/GitHub/PowerRankNFL")

szn = 2020
wk = 17
dens = read.csv(paste0("PowerRankingModeling/OutputData/BootstrappedEstimates/",szn,"_",wk,".csv"))
dens$AdjWEPADiff = scale(dens$AdjWEPADiff)

create_CI = function(vector,CI){
  
  lower = (1 - CI) / 2
  upper = 1- lower
  
  ci = quantile(vector, c(lower,upper))
  return(ci)
}

summary = dens  %>% group_by(
  Team
) %>% summarise(
  LowerCI95 = create_CI(AdjWEPADiff,.95)[1],
  UpperCI95 =create_CI(AdjWEPADiff,.95)[2],
  LowerCI85 = create_CI(AdjWEPADiff,.85)[1],
  UpperCI85 =create_CI(AdjWEPADiff,.85)[2],
  AdjWEPADiff  = mean(AdjWEPADiff),
  Season = unique(Season),
  LatestWeek = unique(LatestWeek)
)

plot_summary = summary %>% 
  arrange(AdjWEPADiff)%>%
  mutate(
    TeamR = paste(Team,32:1)
  ) %>% 
  ggplot(aes(x=factor(TeamR, level = TeamR),AdjWEPADiff )) + 
  
  geom_linerange(alpha = .7, color = 'gray40', linetype = 2,aes(ymin=LowerCI85,
                                                                ymax=UpperCI85))+
  
  geom_point(aes(fill = AdjWEPADiff),alpha = 1, color='gray20',pch=21,size=2.75) +
  
  geom_text(aes(label = round(AdjWEPADiff,1),y=AdjWEPADiff+.2),nudge_x = 0.4,size=2.7,alpha=0.5)  +
  geom_text(aes(label = Team, y=AdjWEPADiff-.2),nudge_x = 0.4,size=2.7,alpha=0.5)  +
  
  coord_flip() + 
  theme_classic()  + 
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = 'none'
  ) + scale_x_discrete(expand = c(.04,.04)) +
  labs(
    y = 'Power Index',
    title = 'NFL Power Rankings',
    subtitle = '2021 Season, weeks 1-5, 85% confidence intervals',
    caption = 'Data: nflfastR | WEPA model: Robby Greer | Power Index model: @adriancm93'
  )

plot_summary

#ggsave(plot_summary,filename = 'SummaryNotAdj.png',height = 6,width = 8,dpi=1000)

#Scale again for plot
colors=nflfastR::teams_colors_logos %>% select(team_abbr,team_color,team_color2) %>% rename(Team=team_abbr) #%>% 
dens=left_join(dens,colors,on='Team')

avg_lst = list()
for (i in 1:3){
  avg_lst[[i]] = data.frame(AdjWEPADiff = sample(dens$AdjWEPADiff,size=500),line=1:500)
  
}

avg = dplyr::bind_rows(avg_lst) %>% group_by(
  line 
) %>% summarise(
  AdjWEPADiff = mean(AdjWEPADiff)
) %>% 
  mutate(
  Team = 'NFL Avg',team_color='black',team_color2='black',
  LatestWeek = max(dens$LatestWeek),
  Season = max(dens$Season)) %>% select(-line)

plot_ = dens# %>% filter(Team %in% c('DAL','ARI'))

plot = rbind(plot_,avg)

fill_lst = plot %>% group_by(
  Team
) %>% summarise(team_color=unique(team_color)) %>% 
  pull('team_color')

color_lst = plot %>% 
  group_by(
    Team) %>% summarise(team_color2=unique(team_color2)) %>% 
  pull('team_color2')

density_plot = plot %>% 
  ggplot(aes(x=AdjWEPADiff)) +
  geom_density(aes(fill=Team,color=Team),alpha=.5,size=1) +
  scale_fill_manual(
    values = fill_lst) +
  scale_color_manual(
    values = color_lst) +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = 'top',
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    x = 'Distribution of estimated Power Index',
    title='Team-strength comparison',
    subtitle = '2021 season, weeks 1-5',
    caption = 'Data: nflfastR | WEPA model: Robby Greer | Analysis: @adriancm93'
  ) 

density_plot

ggsave(density_plot,filename = 'week5.png',height = 8,width = 14,dpi=1000)


# t = summary %>% arrange( AdjWEPADiff)  
# best = as.character(t[32,1])
# worst = as.character(t[1,1])
# best_stats = dens %>% filter(Team == best) %>% pull(AdjWEPADiff)
# worst_stats = dens %>% filter(Team == worst)%>% pull(AdjWEPADiff)
# 
# avg_stats = (best_stats + worst_stats) / 2
# 
# avg = data.frame(AdjWEPADiff = avg_stats) %>% 
#   mutate(
#     Team = 'NFL Avg',
#     team_color='black',
#     team_color2='black',
#     LatestWeek = max(dens$LatestWeek),
#     Season = max(dens$Season))
# 
# avg %>% colnames()
# plot %>% colnames()
# 
# plot_ = dens %>% filter(
#   Team %in% c('DAL','ARI')
# )

