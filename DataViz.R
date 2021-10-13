library(tidyverse)
library(ggplot2)
setwd("~/GitHub/PowerRankNFL")

szn = 2021
wk = 5
summary = read.csv(paste0("PowerRankingModeling/OutputData/SummaryResults/",szn,"_",wk,".csv"))


plot_summary = summary %>% 
  arrange(AdjWEPADiff)%>%
  mutate(
    TeamR = paste(Team,32:1)
  ) %>% 
  ggplot(aes(x=factor(TeamR, level = TeamR),AdjWEPADiff )) + 
  
  geom_linerange(alpha = .7, color = 'gray40', linetype = 2,aes(ymin=LowerCI85,
                                                                ymax=UpperCI85))+
  
  geom_point(aes(fill = AdjWEPADiff),alpha = 1, color='gray20',pch=21,size=2.75) +
  
  geom_text(aes(label = round(AdjWEPADiff,1),y=AdjWEPADiff+.75),nudge_x = 0.4,size=2.7,alpha=0.5)  +
  geom_text(aes(label = Team, y=AdjWEPADiff-.75),nudge_x = 0.4,size=2.7,alpha=0.5)  +
  
  coord_flip() + 
  theme_classic()  + 
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = 'none'
  ) + scale_x_discrete(expand = c(.04,.04)) +
  labs(
    title = 'NFL Power Rankings',
    subtitle = '2021 Season, weeks 1-5, 85% confidence intervals',
    caption = 'Data from nflFastR, WEPA model by Robby Greer. Ranking model by @adriancm93'
  )
plot_summary
ggsave(plot_summary,filename = 'Summary.png',height = 6,width = 8,dpi=600)

dens = read.csv(paste0("PowerRankingModeling/OutputData/BootstrappedEstimates/",szn,"_",wk,".csv"))

#Scale again for plot

dens$AdjWEPADiff = scale(dens$AdjWEPADiff, center = FALSE)

colors=nflfastR::teams_colors_logos %>% select(team_abbr,team_color,team_color2) %>% rename(Team=team_abbr) #%>% 
dens=left_join(dens,colors,on='Team')

d = dens %>% filter(Team == 'DEN') %>% mutate(Team = 'NFL Avg',team_color='black',team_color2='black')

plot = dens %>% filter(
  Team %in% c('DAL','NE')
)
plot = rbind(plot,d)



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
    title='Team-Strength Comparison',
    subtitle = '2021 season, weeks 1-5',
    caption = 'Data from nflfastR and WEPA model by Robby Greer. Analysis: @adriancm93'
  )

density_plot

ggsave(density_plot,filename = 'DALvNE.png',height = 4,width = 7,dpi=800)
