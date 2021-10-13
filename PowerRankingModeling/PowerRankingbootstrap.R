library(tidyverse)
library(nflfastR)
library(caret)
library(boot)

setwd("~/GitHub/PowerRankNFL")

#Load data
maindata <- plyr::ldply(list.files('PowerRankingModeling/OutputData/DataForModeling',full.names = T), read.csv, header=TRUE)


#Functions
effect <- function(data, indices,t) {
  
  d <- data[indices,] 
  
  cols = c('WEPADiff', 'opponent')
  dummies_ <- caret::dummyVars(WEPADiff ~ ., data = d[,cols])
  dummies = predict(dummies_, newdata = d[,cols])
  x = as.matrix(dummies)
  model = glmnet::glmnet(x, d$WEPADiff, alpha = 0, family = 'gaussian', lambda = .001,weights =d$weight )
  preds = predict(model, s = .001, newx = x)
  d$pred = preds
  d$AdjWEPADiff = d$WEPADiff - d$pred
  summarize = d %>% group_by(team) %>%  summarise(AdjWEPADiff = mean(AdjWEPADiff)) %>% arrange(desc(AdjWEPADiff))
  
  df_team = summarize %>% filter(team == t)
  
  return(df_team$AdjWEPADiff)
}
create_CI = function(vector,CI){
  
  lower = (1 - CI) / 2
  upper = 1- lower
  
  ci = quantile(vector, c(lower,upper))
  return(ci)
}


# maindata = maindata %>% 
#    filter(season==szn,week==wk) %>% 
#   mutate(
#   weight = if_else(week_diff <0, .1, weight)
# )

#for (szn in min(maindata$season):max(maindata$season)){

for (szn in 2021:2021){  
  #Filter for season
  seas = dplyr::filter(maindata, season == szn)
  
  for (wk in 5:5){
    
    #Filter for week
    df = maindata %>% filter(season==szn,week==wk)
    
    # df = df %>% mutate(
    #   weight = if_else(week_diff <0, .3, weight)
    # )
    
    df1 = df %>% filter(week_diff >= 0)
    #df2 = df %>% filter(week_diff < 0)
    df = rbind(df1,df1)
    
    # if(wk %in% c(5,6,7)){
    #   df = rbind(df,df)
    # } else{
    #   df = df
    # }
    
    #Run bootstrap
    lst = list()
    for (team in unique(df$team)){
      
      try_again = TRUE
      while (try_again == TRUE){
        results <- try(boot(data=df, statistic=effect,
                            R=600, t=team), silent=TRUE)
        
        if ('try-error' %in% class(results)){
          print(paste(team,'Error: trying again'))
          try_again = TRUE
          
        }
        else{
          print(paste(team,'Model converged'))
          try_again = FALSE
        }
      }
      
      #Get results
      df_out = data.frame('AdjWEPADiff' = results$t,'Team' = team,'Season'=szn, 'LatestWeek' = wk)
      
      rm(results)
      
      #Save to list
      lst[[team]] = df_out
      
      rm(df_out)
      }
    
    rm(df)
    
    #Prepare results
    dataframe = dplyr::bind_rows(lst)
      
    #Summarize by team
    summary = dataframe %>% group_by(
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
    
    #Save files
    file_name = paste0(as.character(szn),'_',as.character(wk),'.csv')
    write.csv(dataframe,paste0('PowerRankingModeling/OutputData/BootstrappedEstimates/',file_name),row.names = F)
    write.csv(summary,paste0('PowerRankingModeling/OutputData/SummaryResults/',file_name),row.names = F)
    }
}


