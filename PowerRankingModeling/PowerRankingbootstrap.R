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
  
  cols = c('WEPADiff', 'opponent','team')
  dummies_ <- caret::dummyVars(WEPADiff ~ ., data = d[,cols])
  dummies = predict(dummies_, newdata = d[,cols])
  x = as.matrix(dummies)
  
  model = glmnet::glmnet(x, d$WEPADiff, alpha = 0, family = 'gaussian', lambda = .001)
  
  stat = data.frame('Team' = rownames(model$beta), 
                    'Coef' = as.vector(model$beta)) %>% 
    filter(grepl('team', Team))  %>% 
    mutate(
      Team = stringr::str_remove(Team, 'team'),
    )  %>%  filter(Team == t) %>% pull(Coef)
  
  return(stat)
}

create_CI = function(vector,CI){
  
  lower = (1 - CI) / 2
  upper = 1- lower
  
  ci = quantile(vector, c(lower,upper))
  return(ci)
}

for (szn in 2012:2013){  
  #Filter for season
  seas = dplyr::filter(maindata, season == szn)
  
  for (wk in 3:6){
    
    #Filter for week
    df = maindata %>% filter(season==szn,week==wk)
    
    df = rbind(df,df)
    
    #Run bootstrap
    lst = list()
    for (team in unique(df$team)){
      
      try_again = TRUE
      while (try_again == TRUE){
        results <- try(boot(data=df, statistic=effect,
                            R=300, t=team), silent=TRUE)
        
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
