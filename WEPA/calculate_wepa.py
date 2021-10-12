## packages used ##
import pandas as pd
import numpy
import statsmodels.api as sm
from scipy.optimize import minimize
import datetime
import time
import WEPAFunctions as wepa

# Inputs
## file paths ##
pbp_folderpath = "WEPAData/pbp/"
game_filepath = "WEPAData/games.csv" 
output_folder = "WEPAData/WEPAbyGame/" ## include final back slash! ##
# season
starting_season = 2021 # select season

current_season = starting_season
## set max season ##
if datetime.date.today().month > 8:
    ending_season = datetime.date.today().year
else:
    ending_season = datetime.date.today().year - 1
    
all_dfs = []
while current_season <= ending_season:
    print('Downloading {0} pbp data...'.format(current_season))
    reg_pbp_df = pd.read_csv(
        'https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{0}.csv.gz'.format(current_season),
        low_memory=False,
        compression='gzip'
    )
    all_dfs.append(reg_pbp_df)
    current_season += 1
pbp_df  = pd.concat(all_dfs)


## prep file ##

## standardize names. Gamefile is already standardized ##

pbp_df['posteam'] = pbp_df['posteam'].replace(wepa.pbp_team_standard_dict)
pbp_df['defteam'] = pbp_df['defteam'].replace(wepa.pbp_team_standard_dict)
pbp_df['penalty_team'] = pbp_df['penalty_team'].replace(wepa.pbp_team_standard_dict)
pbp_df['home_team'] = pbp_df['home_team'].replace(wepa.pbp_team_standard_dict)
pbp_df['away_team'] = pbp_df['away_team'].replace(wepa.pbp_team_standard_dict)

## replace game_id using standardized franchise names ##
pbp_df['game_id'] = (
    pbp_df['season'].astype('str') +
    '_' +
    pbp_df['week'].astype('str').str.zfill(2) +
    '_' +
    pbp_df['away_team'] +
    '_' +
    pbp_df['home_team']
)

## fix some data formatting issues ##
pbp_df['yards_after_catch'] = pd.to_numeric(pbp_df['yards_after_catch'], errors='coerce')

## denote pass or run ##
## seperate offensive and defensive penalties ##
pbp_df['off_penalty'] = numpy.where(pbp_df['penalty_team'] == pbp_df['posteam'], 1, 0)
pbp_df['def_penalty'] = numpy.where(pbp_df['penalty_team'] == pbp_df['defteam'], 1, 0)

## pandas wont group nans so must fill with a value ##
pbp_df['penalty_type'] = pbp_df['penalty_type'].fillna('No Penalty')

## accepted pentalites on no plays need additional detail to determine if they were a pass or run ##
## infer pass plays from the play description ##
pbp_df['desc_based_dropback'] = numpy.where(
    (
        (pbp_df['desc'].str.contains(' pass ', regex=False)) |
        (pbp_df['desc'].str.contains(' sacked', regex=False)) |
        (pbp_df['desc'].str.contains(' scramble', regex=False))
    ),
    1,
    0
)

## infer run plays from the play description ##
pbp_df['desc_based_run'] = numpy.where(
    (
        (~pbp_df['desc'].str.contains(' pass ', regex=False, na=False)) &
        (~pbp_df['desc'].str.contains(' sacked', regex=False, na=False)) &
        (~pbp_df['desc'].str.contains(' scramble', regex=False, na=False)) &
        (~pbp_df['desc'].str.contains(' kicks ', regex=False, na=False)) &
        (~pbp_df['desc'].str.contains(' punts ', regex=False, na=False)) &
        (~pbp_df['desc'].str.contains(' field goal ', regex=False, na=False)) &
        (pbp_df['desc'].str.contains(' to ', regex=False)) &
        (pbp_df['desc'].str.contains(' for ', regex=False))
    ),
    1,
    0
)


## coalesce coded and infered drop backs ##
pbp_df['qb_dropback'] = pbp_df[['qb_dropback', 'desc_based_dropback']].max(axis=1)

## coalesce coaded and infered rush attemps ##
pbp_df['rush_attempt'] = pbp_df[['rush_attempt', 'desc_based_run']].max(axis=1)


## create a specific field for play call ##
pbp_df['play_call'] = numpy.where(
                            pbp_df['qb_dropback'] == 1,
                            'Pass',
                            numpy.where(
                                pbp_df['rush_attempt'] == 1,
                                'Run',
                                numpy.nan
                            )
)

## Structure game file to attach to PBP data ##
## calc margin ##
game_file_df = pd.read_csv(game_filepath, index_col=0)
game_file_df['home_margin'] = game_file_df['home_score'] - game_file_df['away_score']
game_file_df['away_margin'] = game_file_df['away_score'] - game_file_df['home_score']

## flatten file to attach to single team
game_home_df = game_file_df.copy()[['game_id', 'week', 'season', 'home_team', 'home_margin']].rename(columns={
    'home_team' : 'posteam',
    'home_margin' : 'margin',
})
game_away_df = game_file_df.copy()[['game_id', 'week', 'season', 'away_team', 'away_margin']].rename(columns={
    'away_team' : 'posteam',
    'away_margin' : 'margin',
})
flat_game_df = pd.concat([game_home_df,game_away_df], ignore_index=True).sort_values(by=['game_id'])

## calculate game number to split in regressions ##
flat_game_df['game_number'] = flat_game_df.groupby(['posteam', 'season']).cumcount() + 1

## merge to pbp now, so you don't have to merge on every loop ##
pbp_df = pd.merge(
    pbp_df,
    flat_game_df[['posteam','game_id','margin', 'game_number']],
    on=['posteam','game_id'],
    how='left'
)

pbp_df = pbp_df[pbp_df['game_number'] < 17]


## define optimized weights ##
weights_by_season = {}
last_season = pbp_df['season'].max()

for season in range(starting_season,last_season + 1):
    print('Optimizing the {0} season...'.format(season))
    ## set bounds ##
    bound = (-1,2)
    bounds_l = []
    for i in wepa.best_guesses_all_time:
        bounds_l.append(bound)
    ## turn list of bounds into the tuple the optimizer needs ##
    bounds = tuple(bounds_l)
    solution = minimize(wepa.wepa_objective, wepa.best_guesses_all_time, args=(pbp_df.copy(), season, 99), bounds=bounds, method='SLSQP')
    ## add solutions to dictionary ##
    weights_by_season[season] = solution.x

## create wepa ##
wepa_df =wepa.wepa_grade(weights_by_season, pbp_df.copy())

## export ##
wepa_df['epa_net'] = wepa_df['epa'] - wepa_df['epa_against']
wepa_df['epa_net_opponent'] = wepa_df['epa_against'] - wepa_df['epa']
wepa_df = wepa_df[[
    'game_id', 'team', 'opponent', 'season', 'game_number', 'margin', 'margin_against',
    'epa', 'epa_against', 'epa_net', 'epa_net_opponent',
    'wepa', 'd_wepa_against', 'wepa_net',
    'wepa_against', 'd_wepa', 'wepa_net_opponent'
]]

final_name_fix = {
    'OAK':'LV',
    'LAR':'LA'
}
wepa_df.loc[wepa_df ['team'].isin(final_name_fix.keys()),'team'] = wepa_df.loc[wepa_df ['team'].isin(final_name_fix.keys()),'team'].map(final_name_fix)
wepa_df.loc[wepa_df ['opponent'].isin(final_name_fix.keys()),'opponent'] = wepa_df.loc[wepa_df ['opponent'].isin(final_name_fix.keys()),'opponent'].map(final_name_fix)
wepa_df.to_csv('{0}/WEPA_{1}.csv'.format(output_folder,current_season),index=False)
