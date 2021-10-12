'''
Functions to Weight EPA
'''

import pandas as pd
import numpy
import statsmodels.api as sm
from scipy.optimize import minimize
import datetime
import time

#Dictionary to standardize team names
pbp_team_standard_dict = {

    'ARI' : 'ARI',
    'ATL' : 'ATL',
    'BAL' : 'BAL',
    'BUF' : 'BUF',
    'CAR' : 'CAR',
    'CHI' : 'CHI',
    'CIN' : 'CIN',
    'CLE' : 'CLE',
    'DAL' : 'DAL',
    'DEN' : 'DEN',
    'DET' : 'DET',
    'GB'  : 'GB',
    'HOU' : 'HOU',
    'IND' : 'IND',
    'JAC' : 'JAX',
    'JAX' : 'JAX',
    'KC'  : 'KC',
    'LA'  : 'LAR',
    'LAC' : 'LAC',
    'LV'  : 'OAK',
    'MIA' : 'MIA',
    'MIN' : 'MIN',
    'NE'  : 'NE',
    'NO'  : 'NO',
    'NYG' : 'NYG',
    'NYJ' : 'NYJ',
    'OAK' : 'OAK',
    'PHI' : 'PHI',
    'PIT' : 'PIT',
    'SD'  : 'LAC',
    'SEA' : 'SEA',
    'SF'  : 'SF',
    'STL' : 'LAR',
    'TB'  : 'TB',
    'TEN' : 'TEN',
    'WAS' : 'WAS',

}

## create objective function ##
## takes an array of best guesses ##
best_guesses_all_time = [

    1.15,    ## qb_rush ##
    1.30,    ## neutral_second_down_rush ##
    -0.69,    ## incompletion_depth_s ##
    -0.60,    ## non_sack_fumble ##
    -0.15,    ## int ##
    -1.00,    ## goalline ##
    -0.55,    ## scaled_win_prob ##
    1.17,    ## d_qb_rush ##
    1.49,    ## d_neutral_second_down_rush ##
    -1.00,    ## d_incompletion_depth_s ##
    -0.90,    ## d_sack_fumble ##
    -0.75,    ## d_int ##
    -0.85,    ## d_fg ##
    -0.45,    ## d_third_down_pos ##
    -0.20     ## defense_adj ##

]

## define weight names for reference ##
weight_names_list = [

    'qb_rush',
    'neutral_second_down_rush',
    'incompletion_depth_s',
    'non_sack_fumble',
    'int',
    'goalline',
    'scaled_win_prob',
    'd_qb_rush',
    'd_neutral_second_down_rush',
    'd_incompletion_depth_s',
    'd_sack_fumble',
    'd_int',
    'd_fg',
    'd_third_down_pos',
    'defense_adj'

]




## define objective function for optimization ##
def wepa_objective(x, train_df, season_year, seasons_back):
    ## define weights ##
    ## play style ##
    train_df['qb_rush_weight'] = numpy.where((train_df['qb_scramble'] == 1) & (train_df['fumble_lost'] != 1), 1 + x[0], 1)
    train_df['neutral_second_down_rush_weight'] = numpy.where(
        (train_df['down'] == 2) &
        (train_df['play_call'] == 'Run') &
        (train_df['yardline_100'] > 20) &
        (train_df['yardline_100'] < 85) &
        ((train_df['wp'] < .90) | (train_df['wp'] > .10)) &
        (train_df['qb_scramble'] != 1) &
        (train_df['fumble_lost'] != 1) &
        (train_df['epa'] < 0),
        1 + x[1],
        1
    )
    train_df['incompletion_depth_s_weight'] = 1 + numpy.where(
        (train_df['incomplete_pass'] == 1) & (train_df['interception'] != 1),
        numpy.where(numpy.isnan(x[2] * (2 * (1/(1 + numpy.exp(-0.1 * train_df['air_yards'] + .75)) - 0.5))),0,(x[2] * (2 * (1/(1 + numpy.exp(-0.1 * train_df['air_yards'] + .75)) - 0.5)))),
        0
    )
    ## events ##
    train_df['non_sack_fumble_weight'] = numpy.where((train_df['sack'] != 1) & (train_df['fumble_lost'] == 1), 1 + x[3], 1)
    train_df['int_weight'] = numpy.where(train_df['interception'] == 1, 1 + x[4], 1)
    ## contextual ##
    train_df['goalline_weight'] = numpy.where((train_df['yardline_100'] < 3) & (train_df['down'] < 4), 1 + x[5], 1)
    train_df['scaled_win_prob_weight'] = 1 + (-x[6] * numpy.where(train_df['wp'] <= .5, 1/(1+numpy.exp(-10*(2*train_df['wp']-0.5)))-0.5,1/(1+numpy.exp(-10*(2*(1-train_df['wp'])-0.5)))-0.5))
    ## define defensive weights ##
    ## play style ##
    train_df['d_qb_rush_weight'] = numpy.where((train_df['qb_scramble'] == 1) & (train_df['fumble_lost'] != 1), 1 + x[7], 1)
    train_df['d_neutral_second_down_rush_weight'] = numpy.where(
        (train_df['down'] == 2) &
        (train_df['play_call'] == 'Run') &
        (train_df['yardline_100'] > 20) &
        (train_df['yardline_100'] < 85) &
        ((train_df['wp'] < .90) | (train_df['wp'] > .10)) &
        (train_df['qb_scramble'] != 1) &
        (train_df['fumble_lost'] != 1) &
        (train_df['epa'] < 0),
        1 + x[8],
        1
    )
    train_df['d_incompletion_depth_s_weight'] = 1 + numpy.where(
        (train_df['incomplete_pass'] == 1) & (train_df['interception'] != 1),
        numpy.where(numpy.isnan(x[9] * (2 * (1/(1 + numpy.exp(-0.1 * train_df['air_yards'] + .75)) - 0.5))),0,(x[9] * (2 * (1/(1 + numpy.exp(-0.1 * train_df['air_yards'] + .75)) - 0.5)))),
        0
    )
    ## events ##
    train_df['d_sack_fumble_weight'] = numpy.where((train_df['sack'] == 1) & (train_df['fumble_lost'] == 1), 1 + x[10], 1)
    train_df['d_int_weight'] = numpy.where(train_df['interception'] == 1, 1 + x[11], 1)
    train_df['d_fg_weight'] = numpy.where(train_df['play_type'] == 'field_goal', 1 + x[12], 1)
    ## contextual ##
    train_df['d_third_down_pos_weight'] = numpy.where(
        (train_df['down'] == 3) &
        (train_df['epa'] > 0),
        1 + x[13],
        1
    )
    ## add weights to list to build out headers and loops ##
    weight_names = [
        'qb_rush',
        'neutral_second_down_rush',
        'incompletion_depth_s',
        'non_sack_fumble',
        'int',
        'goalline',
        'scaled_win_prob'
    ]
    d_weight_names = [
        'd_qb_rush',
        'd_neutral_second_down_rush',
        'd_incompletion_depth_s',
        'd_sack_fumble',
        'd_int',
        'd_fg',
        'd_third_down_pos'
    ]
    ## create a second list for referencing the specifc weights ##
    weight_values = []
    for weight in weight_names:
        weight_values.append('{0}_weight'.format(weight))
    ## defense ##
    d_weight_values = []
    for weight in d_weight_names:
        d_weight_values.append('{0}_weight'.format(weight))
    ## create structures for aggregation ##
    aggregation_dict = {
        'margin' : 'max', ## game level margin added to each play, so take max to get 1 ##
        'wepa' : 'sum',
        'd_wepa' : 'sum',
        'epa' : 'sum',
    }
    headers = [
        'game_id',
        'posteam',
        'defteam',
        'season',
        'game_number',
        'margin',
        'wepa',
        'd_wepa',
        'epa'
    ]
    ## dictionary to rename second half of the season metrics ##
    rename_to_last_dict = {
        'margin' : 'margin_L8',
        'wepa_net' : 'wepa_net_L8',
    }
    ## disctionary to join oppoenets epa to net out ##
    rename_opponent_dict = {
        'margin' : 'margin_against',
        'wepa' : 'wepa_against',
        'd_wepa' : 'd_wepa_against',
        'epa' : 'epa_against',
    }
    ## create wepa ##
    train_df['wepa'] = train_df['epa']
    for weight in weight_values:
        train_df['wepa'] = train_df['wepa'] * train_df[weight]
    train_df['d_wepa'] = train_df['epa'] * (1 + x[14])
    for weight in d_weight_values:
        train_df['d_wepa'] = train_df['d_wepa'] * train_df[weight]
    ## bound wepa to prevent extreme values from introducing volatility ##
    train_df['wepa'] = numpy.where(train_df['wepa'] > 10, 10, train_df['wepa'])
    train_df['wepa'] = numpy.where(train_df['wepa'] < -10, -10, train_df['wepa'])
    ## defense ##
    train_df['d_wepa'] = numpy.where(train_df['d_wepa'] > 10, 10, train_df['d_wepa'])
    train_df['d_wepa'] = numpy.where(train_df['d_wepa'] < -10, -10, train_df['d_wepa'])
    ## aggregate from pbp to game level ##
    game_level_df = train_df.groupby(['posteam','defteam','season','game_id','game_number']).agg(aggregation_dict).reset_index()
    game_level_df = game_level_df.sort_values(by=['posteam','game_id'])
    game_level_df = game_level_df[headers]
    ## add net epa ##
    ## create an opponent data frame ##
    game_level_opponent_df = game_level_df.copy()
    game_level_opponent_df['posteam'] = game_level_opponent_df['defteam']
    game_level_opponent_df = game_level_opponent_df.drop(columns=['defteam','season','game_number'])
    game_level_opponent_df = game_level_opponent_df.rename(columns=rename_opponent_dict)
    ## merge to main game file ##
    game_level_df = pd.merge(
        game_level_df,
        game_level_opponent_df,
        on=['posteam', 'game_id'],
        how='left'
    )
    ## calculate net wepa and apply defensive adjustment ##
    game_level_df['wepa_net'] = game_level_df['wepa'] - game_level_df['d_wepa_against']
    ## create comparison and regressions ##
    ## split into first and second halves of the season ##
    first_half_df = game_level_df.copy()
    first_half_df = first_half_df[first_half_df['game_number'] <= 8]
    second_half_df = game_level_df.copy()
    second_half_df = second_half_df[(second_half_df['game_number'] > 8) & (second_half_df['game_number'] < 17)]
    first_half_df = first_half_df.drop(columns=['game_id', 'game_number', 'wepa', 'wepa_against'])
    second_half_df = second_half_df.drop(columns=['game_id', 'game_number', 'wepa', 'wepa_against'])
    ## change margins from max to sum (since its only aggregated at the game level) ##
    new_agg_dict = {'margin' : 'sum', 'wepa_net' : 'sum',}
    first_half_df = first_half_df.groupby(['posteam', 'season']).agg(new_agg_dict).reset_index()
    second_half_df = second_half_df.groupby(['posteam', 'season']).agg(new_agg_dict).reset_index()
    ## rename the second half dict ##
    second_half_df = second_half_df.rename(columns=rename_to_last_dict)
    ## join into a single df ##
    final_df = pd.merge(
        first_half_df,
        second_half_df[['posteam', 'season', 'margin_L8']],
        on=['posteam', 'season'],
        how='left'
    )
    ## optimize ##
    ## define windowed dataset ##
    windowed_df = final_df[
        (final_df['season'] <=  season_year) &
        (final_df['season'] >= (season_year + 1) - seasons_back)
    ]
    ## set objective to be rsq ##
    model = sm.OLS(windowed_df['margin_L8'],windowed_df['wepa_net'])
    results = model.fit()
    return 1 - results.rsquared



## define function for calculating wepa given a dictionary of weights ##
def wepa_grade(weight_dict, test_df):
    ## define weights ##
    ## use vectorized mapping to look up weights from a dictionary ##
    ## play style ##
    test_df['qb_rush_weight'] = numpy.where((test_df['qb_scramble'] == 1) & (test_df['fumble_lost'] != 1), 1 + test_df['season'].map(weight_dict).str[0], 1)
    test_df['neutral_second_down_rush_weight'] = numpy.where(
        (test_df['down'] == 2) &
        (test_df['play_call'] == 'Run') &
        (test_df['yardline_100'] > 20) &
        (test_df['yardline_100'] < 85) &
        ((test_df['wp'] < .90) | (test_df['wp'] > .10)) &
        (test_df['qb_scramble'] != 1) &
        (test_df['fumble_lost'] != 1) &
        (test_df['epa'] < 0),
        1 + test_df['season'].map(weight_dict).str[1],
        1
    )
    test_df['incompletion_depth_s_weight'] = 1 + numpy.where(
        (test_df['incomplete_pass'] == 1) & (test_df['interception'] != 1),
        numpy.where(numpy.isnan(test_df['season'].map(weight_dict).str[2] * (2 * (1/(1 + numpy.exp(-0.1 * test_df['air_yards'] + .75)) - 0.5))),0,(test_df['season'].map(weight_dict).str[2] * (2 * (1/(1 + numpy.exp(-0.1 * test_df['air_yards'] + .75)) - 0.5)))),
        0
    )
    ## events ##
    test_df['non_sack_fumble_weight'] = numpy.where((test_df['sack'] != 1) & (test_df['fumble_lost'] == 1), 1 + test_df['season'].map(weight_dict).str[3], 1)
    test_df['int_weight'] = numpy.where(test_df['interception'] == 1, 1 + test_df['season'].map(weight_dict).str[4], 1)
    ## contextual ##
    test_df['goalline_weight'] = numpy.where((test_df['yardline_100'] < 3) & (test_df['down'] < 4), 1 + test_df['season'].map(weight_dict).str[5], 1)
    test_df['scaled_win_prob_weight'] = 1 + (-test_df['season'].map(weight_dict).str[6] * numpy.where(test_df['wp'] <= .5, 1/(1+numpy.exp(-10*(2*test_df['wp']-0.5)))-0.5,1/(1+numpy.exp(-10*(2*(1-test_df['wp'])-0.5)))-0.5))
    ## define defensive weights ##
    ## play style ##
    test_df['d_qb_rush_weight'] = numpy.where((test_df['qb_scramble'] == 1) & (test_df['fumble_lost'] != 1), 1 + test_df['season'].map(weight_dict).str[7], 1)
    test_df['d_neutral_second_down_rush_weight'] = numpy.where(
        (test_df['down'] == 2) &
        (test_df['play_call'] == 'Run') &
        (test_df['yardline_100'] > 20) &
        (test_df['yardline_100'] < 85) &
        ((test_df['wp'] < .90) | (test_df['wp'] > .10)) &
        (test_df['qb_scramble'] != 1) &
        (test_df['fumble_lost'] != 1) &
        (test_df['epa'] < 0),
        1 + test_df['season'].map(weight_dict).str[8],
        1
    )
    test_df['d_incompletion_depth_s_weight'] = 1 + numpy.where(
        (test_df['incomplete_pass'] == 1) & (test_df['interception'] != 1),
        numpy.where(numpy.isnan(test_df['season'].map(weight_dict).str[9] * (2 * (1/(1 + numpy.exp(-0.1 * test_df['air_yards'] + .75)) - 0.5))),0,(test_df['season'].map(weight_dict).str[9] * (2 * (1/(1 + numpy.exp(-0.1 * test_df['air_yards'] + .75)) - 0.5)))),
        0
    )
    ## events ##
    test_df['d_sack_fumble_weight'] = numpy.where((test_df['sack'] == 1) & (test_df['fumble_lost'] == 1), 1 + test_df['season'].map(weight_dict).str[10], 1)
    test_df['d_int_weight'] = numpy.where(test_df['interception'] == 1, 1 + test_df['season'].map(weight_dict).str[11], 1)
    test_df['d_fg_weight'] = numpy.where(test_df['play_type'] == 'field_goal', 1 + test_df['season'].map(weight_dict).str[12], 1)
    ## contextual ##
    test_df['d_third_down_pos_weight'] = numpy.where(
        (test_df['down'] == 3) &
        (test_df['epa'] > 0),
        1 + test_df['season'].map(weight_dict).str[13],
        1
    )
    ## add weights to list to build out headers and loops ##
    weight_names = [
        'qb_rush',
        'neutral_second_down_rush',
        'incompletion_depth_s',
        'non_sack_fumble',
        'int',
        'goalline',
        'scaled_win_prob'
    ]
    d_weight_names = [
        'd_qb_rush',
        'd_neutral_second_down_rush',
        'd_incompletion_depth_s',
        'd_sack_fumble',
        'd_int',
        'd_fg',
        'd_third_down_pos'
    ]
    ## create a second list for referencing the specifc weights ##
    weight_values = []
    for weight in weight_names:
        weight_values.append('{0}_weight'.format(weight))
    ## defense ##
    d_weight_values = []
    for weight in d_weight_names:
        d_weight_values.append('{0}_weight'.format(weight))
    ## create structures for aggregation ##
    aggregation_dict = {
        'margin' : 'max', ## game level margin added to each play, so take max to get 1 ##
        'wepa' : 'sum',
        'd_wepa' : 'sum',
        'epa' : 'sum',
    }
    headers = [
        'game_id',
        'posteam',
        'defteam',
        'season',
        'game_number',
        'margin',
        'wepa',
        'd_wepa',
        'epa'
    ]
    ## dictionary to rename second half of the season metrics ##
    rename_to_last_dict = {
        'margin' : 'margin_L8',
        'wepa_net' : 'wepa_net_L8',
        'epa_net' : 'epa_net_L8',
    }
    ## disctionary to join oppoenets epa to net out ##
    rename_opponent_dict = {
        'margin' : 'margin_against',
        'wepa' : 'wepa_against',
        'd_wepa' : 'd_wepa_against',
        'epa' : 'epa_against',
    }
    ## create wepa ##
    test_df['wepa'] = test_df['epa']
    for weight in weight_values:
        test_df['wepa'] = test_df['wepa'] * test_df[weight]
    test_df['d_wepa'] = test_df['epa'] * (1 + test_df['season'].map(weight_dict).str[14])
    for weight in d_weight_values:
        test_df['d_wepa'] = test_df['d_wepa'] * test_df[weight]
    ## bound wepa to prevent extreme values from introducing volatility ##
    test_df['wepa'] = numpy.where(test_df['wepa'] > 10, 10, test_df['wepa'])
    test_df['wepa'] = numpy.where(test_df['wepa'] < -10, -10, test_df['wepa'])
    ## defense ##
    test_df['d_wepa'] = numpy.where(test_df['d_wepa'] > 10, 10, test_df['d_wepa'])
    test_df['d_wepa'] = numpy.where(test_df['d_wepa'] < -10, -10, test_df['d_wepa'])
    ## aggregate from pbp to game level ##
    game_level_df = test_df.groupby(['posteam','defteam','season','game_id','game_number']).agg(aggregation_dict).reset_index()
    game_level_df = game_level_df.sort_values(by=['posteam','game_id'])
    game_level_df = game_level_df[headers]
    ## add net epa ##
    ## create an opponent data frame ##
    game_level_opponent_df = game_level_df.copy()
    game_level_opponent_df['posteam'] = game_level_opponent_df['defteam']
    game_level_opponent_df = game_level_opponent_df.drop(columns=['defteam','season','game_number'])
    game_level_opponent_df = game_level_opponent_df.rename(columns=rename_opponent_dict)
    ## merge to main game file ##
    game_level_df = pd.merge(
        game_level_df,
        game_level_opponent_df,
        on=['posteam', 'game_id'],
        how='left'
    )
    ## calculate net wepa and apply defensive adjustment ##
    game_level_df['wepa_net'] = game_level_df['wepa'] - game_level_df['d_wepa_against']
    ## rename ##
    game_level_df = game_level_df.rename(columns={'posteam' : 'team', 'defteam' : 'opponent'})
    ## rejoin oppoenent net wepa ##
    game_level_df_opponent = game_level_df.copy()
    game_level_df_opponent = game_level_df_opponent[['opponent', 'game_id', 'wepa_net']].rename(columns={
        'opponent' : 'team',
        'wepa_net' : 'wepa_net_opponent',
    })
    game_level_df = pd.merge(
        game_level_df,
        game_level_df_opponent,
        on=['team', 'game_id'],
        how='left'
    )
    return game_level_df
