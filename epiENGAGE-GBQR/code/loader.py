from pathlib import Path
import glob

from itertools import product

import numpy as np
import pandas as pd

import pymmwr

from datetime import datetime, timedelta
from pandas.tseries.holiday import USFederalHolidayCalendar

def epiweeks_in_year(year):
    """
    Determine if a year has 53 weeks based on the first Thursday rule.
    """
    first_day_of_year = datetime(year, 1, 1)
    day_of_week = first_day_of_year.weekday()  # Monday = 0, Sunday = 6
    first_thursday = first_day_of_year + timedelta((3 - day_of_week) % 7)

    return 53 if first_thursday.timetuple().tm_yday <= 4 else 52

def convert_epiweek_to_season_week(years, epiweeks):
    """
    Convert epiweek to season week.
    Args:
        years: array of years (e.g., [2023, 2023])
        epiweeks: array of epiweeks (e.g., [31, 40])
    Returns:
        season_weeks: array of season weeks
    """
    season_weeks = epiweeks - 30
    update_inds = np.where(season_weeks <= 0)[0]

    for i in update_inds:
        prev_year = years[i] - 1
        season_weeks[i] += epiweeks_in_year(prev_year)

    return season_weeks


def get_season_hol(start_year):
  holiday_cal = USFederalHolidayCalendar()
  hol = holiday_cal.holidays(
    start=datetime(year=start_year, month=7, day=1),
    end=datetime(year=start_year+1, month=6, day=1),
    return_name=True)
    
  hol = hol.reset_index()
  hol.columns = ['date', 'holiday']
  hol = hol.loc[hol['holiday'].isin(['Thanksgiving Day', 'Christmas Day'])]
  
  hol['season'] = str(start_year) + '/' + str(start_year + 1)[-2:]
  
  return hol


def convert_datetime_to_season_week(row, date_col_name):
  # Use pymmwr to get epiweek information
  ew = pymmwr.date_to_epiweek(row[date_col_name].date())
  
  # Format epiweek into 'yyyyww' string
  year = ew.year
  week = ew.week

  return convert_epiweek_to_season_week(np.array([year]), np.array([week]))[0]

def get_holidays():
  hol = pd.concat([get_season_hol(sy) for sy in range(1997, 2024)],
                  ignore_index=True)
  # Apply the correct function to get season weeks
  hol['season_week'] = hol.apply(
      lambda row: convert_datetime_to_season_week(row, 'date'), axis=1
  )
  
  return hol[['season', 'holiday', 'date', 'season_week']]

def adjust_year_based_on_target_end_date(df):
    """
    Adjusts the `year` column based on `wk_end_date` to correctly assign flu seasons.

    - If `wk_end_date` is on or before January 6, the year is adjusted to the previous year.
    - However, if `epiweek` is 1, the year remains unchanged even if `wk_end_date` is before January 6.
    
    This adjustment ensures that flu seasons are assigned correctly without misclassifying early January data.
    """
    # Convert `wk_end_date` to datetime format
    df['wk_end_date'] = pd.to_datetime(df['wk_end_date'])

    # Define condition: Adjust year if `wk_end_date` is ≤ January 6 AND `epiweek` is not 1
    mask = (df['wk_end_date'].dt.month == 1) & (df['wk_end_date'].dt.day <= 6) & (df['epiweek'] != 1)

    # Adjust the year for selected rows
    df.loc[mask, 'year'] = df['year'] - 1

    return df

