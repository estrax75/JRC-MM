function solarDeclin, jday, year_length

  ;% Function to calculate solar_declination
  ;% for each day in jday array
  ;%
  ;% Input parameters:
  ;%   jday 	: Array of days for which to make calculations
  ;%   year_length : Length of year in days for which to calculate
  ;%
  ;%
  ;% Returns:
  ;%    delta_rd          	:fltarr(n_elements(jday)),   (radians)

  ;  % ----------------------------------------------------------
  ;
  ;
  ;  % Calculate Solar declination for each day in array jday

  gam = 2.0 * !PI * jday / year_length;
  delta_rd = 0.006918 $
    - 0.399912 * cos(gam) $
    + 0.07257  * sin(gam) $
    - 0.006758 * cos(2.0 * gam) $
    + 0.000907 * sin(2.0 * gam) $
    - 0.002697 * cos(3.0 * gam) $
    + 0.00148 * sin(3.0 * gam);
  ;doLog, delta_rd
  return, delta_rd
  
END