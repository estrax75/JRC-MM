;function [dl theta_rd] = day_length_sza(lat,jday,year)
;@/library/sun/solarDeclin
;@/library/sun/yearLength
;@/library/sun/sun_Zenith_Angle
function get_dayLengthSza, lat,jday,year
  ;  % Calculate day length and sun zenith angle at noon at various latitudes
  ;  % and various days
  ;  %
  ;  % Input:
  ;  %  lat - vector - Latitude (degrees)
  ;  % jday - vector - day in year
  ;  % year - scalar - year
  ;
  ;  %
  ;  % Output
  ;  %  dl - matrix (latitude,day) - day length (hours)
  ;  %  theta_rd - matrix (latitude, day) - sun zenith angle (radians)
  ; checked! works
  dl = get_daylength(lat,jday);
  ; checked! works
  delta_rd = solarDeclin(jday,yearLength(year));
  
  lat_rd = lat * !DtoR
  ; checked! works
  theta_rd = sun_Zenith_Angle(lat_rd,delta_rd,12);
  return, {dl:dl, theta_rd:theta_rd}
  
END
