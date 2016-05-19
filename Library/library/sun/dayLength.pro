function dayLength, lat_rd, delta_rd
  ;% Function to calculate sun-earth distance, solar_declination and day length
  ;% for each day in jday array and each latitude in lat_rd
  ;% N.B. Sun-earth distance and delta_rd are not dependant on latitude
  ;%
  ;% Modified 7/7/08 J.Challis Mod1
  ;%   Handle case where lat_rd and delta_rd have same dimension
  ;%   i.e. just find dl at lat_rd and delta_rd
  ;%   rather than finding dl for lat_rd samples at each of delta_rd angles
  ;%
  ;% Input parameters:
  ;%   p_jday 	: Array of days for which to make calculations
  ;%   year_length : Length of year in days for which to calculate
  ;%   p_lat_rd	: Array of latitudes in radians for which to make calculations
  ;%
  ;%
  ;% Returns structure:   	{
  ;%    sun_earth_dist	:fltarr(n_elements(jday)),
  ;%    delta_rd          	:fltarr(n_elements(jday)),    ; (in radians)
  ;%    day_length        	:fltarr(n_elements(jday),n_elements(lat_rd))
  ;% 			}
  ;
  ;% ----------------------------------------------------------
  ;
  ;% Calculate Day length at each latitude in lat_rd
  ;
  ;% Mod1
  ;%[tandelta_rd, tanlat_rd] = meshgrid(tan(delta_rd),-tan(lat_rd));
  ;%dl=tandelta_rd .* tanlat_rd ;

  if n_elements(lat_rd) eq n_elements(delta_rd) then begin
    dl=tan(delta_rd) * (- tan(lat_rd)) ;
  endif else begin
    meshRes = meshgrid(tan(delta_rd),-tan(lat_rd));, [n_elements(lat_rd),n_elements(delta_rd)] );
    dl=meshres.meshRows * meshres.meshCols ;
  endelse
  
  ;% Argument to acos must not be > 1
  idxs = where(dl lt -1, count)
  if count ne 0 then dl[idxs]=-1
  ;dl(dl < (-1.0)) = -1.0;
  idxs = where(dl gt 1, count)
  if count ne 0 then dl[idxs]=1
  ;dl(dl >  1.0) = 1.0;
  dl = (2.0 / 15.0)/!dtor * acos(dl);
  
  ;doLog, dl[14,14];
  ;doLog, dl[19,24];
  ;doLog, dl[27,0];
  ;doLog, dl[20,17];
  
  return, dl
 
 end
