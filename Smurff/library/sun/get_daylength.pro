;pro get_daylength
;
;
;dl_str=read_ascii('/modis/pp_images/data/dl_info.txt')
; lat in degrees!!!
function get_daylength, lat, jday

  dlengths=fltarr(n_elements(lat), n_elements(jday))
  
  phi=lat
  
  for i=0, n_elements(jday)-1 do begin
    gamma1 = 2.*!PI*(jday[i]-1)/365
    delta1 = (0.006918 - 0.399912*COS(gamma1) +0.07257*SIN(gamma1) - 0.006758*COS(2*gamma1) $
      + 0.000907*SIN(2*gamma1) - 0.002697*COS(3*gamma1) - 0.00148*SIN(3*gamma1))
    dlengths[*,i]=(2.0*ACOS(-TAN(phi*!PI/180.0)*TAN(delta1))*180/!PI)/15.0
  endfor
  
  return, dlengths
  
end
