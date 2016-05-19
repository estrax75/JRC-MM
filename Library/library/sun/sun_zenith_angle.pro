function sun_zenith_angle,lat_rd,delta_rd,time

  ; Function to calculate Sun Zenith at each latitude and each time
  ; Input Parameters:
  ;   lat_rd 	- latitude at which to calculate (scalar)
  ;   delta_rd 	- solar declination (scalar)
  ;   time     	- array of times (in hours) at which to calculate
  ; 3/2/2006 JC. Modified from original to allow lat_rd & time to be
  ;             arrays of the same length giving latitudes and times at which to
  ;             calculate the sunzenith angle
  ;
  ; Returns:
  ;   sun zenith angle in radians at each point in lat_rd at time time (fltarr(n_elements(lat_rd)))


  alltheta=fltarr(n_elements(lat_rd), n_elements(delta_rd))
  for i=0, n_elements(delta_rd)-1 do begin
    theta_rd=sin(lat_rd)*sin(delta_rd[i])+cos(lat_rd)*cos(delta_rd[i])*cos(float((12 - time)*2.0*!PI/24.0))
    
    theta_rd=theta_rd > (-1.0)
    theta_rd=theta_rd < 1.0
    
    theta_rd=!PI/2.0 - asin(theta_rd)
    alltheta[*,i]=theta_rd
  endfor
  
  return, alltheta
  
end
