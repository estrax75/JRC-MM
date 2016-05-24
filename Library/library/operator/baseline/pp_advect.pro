
pro get_dx_dy,x_nb,y_nb,limit,dx,dy
; Function to return area of each pixel in grid x_nb x y_nb covering longitude lon0 to lon1
; and latitude lat0 to lat1

lat0 = limit[0]
lon0 = limit[1]
lat1 = limit[2]
lon1 = limit[3]

lon_range = lon1 - lon0
lat_range = lat1 - lat0

lon_step = lon_range / x_nb
lat_step = lat_range / y_nb

lat = lat_step * findgen(y_nb) + (lat0 + lat_step/2)


;Earth radius at latitude of satellite image center pixel as measured from earth center
p0lon = lon0 + lon_range / 2.0
p0lat = lat0 + lat_range / 2.0
rad_earth_km = (6356.8 - 6378.1) / 90. * p0lat + 6378.1
rad_earth = rad_earth_km * 1000.0

;Distance rad_ctr as measured from axis earth center -> pol to the satellite center pixel
rad_ctr = rad_earth * sin((90.0 - p0lat) * !DTOR)

;Circumference at latitude p0lat; /360 * delta degree of ctr pixel
;dx/dy = distance in x/y in km valid for the satellite center pixel only
dx = make_array(x_nb,VALUE=rad_ctr * !DTOR * lon_step)

dy = rad_earth * !DTOR * lat_step * cos(lat * !DTOR) / $
                  Cos(p0lon * !DTOR) ;Latitude dependency


end


function find_rel_pixel, sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,ix_bad,ix_bad_nb

  ; Function to return new pixel for every pixel in ix_sea (2d coords in ix_sea_xy)
  ; moved by u_rel_pix in x-direction and v_rel_pix in y-direction
  ; The new pixel must be within the bounds x_nb,y_nb and must be sea

  ; Find new pixel in x & y co-ordinates
  i_x = ix_sea_xy[0,*] + u_rel_pix 
  i_y = ix_sea_xy[1,*] + v_rel_pix

  ; Initialise advection pixel to original pixel plus relative movement
  i = reform(i_y * x_nb + i_x)
  
  ; Set new pixel to -1
  ; where movement goes out of x_nb,y_nb bounds or new pixel is not sea
  ix_bad = where(i_x lt 0 or i_x ge x_nb or i_y lt 0 or i_y ge y_nb or ~sea[i],ix_bad_nb)

  if ix_bad_nb gt 0 then begin
    i[ix_bad] = -1
  endif
    
  return,i

end


function find_advect_pixel, sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,coeff,coeff0

  ; Function to return new pixel for every pixel in ix_sea (2d coords in ix_sea_xy)
  ; moved by u_rel_pix in x-direction and v_rel_pix in y-direction
  ; The new pixel must be within the bounds x_nb,y_nb and must be sea

  i = find_rel_pixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,ix_bad,ix_bad_nb)
  
  ; Move coeff to coeff0
  ; where movement goes out of x_nb,y_nb bounds or new pixel is not sea

  if ix_bad_nb gt 0 then begin
	coeff0[ix_bad] = coeff0[ix_bad] + coeff[ix_bad]
	coeff[ix_bad] = 0.0
  endif
    
  return,i

end



function create_zones, dx,dy,distance,sea,ix_sea,ix_sea_xy,x_nb,y_nb

  sample_nb = n_elements(distance[*,0,0])
  l_nb = n_elements(distance[0,*,0])
  v_nb = n_elements(distance[0,0,*])

  iu = 0
  iv = 1

  zone_nb = 4

  zone = replicate({coeff:0.0, i:0L}, sample_nb,l_nb,zone_nb)

  pixel_area = dx * dy
  
  abs_distance = abs(distance)
  
  ; Check distance is not greater than size of pixel
  ; 14/4/06 - added 0.01 since precision not sufficent (and don't really need double precision)
  ix1 = where(abs_distance[*,*,iu] gt (dx + 0.01) or abs_distance[*,*,iv] gt (dy + 0.01), ix1_nb)
  if ix1_nb gt 0 then MESSAGE,'ERROR - Distance greater than pixel size'
  
  for il = 0,l_nb - 1 do begin

    dx_du = dx - abs_distance[*,il,iu]
    dy_dv = dy - abs_distance[*,il,iv]
    u_rel_pix = -1 + (distance[*,il,iu] gt 0) * 2          ; relative displacement in u (x) direction
    v_rel_pix = -1 + (distance[*,il,iv] gt 0) * 2          ; relative displacement in v (y) direction

    coeff0 = dx_du * dy_dv / pixel_area
    zone[*,il,0].i = ix_sea

    coeff = dx_du * abs_distance[*,il,iv] / pixel_area
    zone[*,il,1].i = find_advect_pixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,0,v_rel_pix,coeff,coeff0)
    zone[*,il,1].coeff = coeff

    coeff = abs_distance[*,il,iu] * dy_dv / pixel_area
    zone[*,il,2].i = find_advect_pixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,0,coeff,coeff0)
    zone[*,il,2].coeff = coeff
	
    coeff = abs_distance[*,il,iu] * abs_distance[*,il,iv] / pixel_area
    zone[*,il,3].i = find_advect_pixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,coeff,coeff0)
    zone[*,il,3].coeff = coeff
	
	zone[*,il,0].coeff = coeff0
	
  endfor


  return, zone
    
end


function find_rel_value,sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,ix_bad,ix_bad_nb,value,bad_value

  ix=find_rel_pixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,ix_bad,ix_bad_nb)
  
  value_old = make_array(x_nb,y_nb)
  value_old[ix_sea] = value

  value_new = value_old[ix]
  
  if ix_bad_nb gt 0 then $
    value_new[ix_bad] = bad_value

  return,value_new
  
end

	

function Amdt_element,sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity,dd, X=x, Y=y

  bad_value = 0.0
  
  if keyword_set(X) then begin
    v_n = find_rel_value(sea,ix_sea,ix_sea_xy,x_nb,y_nb, 1, 0,ix_bad,ix_bad_nb,velocity,bad_value)
    v_p = find_rel_value(sea,ix_sea,ix_sea_xy,x_nb,y_nb,-1, 0,ix_bad,ix_bad_nb,velocity,bad_value)
  endif else begin
    v_n = find_rel_value(sea,ix_sea,ix_sea_xy,x_nb,y_nb, 0, 1,ix_bad,ix_bad_nb,velocity,bad_value)
    v_p = find_rel_value(sea,ix_sea,ix_sea_xy,x_nb,y_nb, 0,-1,ix_bad,ix_bad_nb,velocity,bad_value)
  
  endelse

  ; Prepare return value
  Amdt_element = make_array(SIZE=SIZE(velocity))
  
  Amdt_element = (v_n - v_p) / (2 * dd)

  return, Amdt_element

end
  

function ix_stable_courant, Am, dx, dy, dt, ix
; Function to return number of AM indexes where the Courant condition of stability is NOT fulfilled
; dt  = diffusion and advection time step
; Am = diffusion coefficient (m2.s-1)
; dx = grid-size in x (m)
; dy = grid-size in y (m)
; 
; Returns     ix_nb - number of indexes NOT fulfilling Courant condition of stability
;                ix - indexes NOT fulfilling Courant condition of stability

  ix = where(Am ne 0, ix_nb)
  if ix_nb gt 0 then $
    ix = where(dt gt (0.5 * 1.0 / (Am[ix] * (1.0/dx[ix]^2 + 1.0/dy[ix]^2))), ix_nb)
  return,ix_nb
end


function Amdt,sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity,dx,dy,dt

  C = 0.04

  iu = 0
  iv = 1
  
  u_x = Amdt_element(sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity[*,iu],dx,/X)
  u_y = Amdt_element(sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity[*,iu],dy,/Y)
  v_x = Amdt_element(sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity[*,iv],dx,/X)
  v_y = Amdt_element(sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity[*,iv],dy,/Y)

  Am = sqrt(4 * u_x^2 + 2 * (u_y + v_x)^2 + 4 * v_y^2) * dx * dy * C

  ix_nb = ix_stable_courant(Am, dx, dy, dt, ix)
  
  if ix_nb ne 0 then $
    MESSAGE,'ERROR - Courant stability not satisfied for ' + string(ix_nb) + ' points'

  Amdt = temporary(Am) * dt

  return, Amdt

end

function advect,pp,ix_sea,ix_l,zone

; Function to advect pp according to the current depth
; (advection velocities vary according to depth)


  ix_sea_nb = n_elements(ix_sea)
  zone_nb = n_elements(zone[0,0,*])
  
  pp_new = make_array(SIZE=SIZE(pp))
  
  ip = lindgen(ix_sea_nb)
  izone = lonarr(ix_sea_nb)

  ; Do zone 0 first - this is the amount that stays in the same pixel
  iz = 0L
  zone1 = zone[ip,ix_l,izone+iz]
  pp_new[zone1.i] = pp_new[zone1.i] + pp[ix_sea] * zone1.coeff

  ; Have to do the other zones like this because the index may be -1 (can't advect)
  ; and because a pixel may receive donations from more than one other pixel  
  for iz = 1L,zone_nb - 1 do begin
    zone1 = zone[ip,ix_l,izone+iz]
    ix =  where(zone1.i ge 0,ix_nb)
    if ix_nb gt 0 then begin
      i = zone1[ix].i
	  for j = 0L,ix_nb-1 do $
        pp_new[i[j]] = pp_new[i[j]] + pp[ix_sea[ix[j]]] * zone1[ix[j]].coeff
    endif
  endfor

return,pp_new

end


function find_rel_Qn,Qn,ix
  ; Find Qn at indexes ix, setting other elements to zero

  Qn_new = make_array(TYPE=SIZE(Qn,/TYPE),n_elements(ix))
  
  ix1 = where(ix ne -1,ix1_nb)
  if ix1_nb gt 0 then begin
    Qn_new[ix1] = Qn[ix[ix1]]
  endif
  
  return,Qn_new
  
end


function diffuse, Qn,Amdt,dx2,dy2,ix_sea,ix_i1j,ix_i_1j,ix_ij1,ix_ij_1

; Function to diffuse Qn according to layer (in ix_l)

  Qn_new = make_array(SIZE=SIZE(Qn))

  bad_value=0.0
  
  Qn_i1j = find_rel_Qn(Qn,ix_i1j)
  Qn_i_1j = find_rel_Qn(Qn,ix_i_1j)
  Qn_ij1 = find_rel_Qn(Qn,ix_ij1)
  Qn_ij_1 = find_rel_Qn(Qn,ix_ij_1)
 
  Q2n = 2.0 * Qn[ix_sea]
 
  Qn_new[ix_sea] = Qn[ix_sea] + Amdt * ((Qn_i1j - Q2n + Qn_i_1j) / dx2 + (Qn_ij1 - Q2n + Qn_ij_1) / dy2) 

  return, Qn_new
  
end

function degrade_coeff, temp, dt_h, eps

  return, (1.0 - Tx_deg(temp) * dt_h) > eps

end

function get_time_step, velocity, dx, dy

  iu = 0
  iv = 1

  max_velocity_u = max(abs(velocity[*,*,iu]),dimension = 2)
  max_velocity_v = max(abs(velocity[*,*,iv]),dimension = 2)

  min_dt = fltarr(2)
  ix = where(max_velocity_u gt 0, ix_nb)
  if ix_nb gt 0 then $
    min_dt[iu] = min(dx[ix] / max_velocity_u[ix])
  min_dt_y = 0.0
  ix = where(max_velocity_v gt 0, ix_nb)
  if ix_nb gt 0 then $
    min_dt[iv] = min(dy[ix] / max_velocity_v[ix])

  dt = min(min_dt)

  return,dt

end

; ===============================================
; MAIN PROGRAM

;.compile Tx_deg
;.compile pp_advect

function pp_advect,pp2d,limit,umx2d,vmx2d,ubot2d,vbot2d,tmx2d,tbot2d,depmx2d,depth2d,bfri2d,max_depth

; Function to advect pp


; Constants
; ---------

;Critical friction velocity for POM deposition
u_stard_crit = 0.005   ;m/s = 0.5 cm/s 

;Phytoplankton sinking velocity [m/s]
Vc = 5.0 / 86400.0           

;After max_depth no more advection
;max_depth = 99.9

  
; Initialisations
; ---------------

eps=0.00001


iml = 0
ibl = 1
ibot = 2
l_nb = 3

iu = 0
iv = 1
vel_nb = 2

x_nb = n_elements(umx2d[*,0])
y_nb = n_elements(umx2d[0,*])

sea = FINITE(umx2d)
ix_sea = where(sea,ix_sea_nb)

ix_sea = where(sea,ix_sea_nb)
ix_sea_xy = ARRAY_INDICES(sea,ix_sea)

velocity = fltarr(ix_sea_nb,l_nb,vel_nb)

velocity[*,iml,iu] = umx2d[ix_sea]
velocity[*,iml,iv] = vmx2d[ix_sea]
velocity[*,ibl,iu] = ubot2d[ix_sea]
velocity[*,ibl,iv] = vbot2d[ix_sea]
velocity[*,ibot,iu] = 0.0
velocity[*,ibot,iv] = 0.0

depth = depth2d[ix_sea]
depmx = depmx2d[ix_sea]

get_dx_dy,x_nb,y_nb,limit,dx1d,dy1d
area2d = dx1d # dy1d
dx = reform(dx1d[ix_sea_xy[0,*]])
dy = reform(dy1d[ix_sea_xy[1,*]])

;Compute the advection time step
dt = get_time_step(velocity,dx,dy)

dt_h = dt / 86400.0  ; time step in hours

;Compute depth step at each time step
dd = Vc * dt

zone = create_zones(dx,dy,velocity * dt,sea, ix_sea, ix_sea_xy,x_nb,y_nb)

; Am

Amdt = fltarr(ix_sea_nb,l_nb)

Amdt[*,iml] = Amdt(sea,ix_sea,ix_sea_xy,x_nb,y_nb,reform(velocity[*,iml,*],ix_sea_nb,vel_nb),dx,dy,dt)
Amdt[*,ibl] = Amdt(sea,ix_sea,ix_sea_xy,x_nb,y_nb,reform(velocity[*,ibl,*],ix_sea_nb,vel_nb),dx,dy,dt)


; Degradation fraction
T_deg_coeff = make_array(ix_sea_nb,l_nb,VALUE=-999999.99)
T_deg_coeff[*,iml] = degrade_coeff(tmx2d[ix_sea], dt_h, eps)
; N.B. bottom layer depends on depth so it has to be calculated for each depth

tmx_bot_slope = make_array(ix_sea_nb,VALUE=-999999.99)  ; bad value should never be used
ix = where(depmx ne depth, ix_nb)
if ix_nb gt 0 then $
  tmx_bot_slope[ix] = (tmx2d[ix_sea[ix]] - tbot2d[ix_sea[ix]]) / (depmx[ix] - depth[ix])       
T_deg_coeff[*,ibot] = 1.0

; Deposit fraction
vfri2d = 0.45 * bfri2d

deposit_coeff = make_array(ix_sea_nb,l_nb,VALUE=1.0)
deposit_coeff[*,ibot] = (1.0 - (vfri2d[ix_sea]^2) / (u_stard_crit^2)) > eps

; Constrain Mixed Layer depth [and bottom depth] to be less than max_depth
;depmx = depmx < max_depth
;depth = depth < max_depth

; Find pixels around sea pixels (for use with diffusion)
ix_i1j = find_rel_pixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,1,0,ix_i1j_bad,ix_i1j_bad_nb)
ix_i_1j = find_rel_pixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,-1,0,ix_i_1j_bad,ix_i_1j_bad_nb)
ix_ij1 = find_rel_pixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,0,1,ix_ij1_bad,ix_ij1_bad_nb)
ix_ij_1 = find_rel_pixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,0,-1,ix_ij_1_bad,ix_ij_1_bad_nb)

dx2 = dx^2
dy2 = dy^2

ppdxdy2d = make_array(SIZE=SIZE(pp2d))

ix_sea_pp = where(FINITE(pp2d[ix_sea]),ix_sea_pp_nb)
if ix_sea_pp_nb gt 0 then ppdxdy2d[ix_sea[ix_sea_pp]] = double(pp2d[ix_sea[ix_sea_pp]]) * area2d[ix_sea[ix_sea_pp]]

ppdxdytot = total(ppdxdy2d,/DOUBLE)
print,total(ppdxdy2d,/DOUBLE)

isea = lindgen(ix_sea_nb)

; END INITIALISATIONS
; ===================

; Advect & diffuse pp at surface

current_depth = 0
ix_l = (current_depth gt depmx) + (current_depth ge depth)    ; Find layer depending on current depth (surface here!)

; Advect
ppdxdy2d = advect(ppdxdy2d,ix_sea,ix_l,zone)

; Diffuse
ppdxdy2d = diffuse(ppdxdy2d,Amdt[isea,ix_l],dx2,dy2,ix_sea,ix_i1j,ix_i_1j,ix_ij1,ix_ij_1)

; Sink, Degrade, Advect and Diffuse until bottom is reached

; Sink
for current_depth = 5.0, max_depth, dd do begin

  ix_l = (current_depth gt depmx) + (current_depth ge depth)   ; Find layer depending on current depth

  ; Degrade
  T_deg_coeff[*,ibl] = degrade_coeff(tmx2d[ix_sea] + (current_depth - depmx) * tmx_bot_slope, dt_h, eps)
  ppdxdy2d[ix_sea] = ppdxdy2d[ix_sea] * T_deg_coeff[isea,ix_l]

  ; Advect
  ppdxdy2d = advect(ppdxdy2d,ix_sea,ix_l,zone)

  ; Diffuse
  ppdxdy2d = diffuse(ppdxdy2d,Amdt[isea,ix_l],dx2,dy2,ix_sea,ix_i1j,ix_i_1j,ix_ij1,ix_ij_1)

  PRINT,current_depth,total(ppdxdy2d,/nan,/double)

endfor

PRINT,current_depth,total(ppdxdy2d,/nan,/double)/ppdxdytot

; Do final Degradation

ix_l = (current_depth gt depmx) + (current_depth ge depth)   ; Find layer depending on current depth

T_deg_coeff[*,ibl] = degrade_coeff(tmx2d[ix_sea] + (current_depth - depmx) * tmx_bot_slope, dt_h, eps)

ppdxdy2d[ix_sea] = ppdxdy2d[ix_sea] * T_deg_coeff[isea,ix_l]

; Deposit

ppdxdy2d[ix_sea] = ppdxdy2d[ix_sea] * deposit_coeff[isea,ix_l]
ix = where(vfri2d[ix_sea] GE u_stard_crit AND ppdxdy2d[ix_sea] NE 0.0 AND ix_l eq ibot, ix_nb)
if ix_nb gt 0 then  ppdxdy2d[ix_sea[ix]] = eps

pp_adv2d = make_array(SIZE=SIZE(pp2d))
pp_adv2d[ix_sea] = ppdxdy2d[ix_sea] / area2d[ix_sea]

return, pp_adv2d

end

