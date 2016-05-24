PRO OxyRiskOperator::removeNonResultBands

  self->cleanMainFile
  bandList=self->getBandNames()
  for i=0, n_elements(bandList)-1 do begin
    ;parInfo.outputBandName
    if strupcase(bandList[i]) ne strupcase(self.resultBandName) then self->removeBand, bandList[i]
  endfor
  
END

FUNCTION OxyRiskOperator::findAdvectPixel, sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,coeff,coeff0

  ; Function to return new pixel for every pixel in ix_sea (2d coords in ix_sea_xy)
  ; moved by u_rel_pix in x-direction and v_rel_pix in y-direction
  ; The new pixel must be within the bounds x_nb,y_nb and must be sea

  i = self->findRelPixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,ix_bad,ix_bad_nb)
  
  ; Move coeff to coeff0
  ; where movement goes out of x_nb,y_nb bounds or new pixel is not sea
  
  if ix_bad_nb gt 0 then begin
    coeff0[ix_bad] = coeff0[ix_bad] + coeff[ix_bad]
    coeff[ix_bad] = 0.0
  endif
  
  return,i
  
END

FUNCTION OxyRiskOperator::findRelPixel, sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,ix_bad,ix_bad_nb

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

FUNCTION OxyRiskOperator::findRelValue,sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,ix_bad,ix_bad_nb,value,bad_value

  ix=self->findRelPixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,ix_bad,ix_bad_nb)
  
  value_old = make_array(x_nb,y_nb)
  value_old[ix_sea] = value
  
  value_new = value_old[ix]
  
  if ix_bad_nb gt 0 then $
    value_new[ix_bad] = bad_value
    
  return,value_new
  
END

FUNCTION OxyRiskOperator::getIxStableCourant, Am, dx, dy, dt, ix
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
  
END

FUNCTION OxyRiskOperator::getAmdtElement,sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity,dd, X=x, Y=y

  bad_value = 0.0
  
  if keyword_set(X) then begin
    v_n = self->findRelValue(sea,ix_sea,ix_sea_xy,x_nb,y_nb, 1, 0,ix_bad,ix_bad_nb,velocity,bad_value)
    v_p = self->findRelValue(sea,ix_sea,ix_sea_xy,x_nb,y_nb,-1, 0,ix_bad,ix_bad_nb,velocity,bad_value)
  endif else begin
    v_n = self->findRelValue(sea,ix_sea,ix_sea_xy,x_nb,y_nb, 0, 1,ix_bad,ix_bad_nb,velocity,bad_value)
    v_p = self->findRelValue(sea,ix_sea,ix_sea_xy,x_nb,y_nb, 0,-1,ix_bad,ix_bad_nb,velocity,bad_value)
    
  endelse
  
  ; Prepare return value
  Amdt_element = make_array(SIZE=SIZE(velocity))
  
  Amdt_element = (v_n - v_p) / (2 * dd)
  
  return, Amdt_element
  
END

FUNCTION OxyRiskOperator::findRelQn, Qn, ix
  ; Find Qn at indexes ix, setting other elements to zero

  Qn_new = make_array(TYPE=SIZE(Qn,/TYPE),n_elements(ix))
  
  ix1 = where(ix ne -1,ix1_nb)
  if ix1_nb gt 0 then begin
    Qn_new[ix1] = Qn[ix[ix1]]
  endif
  
  return,Qn_new
  
end

FUNCTION OxyRiskOperator::doDiffuse, Qn,Amdt,dx2,dy2,ix_sea,ix_i1j,ix_i_1j,ix_ij1,ix_ij_1

  ; Function to diffuse Qn according to layer (in ix_l)

  Qn_new = make_array(SIZE=SIZE(Qn))
  
  bad_value=0.0
  
  Qn_i1j = self->findRelQn(Qn,ix_i1j)
  Qn_i_1j = self->findRelQn(Qn,ix_i_1j)
  Qn_ij1 = self->findRelQn(Qn,ix_ij1)
  Qn_ij_1 = self->findRelQn(Qn,ix_ij_1)
  
  Q2n = 2.0 * Qn[ix_sea]
  
  Qn_new[ix_sea] = Qn[ix_sea] + Amdt * ((Qn_i1j - Q2n + Qn_i_1j) / dx2 + (Qn_ij1 - Q2n + Qn_ij_1) / dy2)
  
  return, Qn_new
  
END

FUNCTION OxyRiskOperator::doAdvect,pp,ix_sea,ix_l,zone

  ; Function to advect pp according to the current depth
  ; (advection velocities vary according to depth)


  ix_sea_nb = n_elements(ix_sea)
  zone_nb = n_elements(zone[0,0,*])
  
  mapDataInfo=SIZE(pp)
  pp_new = make_array(SIZE=mapDataInfo)
  
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
  
END

FUNCTION OxyRiskOperator::doDegradeCoeff, temp, dt_h, eps

  return, (1.0 - self->doTxDeg(temp) * dt_h) > eps
  
END

FUNCTION OxyRiskOperator::doAmdt,sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity,dx,dy,dt

  C = 0.04
  
  iu = 0
  iv = 1
  
  u_x = self->getAmdtElement(sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity[*,iu],dx,/X)
  u_y = self->getAmdtElement(sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity[*,iu],dy,/Y)
  v_x = self->getAmdtElement(sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity[*,iv],dx,/X)
  v_y = self->getAmdtElement(sea,ix_sea,ix_sea_xy,x_nb,y_nb,velocity[*,iv],dy,/Y)
  
  Am = sqrt(4 * u_x^2 + 2 * (u_y + v_x)^2 + 4 * v_y^2) * dx * dy * C
  
  ix_nb = self->getIxStableCourant(Am, dx, dy, dt, ix)
  
  if ix_nb ne 0 then $
    MESSAGE,'ERROR - Courant stability not satisfied for ' + string(ix_nb) + ' points'
    
  Amdt = temporary(Am) * dt
  
  return, Amdt
  
END

FUNCTION OxyRiskOperator::createZones, dx,dy,distance,sea,ix_sea,ix_sea_xy,x_nb,y_nb

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
    zone[*,il,1].i = self->findAdvectPixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,0,v_rel_pix,coeff,coeff0)
    zone[*,il,1].coeff = coeff
    
    coeff = abs_distance[*,il,iu] * dy_dv / pixel_area
    zone[*,il,2].i = self->findAdvectPixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,0,coeff,coeff0)
    zone[*,il,2].coeff = coeff
    
    coeff = abs_distance[*,il,iu] * abs_distance[*,il,iv] / pixel_area
    zone[*,il,3].i = self->findAdvectPixel(sea,ix_sea,ix_sea_xy,x_nb,y_nb,u_rel_pix,v_rel_pix,coeff,coeff0)
    zone[*,il,3].coeff = coeff
    
    zone[*,il,0].coeff = coeff0
    
  endfor
  
  
  return, zone
  
END

FUNCTION OxyRiskOperator::getTimeStep, velocity, dx, dy

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
  
END

PRO OxyRiskOperator::getDxDy, x_nb, y_nb, limit, dx,dy
  ; Function to return area of each pixel in grid x_nb x y_nb covering longitude lon0 to lon1
  ; and latitude lat0 to lat1

  lat0 = limit[2]
  lon0 = limit[0]
  lat1 = limit[3]
  lon1 = limit[1]
  
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
  
  dy = rad_earth * !DTOR * abs(lat_step) * cos(lat * !DTOR) / $
    Cos(p0lon * !DTOR) ;Latitude dependency
    
    
END

;FUNCTION OxyRiskOperator::doPPAdvect, pp2d, umx2d, vmx2d, ubot2d, vbot2d, tmx2d, $
;  tbot2d, depmx2d, depth2d, bfri2d, limit, max_depth
FUNCTION OxyRiskOperator::doPPAdvect, pp2dCode, umx2dCode, vmx2dCode, ubot2dCode, vbot2dCode, tmx2dCode, $
    tbot2dCode, depmx2dCode, depth2dCode, bfri2dCode, limit, maxDepth
    
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
  
  ;eps=0.00001
  eps=float(self.app->getKeyValue('EPS'))
  ignoreValue=float(self.app->getKeyValue('NAN_VALUE'))
  
  iml = 0
  ibl = 1
  ibot = 2
  l_nb = 3
  
  iu = 0
  iv = 1
  vel_nb = 2
  
  ; you can use this as mask instead that external file...
  depth=self->getBand(depth2dCode, /NOMASK)
  
  umx2d=self->getBand(umx2dCode, /SETNAN)
  x_nb = n_elements(umx2d[*,0])
  y_nb = n_elements(umx2d[0,*])
  
  ; getMask here...
  ;seaDepth=self->getMaskBand(); seaDepth=depth
  seaDepth=depth
  ;maskCondition='maskBand eq 0'; and finite(maskBand) eq 1
  ;self->configureMaskFromMemory, depth, maskCondition
  
  ;sea = FINITE(umx2d)
  ;ix_sea = where(sea,ix_sea_nb)
  ix_sea = where(seaDepth ne ignoreValue and seaDepth gt 0,ix_sea_nb)
  
  if ix_sea_nb ne 0 then begin
    ix_sea_xy = ARRAY_INDICES(seaDepth,ix_sea)
    
    velocity = fltarr(ix_sea_nb,l_nb,vel_nb)
    
    vmx2d=self->getBand(vmx2dCode, /SETNAN)
    ubot2d=self->getBand(ubot2dCode, /SETNAN)
    vbot2d=self->getBand(vbot2dCode, /SETNAN)
    
    velocity[*,iml,iu] = umx2d[ix_sea]
    velocity[*,iml,iv] = vmx2d[ix_sea]
    velocity[*,ibl,iu] = ubot2d[ix_sea]
    velocity[*,ibl,iv] = vbot2d[ix_sea]
    velocity[*,ibot,iu] = 0.0
    velocity[*,ibot,iv] = 0.0
    
    
    depmx=self->getBand(depmx2dCode, /SETNAN)
    depth = depth[ix_sea]
    depmx = depmx[ix_sea]
    
    self->getDxDy,x_nb,y_nb,limit,dx1d,dy1d
    area2d = dx1d # dy1d
    dx = reform(dx1d[ix_sea_xy[0,*]])
    dy = reform(dy1d[ix_sea_xy[1,*]])
    
    ;Compute the advection time step
    dt = self->getTimeStep(velocity,dx,dy)
    
    dt_h = dt / 86400.0  ; time step in hours
    
    ;Compute depth step at each time step
    dd = Vc * dt
    
    zone = self->createZones(dx,dy,velocity * dt,seaDepth, ix_sea, ix_sea_xy,x_nb,y_nb)
    
    ; Am
    
    Amdt = fltarr(ix_sea_nb,l_nb)
    
    Amdt[*,iml] = self->doAmdt(seaDepth,ix_sea,ix_sea_xy,x_nb,y_nb,reform(velocity[*,iml,*],ix_sea_nb,vel_nb),dx,dy,dt)
    Amdt[*,ibl] = self->doAmdt(seaDepth,ix_sea,ix_sea_xy,x_nb,y_nb,reform(velocity[*,ibl,*],ix_sea_nb,vel_nb),dx,dy,dt)
    
    
    ; Degradation fraction
    T_deg_coeff = make_array(ix_sea_nb,l_nb,VALUE=-999999.99)
    tmx2d=self->getBand(tmx2dCode, /SETNAN)
    T_deg_coeff[*,iml] = self->doDegradeCoeff(tmx2d[ix_sea], dt_h, eps)
    ; N.B. bottom layer depends on depth so it has to be calculated for each depth
    
    tmx_bot_slope = make_array(ix_sea_nb,VALUE=-999999.99)  ; bad value should never be used
    ix = where(depmx ne depth, ix_nb)
    tbot2d=self->getBand(tbot2dCode, /SETNAN)
    if ix_nb gt 0 then $
      tmx_bot_slope[ix] = (tmx2d[ix_sea[ix]] - tbot2d[ix_sea[ix]]) / (depmx[ix] - depth[ix])
    T_deg_coeff[*,ibot] = 1.0
    
    bfri2d=self->getBand(bfri2dCode, /SETNAN)
    ; Deposit fraction
    vfri2d = 0.45 * bfri2d
    
    deposit_coeff = make_array(ix_sea_nb,l_nb,VALUE=1.0)
    deposit_coeff[*,ibot] = (1.0 - (vfri2d[ix_sea]^2) / (u_stard_crit^2)) > eps
    
    ; Constrain Mixed Layer depth [and bottom depth] to be less than max_depth
    ;depmx = depmx < max_depth
    ;depth = depth < max_depth
    
    ; Find pixels around sea pixels (for use with diffusion)
    ix_i1j = self->findRelPixel(seaDepth,ix_sea,ix_sea_xy,x_nb,y_nb,1,0,ix_i1j_bad,ix_i1j_bad_nb)
    ix_i_1j = self->findRelPixel(seaDepth,ix_sea,ix_sea_xy,x_nb,y_nb,-1,0,ix_i_1j_bad,ix_i_1j_bad_nb)
    ix_ij1 = self->findRelPixel(seaDepth,ix_sea,ix_sea_xy,x_nb,y_nb,0,1,ix_ij1_bad,ix_ij1_bad_nb)
    ix_ij_1 = self->findRelPixel(seaDepth,ix_sea,ix_sea_xy,x_nb,y_nb,0,-1,ix_ij_1_bad,ix_ij_1_bad_nb)
    
    dx2 = dx^2
    dy2 = dy^2
    pp2d=self->getBand(pp2dCode, /SETNAN)
    ppdxdy2d = make_array(SIZE=SIZE(pp2d))
    
    ix_sea_pp = where(FINITE(pp2d[ix_sea]),ix_sea_pp_nb)
    if ix_sea_pp_nb gt 0 then ppdxdy2d[ix_sea[ix_sea_pp]] = double(pp2d[ix_sea[ix_sea_pp]]) * area2d[ix_sea[ix_sea_pp]]
    
    ppdxdytot = total(ppdxdy2d,/DOUBLE)
    doLog, total(ppdxdy2d,/DOUBLE)
    
    isea = lindgen(ix_sea_nb)
    
    ; END INITIALISATIONS
    ; ===================
    
    ; Advect & diffuse pp at surface
    
    current_depth = 0
    ix_l = (current_depth gt depmx) + (current_depth ge depth)    ; Find layer depending on current depth (surface here!)
    
    ; Advect
    ppdxdy2d = self->doAdvect(ppdxdy2d,ix_sea,ix_l,zone)
    ;tvMatrix, data, subscribe, mapDataInfo
    ; Diffuse
    ppdxdy2d = self->doDiffuse(ppdxdy2d,Amdt[isea,ix_l],dx2,dy2,ix_sea,ix_i1j,ix_i_1j,ix_ij1,ix_ij_1)
    
    ; Sink, Degrade, Advect and Diffuse until bottom is reached
    
    ; Sink
    for current_depth = 5.0, maxDepth, dd do begin
    
      ix_l = (current_depth gt depmx) + (current_depth ge depth)   ; Find layer depending on current depth
      
      ; Degrade
      T_deg_coeff[*,ibl] = self->doDegradeCoeff(tmx2d[ix_sea] + (current_depth - depmx) * tmx_bot_slope, dt_h, eps)
      ppdxdy2d[ix_sea] = ppdxdy2d[ix_sea] * T_deg_coeff[isea,ix_l]
      
      ; Advect
      ppdxdy2d = self->doAdvect(ppdxdy2d,ix_sea,ix_l,zone)
      if self.app->isTestMode() then tv, bytscl(reverse(ppdxdy2d, 2))
      
      ; Diffuse
      ppdxdy2d = self->doDiffuse(ppdxdy2d,Amdt[isea,ix_l],dx2,dy2,ix_sea,ix_i1j,ix_i_1j,ix_ij1,ix_ij_1)
      if self.app->isTestMode() then tv, bytscl(reverse(ppdxdy2d, 2))
      
      doLog, current_depth, total(ppdxdy2d,/nan,/double)
      
    endfor
    
    doLog, current_depth,total(ppdxdy2d,/nan,/double)/ppdxdytot
    
    ; Do final Degradation
    
    ix_l = (current_depth gt depmx) + (current_depth ge depth)   ; Find layer depending on current depth
    
    T_deg_coeff[*,ibl] = self->doDegradeCoeff(tmx2d[ix_sea] + (current_depth - depmx) * tmx_bot_slope, dt_h, eps)
    
    ppdxdy2d[ix_sea] = ppdxdy2d[ix_sea] * T_deg_coeff[isea,ix_l]
    
    ; Deposit
    
    ppdxdy2d[ix_sea] = ppdxdy2d[ix_sea] * deposit_coeff[isea,ix_l]
    aa=where(ppdxdy2d gt 0, c1)
    if c1 gt 0 then doLog, 'check ppdxdy2d[ix_sea]', LEVEL=4
    ix = where(vfri2d[ix_sea] GE u_stard_crit AND ppdxdy2d[ix_sea] NE 0.0 AND ix_l eq ibot, ix_nb)
    if ix_nb gt 0 then  ppdxdy2d[ix_sea[ix]] = eps
    
    pp_adv2d = make_array(SIZE=SIZE(pp2d))
    pp_adv2d[ix_sea] = ppdxdy2d[ix_sea] / area2d[ix_sea]
  endif else begin
    doLog, 'No sea in file (check '+depth2dCode+' band contents)', LEVEL=4
    pp_adv2d=seaDepth
  endelse
  
  return, pp_adv2d
  
END

;'depmx','depth'/'dep2d','tmx','tbot','umx','ubot','vmx','vbot','bfri'/'ustar','Cadvmx','Cstrat','Cphys_bott_PSA','Cphys_bott_OXYRISK'
;'tmx','Cadvmx','Cphys_bott_PSA','Cphys_bott_OXYRISK'
FUNCTION OxyRiskOperator::doCPOM, pp_adv, PP_TYPE=pp_type

  ; Modified threshold to reflect the fact that pp is now in g.day-1
  ; not month
  if ~keyword_set(pp_type) then $
    MESSAGE,"ERROR - must set type of Primary Production"
    
  case pp_type of
    "chl" : pp_thresh = 0.64  ; pp = sqrt(chlor_a)
    "MODIS" : pp_thresh = 0.025 ; MODIS PP data
    ;  "MARK" : pp_thresh = 0.041 ; Mark's PP data     - 97.5 percentile
    ;  "MARK" : pp_thresh = 0.0355872 ; Mark's PP data - 99 percentile
    ;  "MARK" : pp_thresh = 0.025 ; Mark's PP data - use same as original
    "MARK" : pp_thresh = 0.75 ; Mark's PP data - use same as original but for g per day not month
  else  : MESSAGE,"ERROR - Unknown type of Primary Production: " + pp_type
endcase

CPOM = pp_adv * pp_thresh

return,CPOM

end

FUNCTION OxyRiskOperator::doComputation, archiveDir, year, month, targetMapInfo, COPY=COPY, OPEN=OPEN, filename=filename, TEST=TEST

  maxDepth=float(self.app->getKeyValue('MAX_DEPTH'))
  ppType=self.app->getKeyValue('PP_TYPE')
  ignoreValue=float(self.app->getKeyValue('NAN_VALUE'))
  
  limit=targetMapInfo.mapWindowBoundary
  if n_elements(filename) eq 1 then self->setMainFileName, filename, COPY=COPY, OPEN=OPEN
  
  parInfo=self.app->getParameterByOutputBandName('umx')
  ;umx=self->getBand(parInfo.outputBandName)
  umxCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('vmx')
  ;vmx=self->getBand(parInfo.outputBandName)
  vmxCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('ubot')
  ;ubot=self->getBand(parInfo.outputBandName)
  ubotCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('vbot')
  ;vbot=self->getBand(parInfo.outputBandName)
  vbotCode=parInfo.outputBandName
  
  ;parInfo=self.app->getParameterByOutputBandName('depth')
  parInfo=self.app->getParameterByOutputBandName('dep2d')
  depthCode=parInfo.outputBandName
  depth=self->getBand(depthCode, /NOMASK)
  
  parInfo=self.app->getParameterByOutputBandName('depmx')
  ;depmx=self->getBand(parInfo.outputBandName)
  depmxCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('CTxDeg')
  ;CTxDeg=self->getBand(parInfo.outputBandName)
  CTxDegCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('Cstrat')
  ;Cstrat=self->getBand(parInfo.outputBandName)
  CstratCode=parInfo.outputBandName
  
  ;parInfo=self.app->getParameterByOutputBandName('bfri')
  parInfo=self.app->getParameterByOutputBandName('ustar')
  ;bfri=self->getBand(parInfo.outputBandName)
  bfriCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('tbot')
  ;tbot=self->getBand(parInfo.outputBandName)
  tbotCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('tmx')
  ;tbot=self->getBand(parInfo.outputBandName)
  tmxCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('Cadvmx')
  ;tbot=self->getBand(parInfo.outputBandName)
  cAdvMix=parInfo.outputBandName
  
  ;parInfo=self.app->getParameterByOutputBandName('bfri')
  parInfo=self.app->getParameterByOutputBandName('Cbfri')
  ;cbfri=self->getBand(parInfo.outputBandName)
  cbfriCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('oxysat')
  ;cbfri=self->getBand(parInfo.outputBandName)
  oxysatCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('cAdvbl')
  ;cAdvbl=self->getBand(parInfo.outputBandName)
  cAdvblCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('cAdvMix')
  ;cAdvMixe=self->getBand(parInfo.outputBandName)
  cAdvMixCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('Cblt')
  ;Cblt=self->getBand(parInfo.outputBandName)
  CbltCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('PP')
  ;cbfri=self->getBand(parInfo.outputBandName)
  ppCode=parInfo.outputBandName
  
  pp2d=self->getBand(ppCode, /SETNAN)
  ix_cloud = where(~FINITE(pp2d),ix_cloud_nb)
  ;ix_cloud = where(pp2d ne ignoreValue,ix_cloud_nb)
  if ~keyword_set(TEST) then begin
    pp_adv=self->doPPAdvect(ppCode, umxCode, vmxCode, ubotCode, vbotCode, tmxCode, $
      tbotCode, depmxCode, depthCode, bfriCode, limit, maxDepth)
  endif else begin
    pp_adv=pp2d
  endelse
  
  if ix_cloud_nb gt 0 then $
    pp_adv[ix_cloud] = !VALUES.F_NAN
  ; Cphys_bott - Bottom Layer Physics Index (for PSA)
  ; ----------
  cPom = self->doCPOM(pp_adv, PP_TYPE=ppType)
  ; some bands are previously computed (and stored) in PSA step
  Cbfri=self->getBand(cbfriCode, /SETNAN)
  coxySat=self->getBand(oxySatCode, /SETNAN)
  cstrat=self->getBand(CstratCode, /SETNAN)
  CTxDeg=self->getBand(CTxDegCode, /SETNAN)
  cAdvMix=self->getBand(cAdvMixCode, /SETNAN)
  Cadvbl=self->getBand(cAdvblCode, /SETNAN)
  cblt=self->getBand(CbltCode, /SETNAN)
  
  ; Cphys_bott - Bottom Layer Physics Index (for OXYRISK)
  ; ----------
  
  ;res.Cphys_bott_OXYRISK = Cphys_bott(res.Cbfri, res.Coxy_sat, res.Cstrat, res.CTx_deg, res.Cadvmx, res.Cadvbl, res.Cblt, sea.depth, max_depth, WEIGHT=1)
  CphysBottOXYRISK = self->doCphysBott(Cbfri, coxySat, cstrat, CTxDeg, cAdvMix, Cadvbl, cblt, depth, maxDepth, WEIGHT=1)
  
  oxyRisk = self->doResult(cPom, CphysBottOXYRISK)
  self->addBand, oxyRisk, 'oxyrisk_adv', /MEMORY, /OVERWRITE
  self->removeBand, oxysatCode
  self->addBand, coxySat, oxysatCode, /MEMORY, /OVERWRITE
  
  maskCondition='maskBand eq 0'; and finite(maskBand) eq 1
  self->configureMaskFromMemory, depth, maskCondition
  
END

function OxyRiskOperator::doResult, CPOM, Cphys_bott

  ; OXYRISK
  ; -------

  OXYRISK = ((CPOM + Cphys_bott) / 2.0) < 1.0
  
  return, OXYRISK
  
end

;PRO OxyRiskOperator::fillPPData, periodType, month, year, ppDataDir, ppReadFunction, FOUND=FOUND;, prepareDataDir
;
;  FOUND=0
;  ppCode='PP'
;;  ppFileName=call_function('buildPPVarFileName'+'_'+periodType, $
;;    year, month, periodType, roi, sensor, ppCode, ppDataDir, /FULLPATH)
;  ppFileName=call_function(ppReadFunction+'_'+periodType, $
;    year, month, periodType, roi, sensor, ppCode, ppDataDir, /FULLPATH)
;
;  fInfo=file_info(ppFileName)
;  if fInfo.exists then begin
;    ppParInfo=self.app->getParameterByCode(ppCode)
;    ppDataValues=self->readNcdfVar(ppFileName, ppParInfo.outputBandName, FOUND=FOUND, /REVERSE)
;    if keyword_set(FOUND) then self->addBand, ppDataValues.data, ppCode, /MEMORY
;  endif
;
;END

PRO OxyRiskOperator::fillPPData, periodType, month, year, ppDataDir, ppReadFunction, FOUND=FOUND, ignoreValue=ignoreValue;, prepareDataDir

  FOUND=0
  ppCode='PP'
  ;  ppFileName=call_function('buildPPVarFileName'+'_'+periodType, $
  ;    year, month, periodType, roi, sensor, ppCode, ppDataDir, /FULLPATH)
  
  ;dataInfo=self->readDataFromfile(periodType, month, year, ppDataDir, ppReadFunction, ppCode, FOUND=FOUND, ignoreValue=ignoreValue);, prepareDataDir
  ppFileName=call_function(ppReadFunction+'_'+periodType, $
    year, month, periodType, roi, sensor, ppCode, ppDataDir, /FULLPATH)
    
  fInfo=file_info(ppFileName)
  if fInfo.exists then begin
    ppParInfo=self.app->getParameterByCode(ppCode)
    scaleType=self->readNcdfAttr(ppFileName, ppParInfo.outputBandName, 'scaling', FOUND=FOUNDST);/GLOBAL
    ppDataValues=self->readNcdfVar(ppFileName, ppParInfo.outputBandName, FOUND=FOUND, /REVERSE)
    if FOUNDST and strlowcase(scaleType) eq 'log10' then begin
      ignoreCount=0
      if n_elements(ignoreValue) eq 1 then ignoreIdxs=where(ppDataValues.data eq ignoreValue, ignoreCount)
      ppDataValues.data=10^ppDataValues.data
      if ignoreCount ne 0 then ppDataValues.data[ignoreIdxs]=ignoreValue
    endif
    if keyword_set(FOUND) then self->addBand, ppDataValues.data, ppCode, /MEMORY
  endif
;  if keyword_set(FOUND) then self->addBand, dataInfo.data, ppCode, /MEMORY
  
END

;PRO OxyRiskOperator::fillPPData, periodType, month, year, ppDataDir, ppReadFunction, FOUND=FOUND, ignoreValue=ignoreValue;, prepareDataDir
;
;  FOUND=0
;  ppCode='PP'
;  ;  ppFileName=call_function('buildPPVarFileName'+'_'+periodType, $
;  ;    year, month, periodType, roi, sensor, ppCode, ppDataDir, /FULLPATH)
;  ppFileName=call_function(ppReadFunction+'_'+periodType, $
;    year, month, periodType, roi, sensor, ppCode, ppDataDir, /FULLPATH)
;    
;  fInfo=file_info(ppFileName)
;  if fInfo.exists then begin
;    ppParInfo=self.app->getParameterByCode(ppCode)
;    scaleType=self->readNcdfAttr(ppFileName, ppParInfo.outputBandName, 'scaling', FOUND=FOUNDST);/GLOBAL
;    ppDataValues=self->readNcdfVar(ppFileName, ppParInfo.outputBandName, FOUND=FOUND, /REVERSE)
;    if FOUNDST and strlowcase(scaleType) eq 'log10' then begin
;      ignoreCount=0
;      if n_elements(ignoreValue) eq 1 then ignoreIdxs=where(ppDataValues.data eq ignoreValue, ignoreCount)
;      ppDataValues.data=10^ppDataValues.data
;      if ignoreCount ne 0 then ppDataValues.data[ignoreIdxs]=ignoreValue
;    endif
;    if keyword_set(FOUND) then self->addBand, ppDataValues.data, ppCode, /MEMORY
;  endif
;  
;END

; 1.
FUNCTION OxyRiskOperator::doCadv, uCode, vCode

  ; --- Cadv - Advection Index

  thresh_adv = 0.09 ;m/s
  Cadv = self->doCIndex(self->doUvVelocity(uCode, vCode), thresh_adv)
  
  return,Cadv
  
END

;2.
FUNCTION OxyRiskOperator::doUvVelocity, uCode, vCode

  ;------------------------  Advection  -------------------

  ;u=self->readNcdfVar(self.mainFileName, uCode, REVERSE=REVERSE)
  ;v=self->readNcdfVar(self.mainFileName, vCode, REVERSE=REVERSE)
  u=self->getBand(uCode)
  v=self->getBand(vCode)
  
  return, sqrt(u^2 + v^2)
  
end

;3.
FUNCTION OxyRiskOperator::doCIndex, value, thresh

  eps=float(self.app->getKeyValue('EPS'))
  
  ; Very low value for initialisations of minima
  ;eps=0.00001
  
  Cindex = (1.0 - value / thresh) > eps < 1.0
  
  return, Cindex
  
end


FUNCTION OxyRiskOperator::doCstrat, sigmCode

  ;sign=self->readNcdfVar(self.mainFileName, sigmCode, REVERSE=REVERSE)
  eps=float(self.app->getKeyValue('EPS'))
  
  sigm=self->getBand(sigmCode)
  thresh_sigm = 0.07    ;kg/m4
  
  Cstrat = (sigm / thresh_sigm) > eps < 1.
  
  return, Cstrat
  
END

FUNCTION OxyRiskOperator::doCblt, depthCode, depmxCode, cstratValues

  eps=float(self.app->getKeyValue('EPS'))
  
  ;depth=self->readNcdfVar(self.mainFileName, depthCode, REVERSE=REVERSE)
  ;depmx=self->readNcdfVar(self.mainFileName, depmxCode, REVERSE=REVERSE)
  depth=self->getBand(depthCode, /NOMASK)
  depmx=self->getBand(depmxCode)
  
  blt_min = 0.0
  blt_max = 40.0
  Cstrat_min = 0.2
  Cstrat_max = 1.0
  
  blt = depth - depmx
  
  CbltValues = 0.00005 * blt^3 - 0.0032 * blt^2 + 0.0199 * blt + $
    0.9901
    
  ix = where(blt ge blt_max or blt le blt_min or cstratValues lt Cstrat_min or cstratValues gt Cstrat_max,ix_nb)
  
  if ix_nb gt 0 then CbltValues[ix] = 0.0
  
  return, CbltValues > eps < 1.0
  
END

FUNCTION OxyRiskOperator::doCbfri, bfriCode

  ;bfri=self->readNcdfVar(self.mainFileName, bfriCode, REVERSE=REVERSE)
  bfri=self->getBand(bfriCode)
  
  ; --- Cbfri - Friction Velocity Index
  thresh_bfri = 0.0170
  Cbfri = self->doCIndex(bfri,thresh_bfri)
  
  return,Cbfri
  
  
END

FUNCTION OxyRiskOperator::doCoxySat, tbotCode, sbotCode

  eps=float(self.app->getKeyValue('EPS'))
  ;  tbot=self->readNcdfVar(self.mainFileName, tbotCode, REVERSE=REVERSE)
  ;  sbot=self->readNcdfVar(self.mainFileName, sbotCode, REVERSE=REVERSE)
  tbot=self->getBand(tbotCode)
  sbot=self->getBand(sbotCode)
  
  temp_min = 8.0
  sal_min = 5.0
  temp_max = 22.0
  sal_max = 38.0
  
  ;Weiss 1970 Deep-Sea Research
  oxy_sat_min = self->doOxySat(temp_max, sal_max)
  
  ;Calculate Oxygen Saturation Index
  Coxy_sat = (1. - ((self->doOxySat(tbot, sbot) - oxy_sat_min) / (self->doOxySat(temp_min, sal_min) - oxy_sat_min)))
  
  return, Coxy_sat > eps < 1.0
  
  
END

FUNCTION OxyRiskOperator::doOxySat, teta, sal

  ;Weiss 1970 Deep-Sea Research
  tabs = 273.15 + teta  ; absolute temperature in Kelvin
  oxy_sat = exp(-173.4292 + 249.6339 * (100. / tabs) + 143.3483 * alog(tabs / 100.) $
    -21.8492 * tabs / 100. + sal * (-0.033096 + 0.014259 * tabs / 100. $
    -0.0017 * (tabs / 100.)^2))
    
  return, oxy_sat
END

;=========================================================================
;Function to calculate the bottom degradation velocity given the temperature input variable
;=========================================================================
FUNCTION OxyRiskOperator::doTxDeg, values

  ;degradation rate = f(temperature bottom)
  ;Q10=2 [Eppley 1972]: f(T) = exp(0.07 * T)
  ;Low T values of deg. rate in Heiskanen (PhD) and high T in Avnimelech (1995)

  Tx_deg = 0.0264 * exp(0.07 * values)
  
  return, Tx_deg
  
END

FUNCTION OxyRiskOperator::doCTxDeg, values

  eps=float(self.app->getKeyValue('EPS'))
  Tmin = 2.0
  Tmax = 24.0
  
  Tx_deg_min = self->doTxDeg(Tmin)
  
  ;Calculate the Index
  ;  CTx_deg = ((Tx_deg - Tx_deg_min) / $
  ;    (self->doTxDeg(Tmax) - Tx_deg_min))
  CTx_deg = ((values - Tx_deg_min) / $
    (self->doTxDeg(Tmax) - Tx_deg_min))
    
  return, CTx_deg  > eps  < 1.0
  
END

FUNCTION OxyRiskOperator::doCphysBott, Cbfri, CoxySat, Cstrat, CTxDeg, Cadvmx, Cadvbl, Cblt, depth, maxDepth, WEIGHT=weight
  ; --- Cphys_bott - Bottom Layer Physics Index (for PSA)

  ; weight should be 2 for Cphys_bott used in PSA calculation and
  ;                  1 for                    OXYRISK calculation

  if n_elements(weight) eq 0 then MESSAGE,"ERROR - must set weight for Cbfri"
  
  Cstrat_thresh = 0.2
  
  ; Initialise return array
  
  cphysBott = make_array(SIZE=size(Cstrat),VALUE=!VALUES.F_NAN)
  
  ; Only calculate where depth less than max depth
  
  ix_depth = where(depth le maxDepth,ix_depth_nb)
  
  if ix_depth_nb gt 0 then begin
  
    cphysBott[ix_depth] = (weight * Cbfri[ix_depth]) + CoxySat[ix_depth] + Cstrat[ix_depth] + CTxDeg[ix_depth]
    
    ix_depth_strat = where(Cstrat[ix_depth] lt Cstrat_thresh,ix_depth_strat_nb $
      ,complement=ix_depth_nostrat,ncomplement=ix_depth_nostrat_nb)
      
    if ix_depth_strat_nb gt 0 then begin
      ix = ix_depth[ix_depth_strat]
      cphysBott[ix] = (cphysBott[ix] + Cadvmx[ix]) / (4.0 + weight)
    endif
    
    if ix_depth_nostrat_nb gt 0 then begin
      ix = ix_depth[ix_depth_nostrat]
      cphysBott[ix] = (cphysBott[ix] + Cadvbl[ix] + Cblt[ix]) / (5.0 + weight)
    endif
    
  endif
  
  return,cphysBott
end

FUNCTION OxyRiskOperator::doPSA, CphysBott, CphysSurf

  ; PSA
  ; ---

  PSA = (CphysBott + CphysSurf) / 2.0
  
  return,PSA
  
end

;PRO OxyRiskOperator::writeResult, month, year, archiveDir
;
;  prevLog=self.log
;  self.log=1
;  self->writeAsNCDF, month, year, archiveDir
;  self.log=prevLog
;
;END

PRO OxyRiskOperator::cleanMainFile

  parInfo=self.app->getParameterByOutputBandName('umx')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('vmx')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('ubot')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('vbot')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('rhomax')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('dep2d')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('depmx')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('Cstrat')
  self->removeBand, parInfo.outputBandName
  
  ;parInfo=self.app->getParameterByOutputBandName('bfri')
  parInfo=self.app->getParameterByOutputBandName('ustar')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('tbot')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('sbot')
  self->removeBand, parInfo.outputBandName
  
END


;FUNCTION OxyRiskOperator::buildPSAFileName, archiveDir, year, date, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL
;
;  prefix="EMIS_" ;PSA_
;
;  fileName=string(format='(A, I4, "_", I02, "_MO", ".nc")', prefix, year, date)
;  if keyword_set(INTERVAL) then begin
;    sjday = julday(date,1,year) - julday(1,1,year) + 1;
;    ejday = julday(date+1,1,year) - julday(1,1,year);
;    fileName=string(format='(A, I4, I03, "_", I4, I03, "_MO", ".nc")', prefix, year, sjday, year, ejday)
;  endif
;  if keyword_set(JULDAY) then fileName=string(format='(A, I4, I03, "_", "_MO", ".nc")', prefix, year, date)
;
;  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
;
;END

FUNCTION OxyRiskOperator::buildOperatorResultFileName, dType, displayName, month, year

  ;return, buildPSAVarFileName(dType, displayName, month, year)
  return, call_function('buildPSAVarFileName'+'_'+dType, $
    year, month, dType, roi, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
    
END

PRO OxyRiskOperator::CleanUp

  self-> GenericOperator::Cleanup
  
END

FUNCTION OxyRiskOperator::init, application, workingDir, periodType, ppData, mask=mask, geoData=geoData, fileName=fileName, $
    OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE, bandToExportList=bandToExportList
    
  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, fileName=fileName, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE, bandToExportList=bandToExportList)) then return, 0
  return, 1
  
END

PRO OxyRiskOperator__Define

  Struct = { OxyRiskOperator , $
    Inherits GenericOperator $
    }
    
END