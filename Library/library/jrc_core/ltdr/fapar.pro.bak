PRO FAPAR,sensor,sensorCode,ttasun,ttaview,phisun,phiview,$
  BRF_ToA_BLUE,BRF_ToA_RED,BRF_ToA_NIR,$
  D_BRF_ToA_BLUE,D_BRF_ToA_RED,D_BRF_ToA_NIR, $
  rhoRED, rhoNIR, D_rhoRED, D_rhoNIR, $
  D_rhotildeBLUE, D_rhotildeRED, D_rhotildeNIR, VI, D_VI, toc=TOC,$
  MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX
  ;
  ;
  ; compute the fapar (=VI) and the associated uncertaintied (=D_VI) as function as the bands uncertainties
  ; D_BRF_ToA_BLUE,D_BRF_ToA_RED,D_BRF_ToA_NIR expressed in sigma !
  ;
  ; 2014, NG
  ;
  ; The inputs are
  ; sensor = nickname of instrument
  ; sun,ttaview,phisun,phiview = the angular value of sun zenith, view zenith,
  ; sun azimuth and view azimuth in degrees
  ; BRF_ToA_BLUE,BRF_ToA_RED,BRF_ToA_NIR = the actual value of BRF TOA in blue, red and near-infrared
  ;
  ; 2007, NG
  ;
  ;Convert angular values to radians

  ;stop

  ttasun  *= !dtor
  ttaview *= !dtor
  phisun  *= !dtor
  phiview *= !dtor

  coeffInfo=getSensorCoeffs(sensor, sensorCode, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX)
  
  RahmanCoeffs_RED=coeffInfo.RahmanCoeffs_RED
  RahmanCoeffs_NIR=coeffInfo.RahmanCoeffs_NIR
  avTags=strupcase(tag_names(coeffInfo))
  print, 'veg coeff (b1)-->', RahmanCoeffs_RED
  print, 'veg coeff (b2)-->', RahmanCoeffs_NIR
  idx=where(strupcase('RahmanCoeffs_BLUE') eq avTags, count)
  if count eq 1 then RahmanCoeffs_BLUE=coeffInfo.RahmanCoeffs_BLUE
  G0coeffs=coeffInfo.G0coeffs
  G1coeffs=coeffInfo.G1coeffs
  G2coeffs=coeffInfo.G2coeffs
  ;For each band, 'rectification' value from Rahman
  ;window, 0, xsize=720, ysize=360, title='ttasun '+SENSOR
  ;tv, reverse(congrid(ttasun, 720,360),2)

  ;stop

  ; check order of params
  ; k, theta, rho
  F_red=F(ttasun,ttaview,phisun,phiview,RahmanCoeffs_RED[0],RahmanCoeffs_RED[1],RahmanCoeffs_RED[2])
  F_nir=F(ttasun,ttaview,phisun,phiview,RahmanCoeffs_NIR[0],RahmanCoeffs_NIR[1],RahmanCoeffs_NIR[2])
  IF NOT(KEYWORD_set(toc)) THEN F_blue=F(ttasun,ttaview,phisun,phiview,RahmanCoeffs_BLUE[0],RahmanCoeffs_BLUE[1],RahmanCoeffs_BLUE[2])
;  F_red=F(ttasun,ttaview,phisun,phiview,RahmanCoeffs_RED[0],RahmanCoeffs_RED[1],RahmanCoeffs_RED[2])
;  F_nir=F(ttasun,ttaview,phisun,phiview,RahmanCoeffs_NIR[0],RahmanCoeffs_NIR[1],RahmanCoeffs_NIR[2])
;  IF NOT(KEYWORD_set(toc)) THEN F_blue=F(ttasun,ttaview,phisun,phiview,RahmanCoeffs_BLUE[0],RahmanCoeffs_BLUE[1],RahmanCoeffs_BLUE[2])
  ;
  ;
  ; Estimate 'rectified' channels and associated Deltas
  ; abs() is omitted as derivatives are 1/F() which is always positive.

  rhotildeRED=abs(BRF_ToA_RED/F_red)
  rhotildeNIR=abs(BRF_ToA_NIR/F_nir)
  ;
  ;idx_fine=where( (brf_toa_red-rhotildered) ge 0.0)

  ;res1=histogram(brf_toa_red(idx_fine)-rhotildered(idx_fine), min=min(brf_toa_red(idx_fine)-rhotildered(idx_fine)),max=max(brf_toa_red(idx_fine)-rhotildered(idx_fine)),bin=0.01)
  ;res=histogram(rhotildered, min=min(rhotildered),max=max(rhotildered),bin=0.01)
  ;xpdf=fltarr(N_elements(res))
  ;xpdf1=fltarr(N_elements(res1))
  ;for kl=0, N_elements(res)-1 do xpdf(kl)=min(rhotildered)+kl*0.01
  ;for kl=0, N_elements(res1)-1 do xpdf1(kl)=min(brf_toa_red(idx_fine)-rhotildered(idx_fine))+kl*0.01
  ;
  ;plot, xpdf1, 100.0*res1/total(res1), psym=10, xr=[min(brf_toa_red(idx_fine)-rhotildered(idx_fine)),max(brf_toa_red(idx_fine)-rhotildered(idx_fine))],$
  ;  yr=[0.,100], xtitle='brf_toa_red-RHo Tidle RED',ytitle='Frequency (%)', charsize=1.5
  ;oplot, xpdf1, 100.0*res1/total(res1), psym=10, col=250
  ;
  ;stop
  ;window,/free
  ;
  ;res1=histogram(brf_toa_nir-rhotildenir, min=min(brf_toa_nir-rhotildenir),max=max(0.20),bin=0.01)
  ;res=histogram(rhotildenir, min=min(rhotildenir),max=max(rhotildenir),bin=0.01)
  ;xpdf=fltarr(N_elements(res))
  ;xpdf1=fltarr(N_elements(res1))
  ;for kl=0, N_elements(res)-1 do xpdf(kl)=min(rhotildenir)+kl*0.01
  ;for kl=0, N_elements(res1)-1 do xpdf1(kl)=min(brf_toa_nir-rhotildenir)+kl*0.01
  ;
  ;plot, xpdf1, 100.0*res1/total(res1), psym=10, xr=[min(rhotildenir-rhotildenir),max(rhotildenir-rhotildenir)],$
  ;  yr=[0.,100], xtitle='RHo Tidle NIR',ytitle='Frequency (%)', charsize=1.5
  ;oplot, xpdf1, 100.0*res1/total(res1), psym=10, col=250
  ;plot, brf_toa_nir, rhotildenir, xr=[0.,0.6], psym = 1
  ;
  ;stop
  ;
  IF NOT(KEYWORD_set(toc)) THEN rhotildeBLUE=BRF_ToA_BLUE/F_blue
  ;
  ; equation (23)
  ;
  D_rhotildeRED  = sqrt(D_BRF_ToA_RED^2/F_red^2)
  D_rhotildeNIR  = sqrt(D_BRF_ToA_NIR^2/F_nir^2)

  ;res1=histogram(d_brf_toa_red, min=min(d_brf_toa_red),max=max(d_brf_toa_red),bin=0.001)
  ;res=histogram(d_rhotildered, min=min(d_rhotildered),max=max(d_rhotildered),bin=0.001)
  ;xpdf=fltarr(N_elements(res))
  ;xpdf1=fltarr(N_elements(res1))
  ;for kl=0, N_elements(res)-1 do xpdf(kl)=min(d_rhotildered)+kl*0.001
  ;for kl=0, N_elements(res1)-1 do xpdf1(kl)=min(d_brf_toa_red)+kl*0.001
  ;
  ;plot, xpdf, 100.0*res/total(res), psym=10, xr=[min(d_rhotildered),max(d_rhotildered)],$
  ; yr=[0.,20], xtitle='Sigma RHo Tidle RED',ytitle='Frequency (%)', charsize=1.5
  ; oplot, xpdf1, 100.0*res1/total(res1), psym=10, col=250
  ;
  ;res1=histogram(d_brf_toa_nir, min=min(d_brf_toa_nir),max=max(d_brf_toa_nir),bin=0.001)
  ;res=histogram(d_rhotildenir, min=min(d_rhotildenir),max=max(d_rhotildenir),bin=0.001)
  ;xpdf=fltarr(N_elements(res))
  ;xpdf1=fltarr(N_elements(res1))
  ;for kl=0, N_elements(res)-1 do xpdf(kl)=min(d_rhotildenir)+kl*0.001
  ;for kl=0, N_elements(res1)-1 do xpdf1(kl)=min(d_brf_toa_nir)+kl*0.001
  ;
  ;plot, xpdf, 100.0*res/total(res), psym=10, xr=[min(d_rhotildenir),max(d_rhotildenir)],$
  ; yr=[0.,20], xtitle='Sigma RHo Tidle NIR',ytitle='Frequency (%)', charsize=1.5
  ;oplot, xpdf1, 100.0*res1/total(res1), psym=10, col=250

  ;
  ;stop
  ;
  IF not(KEYWORD_set(toc)) THEN D_rhotildeBLUE = sqrt(D_BRF_ToA_BLUE^2/F_blue^2)

  ;Estimate 'corrected' channels and associated Deltas
  ;
  IF NOT(KEYWORD_set(toc)) THEN BEGIN
    rhoRED=GN(G1coeffs,rhotildeBLUE,rhotildeRED)
    rhoNIR=GN(G2coeffs,rhotildeBLUE,rhotildeNIR)
    ;
    ; equation 19
    ;

    D_rhoRED = sqrt(GN_DIFF_1st(G1coeffs,rhotildeBLUE,rhotildeRED)*$
      GN_DIFF_1st(G1coeffs,rhotildeBLUE,rhotildeRED)*D_rhotildeBLUE^2+$
      ;
      GN_DIFF_2nd(G1coeffs,rhotildeBLUE,rhotildeRED)*$
      GN_DIFF_2nd(G1coeffs,rhotildeBLUE,rhotildeRED)*D_rhotildeRED^2) ;+$
    ;
    ;2.*GN_DIFF_1st(G1coeffs,rhotildeBLUE,rhotildeRED)*$
    ;GN_DIFF_2nd(G1coeffs,rhotildeBLUE,rhotildeRED)*covblueredtilde --> 0

    ;print, 'rhoRED, D_rhoRED'
    ;print, rhoRED, D_rhoRED

    D_rhoNIR = sqrt(GN_DIFF_1st(G2coeffs,rhotildeBLUE,rhotildeNIR)*$
      GN_DIFF_1st(G2coeffs,rhotildeBLUE,rhotildeNIR)*D_rhotildeBLUE^2+$
      ;
      GN_DIFF_2nd(G2coeffs,rhotildeBLUE,rhotildeNIR)*$
      GN_DIFF_2nd(G2coeffs,rhotildeBLUE,rhotildeNIR)*D_rhotildeNIR^2) ;+$
    ;
    ;2.*GN_DIFF_1st(G1coeffs,rhotildeBLUE,rhotildeRED)*$
    ;GN_DIFF_2nd(G1coeffs,rhotildeBLUE,rhotildeRED)*covblueredtilde --> 0

  ENDIF else begin

    rhoRED=rhotildeRED
    rhoNIR=rhotildeNIR

  endelse

  ;Estimate VI (FAPAR) and associated Delta
  VI   = G0(G0coeffs,rhoRED,rhoNIR)
  ;
  ; equation (16)
  ;
  if KEYWORD_set(toc) THEN begin

    D_rhoRED=D_rhotildeRED
    D_rhoNIR=D_rhotildeNIR

  endif

  D_VI  = sqrt(G0_DIFF_1st(G0coeffs,rhoRED,rhoNIR)*$
    G0_DIFF_1st(G0coeffs,rhoRED,rhoNIR)*D_rhoRED^2+ $
    ;
    G0_DIFF_2nd(G0coeffs,rhoRED,rhoNIR)*$
    G0_DIFF_2nd(G0coeffs,rhoRED,rhoNIR)*D_rhoNIR^2)
  ;
  ;window,0
  ;res1=histogram(d_vi, min=min(d_vi),max=max(d_vi),bin=0.001)
  ;xpdf1=fltarr(N_elements(res1))

  ;for kl=0, N_elements(res1)-1 do xpdf1(kl)=min(d_vi)+kl*0.001
  ;
  ;plot, xpdf1, 100.0*res1/total(res1), psym=10, xr=[min(d_vi),max(d_vi)],$
  ; yr=[0.,20], xtitle='Sigma FAPAR',ytitle='Frequency (%)', charsize=1.5
  ;
  ;
  ;print,'look the plot ...'
  ;stop
  ;window,2
  ;plot, d_vi, psym = 1
  ;stop
  ;print,'FAPAR: ',VI
  ;print,'Standard Deviation: ',D_VI
  ;
  ;D_RHONIR=SQRT(D_RHONIR)
  ;D_RHOred=SQRT(D_RHORED)

END
