;function CheckData, blue, red, nir
;
;print, blue(0), red(0), nir(0)
; for SeaWiFS & MERIS
;
;    mask = (((blue le 0.) + (red le 0.) + (nir le 0.)) gt 0.) * 1
;    mask = mask + (((blue ge 0.3) + (red ge 0.5) + (nir ge 0.7)) gt 0) * (mask eq 0) * 2
;    mask = mask + (blue gt nir) * (mask eq 0) * 3
;    mask = mask + (nir lt (1.3*red)) * (mask eq 0) * 4
;
;
; for Landsat
;
;    mask = (((blue le 0) + (red le 0) + (nir le 0)) gt 0) * 1
;    mask = mask + (((blue ge 0.257752) + (red ge 0.484070) + (nir ge 0.683928)) gt 0) * (mask eq 0) * 2
;    mask = mask + (blue gt nir) * (mask eq 0) * 3
;    mask = mask + (nir le (1.26836*red)) * (mask eq 0) * 4
;
;
;
;    return, byte(mask)
;end
;
;function CheckDataTOC, red, nir
;
;
; for AVHRR
;
;    mask = (((red le 0.) + (nir le 0.)) gt 0.) * 1
;    mask = mask + (nir lt -0.05/0.1*red+0.2) * (mask eq 0) * 1
;    mask = mask + (((red ge 0.5) + (nir ge 0.6)) gt 0) * (mask eq 0) * 2
; NEW
;    mask = mask + (nir le (1.50*red)) * (mask eq 0) * 4
;
;    return, byte(mask)
; end

;Angles are given in radians
;
; Rahman's function
;
FUNCTION F,ttasun,ttaview,phisun,phiview, k,T,rhoc
  ; FUNCTION rahman, teta_0, teta_v, phi, xonst_lambda, k_lambda, xb_lambda

  ; print, k, T, rhoc
  ;
  ;

  cosg=cos(ttasun)*cos(ttaview) + sin(ttasun)*sin(ttaview)*cos(abs(phisun-phiview))
  G = sqrt(tan(ttasun)^2 + tan(ttaview)^2 - 2*tan(ttasun)*tan(ttaview)*cos(abs(phisun-phiview)))
  f1=((cos(ttasun)*cos(ttaview))^(k-1.))/((cos(ttasun) + cos(ttaview))^(1.-k))
  f2=(1-T^2.)/((1. + 2*T*cosg + T^2.)^(1.5))
  f3=1.0 + (1.-rhoc)/(1.+G)
  ;stop
  RETURN,f1*f2*f3
END
;====================================================================
;
; Collection of routines to calculate the JRC FAPAR
;====================================================================
FUNCTION G0,G0coeffs,x,y
  numer=G0coeffs[0]*y - G0coeffs[1]*x - G0Coeffs[2]
  denom=(G0coeffs[3]-x)^2. + (G0coeffs[4]-y)^2. + G0Coeffs[5]
  ;
  ; eq. 9
  ;
  RETURN,numer/denom
END

FUNCTION G0_DIFF_1st,G0coeffs,x,y
  ;
  ; first derivative dg/dx
  ; eq. 17
  ;
  numer2 = -G0coeffs[1]
  denom2 = (G0coeffs[3]-x)^2. + (G0coeffs[4]-y)^2. + G0coeffs[5]
  numer1 = 2.*(x-G0coeffs[3])*(-G0coeffs[1]*x + G0coeffs[0]*y - G0coeffs[2])
  denom1 = (denom2)^2.

  RETURN, -numer1/denom1 + numer2/denom2
END

FUNCTION G0_DIFF_2nd,G0coeffs,x,y
  ;
  ; first derivative dg/dy
  ; eq. 18
  ;
  numer2 = G0coeffs[0]
  denom2 = (G0coeffs[3]-x)^2. + (G0coeffs[4]-y)^2. + G0coeffs[5]
  numer1 = 2.*(y-G0coeffs[4])*(-G0coeffs[1]*x + G0coeffs[0]*y - G0coeffs[2])
  denom1 = (denom2)^2.

  RETURN, -numer1/denom1 + numer2/denom2
END

FUNCTION GN,GNCoeffs, z, w
  ;
  ; version FORME OLCI !!!
  ;
  ; eq. 8
  ;
  numer=Gncoeffs[0]*(z+Gncoeffs[1])^2. + GNcoeffs[2]*(w+GNCoeffs[3])^2.+ GNcoeffs[4]*z*w
  denom=GNCoeffs[5]*(GNCoeffs[6]+z)^2. + GNCoeffs[7]*(GnCoeffs[8] + w)^2. + GNCoeffs[9]*z*w + GNcoeffs[10]

  RETURN,numer/denom
END

FUNCTION GN_DIFF_1st,GNCoeffs,x,y
  ;   OLD VERSION 2008
  ;   numer1=B2*GNCOeffs[4] + 2 * GNCoeffs[0]*(GNCoeffs[1]+B1)
  ;   denom1=GNCoeffs[7]*(GNCoeffs[8]+B2)^2. + GNCoeffs[5]*(GnCoeffs[6] + B1)^2. + GNCoeffs[10] + B1*B2*GNcoeffs[9]
  ;   numer2=-(B1*B2*Gncoeffs[4] + GNcoeffs[2]*(GNcoeffs[3] + B2)^2. + GNcoeffs[0]*(GNCoeffs[1]+B1)^2.)*(2.*GNCoeffs[5]*(GNCOeffs[6]+B1) + B2*GNCoeffs[9])
  ;   denom2=numer1^2.
  ;
  ;   first derivative dgn/dx
  ;
  ;  eq. (21)
  ;
  numer1=GNCoeffs[4]*y+2.*GNCoeffs[0]*x+2.*GNCoeffs[0]*GNCoeffs[1]

  denom1=GNCoeffs[5]*(GNCoeffs[6]+x)^2. + GNCoeffs[7]* $
    (GnCoeffs[8] + y)^2. + GNCoeffs[9]*x*y + GNcoeffs[10]

  numer2=(GnCoeffs[0]*(x+GnCoeffs[1])^2+$
    GnCoeffs[2]*(y+GnCoeffs[3])^2+$
    GnCoeffs[4]*x*y)*$
    (2.*GnCoeffs[5]*x+2.*GnCoeffs[5]*GnCoeffs[6]+$
    GnCoeffs[9]*y)

  denom2=denom1^2.

  return,numer1/denom1 - numer2/denom2
END

FUNCTION GN_DIFF_2nd,GNCoeffs,x,y
  ;
  ;   first derivative dgn/dy
  ;
  ;  eq. (22)
  ;
  ;
  numer1=x*GNCOeffs[4] + 2. * GNCoeffs[2]*(GNCoeffs[3]+y)

  denom1=GNCoeffs[5]*(GNCoeffs[6]+x)^2. + GNCoeffs[7]* $
    (GnCoeffs[8] + y)^2. + GNCoeffs[9]*x*y + GNcoeffs[10]


  numer2=(GnCoeffs[0]*(x+GnCoeffs[1])^2+$
    GnCoeffs[2]*(y+GnCoeffs[3])^2+$
    GnCoeffs[4]*x*y)*$
    (2.*GnCoeffs[7]*y+2.*GnCoeffs[7]*GnCoeffs[8]+$
    GnCoeffs[9]*x)

  denom2=denom1^2.

  return,numer1/denom1 - numer2/denom2
END
;====================================================================================
;
; Main program
;
PRO Rectified,sensor,ttasun,ttaview,phisun,phiview,$
  BRF_ToA_BLUE,BRF_ToA_RED,BRF_ToA_NIR,$
  D_BRF_ToA_BLUE,D_BRF_ToA_RED,D_BRF_ToA_NIR, $
  rhoRED, rhoNIR, D_rhoRED, D_rhoNIR, $
  D_rhotildeBLUE, D_rhotildeRED, D_rhotildeNIR, VI, D_VI, toc=TOC
  ;
  ;
  ; compute the rectified over bare soil and the associated uncertaintied (=D_Rect) as function as the bands uncertainties
  ; D_BRF_ToA_BLUE,D_BRF_ToA_RED,D_BRF_ToA_NIR expressed in sigma !
  ;
  ; 2016, NG
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
  ;stop
  CASE sensor OF
    'SEA' : BEGIN
      print,'Coefficients for seaWiFS '
      ;
      ; for vegetated
      ; As in ATBD FAPAR SeaWifs (Version 2.0 - January 22,2002)
      ;G0coeffs=[0.25130709,0.30589629,-0.0048298022,-0.32136740,0.31415914,-0.01074418]
      ;G1coeffs=[-9.8725,-0.027458,2.9144,0.059376,10.904,0.,0.,0.,0.,0.,1.0]
      ;G2coeffs=[-0.66956,-0.16930,-0.071256,-0.090485,-0.81353,-0.035440,-1.3438,-0.41673,-0.45123,-0.99648,0.]
      ; k,Theta,rhoc
      ;RahmanCoeffs_BLUE=[0.56184,-0.04125,0.23265]
      ;RahmanCoeffs_RED= [0.70535,0.03576,-0.44444]
      ;RahmanCoeffs_NIR= [0.86644,-0.00102,0.63149]
    END
    'MER' : BEGIN
      print,'Coefficients for MERIS '
      ;
      ; 2011 VERSION
      ;
      ; vegetated
      ;Meris, as coded in BEAM
      ;
      ;
      ;G0coeffs=[0.255,0.306,-0.0045,-0.32,0.32,-0.005]
      ;G1coeffs=[-9.2615,-0.029011,3.2545,0.055845,9.8268,0.,0.,0.,0.,0.,1.0]
      ;G2coeffs=[-0.47131,-0.21018,-0.045159,0.076505,-0.80707,-0.048362,-1.2471,-0.54507,-0.47602,-1.1027,0.]
      ;k,Theta,rhoc
      ;
      ;RahmanCoeffs_BLUE=[0.56192,-0.04203,0.24012]
      ;RahmanCoeffs_RED =[0.70879,0.037,-0.46273]
      ;RahmanCoeffs_NIR =[0.86523,-0.00123,0.63841]
    END
    'MOD' : BEGIN
      print,'Coefficients for MODIS '
      ; vegetated
      ;Modis
      ;G0coeffs=[0.26130709,0.33489629,-0.0038298022,-0.32136740,0.31415914,-0.010744180]
      ;G1coeffs=[-13.860,-0.018273,1.5824,0.081450,17.092,0.,0.,0.,0.,0.,1.0]
      ;G2coeffs=[-0.036557,-3.5399,8.3076,0.18702,-13.294,0.77034,-4.9048,-2.3630,-2.6733,-37.297,0.]
      ;
      ;k,Theta,rhoc
      ;     RahmanCoeffs_BLUE=[0.56177,-0.03204,0.13704]
      ;    RahmanCoeffs_RED =[0.70116,0.03376,-0.39924]
      ;   RahmanCoeffs_NIR =[0.86830,-0.00081,0.63537]
    END
    'OLCI' : BEGIN
      ;
      ; vegetated
      ;print,'Coefficients for OLCI '
      ;OLCI
      ;G0coeffs=[0.254845,0.28550,-0.00440000,-0.322000,0.321000,-0.005079]
      ;G1coeffs=[-9.1299,-0.028791,3.2,0.054,9.851,0.,0.,0.,0.,0.,1.0]
      ;G2coeffs=[0.0082617,1.1027,0.64661,0.029443,-0.65340,0.19878,-0.95736,0.77296,0.054908,-1.6565,0.]
      ;
      ;     0.64295     1.02238    -0.09158
      ;    -0.35287     0.66978     0.03698
      ;k,Theta,rhoc
      ;RahmanCoeffs_BLUE=[0.51669,-0.04434,0.30402]
      ;RahmanCoeffs_RED =[0.66361,0.03840,-0.39471]
      ;RahmanCoeffs_NIR =[0.86633,-0.00705,0.66537]
      ;
    END
    'AVHRR16' : BEGIN
      print,'Coefficients for AVHRR 16'
      ; vegetated
      ; G0coeffs=[0.25405,0.32031,0.0037285,-0.28734,0.27214,-0.015730]
      ;k,Theta,rhoc
      ;
      ; facosi gives  xonst_lambda, k_lambda, xb_lambda
      ;
      ;
      ;
      ; Soil
      ;
      ;
      ;	S[02-06]
      ; red	=	0.584890    0.842280  -0.0694700
      ; nir	=	0.60614     0.84467    -0.06209
      ;	S[02-08]
      ;
      ; red    =      0.66953     0.86640    -0.05872
      ;
      ; nir	=	0.68645     0.86993    -0.05275
      ;
      ; MM 20160506 coeffs from table 9 pag. 28
      G0coeffs=[0.26676,0.33483,0.002950,-0.29184,0.27581,-0.011802]

      ; MM 20160506 coeffs from table 8 pag. 28
      RahmanCoeffs_RED =[0.99763,-0.06397,0.61133]
      RahmanCoeffs_NIR =[0.84211,-0.02665,0.73745]

    END
    'AVHRR14' : BEGIN
      print,'Coefficients for AVHRR 14 '
      ;
      ; only veg
      ;	 		0.50901     0.92458    -0.06843
      ;			0.74984     0.84273    -0.02358
      ;	  RahmanCoeffs_RED =[0.924580,-0.06843,0.509010]
      ;  RahmanCoeffs_NIR =[0.84273,-0.02358,0.74984]

      ; MM 20160506 coeffs from table 9 pag. 28
      G0coeffs=[0.26676,0.33483,0.002950,-0.29184,0.27581,-0.011802]

      ; MM 20160506 coeffs from table 8 pag. 28
      RahmanCoeffs_RED =[0.95283,-0.05880,0.58979]
      RahmanCoeffs_NIR =[0.84381,-0.02551,0.75070]


    END
  endcase
  ;stop
  ;For each band, 'rectification' value from Rahman
  ;window, 0, xsize=720, ysize=360, title='ttasun '+SENSOR
  ;tv, reverse(congrid(ttasun, 720,360),2)

  ;stop

  F_red=F(ttasun,ttaview,phisun,phiview,RahmanCoeffs_RED[0],RahmanCoeffs_RED[1],RahmanCoeffs_RED[2])
  F_nir=F(ttasun,ttaview,phisun,phiview,RahmanCoeffs_NIR[0],RahmanCoeffs_NIR[1],RahmanCoeffs_NIR[2])
  IF NOT(KEYWORD_set(toc)) THEN F_blue=F(ttasun,ttaview,phisun,phiview,RahmanCoeffs_BLUE[0],RahmanCoeffs_BLUE[1],RahmanCoeffs_BLUE[2])
  ;
  ;
  ; Estimate 'rectified' channels and associated Deltas
  ; abs() is omitted as derivatives are 1/F() which is always positive.

  rhotildeRED=abs(BRF_ToA_RED/F_red)
  rhotildeNIR=abs(BRF_ToA_NIR/F_nir)
  ;
  ;res1=histogram(brf_toa_red, min=min(brf_toa_red),max=max(brf_toa_red),bin=0.01)
  ;res=histogram(rhotildered, min=min(rhotildered),max=max(rhotildered),bin=0.01)
  ;xpdf=fltarr(N_elements(res))
  ;xpdf1=fltarr(N_elements(res1))
  ;for kl=0, N_elements(res)-1 do xpdf(kl)=min(rhotildered)+kl*0.01
  ;for kl=0, N_elements(res1)-1 do xpdf1(kl)=min(brf_toa_red)+kl*0.01
  ;
  ;plot, xpdf, 100.0*res/total(res), psym=10, xr=[min(rhotildered),max(rhotildered)],$
  ; yr=[0.,20], xtitle='RHo Tidle RED',ytitle='Frequency (%)', charsize=1.5
  ;oplot, xpdf1, 100.0*res1/total(res1), psym=10, col=250
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
  VI   = 0.0
  ;
  ; equation (16)
  ;
  if KEYWORD_set(toc) THEN begin

    D_rhoRED=D_rhotildeRED
    D_rhoNIR=D_rhotildeNIR

  endif

  D_VI  = 0.0
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

