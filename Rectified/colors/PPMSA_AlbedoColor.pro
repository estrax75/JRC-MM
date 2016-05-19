PRO PPMSA_AlbedoColor

  NBR_COL = 256
  pcolor = bytarr( 3, NBR_COL )
  color  = bytarr( 3, NBR_COL )

  black = 0
  white = 222
  bgc = 0

  i = 0
  color(*,i)=[bgc, bgc, bgc] & i=i+1
  color(*,i)=[246, 253, 190] & i=i+1
  color(*,i)=[246, 240, 169] & i=i+1
  ;  color(*,i)=[246, 227, 128] & i=i+1
  color(*,i)=[231, 210, 113] & i=i+1
  color(*,i)=[216, 193,  99] & i=i+1
  color(*,i)=[201, 176,  85] & i=i+1
  color(*,i)=[186, 159,  71] & i=i+1
  color(*,i)=[171, 142,  56] & i=i+1
  color(*,i)=[156, 125,  42] & i=i+1
  color(*,i)=[141, 108,  28] & i=i+1
  color(*,i)=[126,  91,  14] & i=i+1
  color(*,i)=[111,  75,   0] & i=i+1
  color(*,i)=[125,  99,   0] & i=i+1
  color(*,i)=[139, 123,   0] & i=i+1
  color(*,i)=[153, 147,   0] & i=i+1
  color(*,i)=[152, 186,   0] & i=i+1
  color(*,i)=[152, 255,   0] & i=i+1
  color(*,i)=[ 74, 134,   0] & i=i+1
  color(*,i)=[ 74, 255,   0] & i=i+1
  color(*,i)=[129,  25,  14] & i=i+1
  color(*,i)=[192,  65,   7] & i=i+1
  color(*,i)=[0,  0,   255] & i=i+1

  aval = NBR_COL * [0.0, 0.05, 0.10, 0.20, 0.30, $
    0.35, 0.40, 0.45, 0.50, 0.55, $
    0.60, 0.65, 0.70, 0.725, 0.750, $
    0.775, 0.80, 0.85, 0.90, 0.95, $
    1.00,1.00]

  ;aval = NBR_COL * [1.0, 0.99, 0.6, 0.55, 0.5, 0.45, 0.40, 0.375, $
  ;0.35, 0.325, 0.3, 0.28, 0.26, 0.24, 0.22, 0.2, 0.18, 0.16, 0.14, $
  ;0.12, 0.1, 0.08,0.06,0.04,0.0]

  ncol = N_ELEMENTS(aval)

  FOR k=1, ncol-1 DO BEGIN
    FOR l=LONG(aval(k-1)),LONG(aval(k))-1 DO BEGIN
      pcolor(*,l) = color(*,k)
    ENDFOR
  ENDFOR

  pcolor(*,0)   = 255
  pcolor(*,255) = 255

  FOR i=0, NBR_COL-1 DO BEGIN
    ; PRINT, i,pcolor(*,i)
    TVLCT, pcolor(0,i), pcolor(1,i), pcolor(2,i), nbr_col-i
  ENDFOR
  R_CURR = pcolor(0,*)
  G_CURR = pcolor(1,*)
  B_CURR = pcolor(2,*)

END
