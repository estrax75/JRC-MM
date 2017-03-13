;Angles are given in radians
;
; Rahman's function
;
;====================================================================
;
; Collection of routines to calculate the JRC FAPAR
;====================================================================
FUNCTION G0,G0coeffs,x,y

  ;
  ; eq. 13 p.19
  ; MM (13)

  numer=G0coeffs[0]*y - G0coeffs[1]*x - G0Coeffs[2]
  denom=(G0coeffs[3]-x)^2. + (G0coeffs[4]-y)^2. + G0Coeffs[5]
  RETURN,numer/denom
END
