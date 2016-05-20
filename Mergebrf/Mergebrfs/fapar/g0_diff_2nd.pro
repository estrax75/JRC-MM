FUNCTION G0_DIFF_2nd,G0coeffs,x,y
  ;
  ; first derivative dg/dy
  ; eq. (24) p. 25
  ;
  ; MM 20160506 change coeffs here???
  numer2 = G0coeffs[0]
  
  denom2 = (G0coeffs[3]-x)^2. + (G0coeffs[4]-y)^2. + G0coeffs[5]
  numer1 = 2.*(y-G0coeffs[4])*(-G0coeffs[1]*x + G0coeffs[0]*y - G0coeffs[2])
  denom1 = (denom2)^2.

  RETURN, -numer1/denom1 + numer2/denom2
END