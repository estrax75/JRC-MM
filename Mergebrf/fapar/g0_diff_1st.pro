FUNCTION G0_DIFF_1st,G0coeffs,x,y
  ;
  ; first derivative dg/dx
  ; eq. (23) p.24
  ;
  numer2 = -G0coeffs[1]
  denom2 = (G0coeffs[3]-x)^2. + (G0coeffs[4]-y)^2. + G0coeffs[5]
  numer1 = 2.*(x-G0coeffs[3])*(-G0coeffs[1]*x + G0coeffs[0]*y - G0coeffs[2])
  denom1 = (denom2)^2.

  RETURN, -numer1/denom1 + numer2/denom2
END