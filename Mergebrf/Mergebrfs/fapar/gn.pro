FUNCTION GN,GNCoeffs, z, w
  ;
  ; version FORME OLCI !!!
  ;
  ; eq. 8
  ; MM (8a)
  ;
  numer=Gncoeffs[0]*(z+Gncoeffs[1])^2. + GNcoeffs[2]*(w+GNCoeffs[3])^2.+ GNcoeffs[4]*z*w
  denom=GNCoeffs[5]*(GNCoeffs[6]+z)^2. + GNCoeffs[7]*(GnCoeffs[8] + w)^2. + GNCoeffs[9]*z*w + GNcoeffs[10]

  RETURN,numer/denom
END