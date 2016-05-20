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
