;Angles are given in radians
;
; Rahman's function
;
;====================================================================
;
; Collection of routines to calculate the JRC FAPAR
;====================================================================
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
