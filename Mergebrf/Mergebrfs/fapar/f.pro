;Angles are given in radians
;
; Rahman's function
;
FUNCTION F,ttasun,ttaview,phisun,phiview, k,T,rhoc
  ; FUNCTION rahman, teta_0, teta_v, phi, xonst_lambda, k_lambda, xb_lambda


  ;
  ; print, k, T, rhoc
  ;
  ; NG eq. 1- 7 p. 16
  ;

  ; MM (6)
  G = sqrt(tan(ttasun)^2 + tan(ttaview)^2 - 2*tan(ttasun)*tan(ttaview)*cos(abs(phisun-phiview)))
  ; MM (7)
  cosg=cos(ttasun)*cos(ttaview) + sin(ttasun)*sin(ttaview)*cos(abs(phisun-phiview))

  ; MM (3)
  f1=((cos(ttasun)*cos(ttaview))^(k-1.))/((cos(ttasun) + cos(ttaview))^(1.-k))
  ; MM (4)
  f2=(1-T^2.)/((1. + 2*T*cosg + T^2.)^(1.5))
  ; MM (5)
  f3=1.0 + (1.-rhoc)/(1.+G)
  ;stop
  ; MM (2)
  RETURN,f1*f2*f3

END