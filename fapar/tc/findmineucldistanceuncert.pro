function FindMinEuclDistanceUncert, fpar, rred, rnir, fpar_u, $
  rred_u, rnir_u, n, nv, $
  outDistance, outFapar_s_p, outRed_s_p, outNir_s_p, outImin_p

  i=0 & imin=0;
  tempD=0 & dmin = 1e300;

  outDistance=fpar & outDistance[*]=dmin
  
  ;  fpar_m = mean(n, fpar);
  ;  rred_m = mean(n, rred);
  ;  rnir_m = mean(n, rnir);
  ;  fpar_m = mean(fpar, /NAN);
  ;  rred_m = mean(rred, /NAN);
  ;  rnir_m = mean(rnir, /NAN);
  ;  fpar_s = stdv(n_elements(fpar), fpar, fpar_m);
  ;  rred_s = stdv(n_elements(rred), rred, rred_m);
  ;  rnir_s = stdv(n_elements(rnir), rnir, rnir_m);
  stat=moment(fpar)
  fpar_m=stat[0] & fpar_s = sqrt(stat[1])
  stat=moment(rred)
  rred_m=stat[0] & rred_s = sqrt(stat[1])
  stat=moment(rnir)
  rnir_m=stat[0] & rnir_s = sqrt(stat[1])

;  fpar_u_m = mean(n, fpar_u);
;  rred_u_m = mean(n, rred_u);
;  rnir_u_m = mean(n, rnir_u);
;  fpar_u_m = mean(fpar_u);
;  rred_u_m = mean(rred_u);
;  rnir_u_m = mean(rnir_u);

  stat=moment(fpar_u)
  fpar_u_m = stat[0]
  stat=moment(rred_u)
  rred_u_m = stat[0]
  stat=moment(rnir_u)
  rnir_u_m = stat[0]

  fpar_w=0;
  rred_w=0;
  rnir_w=0;

  for i=0, n-1 do begin

    fpar_w = (fpar_u[i] > 0) ? fpar_u_m/fpar_u[i] : 1;
    rred_w = (rred_u[i] > 0) ? rred_u_m/rred_u[i] : 1;
    rnir_w = (rnir_u[i] > 0) ? rnir_u_m/rnir_u[i] : 1;

    tempD = ((fpar_s > 0) ? (((fpar[i] - fpar_m)*fpar_w/fpar_s)^2) : 0) $
      + ((rred_s > 0) ? (((rred[i] - rred_m)*rred_w/rred_s)^2) : 0) $
      + ((rnir_s > 0) ? (((rnir[i] - rnir_m)*rnir_w/rnir_s)^2) : 0);

    ; Note: distance[i] is the SQUARED distance!
    outDistance[i] = tempD;
    if (tempD lt dmin) then begin

      dmin = tempD;
      imin = i;
    endif
  endfor

  outFapar_s_p = stdv(n, fpar, fpar[imin]);
  outRed_s_p = stdv(n, rred, rred[imin]);
  outNir_s_p = stdv(n, rnir, rnir[imin]);

  outImin_p = imin;
  return, imin;

end
