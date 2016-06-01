function xkernel, rts,rtv,rfi

  fac=1./!RADEG;0.017453293/
  pi=!PI;3.1415926535
  rtv=abs(rtv*fac)
  rfi=abs(rfi*fac)
  rts=rts*fac
  cts=cos(rts)
  ctv=cos(rtv)
  sts=sin(rts)
  stv=sin(rtv)
  cfi=cos(rfi)
  sfi=sin(rfi)
  cpha=cts*ctv+sts*stv*cfi
  rpha=acos(cpha)
  ;
  ;print, 'rtv, rfi, rts, cts, ctv, sts, stv, cfi, sfi, cpha, rpha'
  ;print, rtv, rfi, rts, cts, ctv, sts, stv, cfi, sfi, cpha, rpha
  ;stop
  ;
  rosselt=(pi/2.-rpha)*cpha+sin(rpha)
  rossthick=(rosselt/(cts+ctv))-pi/4.

  ksi0=1.5*pi/180.
  cksi=cpha
  sksi=sqrt(1.-cksi*cksi)
  ksi=acos(cksi)
  rossthick=2./(3.*pi)*((pi-2.*ksi)*cksi+2.*sksi)/(cts+ctv)*(1.+1./(1.+ksi/ksi0))-1./3.

  tanti=tan(rts)
  tantv=tan(rtv)

  angdist=tanti*tanti+tantv*tantv-2.*tanti*tantv*cfi
  angdist=sqrt(angdist)

  angtemp=1./cts+1./ctv
  cost=2.*sqrt(angdist*angdist+tanti*tanti*tantv*tantv*sfi*sfi)
  cost=cost/angtemp
  if (cost ge 1.) then cost=1.
  if (cost le -1.) then cost=-1.
  tvar=acos(cost)
  sint=sqrt(1.-cost*cost)
  angover=(tvar-sint*cost)*angtemp/pi
  lispars=angover-angtemp+0.5*(1.+cpha)/cts/ctv
  ;print, lispars
  ;  stop
  return, {rossthick:rossthick, lispars:lispars}

end
