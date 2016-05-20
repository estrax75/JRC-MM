;pro makeitglob_, sensor, noaanumber, year, month, day, refDir, outDir
;function corbrdfl, ro1, xts ,nb , xa, xb, RAD=RAD, DEG=DEG
;function corbrdfl, ro1, xts , xtv, nb , xa, xb, RAD=RAD, DEG=DEG
function corbrdfl, ro1, xts , xtv, xphi, nb , xa, xb, RAD=RAD, DEG=DEG

  xxts=45.0
  xxtv=0.0
  xxfi=0.0
  res45=xkernel(xxts,xxtv,xxfi)
  f1=res45.rossthick
  f2=res45.lispars
;  print, 'f1, f2', f1, f2
  
  ; Version from C-library
  ;xxts=0
  ;xxtv=0
  ; MM Version
  xxts=xts
  xxtv=xtv
  xxfi=xphi
  res=xkernel(xxts,xxtv,xxfi)
  f1n=res.rossthick
  f2n=res.lispars
;    print, 'f1n, f2n', f1n, f2n
  res=ro1*(1.+xa*f1n+xb*f2n)/(1.+xa*f1+xb*f2)
  return, res

end