function e_regionalgo_blks_mod488_mod555_to_chlideachla, values, NODATA

  ;[*,0] 488, [*,1] 555
  ;y = -4.8159x3 + 1.1787x2 - 2.8542x - 0.0661
  ;ove R=log_10(Rrs(488/547)
  
  ;emulated novelty
  res = dblarr(2, n_elements(values[0,*]))
  data=n_elements(values[0,*])
  band1=reform(values[0,*])
  band2=reform(values[1,*])


  
  ratio=alog10(band1/band2)


  ;seaWifs coeff
  ;chla=10d^(-0.0024d - 2.4788d*ratio + 1.1106d * ratio^2 - 6.503d * ratio^3)
  ;Modis coeff
  chla=10d^(-0.0661d - 2.8542d*ratio + 1.1787d * ratio^2 - 4.8159d * ratio^3)
  notValidIdxs=where(chla gt 50 or chla le 0 or ~finite(chla), count, ncomplement=ncompl)


  
  res[0,*]=chla
  res[1,*]=1

  
  if count gt 0 then res[1, notValidIdxs]=10.

  
  badIdxs1=where(band1 lt 0, badCount1)
  badIdxs2=where(band2 lt 0, badCount2)
  if total(badCount1+badCount2) gt 1 then stop
  doLog, total(badCount1+badCount2), LEVEL=1
  
  return, res

  
end