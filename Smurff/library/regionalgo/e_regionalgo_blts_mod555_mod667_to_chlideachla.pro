function e_regionalgo_blts_mod555_mod667_to_chlideachla, values, NODATA

  ;[*,0] 555, [*,1] 667
  ;emulated novelty
  res = dblarr(2, n_elements(values[0,*]))
  data=n_elements(values[0,*])
  band1=reform(values[0,*])
  band2=reform(values[1,*])
  
  ratio=alog10(band1/band2)
  
  chla=10d^(2.0805d - 3.8293d*ratio + 1.2524d * ratio^2)
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