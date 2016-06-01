FUNCTION filterdata, band1, band2

  landPixValue=0
  badDataPixValue=1
  iceCloudPixValue=2
  brightSurfacePixValue=3

  categorizedBand=bytarr(size(band1, /DIM))
  
  landIdxes=where((band1 gt 0. and band1 le 0.5) and $
    (band1 gt 0. and band1 le 0.6), count)
  if count gt 0 then categorizedBand[landIdxes]=landPixValue

  badDataIdxes=where(band1 le 0. or band2 le 0., count)
  if count gt 0 then categorizedBand[landIdxes]=badDataPixValue

  iceCloudIdxes=where(band1 ge 0.5 or band2 ge 0.6, count)
  if count gt 0 then categorizedBand[landIdxes]=iceCloudPixValue

  brightIdxes=where((band1 gt 0. and band1 lt 0.5) and $
     (band2 gt 0. and band1 lt 0.7) and $
     (1.3*band1 + 0.018 gt band2), count)
  if count gt 0 then categorizedBand[brightIdxes]=brightSurfacePixValue

  RETURN, categorizedBand

END