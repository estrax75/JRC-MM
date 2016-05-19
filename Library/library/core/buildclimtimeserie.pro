function buildClimTimeSerie, climValues, yearNo

  ;timeSeries=climValues
  dims=size(climValues, /DIM)
  timeSeries=fltarr(dims[0], dims[1], dims[2]*yearNo)
  for i=0, yearNo-1 do timeSeries[*,*,i*dims[2]:(i+1)*dims[2]-1]=climValues
  return, timeSeries

end