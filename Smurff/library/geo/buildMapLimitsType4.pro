function buildMapLimitsType4, operator, latBandName, lonBandName, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE

  lonInfo=operator->readNcdfVar(operator->getMainFileName(), lonBandName, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  latInfo=operator->readNcdfVar(operator->getMainFileName(), latBandName, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)

  lonValues=lonInfo.data
  latValues=latInfo.data
  lastIndex=n_elements(latValues)-1

  obj_destroy, operator

  dims=size(lonInfo.data, /DIM)
  ;dims=size(latInfo.data, /DIM)
  ns=dims[0]
  nl=dims[1]
  
  lonMin=min(lonValues, max=lonMax)
  latMin=min(latValues, max=latMax)
  
  if lonValues[0] lt lonValues[lastIndex] then lonSign=1 else lonSign=-1 
  if latValues[0] lt latValues[lastIndex] then latSign=-1 else latSign=1
;    latSign=1
;    temp=latMin
;    latMin=latMax
;    latMax=temp
;  endelse
  
  lonExt = ABS(lonMax-lonMin)
  latExt = ABS(latMax-latMin)
  stepLon = lonExt/nl
  stepLat = latExt/ns

  ;xPixRes=(latMax-latMin)/ns
  ;yPixRes=(lonMax-lonMin)/nl
  
  ps = [lonSign*stepLon, latSign*stepLat]

  doLog, 'lat', latMax-stepLat*ns, latMax, ns, latMin
  doLog, 'lon', lonMin+stepLon*nl, lonMin, nl, lonMax

  mc = [0.5D, 0.5D, lonMin, latMax]
  doLog, 'ps-->', ps
  doLog, 'mc-->', mc
  mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)

  return, mapInfo
  
end