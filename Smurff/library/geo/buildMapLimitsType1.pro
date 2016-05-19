function buildMapLimitsType1, operator, latBandName, lonBandName, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, ADDPIXEL=ADDPIXEL

  lonInfo=operator->readNcdfVar(operator->getMainFileName(), lonBandName, FOUND=FOUND)
  latInfo=operator->readNcdfVar(operator->getMainFileName(), latBandName, FOUND=FOUND)
  
  obj_destroy, operator
  
  lonValues=lonInfo.data
  latValues=latInfo.data
  
  nl=n_elements(lonValues)
  ns=n_elements(latValues)
  
  lonMin=min(lonValues, max=lonMax)
  latMin=min(latValues, max=latMax)
  
  if keyword_set(ADDPIXEL) then begin
    lonMin=lonMin-(ABS(lonMax-lonMin))/(nl)
    latMax=latMax+(ABS(latMax-latMin))/(ns)
  endif
  stepLon = (ABS(lonMax-lonMin))/(nl-1)
  stepLat = (ABS(latMax-latMin))/(ns-1)
  
  if lonValues[0] lt lonValues[nl-1] then lonSign=1 else lonSign=-1
  if latValues[0] lt latValues[ns-1] then latSign=1 else latSign=-1
  ps = [lonSign*stepLon, latSign*stepLat]
  mc = [0.5D, 0.5D, lonMin, latMax]
  doLog, 'ps-->', ps
  doLog, 'mc-->', mc
  mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)
  mapInfo.ns=nl;+1
  mapInfo.nl=ns;+1
    
  return, mapInfo
  
end