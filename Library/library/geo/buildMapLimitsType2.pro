;function buildMapLimitsType2, operator, latBandName, lonBandName , REVERSE=REVERSE, TRANSPOSE=TRANSPOSE
function buildMapLimitsType2, fileName, latBandName, lonBandName , REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, GRIDASDATASET=GRIDASDATASET, lats=lats, lons=lons, ADDPIXEL=ADDPIXEL

  ;lonInfo=operator->readNcdfVar(operator->getMainFileName(), lonBandName, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  ;latInfo=operator->readNcdfVar(operator->getMainFileName(), latBandName, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  lonInfo=readStandardNcSingleBand(lonBandName, fileName, mask, nanValue=ignoreValue, REVERSE=REVERSE)
  latInfo=readStandardNcSingleBand(latBandName, fileName, mask, nanValue=ignoreValue, REVERSE=REVERSE)

  lonValues=lonInfo.data
  latValues=latInfo.data
  lastIndex=n_elements(latValues)-1

  dims=size(lonInfo.data, /DIM)
  ;dims=size(latInfo.data, /DIM)
  nl=dims[0]
  ns=dims[1]
  
;  upperLeft=[lonValues[0, nl-1], latValues[0, nl-1]]
;  upperRight=[lonValues[ns-1, 0], latValues[ns-1, 0]]
;  lowerLeft=[lonValues[0,0], latValues[0,0]]
;  lowerRight=[lonValues[ns-1, nl-1], latValues[ns-1, nl-1]]

  upperLeft=[lonValues[0, ns-1], latValues[0, ns-1]]
  upperRight=[lonValues[nl-1, ns-1], latValues[nl-1, ns-1]]
  lowerLeft=[lonValues[0,0], latValues[0,0]]
  lowerRight=[lonValues[nl-1, 0], latValues[nl-1, 0]]

  if upperLeft[0] ne lowerLeft[0] or upperLeft[1] ne upperRight[1] or upperRight[0] ne lowerRight[0] or lowerLeft[1] ne lowerRight[1] then begin
    GRIDASDATASET=1
    lats=latValues
    lons=lonValues
    return, 0
  endif    
  
  lonMin=min(lonValues, max=lonMax)
  latMin=min(latValues, max=latMax)
  
  if lonValues[0] lt lonValues[lastIndex] then lonSign=1 else lonSign=-1 
  if latValues[0] lt latValues[lastIndex] then latSign=-1 else latSign=1 
  
  lonExt = ABS(lonMax-lonMin)
  latExt = ABS(latMax-latMin)
  stepLon = lonExt/nl
  stepLat = latExt/ns
  
  ps = [lonSign*stepLon, latSign*stepLat]
  mc = [0.5D, 0.5D, lonMin, latMax]
  doLog, 'ps', ps
  doLog, 'mc', mc
  mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)

  return, mapInfo
  
end