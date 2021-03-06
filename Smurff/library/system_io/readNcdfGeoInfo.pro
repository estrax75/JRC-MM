@../geo/buildMapLimitsType1
@../geo/buildMapLimitsType2
@../geo/buildMapLimitsType3
function readNcdfGeoInfo, fileNames, NOTFOUND=NOTFOUND, REVERSE=REVERSE, GRIDASDATASET=GRIDASDATASET, lats=lats, lons=lons, ADDPIXEL=ADDPIXEL

  COMMON smurffCB, mainApp
  
  GRIDASDATASET=0
  type1=['lat', 'lon']
  type2=['lat2d','lon2d']
  type3=['latitude','longitude']
  
  checks=file_test(fileNames, /READ)
  refFileName=fileNames[(where(checks eq 1))[0]]
  
  gOp=obj_new('GenericOperator', mainApp, tempDir, fileName=refFileName)
  ;for i=0, n_elements(fileNames)-1 do begin
  fake=gOp->readNcdfVar(refFileName, type1[0], FOUND=FOUND); no reverse to avoid cpu useless consumption
  if keyword_set(FOUND) then return, buildMapLimitsType1(gOp, type1[0], type1[1], REVERSE=REVERSE, ADDPIXEL=ADDPIXEL); never reverse?
  fake=gOp->readNcdfVar(refFileName, type2[0], FOUND=FOUND); no reverse to avoid cpu useless consumption
  if keyword_set(FOUND) then return, buildMapLimitsType2(refFileName, type2[0], type2[1], REVERSE=REVERSE, GRIDASDATASET=GRIDASDATASET, lats=lats, lons=lons, ADDPIXEL=ADDPIXEL)
  fake=gOp->readNcdfVar(refFileName, type3[0], FOUND=FOUND); no reverse to avoid cpu useless consumption
  if keyword_set(FOUND) then return, buildMapLimitsType3(refFileName, type3[0], type3[1], REVERSE=REVERSE, GRIDASDATASET=GRIDASDATASET, lats=lats, lons=lons, ADDPIXEL=ADDPIXEL)

  obj_destroy, gOp
  message, 'geo reference unavailable, check file.'
  return, 0
  
end