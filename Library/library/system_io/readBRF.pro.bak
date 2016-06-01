;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function readBRF, folder, fileName, FOUND=FOUND

  avBandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
    'TS', 'TV', 'PHI', 'QA', 'Q1', 'Q2']
  ; 0, 1, 4,5,6,7

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  newFolder=fsObj->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  fName=fsObj->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  ; 0, 1, 4,5,6,7
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    red_avhrr=dataSet.data
    slope_red=dataSet.slope
    offset_red=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[1], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    nir_avhrr=dataSet.data
    slope_nir=dataSet.slope
    offset_nir=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[4], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    ts_avhrr=dataSet.data
    slope_ts=dataSet.slope
    offset_ts=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[5], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    tv_avhrr=dataSet.data
    slope_tv=dataSet.slope
    offset_tv=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[6], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    phi_avhrr=dataSet.data
    slope_phi=dataSet.slope
    offset_phi=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[7], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    brdf_qa_avhrr=dataSet.data
    slope_qa=dataSet.slope
    offset_qa=dataSet.intercept
  endif

  allData=0
  if keyword_set(FOUND) then allData={red_avhrr:red_avhrr, nir_avhrr:nir_avhrr,$
    ts_avhrr:ts_avhrr,tv_avhrr:tv_avhrr,phi_avhrr:phi_avhrr,$
    brdf_qa_avhrr:brdf_qa_avhrr, $
    slope_red:slope_red,slope_nir:slope_nir,$
    slope_ts:slope_ts,slope_tv:slope_tv,slope_phi:slope_phi,$
    slope_qa:slope_qa, $
    offset_red:offset_red,offset_nir:offset_nir,$
    offset_ts:offset_ts,offset_tv:offset_tv,$
    offset_phi:offset_phi,$
    offset_qa:offset_qa}
  return, allData

end