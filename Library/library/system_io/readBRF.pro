;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function readBRF, folder, fileName, FOUND=FOUND, SWITCH_TS_TV=SWITCH_TS_TV, FULL=FULL

  ;  avBandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'PHI', 'QA', 'Q1', 'Q2']
  brfsDSInfo=getStandardBrfDataSetInfo()
  avBandNames=brfsDSInfo.bandNames
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

  sigma_red=0
  if keyword_set(FULL) then begin
    dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[2], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      sigma_red=dataSet.data
      slope_sigma_red=dataSet.slope
      offset_sigma_red=dataSet.intercept
    endif
  endif

  sigma_nir=0
  if keyword_set(FULL) then begin
    dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[3], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      sigma_nir=dataSet.data
      slope_sigma_nir=dataSet.slope
      offset_sigma_nir=dataSet.intercept
    endif
  endif

  if keyword_set(SWITCH_TS_TV) then begin
    avBandNames[4]='TV'
    avBandNames[5]='TS'
  endif else begin
    avBandNames[4]='TS'
    avBandNames[5]='TV'
  endelse

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
  endif else begin
    dataSet=operatorObj->readNcdfVar(fullFileName, 'QA', FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    brdf_qa_avhrr=dataSet.data
    slope_qa=dataSet.slope
    offset_qa=dataSet.intercept
  endelse

  cMask1=0
  if keyword_set(FULL) then begin
    dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[8], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      cMask1=dataSet.data
      slope_nir=dataSet.slope
      offset_nir=dataSet.intercept
    endif
  endif

  cMask2=0
  if keyword_set(FULL) then begin
    dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[9], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      cMask2=dataSet.data
      slope_cMask2=dataSet.slope
      offset_cMask2=dataSet.intercept
    endif
  endif

  allData=0
  if keyword_set(FOUND) then allData={red_avhrr:red_avhrr, nir_avhrr:nir_avhrr,$
    ts_avhrr:ts_avhrr,tv_avhrr:tv_avhrr,phi_avhrr:phi_avhrr,$
    brdf_qa_avhrr:brdf_qa_avhrr, $
    sigma_red: sigma_red, sigma_nir: sigma_nir, cMask1:cMask1, cMask2:cMask2, $
    slope_red:slope_red,slope_nir:slope_nir,$
    slope_ts:slope_ts,slope_tv:slope_tv,slope_phi:slope_phi,$
    slope_qa:slope_qa, $
    offset_red:offset_red,offset_nir:offset_nir,$
    offset_ts:offset_ts,offset_tv:offset_tv,$
    offset_phi:offset_phi,$
    offset_qa:offset_qa}
  return, allData

end