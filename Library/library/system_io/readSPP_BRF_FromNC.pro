;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function readSPP_BRF_FromNC, folder, fileName, bandName, FOUND=FOUND, FULL=FULL, USENAN=USENAN, ORIGDIMS=ORIGDIMS

  ;  avBandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'PHI', 'QA']
  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  brfsDSInfo=getStandardBrfDataSetInfo()
  avBandNames=brfsDSInfo.bandNames
  ; 0, 1, 4,5,6,7

  newFolder=ST_fileSystem->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  if n_elements(bandName) eq 1 then return, ST_operator->readNcdfVar(fullFileName, bandName, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)

  fName=ST_fileSystem->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  ; 0, 1, 4,5,6,7
  dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    red_avhrr=1.*dataSet.data
    slope_red=dataSet.slope
    offset_red=dataSet.intercept
    if countNan gt 0 then begin
      if size(red_avhrr, /TYPE) eq 4 then red_avhrr[nanList]=!VALUES.F_NAN
      if size(red_avhrr, /TYPE) eq 5 then red_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif

  dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[1], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    nir_avhrr=1.*dataSet.data
    slope_nir=dataSet.slope
    offset_nir=dataSet.intercept
    if countNan gt 0 then begin
      if size(nir_avhrr, /TYPE) eq 4 then nir_avhrr[nanList]=!VALUES.F_NAN
      if size(nir_avhrr, /TYPE) eq 5 then nir_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif

  sigma_red=0
  if keyword_set(FULL) then begin
    dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[2], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      countNan=-1
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      sigma_red=1.*dataSet.data
      slope_sigma_red=dataSet.slope
      offset_sigma_red=dataSet.intercept
      if countNan gt 0 then begin
        if size(sigma_red, /TYPE) eq 4 then sigma_red[nanList]=!VALUES.F_NAN
        if size(sigma_red, /TYPE) eq 5 then sigma_red[nanList]=!VALUES.D_NAN
      endif
    endif
  endif

  sigma_nir=0
  if keyword_set(FULL) then begin
    dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[3], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      countNan=-1
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      sigma_nir=1.*dataSet.data
      slope_sigma_nir=dataSet.slope
      offset_sigma_nir=dataSet.intercept
      if countNan gt 0 then begin
        if size(sigma_nir, /TYPE) eq 4 then sigma_nir[nanList]=!VALUES.F_NAN
        if size(sigma_nir, /TYPE) eq 5 then sigma_nir[nanList]=!VALUES.D_NAN
      endif
    endif
  endif

  dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[4], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    ts_avhrr=1.*dataSet.data
    slope_ts=dataSet.slope
    offset_ts=dataSet.intercept
    if countNan gt 0 then begin
      if size(ts_avhrr, /TYPE) eq 4 then ts_avhrr[nanList]=!VALUES.F_NAN
      if size(ts_avhrr, /TYPE) eq 5 then ts_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif

  dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[5], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    tv_avhrr=1.*dataSet.data
    slope_tv=dataSet.slope
    offset_tv=dataSet.intercept
    if countNan gt 0 then begin
      if size(tv_avhrr, /TYPE) eq 4 then tv_avhrr[nanList]=!VALUES.F_NAN
      if size(tv_avhrr, /TYPE) eq 5 then tv_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif

  dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[6], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    ;print, dataSet.fillValue
    phi_avhrr=1.*dataSet.data
    slope_phi=dataSet.slope
    offset_phi=dataSet.intercept
    if countNan gt 0 then begin
      if size(phi_avhrr, /TYPE) eq 4 then phi_avhrr[nanList]=!VALUES.F_NAN
      if size(phi_avhrr, /TYPE) eq 5 then phi_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif else begin
    dataSet=ST_operator->readNcdfVar(fullFileName, 'PHI', FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    ;print, dataSet.fillValue
    phi_avhrr=1.*dataSet.data
    slope_phi=dataSet.slope
    offset_phi=dataSet.intercept
    if countNan gt 0 then begin
      if size(phi_avhrr, /TYPE) eq 4 then phi_avhrr[nanList]=!VALUES.F_NAN
      if size(phi_avhrr, /TYPE) eq 5 then phi_avhrr[nanList]=!VALUES.D_NAN
    endif
  endelse

  dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[7], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    brf_qa_avhrr=dataSet.data
    slope_qa=dataSet.slope
    offset_qa=dataSet.intercept
    if countNan gt 0 then begin
      if size(flag, /TYPE) eq 4 then brf_qa_avhrr[nanList]=!VALUES.F_NAN
      if size(flag, /TYPE) eq 5 then brf_qa_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif else begin
    dataSet=ST_operator->readNcdfVar(fullFileName, 'QA', FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    brf_qa_avhrr=dataSet.data
    slope_qa=dataSet.slope
    offset_qa=dataSet.intercept
    if countNan gt 0 then begin
      if size(brf_qa_avhrr, /TYPE) eq 4 then brf_qa_avhrr[nanList]=!VALUES.F_NAN
      if size(brf_qa_avhrr, /TYPE) eq 5 then brf_qa_avhrr[nanList]=!VALUES.D_NAN
    endif
  endelse

  cMask1=0
  if keyword_set(FULL) then begin
    dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[8], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      countNan=-1
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      cMask1=dataSet.data
      slope_nir=dataSet.slope
      offset_nir=dataSet.intercept
      if countNan gt 0 then begin
        if size(cMask1, /TYPE) eq 4 then cMask1[nanList]=!VALUES.F_NAN
        if size(cMask1, /TYPE) eq 5 then cMask1[nanList]=!VALUES.D_NAN
      endif
    endif
  endif

  cMask2=0
  if keyword_set(FULL) then begin
    dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[9], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      countNan=-1
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      cMask2=dataSet.data
      slope_cMask2=dataSet.slope
      offset_cMask2=dataSet.intercept
      if countNan gt 0 then begin
        if size(cMask2, /TYPE) eq 4 then cMask2[nanList]=!VALUES.F_NAN
        if size(cMask2, /TYPE) eq 5 then cMask2[nanList]=!VALUES.D_NAN
      endif
    endif
  endif

  FOUND=1
  dims=size(red_avhrr, /DIM)
  origDims=dims
  if n_elements(dims) eq 1 then begin
    if dims eq 1 then newDims=2 else newDims=dims
    nred_avhrr=fltarr(newDims, 2) & nred_avhrr[*,0]=red_avhrr & red_avhrr=nred_avhrr
    nnir_avhrr=fltarr(newDims, 2) & nnir_avhrr[*,0]=nir_avhrr & nir_avhrr=nnir_avhrr
    nts_avhrr=fltarr(newDims, 2) & nts_avhrr[*,0]=ts_avhrr & ts_avhrr=nts_avhrr
    ntv_avhrr=fltarr(newDims, 2) & ntv_avhrr[*,0]=tv_avhrr & tv_avhrr=ntv_avhrr
    nphi_avhrr=fltarr(newDims, 2) & nphi_avhrr[*,0]=phi_avhrr & phi_avhrr=nphi_avhrr
    nbrf_qa_avhrr=fltarr(newDims, 2) & nbrf_qa_avhrr[*,0]=brf_qa_avhrr & brf_qa_avhrr=nbrf_qa_avhrr
  endif

  if keyword_set(FOUND) then allData={red_avhrr:red_avhrr, nir_avhrr:nir_avhrr,$
    ts_avhrr:ts_avhrr,tv_avhrr:tv_avhrr,phi_avhrr:phi_avhrr,$
    brf_qa_avhrr:brf_qa_avhrr, $
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