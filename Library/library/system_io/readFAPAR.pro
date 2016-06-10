;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function readFAPAR, folder, fileName, FOUND=FOUND, SWITCH_TS_TV=SWITCH_TS_TV, FULL=FULL, APPLY_SLOPE=APPLY_SLOPE


  catch, error_status

  if error_status NE 0 THEN BEGIN
    ERROR=1
    FOUND=0
    catch, /CANCEL
    msg='Problem with file '+folder+fileName+' check version, contents, existence or read permission.'
    ;errMsg=dialog_message(msg, /ERROR)
    ;message, msg
    return, 0
  endif

  faparDSInfo=getStandardFaparDataSetInfo()
  avBandNames=faparDSInfo.bandNames
  if keyword_set(SWITCH_TS_TV) then print, avBandNames

  ; 0, 1, 2,3,4,5,6,(7),(8),(9),10,11,12

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  newFolder=fsObj->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  fName=fsObj->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  fapar=0
  slope_fapar=1
  offset_fapar=0
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    fapar=1.*dataset.data
    if keyword_set(APPLY_SLOPE) then begin
      fapar=fapar*dataSet.slope+dataSet.intercept
      slope_fapar=1
      offset_fapar=0
    endif else begin
      slope_fapar=dataSet.slope
      offset_fapar=dataSet.intercept
    endelse
  endif

  sigma=0
  slope_sigma=1
  offset_sigma=0
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[1], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    sigma=1.*dataset.data
    if keyword_set(APPLY_SLOPE) then begin
      sigma=sigma*dataSet.slope+dataSet.intercept
      slope_sigma=1
      offset_sigma=0
    endif else begin
      slope_sigma=dataSet.slope
      offset_sigma=dataSet.intercept
    endelse
  endif

  red=0
  slope_red=1
  offset_red=0
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[2], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    red=1.*dataset.data
    if keyword_set(APPLY_SLOPE) then begin
      red=red*dataSet.slope+dataSet.intercept
      slope_red=1
      offset_red=0
    endif else begin
      slope_red=dataSet.slope
      offset_red=dataSet.intercept
    endelse
  endif

  sigma_red=0
  slope_sigma_red=1
  offset_sigma_red=0
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[3], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    sigma_red=1.*dataset.data
    if keyword_set(APPLY_SLOPE) then begin
      sigma_red=sigma_red*dataSet.slope+dataSet.intercept
      slope_sigma_red=1
      offset_sigma_red=0
    endif else begin
      slope_sigma_red=dataSet.slope
      offset_sigma_red=dataSet.intercept
    endelse
  endif

  nir=0
  slope_nir=1
  offset_nir=0
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[4], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    nir=1.*dataset.data
    if keyword_set(APPLY_SLOPE) then begin
      nir=nir*dataSet.slope+dataSet.intercept
      slope_nir=1
      offset_nir=0
    endif else begin
      slope_nir=dataSet.slope
      offset_nir=dataSet.intercept
    endelse
  endif

  sigma_nir=0
  slope_sigma_nir=1
  offset_sigma_nir=0
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[5], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    sigma_nir=1*dataSet.data
    if keyword_set(APPLY_SLOPE) then begin
      sigma_nir=sigma_nir*dataSet.slope+dataSet.intercept
      slope_sigma_nir=1
      offset_sigma_nir=0
    endif else begin
      slope_sigma_nir=dataSet.slope
      offset_sigma_nir=dataSet.intercept
    endelse
  endif

  qa=0
  slope_qa=1
  offset_qa=0
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[6], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    qa=1*dataSet.data
    if keyword_set(APPLY_SLOPE) then begin
      qa=qa*dataSet.slope+dataSet.intercept
      slope_qa=1
      offset_qa=0
    endif else begin
      slope_qa=dataSet.slope
      offset_qa=dataSet.intercept
    endelse
  endif

  if keyword_set(SWITCH_TS_TV) then begin
    temp=avBandNames[7]
    avBandNames[7]=avBandNames[8]
    avBandNames[8]=temp
  endif

  ts=0
  slope_ts=1
  offset_ts=0
  if keyword_set(FULL) then begin
    dataSet=operatorObj->readNcdfVar(fullFileName, AVBANDNAMES[7], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      ts=1*dataSet.data
      if keyword_set(APPLY_SLOPE) then begin
        ts=ts*dataSet.slope+dataSet.intercept
        slope_ts=1
        offset_ts=0
      endif else begin
        slope_ts=dataSet.slope
        offset_ts=dataSet.intercept
      endelse
    endif
  endif

  tv=0
  slope_tv=1
  offset_tv=0
  if keyword_set(FULL) then begin
    dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[8], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      tv=1*dataSet.data
      if keyword_set(APPLY_SLOPE) then begin
        tv=tv*dataSet.slope+dataSet.intercept
        slope_tv=1
        offset_tv=0
      endif else begin
        slope_tv=dataSet.slope
        offset_tv=dataSet.intercept
      endelse
    endif
  endif

  phi=0
  slope_phi=1
  offset_phi=0
  if keyword_set(FULL) then begin
    dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[9], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    if keyword_set(FOUND) then begin
      phi=1*dataSet.data
      if keyword_set(APPLY_SLOPE) then begin
        phi=phi*dataSet.slope+dataSet.intercept
        slope_phi=1
        offset_phi=0
      endif else begin
        slope_phi=dataSet.slope
        offset_phi=dataSet.intercept
      endelse
    endif
  endif

  toc_red=0
  slope_toc_red=1
  offset_toc_red=0
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[10], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    toc_red=1*dataSet.data
    if keyword_set(APPLY_SLOPE) then begin
      toc_red=toc_red*dataSet.slope+dataSet.intercept
      slope_toc_red=1
      offset_toc_red=0
    endif else begin
      slope_toc_red=dataSet.slope
      offset_toc_red=dataSet.intercept
    endelse
  endif

  toc_nir=0
  slope_toc_nir=1
  offset_toc_nir=0
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[11], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    toc_nir=1.*dataSet.data
    if keyword_set(APPLY_SLOPE) then begin
      toc_nir=toc_nir*dataSet.slope+dataSet.intercept
      slope_toc_nir=1
      offset_toc_nir=0
    endif else begin
      slope_toc_nir=dataSet.slope
      offset_toc_nir=dataSet.intercept
    endelse
  endif

  flag=0
  slope_flag=1
  offset_flag=0
  ;if keyword_set(FULL) then begin
  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[12], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    flag=dataSet.data
    if keyword_set(APPLY_SLOPE) then begin
      flag=flag*dataSet.slope+dataSet.intercept
      slope_flag=1
      offset_flag=0
    endif else begin
      slope_flag=dataSet.slope
      offset_flag=dataSet.intercept
    endelse
  endif
  ;endif

  ;dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[12], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)

  if keyword_set(FOUND) then begin
    allData={  fapar:fapar, sigma:sigma, $

      red:red, sigma_red:sigma_red, $
      nir:nir, sigma_nir:sigma_nir, $
      qa:qa, $
      ts:ts, tv:tv, phi:phi, $
      toc_red:toc_red, toc_nir:toc_nir, $
      flag:flag, $
      slope_fapar:slope_fapar,slope_sigma:slope_sigma,$
      slope_nir:slope_nir,slope_red:slope_red,$
      slope_qa:slope_qa, slope_flag:slope_flag, $
      slope_toc_nir:slope_toc_nir,slope_toc_red:slope_toc_red,$
      offset_fapar:offset_fapar,offset_sigma:offset_sigma,$
      offset_nir:offset_nir,offset_red:offset_red,$
      offset_toc_nir:offset_toc_nir, offset_toc_red:offset_toc_red,$
      offset_qa:offset_qa, offset_flag:offset_flag, $
      day:qa, valid:0}
    return, allData
  endif
  return, 0

end