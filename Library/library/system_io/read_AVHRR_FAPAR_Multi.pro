;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function read_AVHRR_FAPAR_Multi, folder, fileName, FOUND=FOUND, SWITCH_TS_TV=SWITCH_TS_TV, $
  FULL=FULL, APPLY_SLOPE=APPLY_SLOPE, varName=varName, offset=offset, count=count, fid=fid


  ;  catch, error_status
  ;
  ;  if error_status NE 0 THEN BEGIN
  ;    ERROR=1
  ;    FOUND=0
  ;    catch, /CANCEL
  ;    msg='Problem with file '+folder+fileName+' check version, contents, existence or read permission.'
  ;    ;errMsg=dialog_message(msg, /ERROR)
  ;    ;message, msg
  ;    return, 0
  ;  endif

  faparDSInfo=getStandardFaparDataSetInfo()
  avBandNames=faparDSInfo.bandNames
  if keyword_set(SWITCH_TS_TV) then print, avBandNames

  ; 0, 1, 2,3,4,5,6,(7),(8),(9),10,11,(12)

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  newFolder=fsObj->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  if keyword_set(FULL) then varToReadIndexes=indgen(n_elements(avBandNames)) else varToReadIndexes=[0,1,2,3,4,5,6,7,8,10,11,12];varToReadIndexes=[0,1,2,3,4,5,6,10,11]; exclude [7,8,9,12]
  varToReadIndexes=indgen(n_elements(avBandNames))

  fName=fsObj->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  if n_elements(varName) eq 1 then begin
    slope=1
    offset=0
    fillValue=2^15
    dataSet=operatorObj->readNcdfVar(fullFileName, varName, slope=slope, intercept=intercept, fillvalue=fillvalue, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, offset=offset, count=count, fid=fid)
    if keyword_set(FOUND) then begin
      valid=1
      FOUND=1
      fapar=1.*dataset.data
      if keyword_set(APPLY_SLOPE) then begin
        fapar=fapar*dataset.slope+dataset.intercept
        slope_fapar=1
        offset_fapar=0
      endif else begin
        slope_fapar=dataset.slope
        offset_fapar=dataset.intercept
      endelse
      return, {fapar:fapar, slope_fapar:slope_fapar, offset_fapar:offset_fapar, fillValue:fillValue, valid:valid}
    endif
    return, 0
  endif

  dataSets=operatorObj->readMultiNcdfVar(fullFileName, avBandNames[varToReadIndexes], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, offset=offset, count=count)

  if keyword_set(FOUND) then begin

    fapar=1.*(*(dataSets[0].data))
    sigma=1.*(*(dataSets[1].data))
    red=1.*(*(dataSets[2].data))
    sigma_red=1.*(*(dataSets[3].data))
    nir=1.*(*(dataSets[4].data))
    sigma_nir=1.*(*(dataSets[5].data))
    qa=1.*(*(dataSets[6].data))
    ts=1.*(*(dataSets[7].data))
    tv=1.*(*(dataSets[8].data))
    phi=1.*(*(dataSets[9].data))
    toc_red=1.*(*(dataSets[10].data))
    toc_nir=1.*(*(dataSets[11].data))
    flag=1.*(*(dataSets[12].data))
    if keyword_set(APPLY) then begin
      fapar=fapar*dataset[0].slope+dataset[0].intercept & slope_fapar=1 & offset_fapar=0
      sigma=slope_sigma*dataset[1].slope+dataset[1].intercept & slope_sigma=1 & offset_sigma=0
      red=slope_red*dataset[2].slope+dataset[2].intercept & slope_red=1 & offset_red=0
      sigma_red=slope_sigma_red*dataset[3].slope+dataset[3].intercept & slope_sigma_red=1 & offset_sigma_red=0
      nir=slope_nir*dataset[4].slope+dataset[4].intercept & slope_nir=1 & offset_nir=0
      sigma_nir=slope_sigma_nir*dataset[5].slope+dataset[5].intercept & slope_sigma_nir=1 & offset_sigma_nir=0
      qa=slope_fapar*dataset[6].slope+dataset[6].intercept & slope_qa=1 & offset_qa=0
      ts=fapar*dataset[7].slope+dataset[7].intercept & slope_ts=1 & offset_ts=0
      tv=fapar*dataset[8].slope+dataset[8].intercept & slope_tv=1 & offset_tv=0
      phi=fapar*dataset[9].slope+dataset[9].intercept & slope_toc_red=1 & offset_phi=0
      toc_red=fapar*dataset[10].slope+dataset[10].intercept & slope_toc_red=1 & offset_toc_red=0
      toc_nir=fapar*dataset[11].slope+dataset[11].intercept & slope_toc_nir=1 & offset_toc_nir=0
      flag=fapar*dataset[12].slope+dataset[12].intercept & slope_flag=1 & offset_flag=0
    endif else begin
      slope_fapar=dataSets[0].slope & offset_fapar=dataSets[0].intercept
      slope_sigma=dataSets[1].slope & offset_sigma=dataSets[1].intercept
      slope_red=dataSets[2].slope & offset_red=dataSets[2].intercept
      slope_sigma_red=dataSets[3].slope & offset_sigma_red=dataSets[3].intercept
      slope_nir=dataSets[4].slope & offset_nir=dataSets[4].intercept
      slope_sigma_nir=dataSets[5].slope & offset_sigma_nir=dataSets[5].intercept
      slope_qa=dataSets[6].slope & offset_qa=dataSets[6].intercept
      slope_ts=dataSets[7].slope & offset_ts=dataSets[7].intercept
      slope_tv=dataSets[8].slope & offset_tv=dataSets[8].intercept
      slope_phi=dataSets[9].slope & offset_phi=dataSets[9].intercept
      slope_toc_red=dataSets[10].slope & offset_toc_red=dataSets[10].intercept
      slope_toc_nir=dataSets[11].slope & offset_toc_nir=dataSets[11].intercept
      slope_flag=dataSets[12].slope & offset_flag=dataSets[12].intercept
    endelse
    allData={  fapar:fapar, sigma:sigma, $
      red:red, sigma_red:sigma_red, $
      nir:nir, sigma_nir:sigma_nir, $
      qa:qa, $
      ts:ts, tv:tv, phi:phi, $
      toc_red:toc_red, toc_nir:toc_nir, $
      flag:flag, $
      slope_fapar:slope_fapar,slope_sigma:slope_sigma,$
      slope_nir:slope_nir,slope_red:slope_red,$
      slope_toc_red:slope_toc_red,slope_toc_nir:slope_toc_nir,$
      offset_fapar:offset_fapar,offset_sigma:offset_sigma,$
      offset_red:offset_red,offset_nir:offset_nir,$
      offset_toc_red:offset_toc_red, offset_toc_nir:offset_toc_nir,$
      day:flag, valid:0}
    return, allData
  endif
  return, 0

end