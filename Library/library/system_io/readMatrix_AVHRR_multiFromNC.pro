;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function readMatrix_AVHRR_multiFromNC, folder, fileName, FOUND=FOUND, $
  FULL=FULL, APPLY_CONVERSION=APPLY_CONVERSION, varName=varName, offset=offset, count=count, fid=fid, $
  PIXELS_PROCESS=PIXELS_PROCESS, PP_CSV=PP_CSV, PP_NC=PP_NC  

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

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

  ; 0, 1, 2,3,4,5,6,(7),(8),(9),10,11,12
  bands=avBandNames[0:12]

  newFolder=ST_fileSystem->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  fName=ST_fileSystem->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  fullInfo=ST_operator->readMultiNcdfVar(fullFileName, bands, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, offset=offset, count=count)

  fapar=0
  slope_fapar=1
  offset_fapar=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dsIndex=0
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    fapar=1.*dataset.data
    if keyword_set(APPLY_CONVERSION) then begin
      fapar=fapar*dataSet.slope+dataSet.intercept
      slope_fapar=1
      offset_fapar=0
    endif else begin
      slope_fapar=dataSet.slope
      offset_fapar=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(fapar, /TYPE) eq 4 then fapar[nanList]=!VALUES.F_NAN
      if size(fapar, /TYPE) eq 5 then fapar[nanList]=!VALUES.D_NAN
    endif
  endif

  sigma=0
  slope_sigma=1
  offset_sigma=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[1], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dsIndex=1
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    sigma=1.*dataset.data
    if keyword_set(APPLY_CONVERSION) then begin
      sigma=sigma*dataSet.slope+dataSet.intercept
      slope_sigma=1
      offset_sigma=0
    endif else begin
      slope_sigma=dataSet.slope
      offset_sigma=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(sigma, /TYPE) eq 4 then sigma[nanList]=!VALUES.F_NAN
      if size(sigma, /TYPE) eq 5 then sigma[nanList]=!VALUES.D_NAN
    endif
  endif

  red=0
  slope_red=1
  offset_red=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[2], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dsIndex=2
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    red=1.*dataset.data
    if keyword_set(APPLY_CONVERSION) then begin
      red=red*dataSet.slope+dataSet.intercept
      slope_red=1
      offset_red=0
    endif else begin
      slope_red=dataSet.slope
      offset_red=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(red, /TYPE) eq 4 then red[nanList]=!VALUES.F_NAN
      if size(red, /TYPE) eq 5 then red[nanList]=!VALUES.D_NAN
    endif
  endif

  sigma_red=0
  slope_sigma_red=1
  offset_sigma_red=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[3], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dsIndex=3
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    sigma_red=1.*dataset.data
    if keyword_set(APPLY_CONVERSION) then begin
      sigma_red=sigma_red*dataSet.slope+dataSet.intercept
      slope_sigma_red=1
      offset_sigma_red=0
    endif else begin
      slope_sigma_red=dataSet.slope
      offset_sigma_red=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(sigma_red, /TYPE) eq 4 then sigma_red[nanList]=!VALUES.F_NAN
      if size(sigma_red, /TYPE) eq 5 then sigma_red[nanList]=!VALUES.D_NAN
    endif
  endif

  nir=0
  slope_nir=1
  offset_nir=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[4], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dsIndex=4
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    nir=1.*dataset.data
    if keyword_set(APPLY_CONVERSION) then begin
      nir=nir*dataSet.slope+dataSet.intercept
      slope_nir=1
      offset_nir=0
    endif else begin
      slope_nir=dataSet.slope
      offset_nir=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(nir, /TYPE) eq 4 then nir[nanList]=!VALUES.F_NAN
      if size(nir, /TYPE) eq 5 then nir[nanList]=!VALUES.D_NAN
    endif
  endif

  sigma_nir=0
  slope_sigma_nir=1
  offset_sigma_nir=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[5], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dsIndex=5
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    sigma_nir=1*dataSet.data
    if keyword_set(APPLY_CONVERSION) then begin
      sigma_nir=sigma_nir*dataSet.slope+dataSet.intercept
      slope_sigma_nir=1
      offset_sigma_nir=0
    endif else begin
      slope_sigma_nir=dataSet.slope
      offset_sigma_nir=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(sigma_nir, /TYPE) eq 4 then sigma_nir[nanList]=!VALUES.F_NAN
      if size(sigma_nir, /TYPE) eq 5 then sigma_nir[nanList]=!VALUES.D_NAN
    endif
  endif

  qa=0
  slope_qa=1
  offset_qa=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[6], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dsIndex=6
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    qa=1*dataSet.data
    if keyword_set(APPLY_CONVERSION) then begin
      qa=qa*dataSet.slope+dataSet.intercept
      slope_qa=1
      offset_qa=0
    endif else begin
      slope_qa=dataSet.slope
      offset_qa=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(qa, /TYPE) eq 4 then qa[nanList]=!VALUES.F_NAN
      if size(qa, /TYPE) eq 5 then qa[nanList]=!VALUES.D_NAN
    endif
  endif

  ts=0
  slope_ts=1
  offset_ts=0
  if keyword_set(FULL) then begin
    ;dataSet=ST_operator->readNcdfVar(fullFileName, AVBANDNAMES[7], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    dsIndex=7
    if keyword_set(FOUND[dsIndex]) then begin
      values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
        slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
      countNan=-1
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      ts=1*dataSet.data
      if keyword_set(APPLY_CONVERSION) then begin
        ts=ts*dataSet.slope+dataSet.intercept
        slope_ts=1
        offset_ts=0
      endif else begin
        slope_ts=dataSet.slope
        offset_ts=dataSet.intercept
      endelse
      if countNan gt 0 then begin
        if size(ts, /TYPE) eq 4 then ts[nanList]=!VALUES.F_NAN
        if size(ts, /TYPE) eq 5 then ts[nanList]=!VALUES.D_NAN
      endif
    endif
  endif

  tv=0
  slope_tv=1
  offset_tv=0
  if keyword_set(FULL) then begin
    ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[8], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    dsIndex=8
    if keyword_set(FOUND[dsIndex]) then begin
      values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
        slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
      countNan=-1
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      tv=1*dataSet.data
      if keyword_set(APPLY_CONVERSION) then begin
        tv=tv*dataSet.slope+dataSet.intercept
        slope_tv=1
        offset_tv=0
      endif else begin
        slope_tv=dataSet.slope
        offset_tv=dataSet.intercept
      endelse
      if countNan gt 0 then begin
        if size(tv, /TYPE) eq 4 then tv[nanList]=!VALUES.F_NAN
        if size(tv, /TYPE) eq 5 then tv[nanList]=!VALUES.D_NAN
      endif
    endif
  endif

  phi=0
  slope_phi=1
  offset_phi=0
  if keyword_set(FULL) then begin
    ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[9], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    dsIndex=9
    if keyword_set(FOUND[dsIndex]) then begin
      values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
        slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
      countNan=-1
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      phi=1*dataSet.data
      if keyword_set(APPLY_CONVERSION) then begin
        phi=phi*dataSet.slope+dataSet.intercept
        slope_phi=1
        offset_phi=0
      endif else begin
        slope_phi=dataSet.slope
        offset_phi=dataSet.intercept
      endelse
      if countNan gt 0 then begin
        if size(phi, /TYPE) eq 4 then phi[nanList]=!VALUES.F_NAN
        if size(phi, /TYPE) eq 5 then phi[nanList]=!VALUES.D_NAN
      endif
    endif
  endif

  toc_red=0
  slope_toc_red=1
  offset_toc_red=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[10], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dsIndex=10
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    toc_red=1*dataSet.data
    if keyword_set(APPLY_CONVERSION) then begin
      toc_red=toc_red*dataSet.slope+dataSet.intercept
      slope_toc_red=1
      offset_toc_red=0
    endif else begin
      slope_toc_red=dataSet.slope
      offset_toc_red=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(toc_red, /TYPE) eq 4 then toc_red[nanList]=!VALUES.F_NAN
      if size(toc_red, /TYPE) eq 5 then toc_red[nanList]=!VALUES.D_NAN
    endif
  endif

  toc_nir=0
  slope_toc_nir=1
  offset_toc_nir=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[11], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dsIndex=11
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    toc_nir=1.*dataSet.data
    if keyword_set(APPLY_CONVERSION) then begin
      toc_nir=toc_nir*dataSet.slope+dataSet.intercept
      slope_toc_nir=1
      offset_toc_nir=0
    endif else begin
      slope_toc_nir=dataSet.slope
      offset_toc_nir=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(toc_nir, /TYPE) eq 4 then toc_nir[nanList]=!VALUES.F_NAN
      if size(toc_nir, /TYPE) eq 5 then toc_nir[nanList]=!VALUES.D_NAN
    endif
  endif

  flag=0
  slope_flag=1
  offset_flag=0
  ;if keyword_set(FULL) then begin
  dsIndex=12
  if keyword_set(FOUND[dsIndex]) then begin
    values=*(fullInfo.data[dsIndex]) & dataSet={data:values, fillValue:fullInfo.fillValues[dsIndex], fillValueExist:fullInfo.fillValueExists[dsIndex], $
      slope:fullInfo.slopes[dsIndex], intercept:fullInfo.intercepts[dsIndex]}
    countNan=-1
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    flag=dataSet.data
    if keyword_set(APPLY_CONVERSION) then begin
      flag=flag*dataSet.slope+dataSet.intercept
      slope_flag=1
      offset_flag=0
    endif else begin
      slope_flag=dataSet.slope
      offset_flag=dataSet.intercept
    endelse
    if countNan gt 0 then begin
      if size(flag, /TYPE) eq 4 then flag[nanList]=!VALUES.F_NAN
      if size(flag, /TYPE) eq 5 then flag[nanList]=!VALUES.D_NAN
    endif
  endif
  ;endif

  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[12], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, offset=offset, count=count)
  ptr_free, fullInfo.data
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