;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function readSPP_AVHRR_FromCSV, folder, fileName, FOUND=FOUND, $
  FULL=FULL, APPLY_CONVERSION=APPLY_CONVERSION, varName=varName, offset=offset, count=count, fid=fid, $
  PIXELS_PROCESS=PIXELS_PROCESS, PP_CSV=PP_CSV, PP_NC=PP_NC, ORIGDIMS=ORIGDIMS

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

  INT_NAN=2^15
  GOOD_QA=fix(16522)
  faparDSInfo=getStandardFaparDataSetInfo()
  avBandNames=faparDSInfo.BANDNAMES
  newFolder=ST_fileSystem->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  csvData=read_csv(fullFileName, HEADER=HEADER, TABLE_HEADER=TABLE_HEADER)

  ; 0, 1, 2,3,4,5,6,(7),(8),(9),10,11,12

  fName=ST_fileSystem->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  if n_elements(varName) eq 1 then begin
    ;slope=1
    ;offset=0
    ;fillValue=2^15
    ;dataSet=ST_operator->readNcdfVar(fullFileName, varName, slope=slope, intercept=intercept, fillvalue=fillvalue, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
    index=where(avBandNames[0] eq HEADER, FOUND)
    if keyword_set(FOUND) then begin
      countNan=-1
      dataSet={data:csvData.(index), slope:1.,$
        intercept:0.,fillValueExist:0,fillValue:INT_NAN}
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      valid=1
      FOUND=1
      data=1.*dataset.data
      if keyword_set(APPLY_CONVERSION) then begin
        data=data*dataset.slope+dataset.intercept
        slope_data=1
        offset_data=0
      endif else begin
        slope_data=dataset.slope
        offset_data=dataset.intercept
      endelse
      if countNan gt 0 then begin
        if size(data, /TYPE) eq 4 then data[nanList]=!VALUES.F_NAN
        if size(data, /TYPE) eq 5 then data[nanList]=!VALUES.D_NAN
      endif
      return, {data:data, slope_data:slope_data, offset_data:offset_data, fillValue:!VALUES.F_NAN, valid:valid}
    endif
    return, 0
  endif

  fapar=0
  slope_fapar=1
  offset_fapar=0
  ;dataSet=ST_operator->readNcdfVar(fullFileName, avBandNames[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, offset=offset, count=count)
  index=where(avBandNames[0] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
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
  index=where(avBandNames[1] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
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
  index=where(avBandNames[2] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
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
  index=where(avBandNames[3] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
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
  index=where(avBandNames[4] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
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
  index=where(avBandNames[5] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    sigma_nir=float(dataSet.data)
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
  index=where(avBandNames[6] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    ;qa=1*dataSet.data
    qa=fix(dataSet.data)
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
    index=where(avBandNames[7] eq HEADER, FOUND)
    if keyword_set(FOUND) then begin
      countNan=-1
      dataSet={data:csvData.(index), slope:1.,$
        intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      ts=float(dataSet.data)
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
    index=where(avBandNames[8] eq HEADER, FOUND)
    if keyword_set(FOUND) then begin
      countNan=-1
      dataSet={data:csvData.(index), slope:1.,$
        intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      tv=float(dataSet.data)
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
    index=where(avBandNames[9] eq HEADER, FOUND)
    if keyword_set(FOUND) then begin
      countNan=-1
      dataSet={data:csvData.(index), slope:1.,$
        intercept:0.,fillValueExist:0,fillValue:!VALUES.F_NAN}
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      phi=float(dataSet.data)
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
  index=where(avBandNames[10] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:INT_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    toc_red=float(dataSet.data)
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
  index=where(avBandNames[11] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:INT_NAN}
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
  index=where(avBandNames[12] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:csvData.(index), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:INT_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    flag=fix(dataSet.data)
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


  FOUND=1
  dims=size(fapar, /DIM)
  origDims=dims
  if n_elements(dims) eq 1 then begin
    if dims eq 1 then newDims=2 else newDims=dims
    nfapar=fltarr(newDims, 2) & nfapar[*,0]=fapar & fapar=nfapar
    nred=fltarr(newDims, 2) & nred[*,0]=red & red=nred
    nsigma_red=fltarr(newDims, 2) & nsigma_red[*,0]=sigma_red & sigma_red=nsigma_red
    ntoc_red=fltarr(newDims, 2) & ntoc_red[*,0]=toc_red & toc_red=ntoc_red
    nnir=fltarr(newDims, 2) & nnir[*,0]=nir & nir=nnir
    nsigma_nir=fltarr(newDims, 2) & nsigma_nir[*,0]=sigma_nir & sigma_nir=nsigma_nir
    ntoc_nir=fltarr(newDims, 2) & ntoc_nir[*,0]=toc_nir & toc_nir=ntoc_nir
    nts=fltarr(newDims, 2) & nts[*,0]=ts & ts=nts
    ntv=fltarr(newDims, 2) & ntv[*,0]=tv & tv=ntv
    nphi=fltarr(newDims, 2) & nphi[*,0]=phi & phi=nphi
    nflag=intarr(newDims, 2) & nflag[*,0]=flag & flag=nflag
    nqa=intarr(newDims, 2) & nqa[*,0]=qa & qa=nqa
  endif
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

end