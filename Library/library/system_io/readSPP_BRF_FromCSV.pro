;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function readSPP_BRF_FromCSV, folder, fileName, FOUND=FOUND, ORIGDIMS=ORIGDIMS, extraData=extraData

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  ;  avBandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'PHI', 'QA']
  INT_NAN=2^15
  GOOD_QA=fix(16522)
  ;brfsDSInfo=getStandardBrfDataSetInfo()
  brfsDSInfo=getStandardSPPBrfDataSetInfo()
  newFolder=ST_fileSystem->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName
  restore, ST_fileSystem->getSPPTemplateFile()
  ;restore, 'spp_template1.sav'
  ;restore, 'spp_template2.sav'
  asciiData=read_ascii(fullFileName, TEMPLATE = SPP_Template)
  ;test2=read_ascii(fullFileName, TEMPLATE = SPP_Template1)
  ;test3=read_ascii(fullFileName, TEMPLATE = SPP_Template2)
  ;csvData=read_csv(fullFileName, HEADER=HEADER, TABLE_HEADER=TABLE_HEADER)
  ;headers=strsplit(HEADER, ':', /EXTRACT, /PRESERVE)
  fieldList=tag_names(asciiData)
  fieldNo=n_elements(fieldList)
  header=''
  for i=0, fieldNo-1 do begin
    thisTitle=strsplit((asciiData.(i))[0], ':', /EXTRACT, /PRESERVE)
    header=[header, thisTitle[0]]
  endfor
  header=header[1:*]

  avBandNames=brfsDSInfo.bandNames
  newFolder=ST_fileSystem->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  fName=ST_fileSystem->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  ;0:'featureId',
  ;1:'start_time',
  ;2:'stop_time',
  ;3:'SREFL_CH1'
  ;4:'SREFL_CH2'
  ;5:'l1_flags'
  ;6:'latitude'
  ;7:'longitude'
  ;8:'SZEN'
  ;9:'VZEN'
  ;10:'RELAZ', $
  ;11:'RADIANCE_CH1'
  ;12;'RADIANCE_CH2'

  ;extradataIdxs=[0,1,2,6,7,11,12]
  extradataIdxs=indgen(fieldNo)
  nExtra=n_elements(extradataIdxs)
  extradata=strarr(nExtra, n_elements(asciiData.(0)))
  ;0:'featureId',
  ;1:'start_time',
  ;2:'stop_time',
  ;6:'latitude'
  ;7:'longitude'
  ;11:'RADIANCE_CH1'
  ;12;'RADIANCE_CH2'
  for i=0, nExtra-1 do begin
    extradata[i,*]=asciiData.(extradataIdxs[i])
  endfor
  index=where(avBandNames[3] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:asciiData.(index)[1:*], slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:INT_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    red_avhrr=1.*dataSet.data
    slope_red=dataSet.slope
    offset_red=dataSet.intercept
    if countNan gt 0 then begin
      if size(red_avhrr, /TYPE) eq 4 then red_avhrr[nanList]=!VALUES.F_NAN
      if size(red_avhrr, /TYPE) eq 5 then red_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif

  index=where(avBandNames[4] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:asciiData.(index)[1:*], slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:INT_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    nir_avhrr=1.*dataSet.data
    slope_nir=dataSet.slope
    offset_nir=dataSet.intercept
    if countNan gt 0 then begin
      if size(nir_avhrr, /TYPE) eq 4 then nir_avhrr[nanList]=!VALUES.F_NAN
      if size(nir_avhrr, /TYPE) eq 5 then nir_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif

  sigma_red=red_avhrr
  sigma_red[*]=!VALUES.F_NAN ;
  sigma_red[*]=0.1 ; ToDo how to deal with NO SIGMA data...
  slope_sigma_red=1
  offset_sigma_red=0
  if keyword_set(FULL) then begin
    index=where('SIGMA_RED' eq HEADER, FOUND)
    if keyword_set(FOUND) then begin
      countNan=-1
      dataSet={data:asciiData.(index)[1:*], slope:1.,$
        intercept:0.,fillValueExist:0,fillValue:INT_NAN}
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

  sigma_nir=nir_avhrr
  sigma_nir[*]=!VALUES.F_NAN
  sigma_nir[*]=0.1 ; ToDo how to deal with NO SIGMA data...
  slope_sigma_nir=1
  offset_sigma_nir=0
  if keyword_set(FULL) then begin
    index=where('SIGMA_NIR' eq HEADER, FOUND)
    if keyword_set(FOUND) then begin
      countNan=-1
      dataSet={data:asciiData.(index)[1:*], slope:1.,$
        intercept:0.,fillValueExist:0,fillValue:INT_NAN}
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

  index=where(avBandNames[8] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:asciiData.(index)[1:*], slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:INT_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    ts_avhrr=1.*dataSet.data
    slope_ts=dataSet.slope
    offset_ts=dataSet.intercept
    if countNan gt 0 then begin
      if size(ts_avhrr, /TYPE) eq 4 then ts_avhrr[nanList]=!VALUES.F_NAN
      if size(ts_avhrr, /TYPE) eq 5 then ts_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif

  index=where(avBandNames[9] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:asciiData.(index)[1:*], slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:INT_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    tv_avhrr=1.*dataSet.data
    slope_tv=dataSet.slope
    offset_tv=dataSet.intercept
    if countNan gt 0 then begin
      if size(tv_avhrr, /TYPE) eq 4 then tv_avhrr[nanList]=!VALUES.F_NAN
      if size(tv_avhrr, /TYPE) eq 5 then tv_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif

  index=where(avBandNames[10] eq HEADER, FOUND)
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:asciiData.(index)[1:*], slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:INT_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    ;print, dataSet.fillValue
    phi_avhrr=1.*dataSet.data
    slope_phi=dataSet.slope
    offset_phi=dataSet.intercept
    if countNan gt 0 then begin
      if size(phi_avhrr, /TYPE) eq 4 then phi_avhrr[nanList]=!VALUES.F_NAN
      if size(phi_avhrr, /TYPE) eq 5 then phi_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif

  index=where(avBandNames[5] eq HEADER, FOUND)
  brf_qa_avhrr=nir_avhrr
  brf_qa_avhrr[*]=GOOD_QA
  slope_qa=1.
  offset_qa=0.
  if keyword_set(FOUND) then begin
    countNan=-1
    dataSet={data:fix(asciiData.(index)[1:*]), slope:1.,$
      intercept:0.,fillValueExist:0,fillValue:INT_NAN}
    if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
    brf_qa_avhrr=dataSet.data
    slope_qa=dataSet.slope
    offset_qa=dataSet.intercept
    if countNan gt 0 then begin
      if size(flag, /TYPE) eq 4 then brf_qa_avhrr[nanList]=!VALUES.F_NAN
      if size(flag, /TYPE) eq 5 then brf_qa_avhrr[nanList]=!VALUES.D_NAN
    endif
  endif else begin
    index=where('QA' eq HEADER, FOUND)
    if keyword_set(FOUND) then begin
      countNan=-1
      dataSet={data:fix(asciiData.(index)[1:*]), slope:1.,$
        intercept:0.,fillValueExist:0,fillValue:INT_NAN}
      if dataSet.fillValueExist then nanList=where(dataSet.data eq dataSet.fillValue, countNan)
      brf_qa_avhrr=dataSet.data
      slope_qa=dataSet.slope
      offset_qa=dataSet.intercept
      if countNan gt 0 then begin
        if size(brf_qa_avhrr, /TYPE) eq 4 then brf_qa_avhrr[nanList]=!VALUES.F_NAN
        if size(brf_qa_avhrr, /TYPE) eq 5 then brf_qa_avhrr[nanList]=!VALUES.D_NAN
      endif
    endif
  endelse

  cMask1=0
  if keyword_set(FULL) then begin
    index=where(avBandNames[8] eq HEADER, FOUND)
    if keyword_set(FOUND) then begin
      countNan=-1
      dataSet={data:asciiData.(index)[1:*], slope:1.,$
        intercept:0.,fillValueExist:0,fillValue:INT_NAN}
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
    index=where(avBandNames[9] eq HEADER, FOUND)
    if keyword_set(FOUND) then begin
      countNan=-1
      dataSet={data:asciiData.(index)[1:*], slope:1.,$
        intercept:0.,fillValueExist:0,fillValue:INT_NAN}
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
  allData={red_avhrr:red_avhrr, nir_avhrr:nir_avhrr,$
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