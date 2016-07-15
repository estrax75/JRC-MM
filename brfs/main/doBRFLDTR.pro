function doBRFLDTR, sourceFile, confDir, year, month, day, sensor, missionCode, noaaCode, resolution, mainVar, checkBaseDir, writeBaseDir, tempDir, $
  testFile=testFile, OVERWRITE=OVERWRITE, HDF=HDF, NC=NC

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  ;Catch, theError
  ;IF theError NE 0 THEN BEGIN
  ;  Catch, /CANCEL
  ;  print, 'fail to create results for dd/mm/yyyy', day, month, year
  ;  RETURN, -1
  ;ENDIF

  ;fname='/space3/storage/products/results/FAPAR/SWF_TO_CMP/SEA01_ORB01_20031221000000_20031231000000_L3_FPA_000001_900S900N1800W1800E_PLC_0500D_PRO.NC'
  ;ST_operator->readHdfFullInfoData, infoVar1b[1], infoVar1b[0], red_avh09, red_slope, red_offset, red_fillvalue, ERROR=ERROR
  
  ;NaN=-9999 ;!VALUES.F_NAN
  ;stop
  INT_NAN=-9999

  outputBaseDir=ST_fileSystem->adjustDirSep(outputBaseDir, /ADD)
  confDir=ST_fileSystem->adjustDirSep(confDir, /ADD)
  tempDir=ST_fileSystem->adjustDirSep(tempDir, /ADD)

  ;yearS=string(year, format='(I04)')
  ;monthS=string(month, format='(I02)')
  ;dayS=string(day, format='(I02)')

  instrument='AVH'
  version='N'+string(noaaCode, format='(I02)');version='001'
  indicator='LAN'
  spatialResolution='0005D'
  level='L2'
  
  resFileNCInfo=build_JRC_BRDF_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
    product, version, 'NC',  indicator=indicator, level, projection=projection)
  resFileHDFInfo=build_JRC_BRDF_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
    product, version, 'HDF',  indicator=indicator, level, projection=projection)

  outputDir=outputBaseDir+resFileNCInfo.filePath
  outputDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)
  resFileNC=outputDir+resFileNCInfo.fileName
  resFileHDF=outputDir+resFileHDFInfo.fileName
  ;resFName=buildBrfFileName_D(sensor, resolution, year, month, day, missionCode, noaaCode, mainVar)
  ;resFileNC=ST_fileSystem->addFileExtension(filePath+resFName, 'nc')
  ;resFileHDF=ST_fileSystem->addFileExtension(filePath+resFName, 'hdf')

  checkNC=file_info(resFileNC)
  checkHDF=file_info(resFileHDF)

  if checkNC[0].size ne 0 and ~keyword_set(OVERWRITE) then NONCWRITE=1
  if checkHDF[0].size ne 0 and ~keyword_set(OVERWRITE) then NOHDFWRITE=1
  if ~keyword_set(HDF) then NOHDFWRITE=1
  if ~keyword_set(NC) then NONCWRITE=1

  ;  if keyword_set(SWITCH_TS_TV) then begin
  ;    switch_ts_tv_BRF, resFileNC, tempDir, outputDir, ST_operator, ST_fileSystem, NC=checkNC[0].size ne 0, HDF=checkHDF[0].size ne 0
  ;    return, 0
  ;  endif
  ;if keyword_set(NONCWRITE) then return, -1
  if keyword_set(NONCWRITE) and keyword_set(NOHDFWRITE) then return, -1

  BRDF_params=getPGEInfo(confDir, file, globDim=globDim)

  ;AVHRR-Land_v004_AVH09C1_NOAA-16_20030101_c20140421105754.nc
  ;fName1=ST_fileSystem->getFileNameInfo(file1, filePath=dir1, extension=ext1)
  ;AVH09C1.A2003001.N16.004.2014111105754.hdf
  fName=ST_fileSystem->getFileNameInfo(sourceFile, filePath=dir, extension=ext)
  ;GLOBAL_L3_GEOG_0.05DEG_001-001_03.NOAA-16_BRF.hdf // GLOBAL_P17_GEOG_0.05DEG_203-203_99.NOAA-14
  ;fName3=ST_fileSystem->getFileNameInfo(file3, filePath=dir3, extension=ext3)
  ;fName4=ST_fileSystem->getFileNameInfo(file4, filePath=dir4, extension=ext4)

  ; source brf file1
  infoVar1b=['SREFL_CH1', sourceFile, fName, dir, ext, 'input_data[0,*]/red_avhrr']

  ST_operator->readHdfFullInfoData, infoVar1b[1], infoVar1b[0], red_avh09, red_slope, red_offset, red_fillvalue, ERROR=ERROR
  if keyword_set(ERROR) then return, -1
  input_data=fltarr(2, globDim)
  red_fillvalue=red_fillvalue[0]
  red_slope=red_slope[0]
  red_offset=red_offset[0]
  noIdxs=where(red_avh09 eq red_fillvalue, count)
  red_avh09=red_avh09*red_slope+red_offset
  if count ne 0 then red_avh09[noIdxs]=INT_NAN
  input_data[0, *]=reform(temporary(red_avh09), globDim)
  red_avh09=0

  infoVar2b=['SREFL_CH2', sourceFile, fName, dir, ext, 'input_data[1,*]/nir_avhrr']

  ST_operator->readHdfFullInfoData, infoVar2b[1], infoVar2b[0], nir_avh09, nir_slope, nir_offset, nir_fillvalue, ERROR=ERROR
  if keyword_set(ERROR) then return, -1
  nir_fillvalue=nir_fillvalue[0]
  nir_slope=nir_slope[0]
  nir_offset=nir_offset[0]
  noIdxs=where(nir_avh09 eq nir_fillvalue, count)
  nir_avh09=nir_avh09*nir_slope+nir_offset
  if count ne 0 then nir_avh09[noIdxs]=INT_NAN
  input_data[1, *]=reform(temporary(nir_avh09), globDim)
  validIdxs=where(input_data[0,*] gt 0 and input_data[1,*] gt 0, validCount, complement=nanidxs)
  nir_avh09=0


  infoVar3b=['SZEN', sourceFile, fName, dir, ext, 'native_ts/ts_avhrr']

  ST_operator->readHdfFullInfoData, infoVar3b[1], infoVar3b[0], ts_avh09, ts_slope, ts_offset, ts_fillvalue, ERROR=ERROR
  if keyword_set(ERROR) then return, -1
  ts_fillvalue=ts_fillvalue[0]
  ts_slope=ts_slope[0]
  ts_offset=ts_offset[0]
  noIdxs=where(ts_avh09 eq ts_fillvalue, count)
  ts_avh09=ts_avh09*ts_slope+ts_offset
  if count ne 0 then ts_avh09[noIdxs]=INT_NAN
  ts_avh09=reform(ts_avh09, globDim)

  infoVar4b=['VZEN', sourceFile, fName, dir, ext, 'vz_data[j]/tv_avhrr']
  ST_operator->readHdfFullInfoData, infoVar4b[1], infoVar4b[0], tv_avh09, tv_slope, tv_offset, tv_fillvalue, ERROR=ERROR
  if keyword_set(ERROR) then return, -1
  tv_fillvalue=tv_fillvalue[0]
  tv_slope=tv_slope[0]
  tv_offset=tv_offset[0]
  noIdxs=where(tv_avh09 eq tv_fillvalue, count)
  tv_avh09=tv_avh09*tv_slope+tv_offset
  if count ne 0 then tv_avh09[noIdxs]=INT_NAN
  tv_avh09=reform(tv_avh09, globDim)

  infoVar5b=['QA', sourceFile, fName, dir, ext, 'QA']

  ST_operator->readHdfFullInfoData, infoVar5b[1], infoVar5b[0], qa_avh09, qa_slope, qa_offset, qa_fillvalue, ERROR=ERROR
  if keyword_set(ERROR) then return, -1
  if n_elements(qa_fillvalue) ne 0 then begin
    qa_fillvalue=qa_fillvalue[0]
    qa_slope=qa_slope[0]
    qa_offset=qa_offset[0]
    noIdxs=where(qa_avh09 eq qa_fillvalue, count)
    qa_avh09=qa_avh09*qa_slope+qa_offset
    if count ne 0 then qa_avh09[noIdxs]=INT_NAN
  endif
  qa_avh09=reform(qa_avh09, globDim)

  infoVar6b=['RELAZ', sourceFile, fName, dir, ext, 'relaz_data/phi_avhrr']

  ST_operator->readHdfFullInfoData, infoVar6b[1], infoVar6b[0], phi_avh09, phi_slope, phi_offset, phi_fillvalue, ERROR=ERROR
  if keyword_set(ERROR) then return, -1
  ; phi "correction"
  phi_fillvalue=phi_fillvalue[0]
  phi_slope=phi_slope[0]
  phi_offset=phi_offset[0]
  notElabIndex=where(phi_avh09 eq -1, count1)
  noIdxs=where(phi_avh09 eq phi_fillvalue, count)
  ;phi_avh09=phi_avh09*phi_slope+phi_offset
  RELAZ=phi_avh09*phi_slope+phi_offset

  SIN_REL_AZ = sin(RELAZ*!pi/180.)
  COS_REL_AZ = cos(RELAZ*!pi/180.)
  GOOD_REL_AZ = atan(SIN_REL_AZ, COS_REL_AZ)
  new_phi_avhrr=good_rel_az*180./!pi
  if count1 ne 0 then new_phi_avhrr[notelabIndex]=INT_NAN
  if count ne 0 then new_phi_avhrr[noIdxs]=INT_NAN
  relphi_avh09=new_phi_avhrr

  n=globDim
  n_non_zero = 0;

  brdf_r_param=fltarr(2,globDim)
  brdf_v_param=fltarr(2,globDim)

  ;input_data[0,*]=input_data[0,*]*slope_red+offset_red
  ;input_data[1,*]=input_data[1,*]*slope_nir+offset_nir
  ;totData=n_elements(input_data[0,*])

  n_non_zero=0
  brdfcorr=0
  sourceData=input_data
  ;rr3=cgi_map_bitwise_flag(qc_avhrr,3)
  rr3=cgi_map_bitwise_flag(qa_avh09,3)
  IDX_SEA= where(rr3 eq 1,complement=landidxs)

  aa=where(sourceData[0,*] gt 0, vcountSource1)
  aa1=where(sourceData[1,*] gt 0, vcountSource2)
  print, 'Source:', vcountSource1, vcountSource2

  ;aa=where(final_red_avhrr gt 0, vcountPrev1)
  ;aa1=where(final_nir_avhrr gt 0, vcountPrev2)
  ;print, 'Prev:', vcountPrev1, vcountPrev2

  print, 'apply mask....'
  sourceData[0,IDX_SEA]=INT_NAN
  sourceData[1,IDX_SEA]=INT_NAN

  ;final_red_avhrr[IDX_SEA]=NaN
  ;final_nir_avhrr[IDX_SEA]=NaN

  computationMask=input_data
  computationMask[*]=0

  ;aa=where(final_red_avhrr gt 0, vcountPrev1)
  ;aa1=where(final_nir_avhrr gt 0, vcountPrev2)
  ;print, 'Prev:', vcountPrev1, vcountPrev2
  ;validIdxs=landidxs
  ;validcount=n_elements(validIdxs)
  native_ts=ts_avh09
  native_tv=tv_avh09
  native_relphi=relphi_avh09
  ts_avh09=0
  tv_avh09=0
  relphi_avh09=0

  validIdxs1=where(input_data[0,*] gt 0 and input_data[1,*] gt 0 and $
    fix(native_ts) ne INT_NAN and $
    fix(native_tv) ne INT_NAN and $
    fix(native_relphi) ne INT_NAN and $
    rr3 ne 1, $
    validCount1, complement=nanidxs1)
  validCount=validCount1
  validIdxs=validIdxs1

  for j=0, validCount-1 do begin
    ;for j=0, totData-1 do begin
    checkIndex=validIdxs[j]
    ;checkIndex=j

    n_non_zero = 0;
    for i=0, 1 do if (input_data[i,checkIndex] gt 0) then n_non_zero++

    brdfcorr = 0;  /* not corrected by default */

    if input_data[0,checkIndex] lt 0 then continue
    if input_data[1,checkIndex] lt 0 then continue
    for i=0,1 do begin
      ;   print, 'brf inputs', input_data[0,checkIndex], input_data[1,checkIndex]
      ndvi_value = NDVI_calc(input_data[0,checkIndex], input_data[1,checkIndex])
      ;   print, 'ndvi', ndvi_value
      brefl = input_data[i,checkIndex]
      lsz = native_ts[checkIndex];
      ;    print, lsz
      BRDFVs = BRDF_params[i].slope_v1[checkIndex];
      BRDFRs = BRDF_params[i].slope_r1[checkIndex];
      ;     print, 'V, R', BRDFVs, BRDFRs
      ndvi_min = BRDF_params[i].NDVImin[checkIndex];
      ndvi_max = BRDF_params[i].NDVImax[checkIndex];
      ;    print, 'min, max', ndvi_min, ndvi_max
      ;if ((i==0) && (j == (1444*7200) + 4105)) printf("X (%f %f), %f %f %f SLOPES %f %f\n", input_data[0][checkIndex], input_data[1][checkIndex], ndvi_value, ndvi_min, ndvi_max, BRDFVs, BRDFRs);

      brdf_r_param[i,checkIndex] = 0.0
      brdf_v_param[i,checkIndex] = 0.0

      computationMask[i,checkIndex]=0
      if ((BRDFVs ne 0) and (BRDFRs ne 0)) then begin

        computationMask[i,checkIndex]=1
        if (ndvi_value lt ndvi_min) then ndvi_value = ndvi_min else if (ndvi_value gt ndvi_max) then ndvi_value = ndvi_max
        ndvi_value = ndvi_value - ndvi_min;

        BRDFV = (BRDFVs*ndvi_value) + BRDF_params[i].intercept_v1[checkIndex];
        BRDFR = (BRDFRs*ndvi_value) + BRDF_params[i].intercept_r1[checkIndex];
        ;    print, 'v,r', brdfv, brdfr
        ;if ((i==0) && (j == (1444*7200) + 4105)) printf("%f %f %f %f\n", BRDFV, BRDFVs, ndvi_value, BRDF_params[i].intercept_v1[checkIndex]);

        brdf_r_param[i,checkIndex] = BRDFR;
        brdf_v_param[i,checkIndex] = BRDFV;

        nvalue = 1;
        if ((BRDFV ge 0.0) and (BRDFR le 0.350)) then begin
          ;     print, 'angles ', native_ts[checkIndex], native_tv[checkIndex], native_relphi[checkIndex]
          res=corbrdfl(brefl, native_ts[checkIndex], native_tv[checkIndex], native_relphi[checkIndex], nvalue, BRDFV, BRDFR);
          ;         print, 'res', res
          input_data[i,checkIndex] = res
          brdfcorr++;
          ;if ((i==0) && (j == (1444*7200) + 4105)) printf("XX %f %f %f %f\n", BRDFV, BRDFVs, ndvi_value, BRDF_params[i].intercept_v1[checkIndex]);
        endif; else input_data[i,checkIndex] = 10.0
      endif else begin
        computationMask[i,checkIndex]=2

        BRDFV = BRDF_params[i].intercept_v1[checkIndex];
        BRDFR = BRDF_params[i].intercept_r1[checkIndex];
        ;     print, 'v,r 2', brdfv, brdfr
        brdf_r_param[i,checkIndex] = BRDFR;
        brdf_v_param[i,checkIndex] = BRDFV;
        ;if ((i==0) && (j == (1444*7200) + 4105)) printf("BRDF paramters %f %f\n", BRDFV, BRDFR)

        nvalue = 1;
        if ((BRDFV ge 0.0) and (BRDFR le 0.350)) then begin
          ;     print, 'angles 2', native_ts[checkIndex], native_tv[checkIndex], native_relphi[checkIndex]
          ;      print, brefl, native_ts[checkIndex], native_tv[checkIndex], native_relphi[checkIndex], nvalue, BRDFV, BRDFR
          res=corbrdfl(brefl, native_ts[checkIndex], native_tv[checkIndex], native_relphi[checkIndex], nvalue, BRDFV, BRDFR);
          ;     print, 'res 2', res
          input_data[i,checkIndex] = res;
          brdfcorr++;
        endif; else input_data[i,checkIndex] = 11.0
      endelse
      val = 0;  /* OK by default */
      ;   stop
    endfor
  endfor

  aa=where(input_data[0,*] gt 0, vcountNew1)
  aa1=where(input_data[1,*] gt 0, vcountNew2)
  print, 'new :', vcountNew1, vcountNew2

  sourceFileName=ST_fileSystem->getFileNameInfo(resFileHDF, filePath=filePath, extension=extension)
  sourceFileName=ST_fileSystem->removeFileExtension(sourceFileName)
  savFileName=ST_fileSystem->addFileExtension(outputDir+sourceFileName, 'sav')

  ; raw (idl) save for test
  ;save, red_brf, nir_brf, new_phi_avhrr, native_relphi, final_nir_avhrr, final_red_avhrr, filename=outFName, /COMPRESS

  ; angles
  a=size(native_tv)

  ; Apply sea on data
  red_brf=reform(input_data[0,*], 7200, 3600)
  nir_brf=reform(input_data[1,*], 7200, 3600)
  new_phi_avhrr=reform(new_phi_avhrr, 7200, 3600)
  native_tv=reform(native_tv, 7200, 3600)
  native_ts=reform(native_ts, 7200, 3600)
  qa=reform(qa_avh09, 7200, 3600)

  ;sigma
  sigma_red=2.*(red_brf*0.05+0.005)
  sigma_nir=2.*(nir_brf*0.05+0.005)

  ; input (not to write)
  sourceData[0,nanidxs]=INT_NAN
  sourceData[1,nanidxs]=INT_NAN
  ; brfs
  red_brf[IDX_SEA]=INT_NAN
  nir_brf[IDX_SEA]=INT_NAN
  ; sigma
  sigma_red[IDX_SEA]=INT_NAN
  sigma_nir[IDX_SEA]=INT_NAN
  ; angles
  native_tv[IDX_SEA]=INT_NAN
  native_ts[IDX_SEA]=INT_NAN
  new_phi_avhrr[IDX_SEA]=INT_NAN
  ; previous brf (not to write)
  ;final_red_avhrr[IDX_SEA]=NaN
  ;final_nir_avhrr[IDX_SEA]=NaN

  ;  aa=where(red_brf gt 0, vcountNew1)
  ;  aa1=where(nir_brf gt 0, vcountNew2)
  ;
  ;  aa=where(final_red_avhrr gt 0, vcountPrev1)
  ;  aa1=where(final_nir_avhrr gt 0, vcountPrev2)
  ;
  ;  sd1=reform(sourceData[0,*], 7200, 3600)
  ;  sd2=reform(sourceData[0,*], 7200, 3600)
  ;  aa=where(sourceData[0,*] gt 0, vcountSource1)
  ;  aa1=where(sourceData[1,*] gt 0, vcountSource2)

  cMask1=reform(reform(computationMask[0,*]), 7200, 3600)
  cMask2=reform(reform(computationMask[1,*]), 7200, 3600)

  ;print, 'New', vcountNew1, vcountNew2
  ;print, 'Previous', vcountPrev1, vcountPrev2
  ;print, 'Source', vcountSource1, vcountSource2

  ;a1=where(cMask1 eq 1) & cMask1[a1]=128 & a2=where(cMask1 eq 2) & cMask1[a2]=255 & a1=where(cMask2 eq 1) & cMask2[a1]=128 & a2=where(cMask2 eq 2) & cMask2[a2]=255
  ;check computation stuff
  ;window, 7, xsize=720, ysize=360, title='computation band1 (bytscl): '+SENSOR
  ;  tv, reverse(rebin(bytscl(cMask1, min=0., max=2.), 720,360),2)
  ;tv, reverse(rebin(cMask1, 720,360),2)
  ;
  ;window, 8, xsize=720, ysize=360, title='computation band2 (bytscl): '+SENSOR
  ;tv, reverse(rebin(cMask2, 720,360),2)
  ;  tv, reverse(rebin(bytscl(cMask2, min=0., max=2.), 720,360),2)
  ;
  ;  window, 0, xsize=720, ysize=360, title='source band1: '+SENSOR
  ;  tv, reverse(rebin(bytscl(sd1, min=0., max=1.0), 720,360),2)

  ;BRDF_params[i].slope_v1[checkIndex];
  ; check BRDF
  ;  b1v=reform(BRDF_params[0].slope_v1, 7200, 3600)
  ;  b1r=reform(BRDF_params[0].slope_r1, 7200, 3600)
  ;  b2v=reform(BRDF_params[1].slope_v1, 7200, 3600)
  ;  b2r=reform(BRDF_params[1].slope_r1, 7200, 3600)
  ;  ;b1v[IDX_SEA]=NaN
  ;
  ;  max=max(b1v, min=min)
  ;  window, 1, xsize=720, ysize=360, title='band1 v slope: '+SENSOR
  ;  tv, reverse(congrid(bytscl(b1v, min=-1, max=1), 720,360),2)
  ;  window, 2, xsize=720, ysize=360, title='band1 r slope:'+SENSOR
  ;  max=max(b1r, min=min)
  ;  tv, reverse(congrid(bytscl(b1r, min=-1, max=1), 720,360),2)
  ;  window, 3, xsize=720, ysize=360, title='band2 v slope:'+SENSOR
  ;  max=max(b2v, min=min)
  ;  tv, reverse(congrid(bytscl(b2v, min=-1, max=1), 720,360),2)
  ;  window, 4, xsize=720, ysize=360, title='band2 r slope:'+SENSOR
  ;  max=max(b2r, min=min)
  ;  tv, reverse(congrid(bytscl(b2r, min=-1, max=1), 720,360),2)
  ;
  ;  tv, reverse(rebin(bytscl(red_brf, min=0., max=1.0), 720,360),2)
  ;  window, 1, xsize=720, ysize=360, title='new band1 brfs: '+SENSOR
  ;  tv, reverse(rebin(bytscl(red_brf, min=0., max=1.0), 720,360),2)
  ;
  ;  window, 2, xsize=720, ysize=360, title='previous band1 brfs: '+SENSOR
  ;  tv, reverse(rebin(bytscl(final_red_avhrr, min=0., max=1.0), 720,360),2)

  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  dayS=string(day, format='(I02)')

  ;filePath='E:\mariomi\Documents\temp'
  ;write_hdf, filePath+path_sep()+fName, $

  satDate=ST_utils->getSysTime(/SATFORMAT)


  ;  bandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'PHI', 'QA', 'Q1', 'Q2']
  ;
  ;  bandMeasaureUnits=    ['-', '-', '-', '-', $
  ;    'deg', 'deg', 'deg', '-', '-', '-']
  brfDSInfo=getStandardBrfDataSetInfo()
  bandNames=brfDSInfo.bandNames
  bandLongNames=brfDSInfo.bandLongNames
  bandStandardNames=brfDSInfo.bandStandardNames
  bandMeasureUnits=brfDSInfo.bandMeasureUnits
  bandIntercepts=brfDSInfo.bandIntercepts
  bandSlopes=brfDSInfo.bandSlopes
  bandDataTypes=brfDSInfo.bandDataTypes
  nanList=brfDSInfo.nanS
  trueminMaxs=brfDSInfo.minMaxs
  header=brfDSInfo.header

  ;  bandIntercepts=lonarr(n_elements(bandNames))
  ;  bandSlopes=[10e-05, 10e-05, 10e-05, 10e-05,$
  ;    10e-03, 10e-03, 10e-03, $
  ;    1, 1, 1]

  ; Watch out ts/tv switched!!!! Jun 06 2016
  dataSets=[ptr_new(red_brf, /NO_COPY), ptr_new(nir_brf, /NO_COPY), $
    ptr_new(sigma_red, /NO_COPY), ptr_new(sigma_nir, /NO_COPY), $
    ptr_new(native_ts, /NO_COPY), ptr_new(native_tv, /NO_COPY), ptr_new(new_phi_avhrr, /NO_COPY), $
    ptr_new(qa, /NO_COPY), $
    ptr_new(cMask1, /NO_COPY), ptr_new(cMask2, /NO_COPY)]

  boundary=[-180.0, 180.0, -90, 90.]

  ;bandDataType=[2,2,2,2,2,2,2,2,1,1]

  ;;
  ;
  ;
  ;  minMaxs=fltarr(n_elements(bandDataType), 2)
  ;  nanList=fltarr(n_elements(bandDataType))
  ;
  ;  minMaxs[*,*]=-1
  ;
  ;  red_brf=*dataSets[0]
  ;  tempMin=min(red_brf>0, max=tempMax)
  ;  minMaxs[0,*]=[tempMin,tempMax]
  ;  nanList[0]=INT_NAN
  ;
  ;  nir_brf=*dataSets[1]
  ;  tempMin=min(nir_brf>0, max=tempMax)
  ;  minMaxs[1,*]=[tempMin,tempMax]
  ;  nanList[1]=INT_NAN
  ;
  ;  sigma_red=*dataSets[2]
  ;  tempMin=min((-5.0 > sigma_red) < 5.0, max=tempMax)
  ;  minMaxs[2,*]=[tempMin,tempMax]
  ;  nanList[2]=INT_NAN
  ;
  ;  sigma_nir=*dataSets[3]
  ;  tempMin=min((-5.0 > sigma_nir) < 5.0, max=tempMax)
  ;  minMaxs[3,*]=[tempMin,tempMax]
  ;  nanList[3]=INT_NAN
  ;
  ;  ;ts=*dataSets[4]
  ;  minMaxs[4,*]=[0.,90.];DATA_RANGE;minMax[0,*]
  ;  nanList[4]=INT_NAN
  ;
  ;  ;tv=*dataSets[5]
  ;  minMaxs[5,*]=[0.,90.];DATA_RANGE;minMax[0,*]
  ;  nanList[5]=INT_NAN
  ;
  ;  ;phi=*dataSets[6]
  ;  minMaxs[6,*]=[-180,180]
  ;  nanList[6]=INT_NAN
  ;
  ;  qa=*dataSets[7]
  ;  tempMin=min(qa, max=tempMax)
  ;  minMaxs[7,*]=[tempMin, tempMax]
  ;  nanList[7]=INT_NAN
  ;
  ;  cMask1=*dataSets[8]
  ;  tempMin=min(cMask1, max=tempMax)
  ;  minMaxs[8,*]=[tempMin, tempMax]
  ;  nanList[8]=INT_NAN
  ;
  ;  cMask2=*dataSets[9]
  ;  tempMin=min(cMask2, max=tempMax)
  ;  minMaxs[9,*]=[tempMin, tempMax]
  ;  nanList[9]=INT_NAN
  ;  ;trueMinMaxs=minMaxs

  ;;
  
  if ~keyword_set(NOHDFWRITE) then write_hdf, resFileHDF, $
    bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
    dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
    postcompression=postcompression, gzipLevel=gzipLevel, NOREVERSE=NOREVERSE, trueMinMaxs=trueMinMaxs, nanlist=nanlist, $
    trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, header=header

  if ~keyword_set(NONCWRITE) then write_georef_ncdf, resFileNC, $
    bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
    dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
    postcompression=postcompression, gzipLevel=gzipLevel, NOREVERSE=NOREVERSE, trueMinMaxs=trueMinMaxs, nanlist=nanlist, $
    trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, header=header
  print, '****'
  print, resFileNC
  print, 'done'
  print, '****'

  red_brf=0
  nir_brf=0
  new_phi_avhrr=0
  native_relphi=0
  final_nir_avhrr=0
  final_red_avhrr=0
  ;  idx_ok=where(red_brf gt 0.0 and final_red_avhrr gt 0.0)
  ;  window, 0, xsize=720, ysize=360, title='red brdfl: '+SENSOR
  ;  tv, reverse(rebin(bytscl(red_brf, min=0., max=1.0), 720,360),2)
  ;  window, 1, xsize=720, ysize=360, title='nir brdfl: '+SENSOR
  ;  tv, reverse(rebin(bytscl(nir_brf, min=0.,max=1.0), 720,360),2)
  ;  ;
  ;  ;window, 11, xsize=720, ysize=360, title='nir input: '+SENSOR
  ;  ;tv, reverse(rebin(bytscl(dataset.data*slope_nir, min=0.,max=1.0), 720,360),2)
  ;
  ;  window, 2, xsize=720, ysize=360, title='nir brdfl: '+SENSOR
  ;  tv, reverse(rebin(bytscl(final_nir_avhrr, min=0., max=1.0), 720,360),2)
  ;  window, 3, xsize=720, ysize=360, title='red brdfl: '+SENSOR
  ;  tv, reverse(rebin(bytscl(final_red_avhrr, min=0., max=1.0), 720,360),2)
  ;
  ;  idx_ok=where(red_brf gt 0. and red_brf lt 1.0)
  ;
  ;  plot, final_red_avhrr(idx_ok(0:100)), yr=[0.,1.5], psym = 4, title='red: new brdf and previous brdf'
  ;  oplot, red_brf(idx_ok(0:100)), psym = 1
  ;
  ;  plot, nir_brf(idx_ok(0:100)), psym = 1, title='nir and red (new brdf)'
  ;  oplot, red_brf(idx_ok(0:100)), psym = 4, color=100
  ;
  ;  plot, final_nir_avhrr(idx_ok(0:100)), psym = 1, title='nir and red (previous avhrr)'
  ;  oplot, final_red_avhrr(idx_ok(0:100)), psym = 4, color=100
  ;
  ;  ; "scatter"
  ;  plot, red_brf(idx_ok(0:6000)),final_red_avhrr(idx_ok(0:6000)), psym = 1, yr=[0.,3.], xr=[0.,3.], title='Scatter red: new brdf Vs previous avhrr'
  ;  plot, nir_brf(idx_ok(0:6000)),final_nir_avhrr(idx_ok(0:6000)), psym = 1, yr=[0.,3.], xr=[0.,3.], title='Scatter nir: new brdf Vs previous avhrr'
  ;
  ;  plot, new_phi_avhrr(idx_ok(0:100)), psym = 1, title='phi (previous and brdf)'
  ;  oplot, native_relphi(idx_ok(0:100)), psym = 4, color=100
  ;final_nir_avhrr1=0
  ;final_red_avhrr1=0

  ;write_all, fileName, input_data, dir=dir, refDims=refDims

  return, 1

END