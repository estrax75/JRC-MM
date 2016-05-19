function merge_BRFGlob, file1, file2, file3, confDir, year, month, day, noaaCode, operatorObj, fsObj, tempDir, testFile=testFile

  NaN=-9999 ;!VALUES.F_NAN

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  sensor='AVHRR'

  BRDF_params=getPGEInfo(confDir, file, globDim=globDim, opObj=operatorObj, fsObj=fsObj)

  ;AVHRR-Land_v004_AVH09C1_NOAA-16_20030101_c20140421105754.nc
  fName1=fsObj->getFileNameInfo(file1, filePath=dir1, extension=ext1)
  ;AVH09C1.A2003001.N16.004.2014111105754.hdf
  fName2=fsObj->getFileNameInfo(file2, filePath=dir2, extension=ext2)
  ;GLOBAL_L3_GEOG_0.05DEG_001-001_03.NOAA-16_BRF.hdf // GLOBAL_P17_GEOG_0.05DEG_203-203_99.NOAA-14
  fName3=fsObj->getFileNameInfo(file3, filePath=dir3, extension=ext3)
  ;fName4=fsObj->getFileNameInfo(file4, filePath=dir4, extension=ext4)

  ;tempDir='E:\mariomi\Documents\temp'
  ; example file
  ;'TS_CH1'
  ;'TS_CH2'

  ; source brf file1
  ; previous brf file3
  infoVar1a=['SREFL_CH1', file1, fName1, dir1, ext1, 'input_data[0,*]/red_avhrr']
  ;infoVar1b=['TS_CH1', file2, fName2, dir2, ext2, 'input_data[0,*]/red_avhrr']
  infoVar1c=['RHO1', file3, fName3, dir3, ext3, 'input_data[0,*]/red_avhrr']

  ;dataSet=operatorObj->readNcdfVar(infoVar1a[1], infoVar1a[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dataSet=operatorObj->readNcdfVar(infoVar1b[1], infoVar1b[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  input_data=fltarr(2, globDim)
  input_data[0, *]=reform(temporary(dataset.data), globDim)
  ;operatorObj->readHdfFullInfoData, infoVar1b[1], infoVar1b[0], red_avhrr
  globInfo=getGlobalInfo(dir3, file3, infoVar1b[0], opObj=operatorObj, fsObj=fsObj, testFile=testFile)
  slope_red=globInfo.slope
  offset_red=globInfo.offset
  ;final_red_avhrr=red_avhrr*slope_red+offset_red
  dataSet=0

  infoVar2a=['SREFL_CH2', file1, fName1, dir1, ext1, 'input_data[1,*]/nir_avhrr']
  infoVar2b=['RHO2', file3, fName3, dir3, ext3, 'input_data[1,*]/nir_avhrr']

  dataSet=operatorObj->readNcdfVar(infoVar2a[1], infoVar2a[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  input_data[1, *]=reform(temporary(dataSet.data), globDim)
  validIdxs=where(input_data[0,*] gt 0 and input_data[1,*] gt 0, validCount, complement=nanidxs)
  ;operatorObj->readHdfFullInfoData, infoVar2b[1], infoVar2b[0], nir_avhrr
  globInfo=getGlobalInfo(dir3, file3, infoVar2b[0], opObj=operatorObj, fsObj=fsObj, testFile=testFile)
  slope_nir=globInfo.slope
  offset_nir=globInfo.offset
  ;final_nir_avhrr=nir_avhrr*slope_nir+offset_nir
  dataSet=0


  ;infoVar3a=['SZEN', file1, fName1, dir1, ext1, 'native_ts/ts_avhrr']
  infoVar3b=['TS', file3, fName3, dir3, ext3, 'native_ts/ts_avhrr']
  ;dataSet=operatorObj->readNcdfVar(infoVar3a[1], infoVar3a[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  operatorObj->readHdfFullInfoData, infoVar3b[1], infoVar3b[0], ts_avhrr
  ;globInfo=getGlobalInfo(dir3, file3, infoVar3b[0], opObj=operatorObj, fsObj=fsObj)
  ;slope_ts=globInfo.slope
  ;offset_ts=globInfo.offset
  ;native_ts1=reform(temporary(dataSet.data), globDim)*0.01
  native_ts=reform(ts_avhrr, globDim)*0.01
  dataSet=0

  ;infoVar4a=['VZEN', file1, fName1, dir1, ext1, 'vz_data[j]/tv_avhrr']
  infoVar4b=['TV', file3, fName3, dir3, ext3, 'vz_data[j]']
  ;dataSet=operatorObj->readNcdfVar(infoVar4a[1], infoVar4a[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  operatorObj->readHdfFullInfoData, infoVar4b[1], infoVar4b[0], tv_avhrr
  ;globInfo=getGlobalInfo(dir3, file3, infoVar4b[0], opObj=operatorObj, fsObj=fsObj)
  ;slope_tv=globInfo.slope
  ;offset_tv=globInfo.offset
  ;native_tv1=reform(temporary(dataSet.data), globDim)*0.01
  native_tv=reform(tv_avhrr, globDim)*0.01
  dataSet=0

  ;N_MAPPING or QA?
  ;infoVar5a=['QA', file1, fName1, dir1, ext1, '']
  ;infoVar5b=['QA', file3, fName3, dir3, ext3, 'N_MAPPING']
  ;dataSet=operatorObj->readNcdfVar(infoVar5a[1], infoVar5a[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  ;qa=reform(temporary(dataSet.data), globDim)
  ;operatorObj->readHdfFullInfoData, infoVar5b[1], infoVar5b[0]
  ;globInfo=getGlobalInfo(dir3, file3, infoVar5b[0], opObj=operatorObj, fsObj=fsObj)
  ;slope_phi=globInfo.slope
  ;offset_phi=globInfo.offset
  dataSet=0
  infoVar6a=['RELAZ', file1, fName1, dir1, ext1, 'relaz_data/phi_avhrr']
  infoVar6b=['PHI', file3, fName3, dir3, ext3, 'relaz_data/phi_avhrr']
  dataSet=operatorObj->readNcdfVar(infoVar6a[1], infoVar6a[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  native_relphi=reform(temporary(dataSet.data), globDim)
  ;operatorObj->readHdfFullInfoData, infoVar6b[1], infoVar6b[0], phi_avhrr
  globInfo=getGlobalInfo(dir3, file3, infoVar6b[0], opObj=operatorObj, fsObj=fsObj, testFile=testFile)
  slope_phi=globInfo.slope
  offset_phi=globInfo.offset
  ;dataSet=0
  ; phi "correction"

  notElabIndex=where(native_relphi eq -1)
  ;RELAZ=phi_avhrr*slope_phi+offset_phi
  RELAZ=native_relphi*slope_phi+offset_phi
  SIN_REL_AZ = sin(RELAZ*!pi/180.)
  COS_REL_AZ = cos(RELAZ*!pi/180.)
  GOOD_REL_AZ = atan(SIN_REL_AZ, COS_REL_AZ)
  new_phi_avhrr=good_rel_az*180./!pi
  new_phi_avhrr[notelabIndex]=NaN
  native_relphi=new_phi_avhrr
  ;plot, new_phi_avhrr
  ;angles(*,*,2)=good_rel_az*180./!pi
  ;angles(*,*,2)=good_rel_az*180./!pi
  ;

  ;infoVar7a=['QC', file1, fName1, dir1, ext1, '??/qc_avhrr']
  ;infoVar7b=['QC', file3, fName3, dir3, ext3, '??/qc_avhrr']
  ;dataSet=operatorObj->readNcdfVar(infoVar7a[1], infoVar7a[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  ;qc_avhrr=reform(temporary(dataSet.data), globDim)
  ;operatorObj->readHdfFullInfoData, infoVar7b[1], infoVar7b[0], qc_avhrr
  ;globInfo=getGlobalInfo(dir3, file3, infoVar7b[0], opObj=operatorObj, fsObj=fsObj)
  ;dataSet=0

  infoVar8a=['QA', file1, fName1, dir1, ext1, '??/brdf_qa_avhrr']
  ;infoVar8b=['QA', file3, fName3, dir3, ext3, '??/brdf_qa_avhrr']
  dataSet=operatorObj->readNcdfVar(infoVar8a[1], infoVar8a[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  qa=reform(temporary(dataSet.data), globDim)
  ;operatorObj->readHdfFullInfoData, infoVar8b[1], infoVar8b[0], brdf_qa_avhrr
  ;globInfo=getGlobalInfo(dir3, file3, infoVar7b[0], opObj=operatorObj, fsObj=fsObj)
  dataSet=0

  n=globDim
  n_non_zero = 0;

  brdf_r_param=fltarr(2,globDim)
  brdf_v_param=fltarr(2,globDim)

  input_data[0,*]=input_data[0,*]*slope_red+offset_red
  input_data[1,*]=input_data[1,*]*slope_nir+offset_nir
  totData=n_elements(input_data[0,*])

  n_non_zero=0
  brdfcorr=0
  sourceData=input_data
  ;rr3=cgi_map_bitwise_flag(qc_avhrr,3)
  rr3=cgi_map_bitwise_flag(qa,3)
  IDX_SEA= where(rr3 eq 1,complement=landidxs)

  aa=where(sourceData[0,*] gt 0, vcountSource1)
  aa1=where(sourceData[1,*] gt 0, vcountSource2)
  print, 'Source:', vcountSource1, vcountSource2

  ;aa=where(final_red_avhrr gt 0, vcountPrev1)
  ;aa1=where(final_nir_avhrr gt 0, vcountPrev2)
  ;print, 'Prev:', vcountPrev1, vcountPrev2

  print, 'apply mask....'
  sourceData[0,IDX_SEA]=NaN
  sourceData[1,IDX_SEA]=NaN

  ;final_red_avhrr[IDX_SEA]=NaN
  ;final_nir_avhrr[IDX_SEA]=NaN

  computationMask=input_data
  computationMask[*]=0

  ;aa=where(final_red_avhrr gt 0, vcountPrev1)
  ;aa1=where(final_nir_avhrr gt 0, vcountPrev2)
  ;print, 'Prev:', vcountPrev1, vcountPrev2
  ;validIdxs=landidxs
  ;validcount=n_elements(validIdxs)

  for j=0, validCount-1 do begin
    ;for j=0, totData-1 do begin
    checkIndex=validIdxs[j]
    ;checkIndex=j

    n_non_zero = 0;
    for i=0, 1 do if (input_data[i,checkIndex] gt 0) then n_non_zero++

    brdfcorr = 0;  /* not corrected by default */

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

  sourceFileName=fsObj->getFileNameInfo(file1, filePath=filePath, extension=extension)
  sourceFileName=fsObj->removeFileExtension(sourceFileName)
  outFName=filePath+'result'+path_sep()+sourceFileName+'.sav'

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
  qa=reform(qa, 7200, 3600)

  ;sigma
  sigma_red=2.*(red_brf*0.05+0.005)
  sigma_nir=2.*(nir_brf*0.05+0.005)

  ; input (not to write)
  sourceData[0,nanidxs]=NaN
  sourceData[1,nanidxs]=NaN
  ; brfs
  red_brf[IDX_SEA]=NaN
  nir_brf[IDX_SEA]=NaN
  ; sigma
  sigma_red[IDX_SEA]=NaN
  sigma_nir[IDX_SEA]=NaN
  ; angles
  native_tv[IDX_SEA]=NaN
  native_ts[IDX_SEA]=NaN
  new_phi_avhrr[IDX_SEA]=NaN
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

  fName='AVHRR_'+'GEOG_0.05DEG'+'_'+years+'_'+months+'_'+days+'_NOAA-N'+strcompress(noaaCode, /REMOVE)+'_BRF'
  ;filePath='E:\mariomi\Documents\temp'
  filePath=tempDir
  ;write_hdf, filePath+path_sep()+fName, $

  utils=obj_new('Utility')
  satDate=utils->getSysTime(/SATFORMAT)


  bandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
    'TS', 'TV', 'PHI', 'QA', 'Q1', 'Q2']

  bandMeasaureUnits=    ['-', '-', '-', '-', $
    'deg', 'deg', 'deg', '-', '-', '-']

  bandIntercepts=lonarr(n_elements(bandNames))
  bandSlopes=[10e-05, 10e-05, 10e-05, 10e-05,$
    10e-03, 10e-03, 10e-03, $
    1, 1, 1]

  dataSets=[ptr_new(red_brf, /NO_COPY), ptr_new(nir_brf, /NO_COPY), $
    ptr_new(sigma_red, /NO_COPY), ptr_new(sigma_nir, /NO_COPY), $
    ptr_new(native_tv, /NO_COPY), ptr_new(native_ts, /NO_COPY), ptr_new(new_phi_avhrr, /NO_COPY), $
    ptr_new(qa, /NO_COPY), $
    ptr_new(cMask1, /NO_COPY), ptr_new(cMask2, /NO_COPY)]

  boundary=[-180.0, 180.0, -90, 90.]

  write_hdf, filePath+path_sep()+fName+'.hdf', $
    bandNames, bandMeasaureUnits, $
    dataSets, bandIntercepts, bandSlopes, tempDir, boundary

  write_georef_ncdf, filePath+path_sep()+fName+'.nc', $
    bandNames, bandMeasaureUnits, $
    dataSets, bandIntercepts, bandSlopes, tempDir, boundary
  

  ;  write_ncdf, filePath+path_sep()+fName+'.ncdf', $
  ;    ['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'PHI', 'QA', 'Q1', 'Q2'], $
  ;    dataSets, boundary

  ;  write_eos, filePath+path_sep()+fName+'.eos', $
  ;    ['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'PHI', 'QA', 'Q1', 'Q2'], $
  ;    dataSets, boundary

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