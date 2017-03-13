pro tbc_do_mean_tc

  ;if platform eq 'AVH09C1' then destTCFile=build_JRC_FPA_AVH_MeanAlg_TenDays_Product_FileName(platform, resolution, year, month, first[type], missionName, missionCode, mainVarName, startDay=first[type], endDay=last[type], destLevel, VERSION='01')
  if platform eq 'AVH09' then begin
    destncFileNameInfo=build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName(platform, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'NC', undef, indicator='LAN')
    desthdfFileNameInfo=build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName(platform, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'HDF', undef, indicator='LAN')
  endif
  ;if sensor eq 'MODIS' then destTCFile=buildMODISFileName_D(sensor, resolution, year, month, first[type], missionName, missionCode, mainVarName, startDay=first[type], endDay=last[type], destLevel, VERSION='01')
  ;destncfilename=ST_fileSystem->addFileExtension(destTCFile, 'NC'); nc
  ;desthdffilename=ST_fileSystem->addFileExtension(destTCFile, 'HDF'); hdf
  destncFileName=destncFileNameInfo.filename
  desthdfFileName=desthdfFileNameInfo.filename
  print, '*********'
  print, 'building mean file: ', outputDir+destncFileName
  print, '*********'

  starttime=systime(1)
  ;compute_mean, expectedDays, data_day1, data_tc
  sm_compute_mean, expectedDays, storeFileInfos, data_tc, 10
  endTime=systime(1)-starttime
  print, 'computed in about:', strcompress(endTime), 'seconds'
  ;avgIdx=[2,5,8,11]

  faparTCDSInfo=getStandardFaparMeanDataSetInfo()
  bandNames=(faparTCDSInfo.bandNames);[avgIdx]
  bandLongNames=(faparTCDSInfo.bandLongNames);[avgIdx]
  bandStandardNames=(faparTCDSInfo.bandStandardNames);[avgIdx]
  bandSlopes=(faparTCDSInfo.bandSlopes);[avgIdx]
  bandMeasureUnits=(faparTCDSInfo.bandMeasureUnits);[avgIdx]
  bandDataTypes=(faparTCDSInfo.bandDataTypes);[avgIdx]
  bandIntercepts=(faparTCDSInfo.bandIntercepts);[avgIdx]
  minMaxs=(faparTCDSInfo.minMaxs);[avgIdx,*]
  nanList=(faparTCDSInfo.nans);[avgIdx]
  header=faparTCDSInfo.header
  scaledminmaxs=faparTCDSInfo.scaledminmaxs

  trueSlopes=bandSlopes
  trueIntercepts=bandIntercepts

  ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
  ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
  ;byteOutput=dataByteScaling(output.fpar, VALUE_BYTES=[0,250])
  remarkableFlags=bSInfo.remarkableFlags
  DATA_NAN=bSInfo.DATA_NAN
  BYTE_NAN=bSInfo.BYTE_NAN
  BYTE_RANGE=bSInfo.BYTE_RANGE

  realFapar=data_tc.fapar
  ;save, true_mean_fapar, fileName=tempDir+destncfilename+'_MEAN.sav'
  ;true_mean_fapar=0

  res=dataByteScaling(data_tc.fapar, data_tc.jrc_flag, $
    DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
    DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
  data_tc.fapar=res.resultData
  trueIntercepts[0]=outIntercept
  trueSlopes[0]=outSlope
  ;output.flag=res.resultFlag

  ;        ;tv, rebin(bytscl(data_tc.sigma), 720, 360) ;*
  ;        res=dataByteScaling(data_tc.sigma, data_tc.flag, $
  ;          DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
  ;          DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
  ;        data_tc.sigma=res.resultData
  ;        trueIntercepts[1]=outIntercept
  ;        trueSlopes[1]=outSlope
  ;data_tc.flag=res.resultFlag

  ;tv, rebin(bytscl(data_tc.dev_temp), 720, 360) ;*
  ;        res=dataByteScaling(data_tc.dev_temp, data_tc.flag, $
  ;          DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
  ;          DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
  ;        data_tc.dev_temp=res.resultData;data_tc.sigma=res.resultData
  ;        trueIntercepts[2]=outIntercept
  ;        trueSlopes[2]=outSlope
  ;        flagTags=strupcase(['fapar', 'sigma', 'dev_temp'])
  ;        tags=tag_names(data_tc)
  ;
  ;        idx_1 = where (data_tc.flag eq 1, c1)
  ;        idx_2 = where (data_tc.flag eq 2, c2)
  ;        idx_3 = where (data_tc.flag eq 3, c3)
  ;
  ;        waterIdxs=where(data_day[0].flag eq 3, watCount)
  ;        for i=0, n_elements(flagTags)-1 do begin
  ;          thisIdx=(where(flagTags[i] eq tags, count))[0]
  ;          if count eq 1 then begin
  ;            if c1 gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), idx_1, remarkableFlags[0])
  ;            if c2 gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), idx_2, remarkableFlags[1])
  ;            if c3 gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), idx_3, remarkableFlags[2])
  ;            if watCount gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), waterIdxs, remarkableFlags[2])
  ;          endif
  ;        endfor

  ;map -9999 on int data
  ;flagTags=strupcase(['red', 'nir'])
  byteFlagTags=['fapar', 'sigma', 'dev_temp']
  intFlagTags=['red', 'nir']
  tags=tag_names(data_tc)

  ; flag 6 where water/snow/ice/cloud
  waterIdxs=where(data_tc.flag eq 6, watCount)
  for i=0, n_elements(flagTags)-1 do begin
    thisIntIdx=(where(intFlagTags[i] eq tags, countInt))[0]
    thisByteIdx=(where(byteFlagTags[i] eq tags, countByte))[0]
    if watCount gt 0 then begin
      if countInt eq 1 then data_tc.(thisIntIdx)=mapQualityFlags(data_tc.(thisIntIdx), waterIdxs, INT_NAN)
      if countByte eq 1 then data_tc.(thisByteIdx)=mapQualityFlags(data_tc.(thisByteIdx), waterIdxs, BYTE_NAN)
    endif
  endfor

  ;        dataSets=[ptr_new(data_tc.day, /NO_COPY),ptr_new(data_tc.nday, /NO_COPY), $
  ;          ptr_new(data_tc.fapar, /NO_COPY), ptr_new(data_tc.sigma, /NO_COPY), ptr_new(data_tc.dev_temp, /NO_COPY), $
  ;          ptr_new(data_tc.red, /NO_COPY), ptr_new(data_tc.sigma_red, /NO_COPY), ptr_new(data_tc.dev_red_temp, /NO_COPY) ,$
  ;          ptr_new(data_tc.nir, /NO_COPY), ptr_new(data_tc.sigma_nir, /NO_COPY), ptr_new(data_tc.dev_red_temp, /NO_COPY), $
  ;          ptr_new(data_tc.flag, /NO_COPY), $
  ;          ptr_new(data_tc.toc_red, /NO_COPY), ptr_new(data_tc.toc_nir, /NO_COPY), $
  ;          ptr_new(data_tc.nday, /NO_COPY)]
  if keyword_set(UNC) then id='unc' else id=''
  ;save, data_tc, filename='E:\mariomi\Desktop\fapar_presentation\tc_result'+id+'.sav'
  dataSets=[ptr_new(data_tc.fapar, /NO_COPY), $
    ptr_new(data_tc.red, /NO_COPY),$
    ptr_new(data_tc.nir, /NO_COPY), $
    ptr_new(data_tc.jrc_flag, /NO_COPY)]

  boundary=[-180.0, 180.0, -90, 90.]

  ;outputDir='E:\mariomi\Documents\projects\ldtr\data\AVHRR\FP\output\'
  if keyword_set(data_dir) then begin
    destncfilename=data_Dir+destncfilename
    desthdffilename=data_Dir+desthdffilename
  endif else begin
    destncfilename=outputDir+destncfilename.filepath+destncfilename
    desthdffilename=outputDir+desthdffilename.filepath+desthdffilename
  endelse
  header.cdr_variable=['cdr_variable', 'FAPAR']

  date_created=ST_utils->getSysTime(/FILECOMPATIBILITY)
  satellite='NOAA '+strcompress(missionCode, /REMOVE)
  time_Coverage_Start=ST_utils->formatDate([year, month, day, 0, 0, 0], template='satellite')
  time_Coverage_End=ST_utils->formatDate([year, month, day, 23, 59, 59], template='satellite')
  header.cdr_variable=['cdr_variable', 'FAPAR']
  header.Process=['process', 'JRC FAPAR TOC algorithm - see QA4ECV ATBD']

  ;most of information coming from getStandardFaparDataSetInfo()
  ;writer did most of the dirty job (setting min, max, add_offset, scale_factor...)
  ;only peculiar variables (fapar & sigma fapar) are still ready and we DON'T need more computation
  ;      if keyword_set(NC) then write_georef_ncdf, destncfilename, $
  ;        bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
  ;        dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, scaledminmaxs=scaledminmaxs, $
  ;        /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
  ;        id=ncFileInfo.filename, satellite=satellite, header=header, $
  ;        date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End
  ;
  ;      if keyword_set(HDF) then write_hdf, resFileHDF, $
  ;        bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
  ;        dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, scaledminmaxs=scaledminmaxs, $
  ;        trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
  ;        id=hdfFileInfo.filename, satellite=satellite, header=header, $
  ;        date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End
  ;;
  write_georef_ncdf, destncfilename, $
    bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
    dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, scaledminmaxs=scaledminmaxs, $
    /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
    id=ncFileInfo.filename, satellite=satellite, header=header, $
    date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End
  print, 'finish writing ncdf'
  ;stop
  write_hdf, desthdffilename, $
    bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
    dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
    trueMinMaxs=minMaxs, nanList=nanList, $
    trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
    header=header

end