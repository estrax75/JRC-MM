;FUNCTION doFaparComposite, sensor, resolution, missionName, level, confDir, sourceDir, tempDir, outputDir, year, month, $
;TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE

FUNCTION doFaparComposite_split, instrument, indicator, spatialResolution, inputLevel, missionName, mainVarName, missionCode, year, month, day, $
  sourceDir, outputDir, tempdir, $
  FIRST_LOOK=FIRST_LOOK, $
  NC=NC, HDF=HDF, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, $
  OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, UNC=UNC, data_dir=data_dir
  ;
  ;
  ;COMMON bigData

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  INT_NAN=2^15
  DATA_RANGE=[0., 1.]
  DATA_NAN=255

  ;month=indgen(12)+1

  inputBaseDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
  outputBaseDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)

  if n_elements(noaanumber) eq 0 then begin
    noaanumber=getAVHRRNOAANumber(year, undef)
    if n_elements(noaanumber) gt 1 then noaanumber=noaanumber[MISSIONOVERLAPINDEX]
    noaanumber=noaanumber[0]
  endif else begin
    noaanumber=fix(noaanumber)
  endelse
  DelidlVar, undef
  print, year, noaanumber

  tInfo=getTimeDerivedInfo(year, month, TA_TYPE, TC_TYPE, xticks_c=xticks_c, xtickname_c=xtickname_c)

  year=string(year, format='(I04)')
  month=string(month, format='(I02)')

  first=tInfo.first
  last=tInfo.last
  inputLevel=tInfo.level
  extraPath=tInfo.extraPath
  ;test...
  ;first=first[1:*]
  ;last=last[1:*]

  if ~(KEYWORD_SET(nodirbuild)) then inputDir=inputBaseDir+tInfo.extraPath else inputDir=inputBaseDir
  outDir=outputBaseDir+path_sep()+tInfo.extraPath+path_sep()

  outputDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)
  sourceDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
  tempDir=ST_fileSystem->adjustDirSep(tempDir, /ADD)
  print, 'working on', year, month, '...'
  dir_in=sourceDir
  dirout=outputDir
  sourceLevel='L1'

  missionCode=getAVHRRNOAANumber(year, undef)
  DelidlVar, undef
  ;
  ;monthDays=utility->calcDayOfMonth([y,m,1,0])
  ;monthDays=6
  test=0
  version='N'+string(missionCode, format='(I02)');version='001'
  ;  if keyword_set(test) then begin
  ;    fLookDir='first_look'
  ;    ;cd, dirout
  ;    firstLookDir=outputDir+fLookDir
  ;    fInfo=file_info(fLookDir)
  ;    if ~(fInfo.exists) then file_mkdir, firstLookDir
  ;    samplefilename='fl_'+new_file+'.gif'
  ;    fullSavFName=firstLookDir+path_sep()+'sav_'+new_file+'.sav'
  ;    fullSampleFName=firstLookDir+path_sep()+samplefilename
  ;    print, 'sampleImage-->', fullSampleFName
  ;    restore, fullSavFName
  ;    data_tc=val_comp
  ;  endif else begin
  for type=0, n_elements(first)-1 do begin
    ; one destination file for each sub period

    ; first test file
    checkFirst=1
    cc=0
    while checkFirst do begin
      firstDay=first[type]+cc
      ;if sensor eq 'AVH09C1' then sourceFileName=build_JRC_FPA_AVH_Daily_Product_FileName(sensor, resolution, year, month, firstDay, missionName, missionCode, mainVarName, level, VERSION='01')
      ncfilename=build_JRC_FPA_AVH_Daily_Product_FileName(instrument, fix(year), fix(month), fix(firstDay), timestamp, temporalResolution, location, spatialResolution, $
        product, version, 'NC',  indicator=indicator, sourceLevel, projection=projection)
      ;if sensor eq 'MODIS' then sourceFileName=buildMODISFileName_D(sensor, resolution, year, month, firstDay, missionName, missionCode, mainVarName, sourceLevel)
      ;hdffilename=ST_fileSystem->addFileExtension(new_file, 'HDF'); hdf

      if keyword_set(data_dir) then begin
        fileDir=data_dir
        ;fileDir='E:\mariomi\Documents\projects\ldtr\data\AVHRR\FP'+path_sep()
      endif else begin
        fileDir=sourceDir+ncfilename.filePath
        fileDir=ST_fileSystem->adjustDirSep(fileDir, /ADD)
        fileDir=fileDir+'v1.5'+path_sep()
        ; new temp test
      endelse
      thisFileName=fileDir+ncfilename.fileName

      ;destDir=outputDir+hdfFileInfo.filePath
      ;destDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
      ;resFileNC=destDir+ncFileInfo.fileName
      ;resFileHDF=destDir+hdfFileInfo.fileName

      print, 'searching...',  fileDir, ncfilename.fileName
      ff1 = (file_search(fileDir, ncfilename.fileName, COUNT=cnt))[0];, /FULL_QUALIFY)
      ;ff2 = FILE_SEARCH(dir_in+hdffilename, COUNT=cnt2)
      nodata=0
      if cnt ne 1 then begin
        cc++
        if cc le last[type] then continue
        nodata=1
        break
      endif
      print, '*********'
      print, 'day-->', first[type]
      print, 'file-->', ff1
      print, '*********'
      storeFileInfo={fDir:fileDir, fName:ncfilename.fileName, fid:-1}
      ;faparData=read_AVHRR_FAPAR(fileDir, ncfilename.fileName, FOUND=FOUND)
      ;faparData.valid=1
      ;histoData=histogram(faparData.fapar)
      ;faparData.fapar=1.*faparData.fapar*faparData.slope_fapar+faparData.offset_fapar

      ;true_single_fapar=faparData.fapar
      ;save, true_single_fapar,fileName=tempDir+ncfilename+'_SINGLE.sav'
      ;true_single_fapar=0

      ;faparData.nir=1.*faparData.nir*faparData.slope_nir+faparData.offset_nir
      ;faparData.red=1.*faparData.red*faparData.slope_red+faparData.offset_red
      checkFirst=0
    endwhile
    if keyword_set(nodata) then message, 'no data for this time composite!!!'
    expectedDays=last[type]-first[type]+1
    foundDays=0


    ;tcStruct=ptrarr(expectedDays)
    ;data_day=replicate(faparData, expectedDays)
    storeFileInfos=replicate(storeFileInfo, expectedDays)
    ;thisSavFile=ST_utils->getSysTime(/FILECOMPATIBILITY)
    ;tempdir=ST_fileSystem->adjustDirSep(tempdir, /ADD)
    ;thisFullSavFile=tempdir+thisSavFile+'.sav'
    ;save, faparData, filename=thisFullSavFile
    ;fapar_files[cc].data_file=thisFullSavFile
    ;faparData=0
    ;if cc ne 0 then fapar_files[0:cc-1].valid=0
    ;storeFileInfos[0:(cc-1)>0].fDir=''
    ;storeFileInfos[0:(cc-1)>0].fName=''
    storeFileInfos[cc+1:*].fDir=''
    storeFileInfos[cc+1:*].fName=''
    for d=firstDay+1, last[type] do begin

      ;if sensor eq 'AVH09C1' then sourceFileName=buildAVHRRFAPARFileName_D(sensor, resolution, year, month, d, missionName, missionCode, mainVarName)
      ;if sensor eq 'MODIS' then sourceFileName=buildMODISFAPARFileName_D(sensor, resolution, year, month, d, missionName, missionCode, mainVarName)
      ncfilename=build_JRC_FPA_AVH_Daily_Product_FileName(instrument, fix(year), fix(month), fix(d), timestamp, temporalResolution, location, spatialResolution, $
        product, version, 'NC',  indicator=indicator, sourceLevel, projection=projection)
      ;if sensor eq 'MODIS' then sourceFileName=buildMODISFileName_D(sensor, resolution, year, month, firstDay, missionName, missionCode, mainVarName, level)
      ;hdffilename=ST_fileSystem->addFileExtension(new_file, 'HDF'); hdf

      if keyword_set(data_dir) then begin
        fileDir=data_dir
        ;fileDir='E:\mariomi\Documents\projects\ldtr\data\AVHRR\FP'+path_sep()
      endif else begin
        fileDir=sourceDir+ncfilename.filePath
        fileDir=ST_fileSystem->adjustDirSep(fileDir, /ADD)
        fileDir=fileDir+'v1.5'+path_sep()
        ; new temp test
      endelse
      thisFileName=fileDir+ncfilename.fileName

      ;destDir=outputDir+hdfFileInfo.filePath
      ;destDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
      ;resFileNC=destDir+ncFileInfo.fileName
      ;resFileHDF=destDir+hdfFileInfo.fileName

      print, 'searching...',  fileDir, ncfilename.fileName
      ff1 = (file_search(fileDir, ncfilename.fileName, COUNT=cnt))[0];, /FULL_QUALIFY)
      ;ff2 = FILE_SEARCH(dir_in+hdffilename, COUNT=cnt2)
      print, '*********'
      print, 'day-->', d
      print, 'file-->', ff1
      print, '*********'
      ;faparData=readFAPAR(dir_in, ncfilename, FOUND=FOUND)
      if cnt eq 1 then begin
        print, 'found'
        storeFileInfos[d-first[type]].fDir=fileDir
        storeFileInfos[d-first[type]].fName=ncfilename.fileName
      endif
      ;faparData=read_AVHRR_FAPAR(fileDir, ncfilename.fileName, FOUND=FOUND)
      ;      if keyword_set(FOUND) then begin
      ;        foundDays++
      ;        faparData.fapar=1.*faparData.fapar*faparData.slope_fapar+faparData.offset_fapar
      ;        faparData.nir=1.*faparData.nir*faparData.slope_nir+faparData.offset_nir
      ;        faparData.red=1.*faparData.red*faparData.slope_red+faparData.offset_red
      ;        faparData.valid=1
      ;        print, 'local index-->',d-first[type]
      ;        ;fapar_files[0:cc-1].valid=0
      ;        ;data_day[d-first[type]]=faparData
      ;        ;~ 1 g each file
      ;        thisSavFile=ST_utils->getSysTime(/FILECOMPATIBILITY)
      ;        thisFullSavFile=tempdir+thisSavFile+'.sav'
      ;        save, faparData, filename=thisFullSavFile
      ;        fapar_files[d-first[type]].data_file=thisFullSavFile
      ;        fapar_files[d-first[type]].valid=1
      ;        faparData=0
      ;      endif

    endfor
    daysInfo=[expectedDays,foundDays]
    data_day1=temporary(data_day)
    destLevel='L3'

    if TA_TYPE eq 'MEAN' then begin

      ;if instrument eq 'AVH09C1' then destTCFile=build_JRC_FPA_AVH_MeanAlg_TenDays_Product_FileName(instrument, resolution, year, month, first[type], missionName, missionCode, mainVarName, startDay=first[type], endDay=last[type], destLevel, VERSION='01')
      if instrument eq 'AVH' then begin
        destncFileNameInfo=build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName(instrument, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'NC', undef, indicator='LAN')
        desthdfFileNameInfo=build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName(instrument, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'HDF', undef, indicator='LAN')
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

      trueSlopes=bandSlopes
      trueIntercepts=bandIntercepts

      ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
      ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
      ;byteOutput=dataByteScaling(output.fpar, VALUE_BYTES=[0,250])
      remarkableFlags=bSInfo.remarkableFlags
      DATA_NAN=bSInfo.DATA_NAN
      BYTE_NAN=bSInfo.BYTE_NAN
      BYTE_RANGE=bSInfo.BYTE_RANGE

      ;true_mean_fapar=data_tc.fapar
      ;save, true_mean_fapar, fileName=tempDir+destncfilename+'_MEAN.sav'
      ;true_mean_fapar=0

      res=dataByteScaling(data_tc.fapar, data_tc.flag, $
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
      save, data_tc, filename='E:\mariomi\Desktop\fapar_presentation\tc_result'+id+'.sav'
      dataSets=[ptr_new(data_tc.fapar, /NO_COPY), $
        ptr_new(data_tc.red, /NO_COPY),$
        ptr_new(data_tc.nir, /NO_COPY), $
        ptr_new(data_tc.flag, /NO_COPY)]

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

      write_georef_ncdf, destncfilename, $
        bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
        dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
        /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, $
        trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
        header=header
      print, 'finish writing ncdf'
      ;stop
      write_hdf, desthdffilename, $
        bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
        dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
        trueMinMaxs=minMaxs, nanList=nanList, $
        trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
        header=header

    endif
    if TA_TYPE eq 'TC' then begin

      starttime=systime(1)
      if instrument eq 'AVH' then begin
        ;  change procedure by TC type (monthly, multiple day...)
        if TC_TYPE eq '5D' or TC_TYPE eq '10D' or TC_TYPE eq '16D' then begin
          tResolution=strsplit(TC_TYPE, 'D', /EXTRACT, /PRESERVE)
          tResolution=STRING(tResolution[0], FORMAT='(i03)')
          tResolution=tResolution+'D'
          destncFileNameInfo=build_JRC_FPA_AVH_TCAlg_DailyInterval_Product_FileName(instrument, fix(year), fix(month), first[type], undef, tResolution, undef, spatialResolution, undef, undef, 'n.NC', undef, indicator='LAN')
          desthdfFileNameInfo=build_JRC_FPA_AVH_TCAlg_DailyInterval_Product_FileName(instrument, fix(year), fix(month), first[type], undef, tResolution, undef, spatialResolution, undef, undef, 'n.HDF', undef, indicator='LAN')
        endif
        if TC_TYPE eq 'MONTHLY' then begin
          destncFileNameInfo=build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName(instrument, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'n.NC', undef, indicator='LAN')
          desthdfFileNameInfo=build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName(instrument, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'n.HDF', undef, indicator='LAN')
        endif
        ;        if TC_TYPE eq 'YEARLY' then begin
        ;          destncFileNameInfo=build_JRC_FPA_AVH_TCAlg_Yearly_Product_FileName(instrument, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'NC', undef, indicator='LAN')
        ;          desthdfFileNameInfo=build_JRC_FPA_AVH_TCAlg_Yearly_Product_FileName(instrument, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'HDF', undef, indicator='LAN')
        ;        endif
      endif
      if n_elements(desthdfFileNameInfo) eq 0 then message, 'TC_TYPE: ' + TC_TYPE + ' not allowed!!!!'
      ;if sensor eq 'MODIS' then destTCFile=buildMODISFileName_TC(sensor, resolution, year, month, first[type], missionName, missionCode, mainVarName, destLevel, startDay=first[type], endDay=last[type])
      if keyword_set(UNC) then subVer='unc'+path_sep() else subVer=''

      ;outputDir='E:\mariomi\Documents\projects\ldtr\data\AVHRR\FP\'
      if keyword_set(data_dir) then begin
        ;fileDir=data_Dir
        fileDir=ST_fileSystem->adjustDirSep(data_Dir, /ADD)
      endif else begin
        fileDir=sourceDir+destncFileNameInfo.filePath
        fileDir=ST_fileSystem->adjustDirSep(fileDir, /ADD)
      endelse
      ;fileDir=outputDir+destncFileNameInfo.filePath
      ;fileDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)+'test'+subVer+path_sep()
      fileDir=fileDir+subVer
      destncfilename=fileDir+destncFileNameInfo.fileName
      desthdffilename=fileDir+desthdfFileNameInfo.fileName

      ; without uncertainties
      ;sm_call_composite, expectedDays, storeFileInfos, data_tc, 10
      ; with uncertainties
      ;if keyword_set(UNC) then sm_call_composite_w_unc, expectedDays, storeFileInfos, data_tc, 10 else sm_call_composite, expectedDays, storeFileInfos, data_tc, 10
      if keyword_set(UNC) then sm_call_composite_w_unc, expectedDays, storeFileInfos, data_tc, 20, prevflag=prevflag else sm_call_composite, expectedDays, storeFileInfos, data_tc, 20, prevflag=prevflag
      ;call_composite, expectedDays, data_day1, data_tc
      endTime=systime(1)-starttime
      print, 'computed in about:', strcompress(endTime), 'seconds'

      ;destncfilename=ST_fileSystem->addFileExtension(destTCFile, 'NC'); nc
      ;desthdffilename=ST_fileSystem->addFileExtension(destTCFile, 'HDF'); hdf
      print, '*********'
      print, 'building tc file: ', desthdffilename
      print, '*********'

      ;faparTCDSInfo=getStandardFaparTCDataSetInfo()
      faparTCDSInfo=getStandardFaparTCDataSetInfo_test()
      bandNames=faparTCDSInfo.bandNames
      bandLongNames=faparTCDSInfo.bandLongNames
      bandStandardNames=faparTCDSInfo.bandLongNames;faparTCDSInfo.bandStandardNames
      bandSlopes=faparTCDSInfo.bandSlopes
      bandMeasureUnits=faparTCDSInfo.bandMeasureUnits
      bandDataTypes=faparTCDSInfo.bandDataTypes
      bandIntercepts=faparTCDSInfo.bandIntercepts
      minMaxs=faparTCDSInfo.minMaxs
      scaledminmaxs=faparTCDSInfo.scaledminmaxs
      nanList=faparTCDSInfo.nans

      trueSlopes=bandSlopes
      trueIntercepts=bandIntercepts
      header=faparTCDSInfo.header

      ;true_tc_fapar=data_tc.fapar
      ;save, true_tc_fapar, fileName=tempDir+ncfilename+'_TC.sav'
      ;true_tc_fapar=0
      ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
      ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
      ;byteOutput=dataByteScaling(output.fpar, VALUE_BYTES=[0,250])
      ;remarkableFlags=bSInfo.remarkableFlags
      ;DATA_NAN=bSInfo.DATA_NAN
      ;BYTE_NAN=bSInfo.BYTE_NAN
      ;BYTE_RANGE=bSInfo.BYTE_RANGE
      validIdxs=where(data_tc.flag eq 0 or data_tc.flag eq 4 or data_tc.flag eq 5, watCount, compl=notValidIdxs, ncompl=notValidCount)
      ;waterIdxs=where(data_tc.flag eq 3, watCount)

      res=dataByteScaling(data_tc.fapar, data_tc.flag, $
        DATA_NAN=!VALUES.F_NAN, BYTE_NAN=faparTCDSInfo.nans[2], $
        DATA_RANGE=faparTCDSInfo.minMaxs[2,*], BYTE_RANGE=faparTCDSInfo.scaledminmaxs[2,*], outSlope, outIntercept)
      ;DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      ;DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
      data_tc.fapar=res.resultData
      if notValidCount gt 0 then data_tc.fapar[notValidIdxs]=faparTCDSInfo.nans[2]
      ; overwrite slope & intercept for fapar band
      trueIntercepts[2]=outIntercept
      trueSlopes[2]=outSlope
      ;output.flag=res.resultFlag

      ;tv, rebin(bytscl(data_tc.sigma), 720, 360) ;*
      res=dataByteScaling(data_tc.sigma, data_tc.flag, $
        DATA_NAN=!VALUES.F_NAN, BYTE_NAN=faparTCDSInfo.nans[3], $
        DATA_RANGE=faparTCDSInfo.minMaxs[3,*], BYTE_RANGE=faparTCDSInfo.scaledminmaxs[3,*], outSlope, outIntercept)
      ;        DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      ;        DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
      data_tc.sigma=res.resultData
      if notValidCount gt 0 then data_tc.sigma[notValidIdxs]=faparTCDSInfo.nans[3]
      ; overwrite slope & intercept for sigma band (fapar)
      trueIntercepts[3]=outIntercept
      trueSlopes[3]=outSlope
      ;data_tc.flag=res.resultFlag

      ;tv, rebin(bytscl(data_tc.dev_temp), 720, 360) ;*
      res=dataByteScaling(data_tc.dev_temp, data_tc.flag, $
        DATA_NAN=!VALUES.F_NAN, BYTE_NAN=faparTCDSInfo.nans[4], $
        DATA_RANGE=faparTCDSInfo.minMaxs[4,*], BYTE_RANGE=faparTCDSInfo.scaledminmaxs[4,*], outSlope, outIntercept)
      ;        DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      ;        DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
      data_tc.dev_temp=res.resultData;data_tc.sigma=res.resultData
      if notValidCount gt 0 then data_tc.dev_temp[notValidIdxs]=faparTCDSInfo.nans[4]
      ; overwrite slope & intercept for dev band (fapar)
      trueIntercepts[4]=outIntercept
      trueSlopes[4]=outSlope

      ;filling Nan
      nanIdx=where(finite(data_tc.red) eq 0, cnt)
      if cnt ne 0 then begin
        ;anglesNan=where(tv_avhrr eq -9999, cntAnglesNan)
        ;ptr_new(data_tc.fapar, /NO_COPY), ptr_new(data_tc.dev_temp, /NO_COPY), ptr_new(data_tc.sigma, /NO_COPY), $
        data_tc.red[nanIdx]=INT_NAN
        data_tc.dev_red_temp[nanIdx]=INT_NAN
        data_tc.sigma_red[nanIdx]=INT_NAN
        data_tc.nir[nanIdx]=INT_NAN
        data_tc.dev_nir_temp[nanIdx]=INT_NAN
        data_tc.sigma_nir[nanIdx]=INT_NAN
        data_tc.toc_red[nanIdx]=INT_NAN
        data_tc.toc_nir[nanIdx]=INT_NAN
        data_tc.ts[nanIdx]=INT_NAN
        data_tc.tv[nanIdx]=INT_NAN
      endif

      badIndexes=where(data_tc.dev_red_temp lt 0, count)
      if count gt 0 then data_tc.dev_red_temp[badIndexes]=INT_NAN
      badIndexes=where(data_tc.dev_nir_temp lt 0, count)
      if count gt 0 then data_tc.dev_nir_temp[badIndexes]=INT_NAN

      byteFlagTags=['fapar', 'sigma', 'dev_temp']
      waterIdxs=where(data_tc.flag eq 3, watCount)
      intFlagTags=['red', 'nir']
      tags=tag_names(data_tc)

;      for i=0, n_elements(flagTags)-1 do begin
;        thisIntIdx=(where(intFlagTags[i] eq tags, countInt))[0]
;        thisByteIdx=(where(byteFlagTags[i] eq tags, countByte))[0]
;        if watCount gt 0 then begin
;          if countInt eq 1 then data_tc.(thisIntIdx)=mapQualityFlags(data_tc.(thisIntIdx), waterIdxs, INT_NAN)
;          if countByte eq 1 then data_tc.(thisByteIdx)=mapQualityFlags(data_tc.(thisByteIdx), waterIdxs, BYTE_NAN)
;        endif
;      endfor

      ;      dataSets=[ptr_new(data_tc.day, /NO_COPY),ptr_new(data_tc.nday, /NO_COPY), $
      ;        ptr_new(data_tc.fapar, /NO_COPY), ptr_new(data_tc.dev_temp, /NO_COPY), ptr_new(data_tc.sigma, /NO_COPY), $
      ;        ptr_new(data_tc.red, /NO_COPY), ptr_new(data_tc.dev_red_temp, /NO_COPY), ptr_new(data_tc.sigma_red, /NO_COPY) ,$
      ;        ptr_new(data_tc.nir, /NO_COPY), ptr_new(data_tc.dev_nir_temp, /NO_COPY), ptr_new(data_tc.sigma_nir, /NO_COPY), $
      ;        ptr_new(data_tc.flag, /NO_COPY)]

      if keyword_set(UNC) then id='_unc' else id=''
      ;save, data_tc, filename='E:\mariomi\Desktop\fapar_presentation\tc_result'+id+'.sav', /COMPRESS
      ;
      ;data_tc1=data_tc
      ;delIdlvar, data_tc
      ;restore, 'E:\mariomi\Desktop\fapar_presentation\tc_resultunc.sav'
      ;diff1=abs(data_tc1.day-data_tc.DAY)
      dataSets_test=[ptr_new(data_tc.day, /NO_COPY),ptr_new(data_tc.nday, /NO_COPY), $
        ptr_new(data_tc.fapar, /NO_COPY), ptr_new(data_tc.dev_temp, /NO_COPY), ptr_new(data_tc.sigma, /NO_COPY), $
        ptr_new(data_tc.red, /NO_COPY), ptr_new(data_tc.dev_red_temp, /NO_COPY), ptr_new(data_tc.sigma_red, /NO_COPY) ,$
        ptr_new(data_tc.nir, /NO_COPY), ptr_new(data_tc.dev_nir_temp, /NO_COPY), ptr_new(data_tc.sigma_nir, /NO_COPY), $
        ptr_new(data_tc.toc_red, /NO_COPY),ptr_new(data_tc.toc_nir, /NO_COPY), $
        ptr_new(data_tc.flag, /NO_COPY),ptr_new(prevflag, /NO_COPY), $
        ptr_new(data_tc.ts, /NO_COPY), ptr_new(data_tc.tv, /NO_COPY)]

      boundary=[-180.0, 180.0, -90, 90.]

      ;destncfilename=outputDir+destncfilename
      ;desthdffilename=outputDir+desthdffilename
      date_created=ST_utils->getSysTime(/FILECOMPATIBILITY)
      satellite='NOAA '+strcompress(missionCode, /REMOVE)
      time_Coverage_Start=ST_utils->formatDate([year, month, first[type], 0, 0, 0], template='satellite')
      time_Coverage_End=ST_utils->formatDate([year, month, last[type], 23, 59, 59], template='satellite')
      header.cdr_variable=['cdr_variable', 'FAPAR']
      header.process='Time composite'

      write_georef_ncdf, destncfilename, $
        bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
        dataSets_test, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
        ;dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
        /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, scaledminmaxs=scaledminmaxs, $
        trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
        header=header, id=destncFileNameInfo.filename, satellite=satellite, $
        date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End
      write_hdf, desthdffilename, $
        bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
        dataSets_test, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
        ;dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
        trueMinMaxs=minMaxs, nanList=nanList, scaledminmaxs=scaledminmaxs, $
        trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
        header=header, id=desthdfFileNameInfo.filename, satellite=satellite, $
        date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End

    endif
  endfor
  ;  endelse
  ;    dims=size(data_tc.fapar, /DIMENSIONS)
  ;    FIRST_LOOK=1
  ;    if keyword_set(FIRST_LOOK) then begin
  ;      fLookDir='first_look'
  ;      ;cd, dirout
  ;      firstLookDir=outputDir+fLookDir
  ;      fInfo=file_info(fLookDir)
  ;      new_file=destTCFile
  ;      if ~(fInfo.exists) then file_mkdir, firstLookDir
  ;      samplefilename='fl_'+new_file+'.gif'
  ;      fullSavFName='fl_'+new_file+'.sav'
  ;      fullSampleFName=firstLookDir+path_sep()+samplefilename
  ;      print, 'sampleImage-->', fullSampleFName
  ;      save, data_tc, fileName=fullSavFName, /COMPRESS
  ;      sampleImg=rebin(data_tc.fapar, dims[0]/10,dims[1]/10)
  ;      minvalue=min(data_tc.fapar, max=maxvalue)
  ;      sampleImg=bytscl(sampleImg)
  ;      LOADCT, 14
  ;      write_gif, fullSavFName, sampleImg
  ;    endif


end