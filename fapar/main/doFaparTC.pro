FUNCTION doFaparTC, sensor, resolution, missionName, confDir, sourceDir, tempDir, outputDir, year, month, $
  MONTHLY=MONTHLY, TENDAYS=TENDAYS, SIXTEENDAYS=SIXTEENDAYS, ELAB_TC=ELAB_TC, ELAB_MEAN=ELAB_MEAN
  ;
  ;
  ;COMMON bigData

  INT_NAN=-9999
  DATA_RANGE=[0., 1.]
  DATA_NAN=255

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  if ~obj_valid(utils) then utils=obj_new('Utility')
  ;
  ;
  ;dir_in='/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/'+sensor+'/FAPAR/'
  ;dirout='/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/'+sensor+'/FAPAR_TC/'
  outputDir=fsObj->adjustDirSep(outputDir, /ADD)
  sourceDir=fsObj->adjustDirSep(sourceDir, /ADD)
  tempDir=fsObj->adjustDirSep(tempDir, /ADD)
  print, 'working on', year, month, '...'
  dir_in=sourceDir
  dirout=outputDir

  if KEYWORD_SET(TENDAYS) then begin
    first=[01,11,21]
    last=[10,20,utils->calcDayOfMonth([year,month,1,0])]
  endif
  if KEYWORD_SET(SIXTEENDAYS) then begin
    first=[01,17]
    last=[16,utils->calcDayOfMonth([year,month,1,0])]
  endif
  ;only for faster test
  ;  IF KEYWORD_SET(SIXTEENDAYS) then begin
  ;    first=[01,4,11,16,21,26]
  ;    last=[3,10,15,20,25,utils->calcDayOfMonth([year,month,1,0])]
  ;  endif
  IF KEYWORD_SET(MONTHLY) then begin
    first=[01]
    last=[utils->calcDayOfMonth([year,month,1,0])]
  endif

  ;for m=0, N_elements(month)-1 do begin
  ;
  ; look for files within the month
  ;
  ;


  ;file_name=sensor+'_'+platform+'_'+year+month(m)+'*'
  ;end_file='L2_MUL_000009_900S900N1800W1800E_PLC_0005D_PRO.HDF'
  ;expidfile = dir_in + file_name+end_file
  missionCode=getAVHRRNOAANumber(year, undef)
  ;
  ;monthDays=utility->calcDayOfMonth([y,m,1,0])
  ;monthDays=6
  test=0
  if keyword_set(test) then begin
    fLookDir='first_look'
    ;cd, dirout
    firstLookDir=outputDir+fLookDir
    fInfo=file_info(fLookDir)
    if ~(fInfo.exists) then file_mkdir, firstLookDir
    samplefilename='fl_'+new_file+'.gif'
    fullSavFName=firstLookDir+path_sep()+'sav_'+new_file+'.sav'
    fullSampleFName=firstLookDir+path_sep()+samplefilename
    print, 'sampleImage-->', fullSampleFName
    restore, fullSavFName
    data_tc=val_comp
  endif else begin
    for type=0, n_elements(first)-1 do begin
      ; one destination file for each sub period

      ; first test file
      checkFirst=1
      cc=0
      while checkFirst do begin
        firstDay=first[type]+cc
        if sensor eq 'AVH09C1' then sourceFileName=buildAVHRRFAPARFileName_D(sensor, resolution, year, month, firstDay, missionName, missionCode, mainVarName)
        if sensor eq 'MODIS' then sourceFileName=buildMODISFAPARFileName_D(sensor, resolution, year, month, firstDay, missionName, missionCode, mainVarName)
        ncfilename=fsObj->addFileExtension(sourceFileName, 'NC'); nc
        ;hdffilename=fsObj->addFileExtension(new_file, 'HDF'); hdf

        ff1 = FILE_SEARCH(dir_in+ncfilename, COUNT=cnt);, /FULL_QUALIFY)
        ;ff2 = FILE_SEARCH(dir_in+hdffilename, COUNT=cnt2)
        if cnt ne 1 then begin
          cc++
          continue
        endif
        print, '*********'
        print, 'day-->', first[type]
        print, 'file-->', ff1
        print, '*********'
        faparData=readFAPAR(dir_in, ncfilename, FOUND=FOUND)
        faparData.valid=1
        faparData.fapar=1.*faparData.fapar*faparData.slope_fapar+faparData.offset_fapar
        faparData.nir=1.*faparData.nir*faparData.slope_nir+faparData.offset_nir
        faparData.red=1.*faparData.red*faparData.slope_red+faparData.offset_red
        checkFirst=0
      endwhile

      expectedDays=last[type]-first[type]+1
      foundDays=0


      ;tcStruct=ptrarr(expectedDays)
      data_day=replicate(faparData, expectedDays)
      if cc ne 0 then data_day[0:cc-1].valid=0
      data_day[cc+1:*].valid=0
      ;data_day=replicate(faparData, expectedDays)
      ;fullDayInfo={}
      for d=firstDay+1, last[type] do begin

        if sensor eq 'AVH09C1' then sourceFileName=buildAVHRRFAPARFileName_D(sensor, resolution, year, month, d, missionName, missionCode, mainVarName)
        if sensor eq 'MODIS' then sourceFileName=buildMODISFAPARFileName_D(sensor, resolution, year, month, d, missionName, missionCode, mainVarName)

        ncfilename=fsObj->addFileExtension(sourceFileName, 'NC'); nc
        ;hdffilename=fsObj->addFileExtension(new_file, 'HDF'); hdf

        ff1 = FILE_SEARCH(dir_in+ncfilename, COUNT=cnt1);, /FULL_QUALIFY)
        ;ff2 = FILE_SEARCH(dir_in+hdffilename, COUNT=cnt2)
        print, '*********'
        print, 'day-->', d
        print, 'file-->', ff1
        print, '*********'
        faparData=readFAPAR(dir_in, ncfilename, FOUND=FOUND)
        if keyword_set(FOUND) then begin
          foundDays++
          faparData.fapar=1.*faparData.fapar*faparData.slope_fapar+faparData.offset_fapar
          faparData.nir=1.*faparData.nir*faparData.slope_nir+faparData.offset_nir
          faparData.red=1.*faparData.red*faparData.slope_red+faparData.offset_red
          faparData.valid=1
          print, 'local index-->',d-first[type]
          data_day[d-first[type]]=faparData
          faparData=0
        endif

      endfor
      daysInfo=[expectedDays,foundDays]
      data_day1=data_day
      ;ELAB_MEAN=1
      if keyword_set(ELAB_MEAN) then begin

        if sensor eq 'AVH09C1' then destTCFile=buildAVHRRFAPARFileName_Mean(sensor, resolution, year, month, first[type], missionName, missionCode, mainVarName, startDay=first[type], endDay=last[type])
        if sensor eq 'MODIS' then destTCFile=buildMODISFAPARFileName_Mean(sensor, resolution, year, month, first[type], missionName, missionCode, mainVarName, startDay=first[type], endDay=last[type])
        destncfilename=fsObj->addFileExtension(destTCFile, 'NC'); nc
        desthdffilename=fsObj->addFileExtension(destTCFile, 'HDF'); hdf
        print, '*********'
        print, 'building mean file: ', destTCFile
        print, '*********'

        compute_mean, expectedDays, data_day1, data_tc
        avgIdx=[2,5,8]
        
        faparTCDSInfo=getStandardFaparTCDataSetInfo_new()
        bandNames=(faparTCDSInfo.bandNames)[avgIdx]
        bandLongNames=(faparTCDSInfo.bandLongNames)[avgIdx]
        bandSlopes=(faparTCDSInfo.bandSlopes)[avgIdx]
        bandMeasureUnits=(faparTCDSInfo.bandMeasureUnits)[avgIdx]
        bandDataTypes=(faparTCDSInfo.bandDataTypes)[avgIdx]
        bandIntercepts=(faparTCDSInfo.bandIntercepts)[avgIdx]
        minMaxs=(faparTCDSInfo.minMaxs)[avgIdx,*]
        nanList=(faparTCDSInfo.nans)[avgIdx]

        trueSlopes=bandSlopes
        trueIntercepts=bandIntercepts
        versionDate=faparTCDSInfo.versionDate
        versionNumber=faparTCDSInfo.versionNumber

        TYPE1=1
        ZEROISNAN=keyword_set(TYPE1) ;0
        TWO51_TO_TWO55_ISNAN=keyword_set(TYPE2) ;0
        bSInfo=getByteScalingSetting(ZEROISNAN=keyword_set(TYPE1), TWO51_TO_TWO55_ISNAN=keyword_set(TYPE2))
        ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
        ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
        ;byteOutput=dataByteScaling(output.fpar, VALUE_BYTES=[0,250])
        remarkableFlags=bSInfo.remarkableFlags
        DATA_NAN=bSInfo.DATA_NAN
        BYTE_NAN=bSInfo.BYTE_NAN
        BYTE_RANGE=bSInfo.BYTE_RANGE
        res=dataByteScaling(data_tc.fapar, data_tc.flag, FLAG_VALUES=[9,10], $
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
        flagTags=strupcase(['red', 'nir', 'sigma_red', 'sigma_nir', 'dev_red_temp', 'dev_nir_temp'])
        tags=tag_names(data_tc)

        idx_1 = where (data_tc.flag eq 1, c1)
        idx_2 = where (data_tc.flag eq 2, c2)
        idx_3 = where (data_tc.flag eq 3, c3)

        waterIdxs=where(data_day[0].flag eq 3, watCount)
        for i=0, n_elements(flagTags)-1 do begin
          thisIdx=(where(flagTags[i] eq tags, count))[0]
          nanIdxs=where(data_tc.(thisIdx) gt 250, count)
          ;nanIdxs = where (data_tc.flag eq 3, c3)
          if count gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), nanIdxs, INT_NAN)
          if watCount gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), waterIdxs, INT_NAN)
        endfor

;        dataSets=[ptr_new(data_tc.day, /NO_COPY),ptr_new(data_tc.nday, /NO_COPY), $
;          ptr_new(data_tc.fapar, /NO_COPY), ptr_new(data_tc.sigma, /NO_COPY), ptr_new(data_tc.dev_temp, /NO_COPY), $
;          ptr_new(data_tc.red, /NO_COPY), ptr_new(data_tc.sigma_red, /NO_COPY), ptr_new(data_tc.dev_red_temp, /NO_COPY) ,$
;          ptr_new(data_tc.nir, /NO_COPY), ptr_new(data_tc.sigma_nir, /NO_COPY), ptr_new(data_tc.dev_red_temp, /NO_COPY), $
;          ptr_new(data_tc.flag, /NO_COPY), $
;          ptr_new(data_tc.toc_red, /NO_COPY), ptr_new(data_tc.toc_nir, /NO_COPY), $
;          ptr_new(data_tc.nday, /NO_COPY)]
        dataSets=[ptr_new(data_tc.fapar, /NO_COPY), $
          ptr_new(data_tc.red, /NO_COPY),$
          ptr_new(data_tc.nir, /NO_COPY)]

        boundary=[-180.0, 180.0, -90, 90.]

        destncfilename=outputDir+destncfilename
        desthdffilename=outputDir+desthdffilename
        write_georef_ncdf, destncfilename, $
          bandNames, bandLongNames, bandMeasureUnits, $
          dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
          /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, $
          trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, versionDate=versionDate, versionNumber=versionNumber
        write_hdf, desthdffilename, $
          bandNames, bandLongNames, bandMeasureUnits, $
          dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
          /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, $
          trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, versionDate=versionDate, versionNumber=versionNumber

      endif else begin

        call_composite, expectedDays, data_day1, data_tc

        if sensor eq 'AVH09C1' then destTCFile=buildAVHRRFAPARFileName_TC(sensor, resolution, year, month, first[type], missionName, missionCode, mainVarName, startDay=first[type], endDay=last[type])
        if sensor eq 'MODIS' then destTCFile=buildMODISFAPARFileName_TC(sensor, resolution, year, month, first[type], missionName, missionCode, mainVarName, startDay=first[type], endDay=last[type])
        destncfilename=fsObj->addFileExtension(destTCFile, 'NC'); nc
        desthdffilename=fsObj->addFileExtension(destTCFile, 'HDF'); hdf
        print, '*********'
        print, 'building tc file: ', destTCFile
        print, '*********'

        faparTCDSInfo=getStandardFaparTCDataSetInfo_new()
        bandNames=faparTCDSInfo.bandNames
        bandLongNames=faparTCDSInfo.bandLongNames
        bandSlopes=faparTCDSInfo.bandSlopes
        bandMeasureUnits=faparTCDSInfo.bandMeasureUnits
        bandDataTypes=faparTCDSInfo.bandDataTypes
        bandIntercepts=faparTCDSInfo.bandIntercepts
        minMaxs=faparTCDSInfo.minMaxs
        nanList=faparTCDSInfo.nans

        trueSlopes=bandSlopes
        trueIntercepts=bandIntercepts
        versionDate=faparTCDSInfo.versionDate
        versionNumber=faparTCDSInfo.versionNumber

        TYPE1=1
        ZEROISNAN=keyword_set(TYPE1) ;0
        TWO51_TO_TWO55_ISNAN=keyword_set(TYPE2) ;0
        bSInfo=getByteScalingSetting(ZEROISNAN=keyword_set(TYPE1), TWO51_TO_TWO55_ISNAN=keyword_set(TYPE2))
        ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
        ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
        ;byteOutput=dataByteScaling(output.fpar, VALUE_BYTES=[0,250])
        remarkableFlags=bSInfo.remarkableFlags
        DATA_NAN=bSInfo.DATA_NAN
        BYTE_NAN=bSInfo.BYTE_NAN
        BYTE_RANGE=bSInfo.BYTE_RANGE
        res=dataByteScaling(data_tc.fapar, data_tc.flag, FLAG_VALUES=[9,10], $
          DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
          DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
        data_tc.fapar=res.resultData
        trueIntercepts[0]=outIntercept
        trueSlopes[0]=outSlope
        ;output.flag=res.resultFlag

        ;tv, rebin(bytscl(data_tc.sigma), 720, 360) ;*
        res=dataByteScaling(data_tc.sigma, data_tc.flag, $
          DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
          DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
        data_tc.sigma=res.resultData
        trueIntercepts[1]=outIntercept
        trueSlopes[1]=outSlope
        ;data_tc.flag=res.resultFlag

        ;tv, rebin(bytscl(data_tc.dev_temp), 720, 360) ;*
        res=dataByteScaling(data_tc.dev_temp, data_tc.flag, $
          DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
          DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
        data_tc.dev_temp=res.resultData;data_tc.sigma=res.resultData
        trueIntercepts[2]=outIntercept
        trueSlopes[2]=outSlope
        flagTags=strupcase(['fapar', 'sigma', 'dev_temp'])
        tags=tag_names(data_tc)

        idx_1 = where (data_tc.flag eq 1, c1)
        idx_2 = where (data_tc.flag eq 2, c2)
        idx_3 = where (data_tc.flag eq 3, c3)

        waterIdxs=where(data_day[0].flag eq 3, watCount)
        for i=0, n_elements(flagTags)-1 do begin
          thisIdx=(where(flagTags[i] eq tags, count))[0]
          if count eq 1 then begin
            if c1 gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), idx_1, remarkableFlags[0])
            if c2 gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), idx_2, remarkableFlags[1])
            if c3 gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), idx_3, remarkableFlags[2])
            if watCount gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), waterIdxs, remarkableFlags[2])
          endif
        endfor

        ;map -9999 on int data
        flagTags=strupcase(['red', 'nir', 'sigma_red', 'sigma_nir', 'dev_red_temp', 'dev_nir_temp'])
        for i=0, n_elements(flagTags)-1 do begin
          thisIdx=(where(flagTags[i] eq tags, count))[0]
          nanIdxs=where(data_tc.(thisIdx) gt 250, count)
          ;nanIdxs = where (data_tc.flag eq 3, c3)
          if count gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), nanIdxs, INT_NAN)
          if watCount gt 0 then data_tc.(thisIdx)=mapQualityFlags(data_tc.(thisIdx), waterIdxs, INT_NAN)
        endfor

        dataSets=[ptr_new(data_tc.day, /NO_COPY),ptr_new(data_tc.nday, /NO_COPY), $
          ptr_new(data_tc.fapar, /NO_COPY), ptr_new(data_tc.sigma, /NO_COPY), ptr_new(data_tc.dev_temp, /NO_COPY), $
          ptr_new(data_tc.red, /NO_COPY), ptr_new(data_tc.sigma_red, /NO_COPY), ptr_new(data_tc.dev_red_temp, /NO_COPY) ,$
          ptr_new(data_tc.nir, /NO_COPY), ptr_new(data_tc.sigma_nir, /NO_COPY), ptr_new(data_tc.dev_red_temp, /NO_COPY), $
          ptr_new(data_tc.flag, /NO_COPY), $
          ptr_new(data_tc.toc_red, /NO_COPY), ptr_new(data_tc.toc_nir, /NO_COPY), $
          ptr_new(data_tc.nday, /NO_COPY)]

        boundary=[-180.0, 180.0, -90, 90.]

        destncfilename=outputDir+destncfilename
        desthdffilename=outputDir+desthdffilename
        write_georef_ncdf, destncfilename, $
          bandNames, bandLongNames, bandMeasureUnits, $
          dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
          /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, $
          trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, versionDate=versionDate, versionNumber=versionNumber
        write_hdf, desthdffilename, $
          bandNames, bandLongNames, bandMeasureUnits, $
          dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
          /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, $
          trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, versionDate=versionDate, versionNumber=versionNumber

      endelse
    endfor
  endelse
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