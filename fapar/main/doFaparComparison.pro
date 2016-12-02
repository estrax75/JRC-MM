;function makeitglob_new, sensor, noaanumber, year, month, day
;@../../Library/library/jrc_core/mapQualityFlags
;@../../Library/library/jrc_core/mapQualityFlags
;
;
;
;
;instrument='AVH'
;indicator='LAN'
;spatialResolution='0005D'
;level='L2'
;missionName='N'

function doFaparComparison, confDir, sensors, sourceDirs, mainVarNames, sourceFormats, $
  missionNames, missionCodes, resolutions, level, $
  tempDir, outputBaseDir, plotDir, $
  year, month, HDF=HDF, NC=NC, $
  OVERWRITE=OVERWRITE, NODIRBUILD=NODIRBUILD, $
  TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  DATA_RANGE=[0.,1.]
  varName='FAPAR'

  ;NaN=-9999 ;!VALUES.F_NAN
  tInfo=getTimeDerivedInfo(year, month, TA_TYPE, TC_TYPE, xticks_c=xticks_c, xtickname_c=xtickname_c)
  first=tInfo.first
  last=tInfo.last
  extraPath=tInfo.extraPath
  level=tInfo.level

  ;outDir=outputBaseDir+tInfo.extraPath+path_sep()+'HM'+path_sep()
  outDir=outputBaseDir

  plotDir=ST_fileSystem->adjustDirSep(plotDir, /ADD)
  outDir=ST_fileSystem->adjustDirSep(outDir, /ADD)
  tempDir=ST_fileSystem->adjustDirSep(tempDir, /ADD)

  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  elementsToCompare=n_elements(sensors)
  inputFiles=strarr(elementsToCompare)
  fullFileNames=strarr(elementsToCompare)

  ; Monthly AVHRR

  for j=0, n_elements(first)-1 do begin
    if TC_TYPE eq 'DAILY' then begin
      ncoutfileInfo=build_JRC_FPA_Diff_Daily_Product_FileName(instrument, year, month, first[j], timestamp, temporalResolution, location, resolutions[0], product, '', 'NC',$
        indicator='LAN', level, projection=projection);, resolutions[i]
      hdfoutfileInfo=build_JRC_FPA_Diff_Daily_Product_FileName(instrument, year, month, first[j], timestamp, temporalResolution, location, resolutions[0], product, '', 'NC',$
        indicator='LAN', level, projection=projection);, resolutions[i]
    endif
    if TC_TYPE eq 'TC' then outputFile=build_JRC_FPA_Diff_TCAlg_Monthly_Product_FileName('', resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])
    if TA_TYPE eq 'MEAN' then outputFile=build_JRC_FPA_Diff_AVH_MeanAlg_Monthly_Product_FileName('', resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])

    diffFileName=(strsplit(ncoutfileInfo.fileName, '.', /EXTRACT, /PRESERVE))[0]
    baseDir=ST_fileSystem->adjustDirSep(outputBaseDir, /ADD)

    fileDir=baseDir+ncoutfileInfo.filePath
    fileDir=ST_fileSystem->adjustDirSep(fileDir, /ADD)
    ncoutfilename=fileDir+ncoutfileInfo.fileName

    fileDir=baseDir+hdfoutfileInfo.filePath
    fileDir=ST_fileSystem->adjustDirSep(fileDir, /ADD)
    hdfoutfilename=fileDir+hdfoutfileInfo.fileName

    checkNC=file_info(ncoutfilename)
    checkHDF=file_info(hdfoutfilename)

    if keyword_set(NC) and ((checkNC[0].size eq 0) or keyword_set(OVERWRITE)) then NOWRITENC=0 else NOWRITENC=1
    if keyword_set(HDF) and ((checkHDF[0].size eq 0) or keyword_set(OVERWRITE)) then NOWRITEHDF=0 else NOWRITEHDF=1

    if (keyword_set(NOWRITEHDF) and keyword_set(NOWRITENC)) then continue

    matrixS=ptrarr(elementsToCompare)
    skip=0
    for i=0, elementsToCompare-1 do begin
      sourceDirs[i]=ST_fileSystem->adjustDirSep(sourceDirs[i], /ADD)
      if sensors[i] eq 'AVH' then begin
        missionCode=(getAVHRRNOAANumber(year, undef))[0]
        DelIdlVAr, version
        ;if TC_TYPE eq 'DAILY' then inputFile=buildAVHRRFAPARFileName_D(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j]) else inputFiles[i]=buildAVHRRFAPARFileName_TC(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j], VERSION='01')
        if TC_TYPE eq 'DAILY' then inputFileInfo=build_JRC_FPA_AVH_Daily_Product_FileName(sensors[i], year, month, first[j], timestamp, temporalResolution, location, resolutions[0], product, version, sourceFormats[i],$
          indicator='LAN', level, projection=projection)
        ;ncFileInfo=build_JRC_FPA_AVH_Daily_Product_FileName(sensors[i], year, month, day, timestamp, temporalResolution, location, spatialResolution, $
        ;  product, version, 'NC',  indicator=indicator, level, projection=projection)
        if TC_TYPE eq 'TC' then inputFileInfo=build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])
        if TA_TYPE eq 'MEAN' then inputFileInfo=build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])
        DelIdlVAr, version
        inputFile=inputFileInfo.fileName
        checkDir=sourceDirs[i]+inputFileInfo.filePath  
      endif
      if sensors[i] eq 'SWF' then begin
        missionCode=0
        if TC_TYPE eq 'DAILY' then inputFile=buildSWFFAPARFileName_D(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j]); else inputFiles[i]=buildSWFFAPARFileName_TC(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])
        if TC_TYPE eq 'MONTHLY' then inputFile=buildSWFFAPARFileName_M(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j]); else inputFiles[i]=buildSWFFAPARFileName_TC(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])
        inputFile=ST_fileSystem->addFileExtension(inputFile, sourceFormats[i])
        checkDir=['/space4/storage/products/fapar_products/jrc/GLOBAL_PLC_0.05/daily', '/net/netsea2/vol/vol22_h07/aargau5/data/fapar_products/jrc/GLOBAL_PLC_0.05/daily']  
      endif
      ;SWF /net/netsea2/vol/vol22_h07/aargau5/data/fapar_products/jrc/GLOBAL_PLC_0.05/daily
      ;SWF /space4/storage/products/fapar_products/jrc/GLOBAL_PLC_0.05/daily

      ;fullFileNames[i]=sourceDirs[i]+inputFile
      ; force swf search
      
      ;check=file_info(fullFileNames[i])
      for k=0, n_elements(checkDir)-1 do begin
        findFile=(file_search(checkDir[k], inputFile, COUNT=check))[0]
        if check gt 1 then begin
          check=1
          fullFileN=findFile
          break
        endif
      endfor

      if check ne 1 then begin
            print, 'skip:', inputFile, 'file does''nt exist'
            skip=1
            break
      endif
      inputFiles[i]=ST_fileSystem->getFileNameInfo(findFile, filePath=filePath, extension=extension)
      sourceDirs[i]=filePath
      FOUND=0
      print, 'reading: ...'+sourceDirs[i]+inputFiles[i]
      if sensors[i] eq 'SWF' then res=read_SWF_FAPAR(sourceDirs[i], inputFiles[i], FOUND=FOUND, /REVERSE, /APPLY)
      if sensors[i] eq 'AVH' then res=read_AVHRR_FAPAR(sourceDirs[i], inputFiles[i], FOUND=FOUND, varName=varName, /APPLY)
      if FOUND then begin
        validIdxs=where(res.fapar ge 0.0 and res.fapar le 1.0, count, COMPLEMENT=setNan, ncomplement=ncomplement)
        if ncomplement gt 0 then res.fapar[setNan]=!VALUES.F_NAN
        matrixS[i]=ptr_new(res.fapar, /NO_COPY)
      endif else begin
        print, 'skip:', diffFileName, 'file corrupted'
        skip=1
        break
      endelse
    endfor
    ;
    ; 0      Band 1 BRF corrected;
    ;        1 -- yes
    ;        0 -- no
    ; 5      Band 2 BRF corrected;
    ;        1 -- yes
    ;        0 -- no
    ;
    ;
    ;=======================================================================================

    ;    rrq1=cgi_map_bitwise_flag(qa_avhrr,0)
    ;    rrq2=cgi_map_bitwise_flag(qa_avhrr,5)
    ;    ;
    ;    idx_nocorr=where (rrq1 eq 0 or rrq2 eq 0)
    ;window,0, xsize=720*2, ysize=360*2
    ;tvscl, reverse(congrid(rrq1, 720*2, 360*2), 2)

    ;=====================================================================================
    ;
    ;
    ;1      Pixel is cloudy;
    ;       1 -- yes
    ;        0 -- no

    ;rr1=cgi_map_bitwise_flag(qc_avhrr,1)
    ;    rr1=cgi_map_bitwise_flag(qa_avhrr,1)
    ;
    ;
    ; 2      Pixel contains cloud shadow;
    ;        1 -- yes
    ;        0 -- no
    ;
    ;rr2=cgi_map_bitwise_flag(qc_avhrr,2)
    ;    rr2=cgi_map_bitwise_flag(qa_avhrr,2)
    ;
    ;  9 channel 2 value is invalid 1 = yes, 0 = no
    ;  8 Channel 1 value is invalid 1 = yes, 0 = no
    ;
    ;rr21=cgi_map_bitwise_flag(qc_avhrr,9)
    ;rr22=cgi_map_bitwise_flag(qc_avhrr,8)
    ;    rr21=cgi_map_bitwise_flag(qa_avhrr,9)
    ;    rr22=cgi_map_bitwise_flag(qa_avhrr,8)
    ;    ;
    ;    ;
    ;    idxbad=where(rr21 eq 1 or rr22 eq 1)
    ;    ;
    ;    ; 3      Pixel is over water;
    ;    ;         1 -- yes
    ;    ;         0 -- no
    ;    ;rr3=cgi_map_bitwise_flag(qc_avhrr,3)
    ;    rr3=cgi_map_bitwise_flag(qa_avhrr,3)
    ;
    ;
    ;
    ;
    ;    idx_mask = where(rr1 eq 1) ; or rr2 eq 1)    ;----> cloud
    ;    idx_mask2 = where(rr21 eq 1 or rr22 eq 1)    ;----> invalid
    ;    IDX_SEA=  where(rr3 eq 1)
    ;
    ;    MASK_AVHRR(IDX_SEA)= 3

    if skip ne 1 then begin
      ;difference=abs(*matrixS[1]-*matrixS[0])
      difference=(*matrixS[1]-*matrixS[0])

      ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
      ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
      ;byteOutput=dataByteScaling(output.fpar, VALUE_BYTES=[0,250])
      remarkableFlags=bSInfo.remarkableFlags
      DATA_NAN=bSInfo.DATA_NAN
      BYTE_NAN=bSInfo.BYTE_NAN
      BYTE_RANGE=bSInfo.BYTE_RANGE

      DATA_NAN=2^15
      faparDiffInfo=getStandardDiffDataSetInfo()

      bandNames=faparDiffInfo.bandNames
      bandLongNames=faparDiffInfo.bandLongNames
      bandStandardNames=faparDiffInfo.bandStandardNames
      bandSlopes=faparDiffInfo.bandSlopes
      bandMeasureUnits=faparDiffInfo.bandMeasureUnits
      bandDataTypes=faparDiffInfo.bandDataTypes
      bandIntercepts=faparDiffInfo.bandIntercepts
      minMaxs=faparDiffInfo.minMaxs
      nanList=faparDiffInfo.nans

      trueSlopes=bandSlopes
      trueIntercepts=bandIntercepts
      header=faparDiffInfo.header

      fapar1=*(matrixS[0])
      fapar2=*(matrixS[1])
      negIdxs=where(finite(difference) ne 1 or finite(fapar1) ne 1 or finite(fapar2) ne 1, count, ncompl=ncompl, complement=complement)
      xtitle='SWF' & ytitle='AVHRR' & title='Fapar Comparison '+yearS+'-'+monthS+'-'+string(first[j], format='(I02)')+'_'+string(last[j], format='(I02)')
      ; without removing Nan
      scatplotFileName=plotDir+'scatter_with_nan_'+diffFileName
      print, 'scatplotFileName (nan):', scatplotFileName
      plotscat, reform(fapar1, n_elements(fapar1)), reform(fapar2, n_elements(fapar2)),  $
        xtitle=xtitle, ytitle=ytitle, title=title, /STAT, $
        filename=scatplotFileName
      ; removing wherever we have Nan
      scatplotFileName=plotDir+'scatter_without_nan_'+diffFileName
      plotscat, reform(fapar1[complement], n_elements(complement)), reform(fapar2(complement), n_elements(complement)),  $
        xtitle=xtitle, ytitle=ytitle, title=title, /STAT, $
        filename=scatplotFileName
      negIdxs1=where(finite(fapar1) ne 1, count)
      negIdxs2=where(finite(fapar2) ne 1, count)

      ;      res=dataByteScaling(difference, flag, $
      ;        DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      ;        DATA_RANGE=[-1,+1], BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
      ;difference=res.resultData
      ;trueIntercepts[0]=outIntercept
      ;trueSlopes[0]=outSlope
      ;if count gt 0 then difference[negIdxs]=BYTE_NAN
      if count gt 0 then difference[negIdxs]=DATA_NAN

      ;negIdxs=where(finite(*(matrixS[0])) ne 1, count)
      ;      res=dataByteScaling(*(matrixS[0]), flag, $
      ;        DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      ;        DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
      ;      fapar1=res.resultData
      ;      trueIntercepts[1]=outIntercept
      ;      trueSlopes[1]=outSlope
      ;if count gt 0 then fapar1[negIdxs]=BYTE_NAN
      if count gt 0 then fapar1[negIdxs1]=DATA_NAN

      ;negIdxs=where(finite(*(matrixS[0])) ne 1, count)
      ;      res=dataByteScaling(*(matrixS[1]), flag, $
      ;        DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      ;        DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
      ;      fapar2=res.resultData
      ;      trueIntercepts[2]=outIntercept
      ;      trueSlopes[2]=outSlope
      ;if count gt 0 then fapar2[negIdxs]=BYTE_NAN
      if count gt 0 then fapar2[negIdxs2]=DATA_NAN

      ;    flagTags=strupcase(['fpar', 'sigma'])
      ;    tags=tag_names(output)
      ;
      ;    for i=0, n_elements(flagTags)-1 do begin
      ;      thisIdx=(where(flagTags[i] eq tags, count))[0]
      ;      if count eq 1 then begin
      ;        output.(thisIdx)=mapQualityFlags(output.(thisIdx), idx_1, remarkableFlags[0])
      ;        output.(thisIdx)=mapQualityFlags(output.(thisIdx), idx_2, remarkableFlags[1])
      ;        output.(thisIdx)=mapQualityFlags(output.(thisIdx), idx_3, remarkableFlags[2])
      ;      endif
      ;    endfor

      ;
      ;
      ;
      ;
      ;idx_4 = where (flag_angles eq 1)
      ;idx_5 = where (flag_angles eq 2)
      ;
      ;
      ; create output file
      ;
      ;
      if keyword_set(FIRST_LOOK) then begin
        fLookDir='first_look'
        ;cd, dirout
        firstLookDir=outDir+fLookDir
        fInfo=file_info(fLookDir)
        if ~(fInfo.exists) then file_mkdir, firstLookDir
        sampleImg=rebin(difference, dims[0]/10,dims[1]/10)
        minvalue=min(difference, max=maxvalue)
        sampleImg=bytscl(sampleImg)
        samplefilename='fl_'+new_file+'.gif'
        fullSampleFName=firstLookDir+path_sep()+samplefilename
        LOADCT, 14
        print, 'sampleImage-->', fullSampleFName
        write_gif, fullSampleFName, sampleImg
      endif

      dataSets=[ptr_new(difference, /NO_COPY), $
        ptr_new(fapar1, /NO_COPY), $
        ptr_new(fapar2, /NO_COPY)];MASK_avhrr

      boundary=[-180.0, 180.0, -90, 90.]
      filePath=outDir

      bandNames[1]='fapar_'+TC_TYPE+sensors[0]
      bandNames[2]='fapar_'+TC_TYPE+sensors[1]
      bandStandardNames[1]=bandNames[1]
      bandStandardNames[2]=bandNames[2]
      bandLongNames[1]=bandNames[1]
      bandLongNames[2]=bandNames[2]

      if keyword_set(NC) then begin
        print,'Write the results in ',ncoutfilename
        write_georef_ncdf, ncoutfilename, $
          bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
          dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
          /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
          header=header
      endif

      if keyword_set(HDF) then begin
        print,'Write the results in ',hdfoutfilename
        write_hdf, hdfoutfilename, $
          bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
          dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
          trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
          header=header
      endif
    endif
    print, '**', diffFileName, '**done**'
    ptr_free, matrixS

  endfor

  print, '**done**'

end

