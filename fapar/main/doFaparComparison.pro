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
  TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, cloudtype=cloudtype

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  device, decomposed=0
  loadct, 20
  DATA_RANGE=[0.,1.]
  varName='FAPAR'
  nslice=20

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
    if TC_TYPE eq 'MONTHLY' then begin
      ;      ncoutfileInfo=build_JRC_FPA_Diff_TCAlg_Monthly_Product_FileName(instrument, year, month, first[j], timestamp, temporalResolution, location, resolutions[0], product, '', 'NC',$
      ;        indicator='LAN', level, projection=projection)
      ncoutfileInfo=build_JRC_FPA_Diff_TCAlg_Monthly_Product_FileName(instrument, year, month, first[j], timestamp, temporalResolution, location, resolutions[0], product, '', 'CLOUDTYPE_'+strcompress(cloudtype, /REMOVE)+'.NC',$
        indicator='LAN', level, projection=projection)
      hdfoutfileInfo=build_JRC_FPA_Diff_TCAlg_Monthly_Product_FileName(instrument, year, month, first[j], timestamp, temporalResolution, location, resolutions[0], product, '', 'CLOUDTYPE_'+strcompress(cloudtype, /REMOVE)+'.HDF',$
        indicator='LAN', level, projection=projection)
    endif

    ;if TA_TYPE eq 'MEAN' then outputFile=build_JRC_FPA_Diff_AVH_MeanAlg_Monthly_Product_FileName('', resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])

    diffFileName=(strsplit(ncoutfileInfo.fileName, '.', /EXTRACT, /PRESERVE))
    resdiffFileName=''
    for i=0, n_elements(diffFileName)-2 do resdiffFileName=resdiffFileName+'_'+diffFileName[i]
    resdiffFileName=strmid(resdiffFileName, 1, strlen(resdiffFileName)-1)

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

    matrixF=ptrarr(elementsToCompare)
    matrixR=ptrarr(elementsToCompare)
    matrixN=ptrarr(elementsToCompare)
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
        if TC_TYPE eq 'MONTHLY' then inputFileInfo=build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName(sensors[i], year, month, first[j], timestamp, temporalResolution, location, resolutions[0], product, version, 'CLOUDTYPE_'+strcompress(cloudtype, /REMOVE)+'.'+sourceFormats[i],$
          indicator='LAN', level, projection=projection)
        ;if TA_TYPE eq 'MEAN' then inputFileInfo=build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])
        DelIdlVAr, version
        inputFile=inputFileInfo.fileName
        ;checkDir=sourceDirs[i]+inputFileInfo.filePath
        checkDir=sourceDirs[i];+inputFileInfo.filePath
      endif
      if sensors[i] eq 'SWF' then begin
        missionCode=0
        if TC_TYPE eq 'DAILY' then inputFile=buildSWFFAPARFileName_D(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j]); else inputFiles[i]=buildSWFFAPARFileName_TC(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])
        if TC_TYPE eq 'MONTHLY' then inputFile=buildSWFFAPARFileName_M(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], 'L2', startDay=first[j], endDay=last[j]); else inputFiles[i]=buildSWFFAPARFileName_TC(sensors[i], resolutions[i], year, month, first[j], missionNames[i], missionCode, mainVarNames[i], level, startDay=first[j], endDay=last[j])
        inputFile=ST_fileSystem->addFileExtension(inputFile, sourceFormats[i])
        checkDir=sourceDirs[i]
        ;checkDir=['/space4/storage/products/fapar_products/jrc/GLOBAL_PLC_0.05/daily', '/net/netsea2/vol/vol22_h07/aargau5/data/fapar_products/jrc/GLOBAL_PLC_0.05/daily']
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
      offset=7200.*10/nslice
      if sensors[i] eq 'SWF' then begin
        delIdlvar, varName
        delIdlvar, faparRes
        delIdlvar, redRes
        delIdlvar, nirRes
        faparRes=read_SWF_FAPAR(sourceDirs[i], inputFiles[i], FOUND=FOUND, /REVERSE, /APPLY)
        varName='Mean:BRF_Rec_R'
        delidlvar, FOUND
        redRes=read_SWF_data(sourceDirs[i], inputFiles[i], varname, FOUND=FOUND, /REVERSE, /APPLY)
        varName='Mean:BRF_Rec_N'
        delidlvar, FOUND
        nirRes=read_SWF_data(sourceDirs[i], inputFiles[i], varname, FOUND=FOUND, /REVERSE, /APPLY)
      endif
      if sensors[i] eq 'AVH' then begin
        delIdlvar, varName
        delIdlvar, faparRes
        delIdlvar, redRes
        delIdlvar, nirRes
        faparRes=read_AVHRR_FAPAR(sourceDirs[i], inputFiles[i], FOUND=FOUND, varName=varName, /APPLY)
        varName='RECTIFIED_RED';varName='Rectified_BAND_2'
        delidlvar, FOUND
        redRes=read_AVHRR(sourceDirs[i], inputFiles[i], FOUND=FOUND, varName=varName, /APPLY)
        if ~keyword_set(FOUND) then begin
          varName='Rectified_BAND_1'
          redRes=read_AVHRR(sourceDirs[i], inputFiles[i], FOUND=FOUND, varName=varName, /APPLY)
        endif
        delidlvar, FOUND
        varName='RECTIFIED_NIR';varName='Rectified_BAND_1'
        nirRes=read_AVHRR(sourceDirs[i], inputFiles[i], FOUND=FOUND, varName=varName, /APPLY)
        if ~keyword_set(FOUND) then begin
          varName='Rectified_BAND_2'
          nirRes=read_AVHRR(sourceDirs[i], inputFiles[i], FOUND=FOUND, varName=varName, /APPLY)
        endif
        varName='LDTR_FLAG'
        qa=read_AVHRR(sourceDirs[i], inputFiles[i], FOUND=FOUNDQA, varName=varName)
        if keyword_set(FOUNDQA) then begin
          qa=qa.data
          countCloud=0
          if cloudtype le 2 then begin
            checkCloud1=cgi_map_bitwise_flag(fix(qa),1)
            checkCloud2=cgi_map_bitwise_flag(fix(qa),2)
            if cloudtype eq 0 then cloudNaN=where(checkCloud1 eq 1 or checkCloud2 eq 1, countCloud)
            if cloudtype eq 1 then cloudNaN=where(checkCloud1 eq 1, countCloud)
            if cloudtype eq 2 then cloudNaN=where(checkCloud2 eq 1, countCloud)
          endif
          if countCloud gt 0 then begin
            faparRes.fapar[cloudNaN]=!VALUES.F_NAN
            nirRes.data[cloudNaN]=!VALUES.F_NAN
            redRes.data[cloudNaN]=!VALUES.F_NAN
            ;faparData.toc_nir[cloudNaN]=!VALUES.F_NAN
            ;faparData.toc_red[cloudNaN]=!VALUES.F_NAN
          endif
        endif
      endif
      fapardata=faparRes.fapar;[offset,1950:2100]
      nirdata=nirRes.data;[offset,1950:2100]
      reddata=redRes.data;[offset,1950:2100]
      if FOUND then begin
        validIdxsF=where(finite(faparData) eq 1 and faparData gt 0.0 and faparData lt 1.0, countF, COMPLEMENT=setNanF, ncomplement=ncomplementF)
        validIdxsR=where(finite(reddata) eq 1 and reddata gt 0.0 and reddata lt 1.0, countR, COMPLEMENT=setNanR, ncomplement=ncomplementR)
        validIdxsN=where(finite(nirdata) eq 1 and nirdata gt 0.0 and nirdata lt 1.0, countN, COMPLEMENT=setNanN, ncomplement=ncomplementN)
        if ncomplementF gt 0 then faparData[setNanF]=!VALUES.F_NAN
        if ncomplementR gt 0 then redData[setNanR]=!VALUES.F_NAN
        if ncomplementN gt 0 then nirData[setNanN]=!VALUES.F_NAN
        nirdata=nirdata[offset,1950:2100];=1.
        reddata=reddata[offset,1950:2100];=1.
        fapardata=fapardata[offset,1950:2100];=1.
        ;tvscl, congrid(faparData, 720, 360), /NAN
        ;tvscl, congrid(nirData, 720, 360), /NAN
        ;tvscl, congrid(redData, 720, 360), /NAN
        matrixF[i]=ptr_new(faparData, /NO_COPY)
        matrixR[i]=ptr_new(redData, /NO_COPY)
        matrixN[i]=ptr_new(nirData, /NO_COPY)
      endif else begin
        print, 'skip:', diffFileName, 'file corrupted/unavailable'
        skip=1
        break
      endelse
    endfor

    if skip ne 1 then begin
      ;difference=abs(*matrixS[1]-*matrixS[0])
      differenceF=(*matrixF[1]-*matrixF[0])
      differenceN=(*matrixN[1]-*matrixN[0])
      differenceR=(*matrixR[1]-*matrixR[0])

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
      header=faparDiffInfo.header
      scaledminmaxs=faparDiffInfo.scaledminmaxs

      trueSlopes=bandSlopes
      trueIntercepts=bandIntercepts

      fapar1=reform(*(matrixF[0]))
      fapar2=reform(*(matrixF[1]))
      red1=reform(*(matrixR[0]))
      red2=reform(*(matrixR[1]))
      nir1=reform(*(matrixN[0]))
      nir2=reform(*(matrixN[1]))

      negIdxs=where(finite(nir1) ne 1 or finite(red1) ne 1, count, ncompl=ncompl, complement=complement)
      fullNir1=nir1[complement] & fullRed1=red1[complement]
      negIdxs=where(finite(nir2) ne 1 or finite(red2) ne 1, count, ncompl=ncompl, complement=complement)
      fullNir2=nir2[complement] & fullRed2=red2[complement]

      ;; fapar SWF Vs
      negIdxs=where(finite(differenceF) ne 1 or finite(fapar1) ne 1 or finite(fapar2) ne 1, count, ncompl=ncompl, complement=complement)
      xtitle=sensors[0] & ytitle=sensors[1] & title='Fapar Comparison '+yearS+'-'+monthS+'-'+string(first[j], format='(I02)')+'_'+string(last[j], format='(I02)')
      scatplotFileName=plotDir+'scatter_'+'CT'+strcompress(cloudtype, /remove)+'_FPA'+resdiffFileName
      if ncompl gt 1 then plotscat, reform(fapar1[complement], n_elements(complement)), reform(fapar2(complement), n_elements(complement)),  $
        xtitle=xtitle, ytitle=ytitle, title=title, /STAT, $
        filename=scatplotFileName else print, 'skip for too many NaN'
      scatterFapar1=fapar1[complement] & scatterfapar2=fapar2[complement]

      negIdxs=where(finite(differenceN) ne 1 or finite(nir1) ne 1 or finite(nir2) ne 1, count, ncompl=ncompl, complement=complement)
      xtitle=sensors[0] & ytitle=sensors[1] & title='Nir Comparison '+yearS+'-'+monthS+'-'+string(first[j], format='(I02)')+'_'+string(last[j], format='(I02)')
      scatplotFileName=plotDir+'scatter_'+'CT'+strcompress(cloudtype, /remove)+'_NIR'+resdiffFileName
      if ncompl gt 1 then plotscat, reform(nir1[complement], n_elements(complement)), reform(nir2(complement), n_elements(complement)),  $
        xtitle=xtitle, ytitle=ytitle, title=title, /STAT, $
        filename=scatplotFileName else print, 'skip for too many NaN'
      scatternir1=nir1[complement] & scatternir2=nir2[complement]

      negIdxs=where(finite(differenceR) ne 1 or finite(red1) ne 1 or finite(red2) ne 1, count, ncompl=ncompl, complement=complement)
      xtitle=sensors[0] & ytitle=sensors[1] & title='Red Comparison '+yearS+'-'+monthS+'-'+string(first[j], format='(I02)')+'_'+string(last[j], format='(I02)')
      scatplotFileName=plotDir+'scatter_'+'CT'+strcompress(cloudtype, /remove)+'_RED'+resdiffFileName
      if ncompl gt 1 then plotscat, reform(red1[complement], n_elements(complement)), reform(red2(complement), n_elements(complement)),  $
        xtitle=xtitle, ytitle=ytitle, title=title, /STAT, $
        filename=scatplotFileName else print, 'skip for too many NaN'
      scatterred1=red1[complement] & scatterred2=red2[complement]

      negIdxs1=where(finite(fapar1) ne 1, count)
      negIdxs2=where(finite(fapar2) ne 1, count)

      ;      fapar1All[j]=ptr_new(reform(fapar1[complement], /NO_COPY)
      ;      fapar2All[j]=ptr_new(reform(fapar2[complement], /NO_COPY)
      ;      nir1All[j]=ptr_new(reform(nir1[complement], /NO_COPY)
      ;      nir2All[j]=ptr_new(reform(nir2[complement], /NO_COPY)
      ;      red1All[j]=ptr_new(reform(red1[complement], /NO_COPY)
      ;      red2All[j]=ptr_new(reform(red2[complement], /NO_COPY)
      if count gt 0 then begin
        differenceF[negIdxs]=DATA_NAN
        differenceR[negIdxs]=DATA_NAN
        differenceN[negIdxs]=DATA_NAN
      endif

      if count gt 0 then fapar1[negIdxs1]=DATA_NAN
      if count gt 0 then fapar2[negIdxs2]=DATA_NAN

      if n_elements(fullFapar1Data) eq 0 then begin
        fullFNir1=fullNir1
        fullFNir2=fullNir2
        fullFRed1=fullRed1
        fullFRed2=fullRed2
        fullFapar1Data=scatterfapar1
        fullFapar2Data=scatterfapar2
        fullRed1Data=scatterred1
        fullRed2Data=scatterred2
        fullNir1Data=scatternir1
        fullNir2Data=scatternir2
      endif else begin
        fullFNir1=[fullFNir1,fullNir1]
        fullFNir2=[fullFNir2,fullNir2]
        fullFRed1=[fullFRed1,fullRed1]
        fullFRed2=[fullFRed2,fullRed2]
        fullFapar1Data=[scatterfapar1,fullFapar1Data]
        fullFapar2Data=[scatterfapar2,fullFapar2Data]
        fullRed1Data=[scatterred1,fullRed1Data]
        fullRed2Data=[scatterred2,fullRed2Data]
        fullNir1Data=[scatternir1,fullNir1Data]
        fullNir2Data=[scatternir2, fullNir2Data]
      endelse

      if keyword_set(FIRST_LOOK) then begin
        fLookDir='first_look'
        ;cd, dirout
        firstLookDir=outDir+fLookDir
        fInfo=file_info(fLookDir)
        if ~(fInfo.exists) then file_mkdir, firstLookDir
        sampleImg=rebin(differenceF, dims[0]/10,dims[1]/10)
        minvalue=min(differenceF, max=maxvalue)
        sampleImg=bytscl(sampleImg)
        samplefilename='fl_'+new_file+'.gif'
        fullSampleFName=firstLookDir+path_sep()+samplefilename
        LOADCT, 14
        print, 'sampleImage-->', fullSampleFName
        write_gif, fullSampleFName, sampleImg
      endif

      dataSets=[ptr_new(differenceF, /NO_COPY), $
        ptr_new(fapar1, /NO_COPY), $
        ptr_new(fapar2, /NO_COPY)];MASK_avhrr

      boundary=[-180.0, 180.0, -90, 90.]
      filePath=outDir

      date_created=ST_utils->getSysTime(/FILECOMPATIBILITY)
      satellite='NOAA '+strcompress(missionCode, /REMOVE)
      time_Coverage_Start=ST_utils->formatDate([year, month, first[j], 0, 0, 0], template='satellite')
      time_Coverage_End=ST_utils->formatDate([year, month, last[j], 23, 59, 59], template='satellite')
      header.cdr_variable=['cdr_variable', 'FAPAR']
      header.Process=['process', 'JRC FAPAR TOC algorithm - see QA4ECV ATBD']

      bandNames[1]='fapar_'+TC_TYPE+sensors[0]
      bandNames[2]='fapar_'+TC_TYPE+sensors[1]
      bandStandardNames[1]=bandNames[1]
      bandStandardNames[2]=bandNames[2]
      bandLongNames[1]=bandNames[1]
      bandLongNames[2]=bandNames[2]

      ;      write_georef_ncdf, destncfilename, $
      ;        bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
      ;        dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, scaledminmaxs=scaledminmaxs, $
      ;        /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
      ;        id=ncFileInfo.filename, satellite=satellite, header=header, $
      ;        date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End

      if keyword_set(NC) then begin
        print,'Write the results in ',ncoutfilename
        write_georef_ncdf, ncoutfilename, $
          bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
          dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, scaledminmaxs=scaledminmaxs, $
          /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
          id=inputFileInfo.filename, satellite=satellite, header=header, $
          date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End
      endif

      if keyword_set(HDF) then begin
        print,'Write the results in ',hdfoutfilename
        write_hdf, hdfoutfilename, $
          bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
          dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, scaledminmaxs=scaledminmaxs, $
          /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
          id=inputFileInfo.filename, satellite=satellite, header=header, $
          date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End
      endif
    endif

    print, '**', resdiffFileName, '**done**'
    ptr_free, matrixF
    ptr_free, matrixR
    ptr_free, matrixN

  endfor
  idx=where(finite(fullFapar1Data) eq 1 and finite(fullFapar2Data) eq 1, cc)
  scatplotFileName=plotDir+'fullscatter_'+'CT'+strcompress(cloudtype, /remove)+'_fapar'
  scatplotFileNameCol=scatplotFileName+'_col'
  scatplotFileNameBW=scatplotFileName+'_BW'
  ;  fakeDim1=reform(fullFapar1Data[idx], cc)
  ;  fakeDim1=reform([fakeDim1,fakeDim1,fakeDim1,fakeDim1,fakeDim1])
  ;  fakeDim2=reform(fullFapar2Data[idx], cc)
  ;  fakeDim2=reform([fakeDim2,fakeDim2,fakeDim2,fakeDim2,fakeDim2])
  ;min2=min([fullFapar1Data, fullFapar2Data]), max2=max([fullFapar1Data, fullFapar2Data])
  if cc gt 10000 then begin
    plotscat, reform(fullFapar1Data[idx], cc), reform(fullFapar2Data[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title='fapar', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
  endif else begin
    plotscat, reform(fullFapar1Data[idx], cc), reform(fullFapar2Data[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title='fapar', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
    cgPS_Open, scatplotFileNameBW
    cgscatter2d, reform(fullFapar1Data[idx], cc), reform(fullFapar2Data[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title='fapar', /SCAT, /RMSD, /SIMPLEFIT, /COEFFICIENT, /ONEONEFITLINE, $
      XRange=[0., 1.], YRANGE=[0., 1.]
    cgPS_Close
  endelse
  scatplotFileName=plotDir+'fullscatter_'+'CT'+strcompress(cloudtype, /remove)+'_red'
  scatplotFileNameCol=scatplotFileName+'_col'
  scatplotFileNameBW=scatplotFileName+'_BW'
  idx=where(finite(fullRed1Data) eq 1 and finite(fullRed2Data) eq 1, cc)
  if cc gt 10000 then begin
    plotscat, reform(fullRed1Data[idx], cc), reform(fullRed2Data[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title='red', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
  endif else begin
    plotscat, reform(fullRed1Data[idx], cc), reform(fullRed2Data[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title='red', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
    cgPS_Open, scatplotFileNameBW
    cgscatter2d, reform(fullRed1Data[idx], cc), reform(fullRed2Data[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title='red', /SCAT, /RMSD, /SIMPLEFIT, /COEFFICIENT, /ONEONEFITLINE, $
      XRange=[0., 1.], YRANGE=[0., 1.]
    cgPS_Close
  endelse
  scatplotFileName=plotDir+'fullscatter_'+'CT'+strcompress(cloudtype, /remove)+'_nir'
  scatplotFileNameCol=scatplotFileName+'_col'
  scatplotFileNameBW=scatplotFileName+'_BW'
  idx=where(finite(fullNir1Data) eq 1 and finite(fullNir2Data) eq 1, cc)
  if cc gt 10000 then begin
    plotscat, reform(fullNir1Data[idx], cc), reform(fullNir2Data[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title='nir', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
  endif else begin
    plotscat, reform(fullNir1Data[idx], cc), reform(fullNir2Data[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title='nir', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
    cgPS_Open, scatplotFileNameBW
    cgscatter2d, reform(fullNir1Data[idx], cc), reform(fullNir2Data[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title='nir', /SCAT, /RMSD, /SIMPLEFIT, /COEFFICIENT, /ONEONEFITLINE, $
      XRange=[0., 1.], YRANGE=[0., 1.]
    cgPS_Close
  endelse

  scatplotFileName=plotDir+'fullscatter_'+sensors[0]+'_CT'+strcompress(cloudtype, /remove)+'_rednir'
  scatplotFileNameCol=scatplotFileName+'_col'
  scatplotFileNameBW=scatplotFileName+'_BW'
  idx=where(finite(fullFNir1) eq 1 and finite(fullFRed1) eq 1, cc)
  xtitle='Red' & ytitle='Nir'
  if cc gt 10000 then begin
    plotscat, reform(fullFRed1[idx], cc), reform(fullFNir1[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title=sensors[0]+' red Vs nir', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
  endif else begin
    plotscat, reform(fullFRed1[idx], cc), reform(fullFNir1[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title=sensors[0]+' red Vs nir', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
    cgPS_Open, scatplotFileNameBW
    cgscatter2d, reform(fullFRed1[idx], cc), reform(fullFNir1[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title=sensors[0]+' red Vs nir', /SCAT, /RMSD, /SIMPLEFIT, /COEFFICIENT, /ONEONEFITLINE, $
      XRange=[0., 1.], YRANGE=[0., 1.]
    cgPS_Close
  endelse

  scatplotFileName=plotDir+'fullscatter_'+sensors[1]+'_CT'+strcompress(cloudtype, /remove)+'_rednir'
  scatplotFileNameCol=scatplotFileName+'_col'
  scatplotFileNameBW=scatplotFileName+'_BW'
  idx=where(finite(fullFNir2) eq 1 and finite(fullFRed2) eq 1, cc)
  xtitle='Red' & ytitle='Nir'
  if cc gt 10000 then begin
    plotscat, reform(fullFRed2[idx], cc), reform(fullFNir2[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title=sensors[1]+' red Vs nir', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
  endif else begin
    plotscat, reform(fullFRed2[idx], cc), reform(fullFNir2[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title=sensors[1]+' red Vs nir', /STAT, $
      filename=scatplotFileNameCol, bin1=0.01, bin2=0.01, thresh=0
    cgPS_Open, scatplotFileNameBW
    cgscatter2d, reform(fullFRed2[idx], cc), reform(fullFNir2[idx], cc),  $
      xtitle=xtitle, ytitle=ytitle, title=sensors[1]+' red Vs nir', /SCAT, /RMSD, /SIMPLEFIT, /COEFFICIENT, /ONEONEFITLINE, $
      XRange=[0., 1.], YRANGE=[0., 1.]
    cgPS_Close
  endelse

  print, '**done**'

end

