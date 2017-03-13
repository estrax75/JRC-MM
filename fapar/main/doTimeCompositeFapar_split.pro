;FUNCTION doFaparComposite, sensor, resolution, missionName, level, confDir, sourceDir, tempDir, outputDir, year, month, $
;TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE

FUNCTION doTimeCompositeFapar_split, platform, indicator, spatialResolution, inputLevel, missionName, mainVarName, missionCode, year, month, day, $
  sourceDir, outputDir, tempdir, $
  NC=NC, CSV=CSV, HDF=HDF, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, $
  TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, data_dir=data_dir, $
  CLOUDTYPE=CLOUDTYPE, APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, SIGMA_WEIGHTED=SIGMA_WEIGHTED, $
  PIXELS_PROCESS=PIXELS_PROCESS, IGNORE_MISSING_DATA=IGNORE_MISSING_DATA,FIRST_LOOK=FIRST_LOOK, OVERWRITE=OVERWRITE
  ;
  ;
  ;COMMON bigData

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons
  ;print, 'SKIP_OUTLIERS=', keyword_set(SKIP_OUTLIERS)
  print, 'CLOUDTYPE=', keyword_set(CLOUDTYPE)
  print, 'APPLY_HIGH_SIGMA=', keyword_set(APPLY_HIGH_SIGMA)
  print, 'REMOVE_CLOUD=', keyword_set(REMOVE_CLOUD)
  print, 'SIGMA_WEIGHTED=', keyword_set(SIGMA_WEIGHTED)

  outVersion='V_'
  outVersion=outVersion+'CLOUDTYPE_'+strcompress(cloudType, /REMOVE)

  ;  outVersion=outVersion+'_SKIPOUTLIERS_'
  ;  if keyword_set(SKIP_OUTLIERS) then outVersion=outVersion+'YES' else outVersion=outVersion+'NO'

  outVersion=outVersion+'_REMOVECLOUD_'
  if keyword_set(REMOVE_CLOUD) then outVersion=outVersion+'YES' else outVersion=outVersion+'NO'

  outVersion=outVersion+'_APPLYHIGHSIGMA_'
  if keyword_set(APPLY_HIGH_SIGMA) then outVersion=outVersion+'YES' else outVersion=outVersion+'NO'

  outVersion=outVersion+'_SIGMAWEIGHTED_'
  if keyword_set(SIGMA_WEIGHTED) then outVersion=outVersion+'YES' else outVersion=outVersion+'NO'

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

  if n_elements(missionCode) ne 1 then missionCode=getAVHRRNOAANumber(year, undef)
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
      if ~keyword_set(PIXELS_PROCESS) then begin
        sourceInfo=build_JRC_FPA_AVH_Daily_Product_FileName(platform, fix(year), fix(month), fix(firstDay), timestamp, temporalResolution, location, spatialResolution, $
          product, version, 'NC',  indicator=indicator, sourceLevel, projection=projection)
        minSize=10000
      endif else begin
        sourceInfo=build_JRC_FPA_AVH_Daily_Product_FileName(platform, fix(year), fix(month), fix(firstDay), timestamp, temporalResolution, location, spatialResolution, $
          product, version, 'csv',  indicator=indicator, sourceLevel, projection=projection)
        minSize=20
      endelse
      ;if sensor eq 'MODIS' then sourceFileName=buildMODISFileName_D(sensor, resolution, year, month, firstDay, missionName, missionCode, mainVarName, sourceLevel)
      ;hdffilename=ST_fileSystem->addFileExtension(new_file, 'HDF'); hdf

      if keyword_set(data_dir) then begin
        fileDir=data_dir
        ;fileDir='E:\mariomi\Documents\projects\ldtr\data\AVHRR\FP'+path_sep()
      endif else begin
        fileDir=sourceDir+sourceInfo.filePath
        fileDir=ST_fileSystem->adjustDirSep(fileDir, /ADD)
        ;fileDir=fileDir+'v1.5'+path_sep()
        ;fileDir=fileDir+path_sep()
        ; new temp test
      endelse
      thisFileName=fileDir+sourceInfo.fileName

      ;destDir=outputDir+hdfFileInfo.filePath
      ;destDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
      ;resFileNC=destDir+ncFileInfo.fileName
      ;resFileHDF=destDir+hdfFileInfo.fileName

      print, 'searching...',  fileDir+path_sep()+sourceInfo.fileName
      ;ff1 = (file_search(fileDir, sourceInfo.fileName, COUNT=cnt))[0];, /FULL_QUALIFY)
      testInfo= file_info(fileDir+path_sep()+sourceInfo.fileName)
      ff1=testInfo.name
      cnt=0
      if testInfo.size gt minSize and testInfo.read eq 1 then begin
        cnt=1
        faparData=read_AVHRR(fileDir, sourceInfo.fileName, varname='FAPAR', PIXELS_PROCESS=PIXELS_PROCESS, /CSV, origDims=origDims, FOUND=FOUND)
        if ~keyword_set(FOUND) then message, 'corrupted file'+ fileDir + path_sep() + sourceInfo.fileName
        dataDim=size(faparData.(0), /DIM)
      endif
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
      storeFileInfo={fDir:fileDir, fName:sourceInfo.fileName, fid:-1}
      ;faparData=read_AVHRR_FAPAR(fileDir, sourceInfo.fileName, FOUND=FOUND)
      ;faparData.valid=1
      ;histoData=histogram(faparData.fapar)
      ;faparData.fapar=1.*faparData.fapar*faparData.slope_fapar+faparData.offset_fapar

      ;true_single_fapar=faparData.fapar
      ;save, true_single_fapar,fileName=tempDir+sourceInfo+'_SINGLE.sav'
      ;true_single_fapar=0

      ;faparData.nir=1.*faparData.nir*faparData.slope_nir+faparData.offset_nir
      ;faparData.red=1.*faparData.red*faparData.slope_red+faparData.offset_red
      checkFirst=0
    endwhile
    if keyword_set(nodata) then begin
      if keyword_set(IGNORE_MISSING_DATA) then print, 'no data for this time composite!!! (fileDir='+fileDir+' year='+year+' month='+month+')' else message, 'no data for this time composite!!! (fileDir='+fileDir+' year='+year+' month='+month+')'
      return, -1
    endif
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
    storeFileInfos[*].fDir=''
    storeFileInfos[*].fName=''
    storeFileInfos[firstDay]=storeFileInfo
    for d=firstDay+1, last[type] do begin

      ;if sensor eq 'AVH09C1' then sourceFileName=buildAVHRRFAPARFileName_D(sensor, resolution, year, month, d, missionName, missionCode, mainVarName)
      ;if sensor eq 'MODIS' then sourceFileName=buildMODISFAPARFileName_D(sensor, resolution, year, month, d, missionName, missionCode, mainVarName)
      if ~keyword_set(PIXELS_PROCESS) then begin
        sourceInfo=build_JRC_FPA_AVH_Daily_Product_FileName(platform, fix(year), fix(month), fix(d), timestamp, temporalResolution, location, spatialResolution, $
          product, version, 'NC',  indicator=indicator, sourceLevel, projection=projection)
      endif else begin
        sourceInfo=build_JRC_FPA_AVH_Daily_Product_FileName(platform, fix(year), fix(month), fix(d), timestamp, temporalResolution, location, spatialResolution, $
          product, version, 'csv',  indicator=indicator, sourceLevel, projection=projection)
        minSize=20
      endelse
      ;if sensor eq 'MODIS' then sourceFileName=buildMODISFileName_D(sensor, resolution, year, month, firstDay, missionName, missionCode, mainVarName, level)
      ;hdffilename=ST_fileSystem->addFileExtension(new_file, 'HDF'); hdf

      if keyword_set(data_dir) then begin
        fileDir=data_dir
        ;fileDir='E:\mariomi\Documents\projects\ldtr\data\AVHRR\FP'+path_sep()
      endif else begin
        fileDir=sourceDir+sourceInfo.filePath
        fileDir=ST_fileSystem->adjustDirSep(fileDir, /ADD)
        ;fileDir=fileDir+'v1.5'+path_sep()
        fileDir=fileDir+path_sep()
        ; new temp test
      endelse
      thisFileName=fileDir+sourceInfo.fileName

      ;destDir=outputDir+hdfFileInfo.filePath
      ;destDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
      ;resFileNC=destDir+ncFileInfo.fileName
      ;resFileHDF=destDir+hdfFileInfo.fileName

      print, 'searching...',  fileDir, sourceInfo.fileName
      ;ff1 = (file_search(fileDir, sourceInfo.fileName, COUNT=cnt))[0];, /FULL_QUALIFY)
      testInfo= file_info(fileDir+path_sep()+sourceInfo.fileName)
      ff1=testInfo.name
      cnt=0
      if testInfo.size gt minSize and testInfo.read eq 1 then cnt=1
      ;print, file_info(fileDir+path_sep()+sourceInfo.fileName)
      ;ff2 = FILE_SEARCH(dir_in+hdffilename, COUNT=cnt2)
      print, '*********'
      print, 'day-->', d
      print, 'file-->', ff1
      print, '*********'
      ;faparData=readFAPAR(dir_in, sourceInfo, FOUND=FOUND)
      if cnt eq 1 then begin
        print, 'found'
        storeFileInfos[d-first[type]].fDir=fileDir
        storeFileInfos[d-first[type]].fName=sourceInfo.fileName
      endif
      ;faparData=read_AVHRR_FAPAR(fileDir, sourceInfo.fileName, FOUND=FOUND)
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

      tbc_do_mean_tc

    endif
    if TA_TYPE eq 'TC' then begin

      starttime=systime(1)
      satellite='NOA'+strcompress(missionCode, /REMOVE)
      if n_elements(cloudType) eq 0 then cloudType=1
      if platform eq 'AVH09' then begin
        ;  change procedure by TC type (monthly, multiple day...)
        if TC_TYPE eq '5D' or TC_TYPE eq '8D' or TC_TYPE eq '10D' or TC_TYPE eq '16D' then begin
          ;tResolution=strsplit(TC_TYPE, 'D', /EXTRACT, /PRESERVE)
          ;tResolution=STRING(tResolution[0], FORMAT='(i03)')
          ;tResolution=tResolution+'D'
          tResolution=STRING(fix(year), FORMAT='(i04)')
          tResolution=tResolution+STRING(fix(month), FORMAT='(i02)')
          tResolution=tResolution+STRING(last[type], FORMAT='(i02)')
          if ~keyword_set(PIXELS_PROCESS) then begin
            destncFileNameInfo=build_JRC_FPA_AVH_TCAlg_DailyInterval_Product_FileName(platform, fix(year), fix(month), first[type], undef, tResolution, undef, spatialResolution, undef, outVersion, 'NC', undef, indicator='LAN', mission=satellite, BUILD=2)
            desthdfFileNameInfo=build_JRC_FPA_AVH_TCAlg_DailyInterval_Product_FileName(platform, fix(year), fix(month), first[type], undef, tResolution, undef, spatialResolution, undef, outVersion, 'HDF', undef, indicator='LAN', mission=satellite, BUILD=2)
          endif else begin
            destncFileNameInfo=build_JRC_FPA_AVH_TCAlg_DailyInterval_Product_FileName(platform, fix(year), fix(month), first[type], undef, tResolution, undef, spatialResolution, undef, outVersion, 'NC', undef, indicator='LAN', mission=satellite, BUILD=2)
            destcsvFileNameInfo=build_JRC_FPA_AVH_TCAlg_DailyInterval_Product_FileName(platform, fix(year), fix(month), first[type], undef, tResolution, undef, spatialResolution, undef, outVersion, 'csv', undef, indicator='LAN', mission=satellite, BUILD=2)
          endelse
        endif
        if TC_TYPE eq 'MONTHLY' then begin
          if ~keyword_set(PIXELS_PROCESS) then begin
            destncFileNameInfo=build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName(platform, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, outVersion, 'NC', undef, indicator='LAN', mission=satellite, BUILD=2)
            desthdfFileNameInfo=build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName(platform, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, outVersion, 'HDF', undef, indicator='LAN', mission=satellite, BUILD=2)
          endif else begin
            destncFileNameInfo=build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName(platform, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, outVersion, 'NC', undef, indicator='LAN', mission=satellite, BUILD=2)
            destcsvFileNameInfo=build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName(platform, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, outVersion, 'csv', undef, indicator='LAN', mission=satellite, BUILD=2)
          endelse
        endif
        ;        if TC_TYPE eq 'YEARLY' then begin
        ;          destncFileNameInfo=build_JRC_FPA_AVH_TCAlg_Yearly_Product_FileName(platform, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'NC', undef, indicator='LAN')
        ;          desthdfFileNameInfo=build_JRC_FPA_AVH_TCAlg_Yearly_Product_FileName(platform, fix(year), fix(month), first[type], undef, undef, undef, spatialResolution, undef, undef, 'HDF', undef, indicator='LAN')
        ;        endif
      endif
      if n_elements(destncFileNameInfo) eq 0 then message, 'TC_TYPE: ' + TC_TYPE + ' not allowed!!!!'
      ;if sensor eq 'MODIS' then destTCFile=buildMODISFileName_TC(sensor, resolution, year, month, first[type], missionName, missionCode, mainVarName, destLevel, startDay=first[type], endDay=last[type])
      subVer=''
      ;if keyword_set(UNC) then subVer='unc'+path_sep() else subVer=''

      ;outputDir='E:\mariomi\Documents\projects\ldtr\data\AVHRR\FP\'
      if keyword_set(data_dir) then begin
        ;fileDir=data_Dir
        fileDir=ST_fileSystem->adjustDirSep(data_Dir, /ADD)
      endif else begin
        if keyword_set(CSV) then fileDir=sourceDir+destcsvFileNameInfo.filePath else fileDir=sourceDir+destncFileNameInfo.filePath
        fileDir=ST_fileSystem->adjustDirSep(fileDir, /ADD)
      endelse
      ;fileDir=outputDir+destncFileNameInfo.filePath
      ;fileDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)+'test'+subVer+path_sep()
      fileDir=fileDir+subVer
      if ~keyword_set(PIXELS_PROCESS) then begin
        destncfilename=fileDir+destncFileNameInfo.fileName
        desthdffilename=fileDir+desthdfFileNameInfo.fileName
      endif else begin
        destncfilename=fileDir+destncFileNameInfo.fileName
        destcsvfilename=fileDir+destcsvFileNameInfo.fileName
      endelse

      if dataDim[0]*dataDim[1] gt 1e+6 then nslice=10 else nslice=1
      if keyword_set(SIGMA_WEIGHTED) then begin
        sm_call_composite_both, expectedDays, storeFileInfos, data_tc, nslice, dataDim, $
          CLOUDTYPE=CLOUDTYPE, APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, SIGMA_WEIGHTED=SIGMA_WEIGHTED, $
          PIXELS_PROCESS=PIXELS_PROCESS, CSV=keyword_set(PIXELS_PROCESS) ;
      endif else begin
        sm_call_composite_both, expectedDays, storeFileInfos, data_tc, nslice, dataDim, $
          CLOUDTYPE=CLOUDTYPE, APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, SIGMA_WEIGHTED=SIGMA_WEIGHTED, $
          PIXELS_PROCESS=PIXELS_PROCESS, CSV=keyword_set(PIXELS_PROCESS) ;
      endelse
      ;call_composite, expectedDays, data_day1, data_tc
      ;;
      ;      fNames=['both', 'only_cloudy', 'only_shadow_cloud', 'no_mask']
      ;      ;tempDir='E:\mariomi\Documents\projects\ldtr\data\pics\avhrr\'
      ;
      ;      restore, filename='fpa_'+strcompress(cloudtype, /REMOVE)+'.sav'
      ;      device, decomposed=0
      ;      faparcolor
      ;      titles=['mask bit 1 or 2 (cloudy/shadow cloud)', 'mask bit 1 (cloudy)', 'mask bit 2 (shadow cloud)', 'no mask']
      ;      inputFiles=['fpa_'+strcompress(cloudtype, /REMOVE), 'red_'+strcompress(cloudtype, /REMOVE), 'nir_'+strcompress(cloudtype, /REMOVE)]
      ;      yMinMax=[0., 1.]
      ;      for jj=0, n_elements(inputFiles)-1 do begin
      ;        ;window, jj+3, title=inputFiles[jj]+' - '+titles[cloudtype]
      ;        restore, filename=inputFiles[jj]+'.sav'
      ;        device, decomposed=0
      ;        plot, reform(all_day_data[*,0]), yr=yMinMax, min=0.01, psym = 3, max=0.9, title=inputFiles[jj]+' - '+titles[cloudtype]
      ;        nData=float(n_elements(all_day_data[*,0]))
      ;        nday=float(n_elements(all_day_data[0,*]))
      ;        for t=0, nday-1 do begin
      ;          oplot, reform(all_day_data[*,t]), col=fix(float(t)*255/nday), psym = 2, min=0.01
      ;          oplot, reform(meandata)+reform(stddata[t]), col=fix(float(t)*255/nday), min=0.01
      ;          plots, [0., .05], [1.*t/nday,1.*t/nday], /NORM, col=fix(float(t)*255/nday), thick=4.
      ;          xyouts, .05, 1.*t/nday, string(t+1, format='(I02)'), /NORM, col=fix(float(t)*255/nday), charsize=1.2, ALIGN=1.;fix(float(t)*255/nday)
      ;        endfor
      ;        device, decomposed=1
      ;        ;oplot, reform(stdmean), min=0.01, col=0l, thick=2.5
      ;        oplot, reform(meandata), line = 0, min=0.01, thick=2.5, color=255l*255*255
      ;        oplot, reform(meandata)+ reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, max=0.9, linestyle=3
      ;        oplot, reform(meandata)- reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, linestyle=3
      ;        offset=7200.*10/nslice
      ;        if jj eq 0 then begin
      ;          dataToPlot=reform(data_tc.fapar(offset,1950:2100))
      ;          nanToFill=where(finite(dataToPlot) eq 0 or dataToPlot le 0, cnt)
      ;          ;oplot, dataToPlot, min=0.01, col=255l, thick=1.5, psym=6
      ;          for kk=0, cnt-1 do begin
      ;            values=reform(all_day_data[nanToFill[kk],*])
      ;            idx=where(values ne 0, cnt2)
      ;            if cnt2 gt 0 then values=values[idx]
      ;            replaceV=mean(values, /NAN)
      ;            if replaceV ne 0 then dataToPlot[nanToFill[kk]]=replaceV
      ;          endfor
      ;          nanToFill1=where(finite(dataToPlot) eq 0 or dataToPlot le 0, cnt1)
      ;          ;print, cnt1, cnt
      ;          oplot, dataToPlot, min=0.01, col=255l*256*256, thick=1.5, linestyle=0
      ;          oplot, dataToPlot, min=0.01, col=255l*256*256, thick=1.5, psym=4
      ;        endif
      ;        if jj eq 1 then begin
      ;          dataToPlot=reform(data_tc.red(offset,1950:2100))
      ;          nanToFill=where(finite(dataToPlot) eq 0 or dataToPlot le 0, cnt)
      ;          ;oplot, dataToPlot, min=0.01, col=255l, thick=1.5, psym=6
      ;          for kk=0, cnt-1 do begin
      ;            values=reform(all_day_data[nanToFill[kk],*])
      ;            idx=where(values ne 0, cnt2)
      ;            if cnt2 gt 0 then values=values[idx]
      ;            replaceV=mean(values, /NAN)
      ;            if replaceV ne 0 then dataToPlot[nanToFill[kk]]=replaceV
      ;          endfor
      ;          nanToFill1=where(finite(dataToPlot) eq 0 or dataToPlot le 0, cnt1)
      ;          ;print, cnt1, cnt
      ;          oplot, dataToPlot, min=0.01, col=255l*256*256, thick=1.5, linestyle=0
      ;          oplot, dataToPlot, min=0.01, col=255l*256*256, thick=1.5, psym=4
      ;        endif
      ;        if jj eq 2 then begin
      ;          dataToPlot=reform(data_tc.nir(offset,1950:2100))
      ;          nanToFill=where(finite(dataToPlot) eq 0 or dataToPlot le 0, cnt)
      ;          ;oplot, dataToPlot, min=0.01, col=255l, thick=1.5, psym=6
      ;          for kk=0, cnt-1 do begin
      ;            values=reform(all_day_data[nanToFill[kk],*])
      ;            idx=where(values ne 0, cnt2)
      ;            if cnt2 gt 0 then values=values[idx]
      ;            replaceV=mean(values, /NAN)
      ;            if replaceV ne 0 then dataToPlot[nanToFill[kk]]=replaceV
      ;          endfor
      ;          nanToFill1=where(finite(dataToPlot) eq 0 or dataToPlot le 0, cnt1)
      ;          ;print, cnt1, cnt
      ;          oplot, dataToPlot, min=0.01, col=255l*256*256, thick=1.5, linestyle=0
      ;          oplot, dataToPlot, min=0.01, col=255l*256*256, thick=1.5, psym=4
      ;        endif
      ;
      ;        plotimg=tvrd(true=1)
      ;        fName=tempDir+inputFiles[jj]+'_'+fNames[cloudtype]+'_'+destncFileNameInfo.fileName+'.png'
      ;        write_png,fName,plotimg
      ;      endfor
      ;;;;

      ;data_tc.fapar(offset,1950:2100)=1.
      endTime=systime(1)-starttime
      print, 'computed in about:', strcompress(endTime), 'seconds'

      ;destncfilename=ST_fileSystem->addFileExtension(destTCFile, 'NC'); nc
      ;desthdffilename=ST_fileSystem->addFileExtension(destTCFile, 'HDF'); hdf

      faparTCDSInfo=getStandardFaparTCDataSetInfo()
      ;faparTCDSInfo=getStandardFaparTCDataSetInfo_test()
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

      if ~keyword_set(PIXELS_PROCESS) then begin
        print, '*********'
        print, 'building tc file: ', desthdffilename
        print, '*********'

        trueSlopes=bandSlopes
        trueIntercepts=bandIntercepts
        header=faparTCDSInfo.header

        validIdxs=where(data_tc.jrc_flag eq 0 or data_tc.jrc_flag eq 4 or data_tc.jrc_flag eq 5, watCount, compl=notValidIdxs, ncompl=notValidCount)
        realFapar=data_tc.fapar

        res=dataByteScaling(data_tc.fapar, data_tc.jrc_flag, $
          DATA_NAN=!VALUES.F_NAN, BYTE_NAN=nanList[2], $
          DATA_RANGE=minMaxs[2,*], BYTE_RANGE=scaledminmaxs[2,*], outSlope, outIntercept)
        data_tc.fapar=res.resultData
        if notValidCount gt 0 then data_tc.fapar[notValidIdxs]=nanList[2]
        ; overwrite slope & intercept for ONLY fapar-related variable (fapar)
        trueIntercepts[2]=outIntercept
        trueSlopes[2]=outSlope

        res=dataByteScaling(data_tc.sigma, data_tc.jrc_flag, $
          DATA_NAN=!VALUES.F_NAN, BYTE_NAN=nanList[3], $
          DATA_RANGE=minMaxs[3,*], BYTE_RANGE=scaledminmaxs[3,*], outSlope, outIntercept)
        data_tc.sigma=res.resultData
        if notValidCount gt 0 then data_tc.sigma[notValidIdxs]=nanList[3]
        ; overwrite slope & intercept for ONLY fapar-related variable (sigma)
        trueIntercepts[3]=outIntercept
        trueSlopes[3]=outSlope

        res=dataByteScaling(data_tc.dev_temp, data_tc.jrc_flag, $
          DATA_NAN=!VALUES.F_NAN, BYTE_NAN=nanList[4], $
          DATA_RANGE=minMaxs[4,*], BYTE_RANGE=scaledminmaxs[4,*], outSlope, outIntercept)
        data_tc.dev_temp=res.resultData;data_tc.sigma=res.resultData
        if notValidCount gt 0 then data_tc.dev_temp[notValidIdxs]=nanList[4]
        ; overwrite slope & intercept for ONLY fapar-related variable (dev_temp)
        trueIntercepts[4]=outIntercept
        trueSlopes[4]=outSlope

        ;filling Nan
        nanIdx=where(finite(data_tc.red) eq 0, cnt)
        if cnt ne 0 then begin
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
          data_tc.phi[nanIdx]=INT_NAN
        endif

        badIndexes=where(data_tc.dev_red_temp lt 0, count)
        if count gt 0 then data_tc.dev_red_temp[badIndexes]=INT_NAN
        badIndexes=where(data_tc.dev_nir_temp lt 0, count)
        if count gt 0 then data_tc.dev_nir_temp[badIndexes]=INT_NAN

        dataSets_test=[ptr_new(data_tc.day, /NO_COPY),ptr_new(data_tc.nday, /NO_COPY), $
          ptr_new(data_tc.fapar, /NO_COPY), ptr_new(data_tc.dev_temp, /NO_COPY), ptr_new(data_tc.sigma, /NO_COPY), $
          ptr_new(data_tc.red, /NO_COPY), ptr_new(data_tc.dev_red_temp, /NO_COPY), ptr_new(data_tc.sigma_red, /NO_COPY) ,$
          ptr_new(data_tc.nir, /NO_COPY), ptr_new(data_tc.dev_nir_temp, /NO_COPY), ptr_new(data_tc.sigma_nir, /NO_COPY), $
          ptr_new(data_tc.jrc_flag, /NO_COPY), ptr_new(data_tc.ltdr_flag, /NO_COPY), $
          ptr_new(data_tc.toc_red, /NO_COPY),ptr_new(data_tc.toc_nir, /NO_COPY), $
          ptr_new(data_tc.phi, /NO_COPY),ptr_new(data_tc.ts, /NO_COPY), ptr_new(data_tc.tv, /NO_COPY)]

        boundary=[-180.0, 180.0, -90, 90.]

        date_created=ST_utils->getSysTime(/FILECOMPATIBILITY)
        satellite='NOAA '+strcompress(missionCode, /REMOVE);+'Vs '+'SWF'
        time_Coverage_Start=ST_utils->formatDate([year, month, first[type], 0, 0, 0], template='satellite')
        time_Coverage_End=ST_utils->formatDate([year, month, last[type], 23, 59, 59], template='satellite')
        header.cdr_variable=['cdr_variable', 'FAPAR_TC']
        header.process=['process', 'Time Composite']
        header.title=['title', 'Time Composite on FAPAR']

        write_georef_ncdf, /postcompression, destncfilename, $
          bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
          dataSets_test, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
          /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, scaledminmaxs=scaledminmaxs, $
          trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
          header=header, id=destncFileNameInfo.filename, satellite=satellite, $
          date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End, /THUMB
        write_hdf, desthdffilename, /postcompression, $
          bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
          dataSets_test, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
          trueMinMaxs=minMaxs, nanList=nanList, scaledminmaxs=scaledminmaxs, $
          trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
          header=header, id=desthdfFileNameInfo.filename, satellite=satellite, $
          date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End
      endif else begin
        outPar=n_elements(bandNames)
        i=0
        nanIdx=where(finite(data_tc.red) eq 0, cnt)
        if cnt ne 0 then begin
          data_tc.red[nanIdx]=!VALUES.F_NAN
          data_tc.dev_temp[nanIdx]=!VALUES.F_NAN
          data_tc.dev_red_temp[nanIdx]=!VALUES.F_NAN
          data_tc.sigma_red[nanIdx]=!VALUES.F_NAN
          data_tc.nir[nanIdx]=!VALUES.F_NAN
          data_tc.dev_nir_temp[nanIdx]=!VALUES.F_NAN
          data_tc.sigma_nir[nanIdx]=!VALUES.F_NAN
          data_tc.toc_red[nanIdx]=!VALUES.F_NAN
          data_tc.toc_nir[nanIdx]=!VALUES.F_NAN
          data_tc.ts[nanIdx]=!VALUES.F_NAN
          data_tc.tv[nanIdx]=!VALUES.F_NAN
          data_tc.phi[nanIdx]=!VALUES.F_NAN
        endif

        resMatrix=strarr(outPar, n_elements(data_tc.day))
        i=0
        if (finite(data_tc.fapar))[0] ne 1 then stop
        resMatrix[i,*]=string(reform(data_tc.day), format='(I3)') & i++
        resMatrix[i,*]=string(reform(data_tc.nday), format='(I3)') & i++
        resMatrix[i,*]=string(reform(data_tc.fapar), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.dev_temp), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.sigma), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.red), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.dev_red_temp), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.sigma_red), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.nir), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.dev_nir_temp), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.sigma_nir), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.jrc_flag), format='(I3)') & i++
        resMatrix[i,*]=string(reform(data_tc.ltdr_flag), format='(I7)') & i++
        resMatrix[i,*]=string(reform(data_tc.toc_red), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.toc_nir), format='(F8.6)') & i++
        resMatrix[i,*]=string(reform(data_tc.phi), format='(F7.3)') & i++
        resMatrix[i,*]=string(reform(data_tc.ts), format='(F7.3)') & i++
        resMatrix[i,*]=string(reform(data_tc.tv), format='(F7.3)') & i++
        ;for k=0, origDims[0]-1 do resMatrix[i:i+extraDim[0]-1,k]=extradata[*,k+1]
        write_csv, destcsvfilename, resMatrix[*,0:(origDims-1)>1], header=bandNames
        if origDims eq 1 then ST_fileSystem->workAroundCSVFile, destcsvfilename
        print, 'pixel process: done'
        print, 'pixel process: done'
      endelse

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