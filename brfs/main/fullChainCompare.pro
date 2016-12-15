;fullChainCompare, 3000, 1999, 6
pro fullChainCompare, latPixelValue, year, month, TOC=TOC, SWF=SWF, BRF=BRF, RESET=RESET, cloudtype=cloudtype;, sourceFile, confDir, year, month, day, sensor, missionCode, noaaCode, resolution, mainVar, outputBaseDir, tempDir, $
  ;testFile=testFile, OVERWRITE=OVERWRITE, HDF=HDF, NC=NC

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem
  COMMON winInfo, winIndex

  declareSingleTons
  if n_elements(winIndex) eq 0 then winIndex=0
  if n_elements(cloudtype) ne 1 then cloudtype=0

  ; close all windows...
  if keyword_set(RESET) then cgCleanup
  ;Catch, theError
  ;IF theError NE 0 THEN BEGIN
  ;  Catch, /CANCEL
  ;  print, 'fail to create results for dd/mm/yyyy', day, month, year
  ;  RETURN, -1
  ;ENDIF

  ;fname='/space3/storage/products/results/FAPAR/SWF_TO_CMP/SEA01_ORB01_20031221000000_20031231000000_L3_FPA_000001_900S900N1800W1800E_PLC_0500D_PRO.NC'
  ;ST_operator->readHdfFullInfoData, infoVar1b[1], infoVar1b[0], red_avhL1, red_slope, red_offset, red_fillvalue, ERROR=ERROR

  ;NaN=-9999 ;!VALUES.F_NAN
  ;stop
  ;INT_NAN=2^15

  ;outputBaseDir=ST_fileSystem->adjustDirSep(outputBaseDir, /ADD)
  ;confDir=ST_fileSystem->adjustDirSep(confDir, /ADD)
  ;tempDir=ST_fileSystem->adjustDirSep(tempDir, /ADD)

  ;sourceFile='E:/mariomi/Documents/Downloads/patmosx_v05r03_NOAA-L2_asc_d1988L223_c20140306.nc'
  ;fname='patmosx_v05r03_NOAA-L2_asc_d1988L223_c20140306.nc'
  workDir='C:\data\input\'
  workDir=ST_fileSystem->adjustDirSep(workDir, /ADD)
  tempDir='C:\data\comparisons'

  AVH02C1dir=workDir+'AVH02C1'+path_sep()
  AVH09C1dir=workDir+'AVH09C1'+path_sep()
  BRDFdir=workDir+'BRDF'+path_sep()
  AVHFPAdir=workDir+'FPA'+path_sep()
  SWFFPAdir=workDir+'SWF'+path_sep()
  xoffset=7200.*10/20
  ystartPos=1950
  yendPos=ystartPos+150

  monthDays=ST_utils->calcDayOfMonth([year,month,1,0])
  all_day_dataFPA=fltarr(yendPos-ystartPos+1, monthDays)
  all_day_dataRed=all_day_dataFPA
  all_day_dataNir=all_day_dataFPA
  one=byte(all_day_dataNir)
  sumFPA=fltarr(yendPos-ystartPos+1)
  countFPA=sumFPA
  sumRed=sumFPA
  countRed=sumFPA
  sumNir=sumFPA
  countNir=sumFPA
  one[*]=1b

  startDayString=string(year, format='(I4)')+string(month, format='(I02)')+string(1, format='(I02)')+'000000'
  endDayString=string(year, format='(I4)')+string(month, format='(I02)')+string(monthDays, format='(I02)')
  thisSWFfaparFileM=file_search(SWFFPAdir+string(year, format='(I4)')+path_sep()+string(month, format='(I02)')+path_sep()+'SEA01_ORB01_'+startDayString+'_'+endDayString+'*.HDF', count=countFPAFile)

  sourceFileName=ST_fileSystem->getFileNameInfo(thisSWFfaparFileM, filePath=filePath, extension=extension)
  ;if countFPAFile gt 0 then thisSWFfaparFile=ST_fileSystem->getFileNameInfo(thisSWFfaparFile[0], filePath=SWFFPAdir, extension=extension) else continue

  ; read fapar, nir, red and plot...

  ;  res=read_SWF_Data(filePath, sourceFileName, 'Standard_Deviation:sd_SeaVI', FOUND=FOUND, /REVERSE, /APPLY)
  ;  monthswfstddev=res.data

  if keyword_set(TOC) or keyword_set(BRF) then begin
    if keyword_set(TOC) then begin
      restore, filename='tocdata_199906_CT'+strcompress(cloudType, /REMOVE)+'.sav'
      all_day_dataFpa=TOCRed
      all_day_dataRed=TOCRed
      all_day_dataNir=TOCNir
      typeflag='toc_avhrr_CT'+strcompress(cloudType, /REMOVE)
    endif
    if keyword_set(BRF) then begin
      restore, filename='brfdata_199906_CT'+strcompress(cloudType, /REMOVE)+'.sav'
      typeflag='rect_brf_avhrr_CT'+strcompress(cloudType, /REMOVE)
      all_day_dataFpa=Red
      all_day_dataRed=Red
      all_day_dataNir=Nir
    endif
    nData=float(n_elements(all_day_dataFpa[*,0]))
    nday=float(n_elements(all_day_dataFpa[0,*]))
    one=byte(all_day_dataFpa)
    one[*]=1b
    fpasub=reform(all_day_dataFpa[*,0])

    for i=0, nday-1 do begin

      fpasub=reform(all_day_dataFpa[*,i])
      idx=where(finite(fpasub) eq 1 and fpasub gt 0 and fpasub lt 1)
      sumFPA[idx]=sumFPA[idx]+fpasub[idx]
      countFPA[idx]=countFPA[idx]+one[idx]

      Redsub=reform(all_day_dataRed[*,i])
      idx=where(finite(redsub) eq 1 and redsub gt 0 and redsub lt 1)
      sumRed[idx]=sumRed[idx]+RedSub[idx]
      countRed[idx]=countRed[idx]+one[idx]

      nirsub=reform(all_day_dataNir[*,i])
      idx=where(finite(nirsub) eq 1 and nirsub gt 0 and nirsub lt 1)
      sumNir[idx]=sumNir[idx]+Nirsub[idx]
      countNir[idx]=countNir[idx]+one[idx]

    endfor
  endif else begin
    typeflag='rect_brf_swf_CT'+strcompress(cloudType, /REMOVE)
    res=read_SWF_Fapar(filePath, sourceFileName, FOUND=FOUND, /REVERSE, /APPLY)
    monthswfFapar=res.fapar

    res=read_SWF_Data(filePath, sourceFileName, 'Mean:BRF_Rec_R', FOUND=FOUND, /REVERSE, /APPLY)
    monthSwfred=res.data
    res=read_SWF_Data(filePath, sourceFileName, 'Mean:BRF_Rec_N', FOUND=FOUND, /REVERSE, /APPLY)
    monthswfnir=res.data
    for day=1, monthDays do begin

      yearDay=ST_utils->calcDayOfYear([year,month,day,0])+1

      startDayString=string(year, format='(I4)')+string(month, format='(I02)')+string(day, format='(I02)')
      endDayString=startDayString
      startDayString=startDayString+'000000'
      thistoaFile=file_search(AVH02C1dir+string(year, format='(I4)')+path_sep()+string(month, format='(I02)')+path_sep()+'AVH02C1.A'+string(year, format='(I4)')+string(yearday, format='(I03)')+'*.*', count=countTOA)
      thistocFile=file_search(AVH09C1dir+string(year, format='(I4)')+path_sep()+string(month, format='(I02)')+path_sep()+'AVH09C1.A'+string(year, format='(I4)')+string(yearday, format='(I03)')+'*.*', count=countTOC)
      thisbrfFile=file_search(BRDFdir+string(year, format='(I4)')+path_sep()+string(month, format='(I02)')+path_sep()+'AVH_'+path_sep()+string(year, format='(I4)')+string(month, format='(I02)')+string(day, format='(I02)')+'*.NC', count=countBRF)
      thisSWFfaparFile=file_search(SWFFPAdir+string(year, format='(I4)')+path_sep()+string(month, format='(I02)')+path_sep()+'SEA01_ORB01_'+startDayString+'_'+endDayString+'*.HDF', count=countFPAFile)

      sourceFileName=ST_fileSystem->getFileNameInfo(thisSWFfaparFile[0], filePath=filePath, extension=extension)
      ;if countFPAFile gt 0 then thisSWFfaparFile=ST_fileSystem->getFileNameInfo(thisSWFfaparFile[0], filePath=SWFFPAdir, extension=extension) else continue

      ; read fapar, nir, red and plot...

      print, day, sourceFileName
      res=read_SWF_Fapar(filePath, sourceFileName, FOUND=FOUND, /REVERSE, /APPLY)
      swf_fapar=res.fapar
      res=read_SWF_Data(filePath, sourceFileName, 'Mean:BRF_Rec_R', FOUND=FOUND, /REVERSE, /APPLY)
      swf_red=res.data
      res=read_SWF_Data(filePath, sourceFileName, 'Mean:BRF_Rec_N', FOUND=FOUND, /REVERSE, /APPLY)
      swf_nir=res.data

      ;window, 1
      ;tvscl, congrid(swf_fapar, 720, 360), /NAN
      ;swf_fapar[xoffset-2,ystartPos:yendPos]=5.
      ;swf_fapar[xoffset-1,ystartPos:yendPos]=5.
      ;swf_fapar[xoffset,ystartPos:yendPos]=5.
      ;swf_fapar[xoffset+1,ystartPos:yendPos]=5.
      ;swf_fapar[xoffset+2,ystartPos:yendPos]=5.
      ;window, 2
      ;tvscl, congrid(swf_fapar, 720, 360), /NAN

      ;print, '*****', day
      ;print, day+1, min(swf_fapar[xoffset,ystartPos:yendPos], /NAN), max(swf_fapar[xoffset,ystartPos:yendPos], /NAN)
      fpasub=reform(swf_fapar[xoffset,ystartPos:yendPos])
      idx=where(finite(fpasub) eq 1 and fpasub gt 0 and fpasub lt 1)
      sumFPA[idx]=sumFPA[idx]+fpasub[idx]
      countFPA[idx]=countFPA[idx]+one[idx]
      all_day_dataFPA[*, day-1]=fpasub

      redSub=reform(swf_red[xoffset,ystartPos:yendPos])
      idx=where(finite(redsub) eq 1 and redsub gt 0 and redsub lt 1)
      sumRed[idx]=sumRed[idx]+redsub[idx]
      countRed[idx]=countRed[idx]+one[idx]
      all_day_dataRed[*, day-1]=redSub

      nirSub=reform(swf_nir[xoffset,ystartPos:yendPos])
      idx=where(finite(nirsub) eq 1 and nirsub gt 0 and nirsub lt 1)
      sumNir[idx]=sumNir[idx]+nirsub[idx]
      countNir[idx]=countNir[idx]+one[idx]
      all_day_dataNir[*, day-1]=nirSub
      ;    save, all_day_data, meandata, stddata, stdmean, filename='fpa.sav'
      ;    all_day_data=reform(data_in(*).fapar(0,1950:2100))
      ;    meandata=reform(mean_field.fapar(0,1950:2100))
      ;    stddata=reform(std_field.fapar(*,0,1950:2100))
      ;    stdmean=reform(std_mean.temp(0,1950:2100))
      ;    save, all_day_data, meandata, stddata, stdmean, filename='fpa.sav'
      ;    all_day_data=reform(data_in(*).red(0,1950:2100))
      ;    meandata=reform(mean_field.red(0,1950:2100))
      ;    stddata=reform(std_field.red(*,0,1950:2100))
      ;    stdmean=reform(std_mean.red(0,1950:2100))
      ;    save, all_day_data, meandata, stddata, stdmean, filename='red.sav'
      ;    all_day_data=reform(data_in(*).nir(0,1950:2100))
      ;    meandata=reform(mean_field.nir(0,1950:2100))
      ;    stddata=reform(std_field.nir(*,0,1950:2100))
      ;    stdmean=reform(std_mean.nir(0,1950:2100))
      ;    save, all_day_data, meandata, stddata, stdmean, filename='nir.sav'
      ;    thisAVHRfaparFile=file_search(AVHFPAdir+string(year, format='(I4)')+path_sep()+string(month, format='(I02)')+path_sep()+'*'+string(year, format='(I4)')+string(month, format='(I02)')+'*N'+string(day, format='(I02)')+'.NC', count=countFPAFile)
      ;    print, thistoaFile
      ;    print, thistocFile
      ;    print, thisbrfFile
      ;    print, thisSWFfaparFile
      ;    print, thisAVHRfaparFile

    endfor

  endelse


  ;fNames=['both', 'only_cloudy', 'only_shadow_cloud', 'no_mask']
  ;tempDir='E:\mariomi\Documents\projects\ldtr\data\pics\avhrr\'


  ;restore, filename='fpa.sav'
  device, decomposed=0
  faparcolor
  ;titles=['mask bit 1 or 2 (cloudy/shadow cloud)', 'mask bit 1 (cloudy)', 'mask bit 2 (shadow cloud)', 'no mask']
  inputFiles=['fpa', 'red', 'nir']
  yMinMax=[0., 1.]
  for jj=0, n_elements(inputFiles)-1 do begin
    window, jj, title=inputFiles[jj]+' '+typeflag
    ;restore, filename=inputFiles[jj]+'.sav'
    if inputFiles[jj] eq 'fpa' then all_day_data=all_day_dataFPA
    if inputFiles[jj] eq 'red' then all_day_data=all_day_dataRed
    if inputFiles[jj] eq 'nir' then all_day_data=all_day_dataNir
    device, decomposed=0
    ;plot, reform(all_day_data[*,0]), yr=[0.,0.6], min=0.01, psym = 3, max=0.9, title=inputFiles[jj]+' SWF'
    plot, reform(all_day_data[*,0]), yr=yMinMax, min=0.01, psym = 3, max=0.9, title=inputFiles[jj]+' '+typeflag
    nData=float(n_elements(all_day_data[*,0]))
    nday=float(n_elements(all_day_data[0,*]))
    for t=0, nday-1 do begin
      print, min(all_day_data[*,t], /NAN), max(all_day_data[*,t], /NAN)
      oplot, reform(all_day_data[*,t]), col=fix(float(t)*255/nday), psym = 2, min=0.01
      ;oplot, reform(all_day_data[*,t]), col=fix(float(t)*255/nday), min=0.01
      ;oplot, reform(meandata)+reform(stddata[t]), col=fix(float(t)*255/nday), min=0.01
      plots, [0., .05], [1.*t/nday,1.*t/nday], /NORM, col=fix(float(t)*255/nday), thick=4.
      xyouts, .05, 1.*t/nday, string(t+1, format='(I02)'), /NORM, col=fix(float(t)*255/nday), charsize=1.2, ALIGN=1.;fix(float(t)*255/nday)
    endfor
    device, decomposed=1
    ;oplot, reform(stdmean), min=0.01, col=0l, thick=2.5
    if jj eq 0 then begin
      thisMean=reform(sumFpa/countFpa)
      oplot, thisMean, line = 0, min=0.01, thick=2.5, color=255l*255*255
      ;oplot, reform(meandata)+ reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, max=0.9, linestyle=3
      ;oplot, reform(meandata)- reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, linestyle=3
      if n_elements(monthswffapar) ne 0 then oplot, monthswffapar[xoffset,ystartPos:yendPos], min=0.01, col=255l*256*256, thick=1.5, linestyle=0
    endif
    if jj eq 1 then begin
      thisMean=reform(sumRed/countRed)
      oplot, thisMean, line = 0, min=0.01, thick=2.5, color=255l*255*255
      ;oplot, reform(meandata)+ reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, max=0.9, linestyle=3
      ;oplot, reform(meandata)- reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, linestyle=3
      if n_elements(monthswfred) ne 0 then oplot, monthswfred[xoffset,ystartPos:yendPos], min=0.01, col=255l*256*256, thick=1.5, linestyle=0
    endif
    if jj eq 2 then begin
      thisMean=reform(sumNir/countNir)
      oplot, thisMean, line = 0, min=0.01, thick=2.5, color=255l*255*255
      ;oplot, reform(meandata)+ reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, max=0.9, linestyle=3
      ;oplot, reform(meandata)- reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, linestyle=3
      if n_elements(monthswfnir) ne 0 then oplot, monthswfnir[xoffset,ystartPos:yendPos], min=0.01, col=255l*256*256, thick=1.5, linestyle=0
    endif
    ;load tc of swf here...
    ;    offset=7200.*10/nslice

    plotimg=tvrd(true=1)
    ;fName=tempDir+inputFiles[jj]+'_'+fNames[cloudtype]+'_'+destncFileNameInfo.fileName+'.png'
    fName=tempDir+path_sep()+inputFiles[jj]+'_'+typeflag+'_'+string(year, format='(I4)')+string(month, format='(I02)')+'.png'
    write_png,fName,plotimg
  endfor

  ;  ncdfread, 'refl_0_65um_nom', workDir+thistoaFile, red_toa, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;  ncdfread, 'refl_0_86um_nom', workDir+thistoaFile, nir_toa, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;
  ;  ncdfread, 'SREFL_CH1', workDir+thistocFile, red_toa, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;  ncdfread, 'SREFL_CH2', workDir+thistocFile, nir_toa, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;
  ;  data=readBRF(BRDFdir, thisbrfFile, FOUND=FOUND)
  ;
  ;  res=read_SWF_FAPAR(SWFFPAdir, thisSWFfaparFile, FOUND=FOUND, /REVERSE, /APPLY)
  ;  res=read_AVHRR_FAPAR(AVHFPAdir, AVHFPAdir, FOUND=FOUND, varName=varName, /APPLY)
  ;
  ;  backcols=['Dark Gray', 'Snow']
  ;  pencols=['Blue','Red']
  ;
  ;  ; store aggregation of days
  ;  fullRedDataTocL1=0.0
  ;  fullRedDataToaL1=0.0
  ;  fullRedDataBrfL1=0.0
  ;  fullRedDataTocL2=0.0
  ;  fullRedDataToaL2=0.0
  ;  fullRedDataBrfL2=0.0
  ;
  ;  fullNirDataTocL1=0.0
  ;  fullNirDataToaL1=0.0
  ;  fullNirDataBrfL1=0.0
  ;  fullNirDataTocL2=0.0
  ;  fullNirDataToaL2=0.0
  ;  fullNirDataBrfL2=0.0
  ;
  ;  filesToCompare=(n_elements(allbrfFilesL1)<n_elements(allbrfFilesL2))
  ;  meanListRedToaL1=fltarr(filesToCompare)
  ;  meanListRedTocL1=fltarr(filesToCompare)
  ;  meanListRedBrfL1=fltarr(filesToCompare)
  ;
  ;  meanListNirToaL1=fltarr(filesToCompare)
  ;  meanListNirTocL1=fltarr(filesToCompare)
  ;  meanListNirBrfL1=fltarr(filesToCompare)
  ;
  ;  meanListRedToaL2=fltarr(filesToCompare)
  ;  meanListRedTocL2=fltarr(filesToCompare)
  ;  meanListRedBrfL2=fltarr(filesToCompare)
  ;
  ;  meanListNirToaL2=fltarr(filesToCompare)
  ;  meanListNirTocL2=fltarr(filesToCompare)
  ;  meanListNirBrfL2=fltarr(filesToCompare)
  ;
  ;  for i=0, filesToCompare-1 do begin
  ;    toaNameL1=alltoaFilesL1[i] & toc1NameL1=alltoc1FilesL1[i] & brfNameL1=allbrfFilesL1[i]
  ;    toaNameL2=alltoaFilesL2[i] & toc1NameL2=alltoc1FilesL2[i] & brfNameL2=allbrfFilesL2[i]
  ;
  ;    toaNameL1=ST_fileSystem->getFileNameInfo(toaNameL1, filePath=filePath, extension=extension)
  ;    toc1NameL1=ST_fileSystem->getFileNameInfo(toc1NameL1, filePath=filePath, extension=extension)
  ;    brfNameL1=ST_fileSystem->getFileNameInfo(brfNameL1, filePath=filePath, extension=extension)
  ;
  ;    infoVarTOACh1=['refl_0_65um_nom', workDir+toaNameL1, toaNameL1, workDir, ext, 'input_data[1,*]/toa_red_avhrr']
  ;    infoVarTOACh2=['refl_0_86um_nom', workDir+toaNameL1, toaNameL1, workDir, ext, 'input_data[1,*]/toa_nir_avhrr']
  ;    infoVarTOACldM=['cloud_mask', workDir+toaNameL1, toaNameL1, workDir, ext, 'input_data[1,*]/cloud_mask']
  ;
  ;    infoVarTOC1Ch1=['SREFL_CH1', workDir+toc1NameL1, toc1NameL1, workDir, ext, 'input_data[1,*]/toc_red_avhrr']
  ;    infoVarTOC1Ch2=['SREFL_CH2', workDir+toc1NameL1, toc1NameL1, workDir, ext, 'input_data[1,*]/toc_nir_avhrr']
  ;
  ;    infoVarBRFCh1=['BRF_BAND_1', workDir+brfNameL1, brfNameL1, workDir, ext, 'input_data[1,*]/brf_red_avhrr']
  ;    infoVarBRFCh2=['BRF_BAND_2', workDir+brfNameL1, brfNameL1, workDir, ext, 'input_data[1,*]/brf_nir_avhrr']
  ;
  ;    ; read toa red
  ;    ncdfread, infoVarTOACh1[1], infoVarTOACh1[0], red_toa, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;    noIdxs=where(red_toa eq red_fillvalue, count)
  ;    red_toaL1=1.*red_toa*red_slope+red_offset
  ;    if count ne 0 then red_toa[noIdxs]=!VALUES.F_NAN
  ;    ; read toa nir
  ;    ncdfread, infoVarTOACh2[1], infoVarTOACh2[0], nir_toa, nir_slope, nir_offset, dim, nir_fillvalue, ERROR=ERROR
  ;    noIdxs=where(nir_toa eq nir_fillvalue, count)
  ;    nir_toaL1=1.*nir_toa*nir_slope+nir_offset
  ;    if count ne 0 then nir_toa[noIdxs]=!VALUES.F_NAN
  ;    ; read cloud mask
  ;    ncdfread, infoVarTOACldM[1], infoVarTOACldM[0], cloud_mask, cloud_slope, cloud_offset, dim, cloud_fillvalue, ERROR=ERROR
  ;    noIdxs=where(cloud_mask eq cloud_fillvalue or cloud_mask eq 2 or cloud_mask eq 3, count)
  ;    cloud_maskL1=1.*cloud_mask; no slope, no offset
  ;    if count ne 0 then cloud_maskL1[noIdxs]=!VALUES.F_NAN
  ;
  ;    ;window, i*10+1, title='TOA - RED'
  ;    ;tvscl, congrid(red_toa, 3600/3, 1800/3), /NAN
  ;    ;window, i*10+2, title='TOA - NIR'
  ;    ;tvscl, congrid(nir_toa, 3600/3, 1800/3), /NAN
  ;
  ;    ; read toc red
  ;    toaNameL1=ST_fileSystem->getFileNameInfo(toaNameL1, filePath=filePath, extension=extension)
  ;    ncdfread, infoVarTOC1Ch1[1], infoVarTOC1Ch1[0], red_toc, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;    noIdxs=where(red_toc eq red_fillvalue, count)
  ;    red_toc=1.*red_toc*red_slope+red_offset
  ;    if count ne 0 then red_toc[noIdxs]=!VALUES.F_NAN
  ;    red_tocL1=reverse(temporary(red_toc),2)
  ;    ; read toc nir
  ;    ncdfread, infoVarTOC1Ch2[1], infoVarTOC1Ch2[0], nir_toc, nir_slope, nir_offset, dim, nir_fillvalue, ERROR=ERROR
  ;    noIdxs=where(nir_toc eq nir_fillvalue, count)
  ;    nir_toc=1.*nir_toc*nir_slope+nir_offset
  ;    if count ne 0 then nir_toc[noIdxs]=!VALUES.F_NAN
  ;    nir_tocL1=reverse(temporary(nir_toc),2)
  ;    ;window, 3+i*10, title='TOC - RED ('+labels[i]+')'
  ;    ;tvscl, congrid(red_toc, 3600/3, 1800/3), /NAN
  ;    ;window, 4+i*10, title='TOC - NIR ('+labels[i]+')'
  ;    ;tvscl, congrid(nir_toc, 3600/3, 1800/3), /NAN
  ;
  ;    ; read brf red
  ;    ncdfread, infoVarBRFCh1[1], infoVarBRFCh1[0], red_brf, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;    noIdxs=where(red_brf eq red_fillvalue, count)
  ;    red_brfL1=1.*red_brf*red_slope+red_offset
  ;    if count ne 0 then red_brfL1[noIdxs]=!VALUES.F_NAN
  ;    ; read brf nir
  ;    ncdfread, infoVarBRFCh2[1], infoVarBRFCh2[0], nir_brf, nir_slope, nir_offset, dim, nir_fillvalue, ERROR=ERROR
  ;    noIdxs=where(nir_brf eq nir_fillvalue, count)
  ;    nir_brfL1=1.*nir_brf*nir_slope+nir_offset
  ;    if count ne 0 then nir_brfL1[noIdxs]=!VALUES.F_NAN
  ;    ;window, 5+i*10, title='BRF - RED ('+labels[i]+')'
  ;    ;tvscl, congrid(red_brf, 3600/3, 1800/3), /NAN
  ;    ;window, 6+10*i, title='BRF - NIR ('+labels[i]+')'
  ;    ;tvscl, congrid(nir_brf, 3600/3, 1800/3), /NAN
  ;
  ;    ; enlarge TOA to same resolution as Brf/Toc
  ;    red_toaL1=congrid(red_toaL1, 7200, 3600)
  ;    nir_toaL1=congrid(nir_toaL1, 7200, 3600)
  ;
  ;    toaNameL2=ST_fileSystem->getFileNameInfo(toaNameL2, filePath=filePath, extension=extension)
  ;    toc1NameL2=ST_fileSystem->getFileNameInfo(toc1NameL2, filePath=filePath, extension=extension)
  ;    brfNameL2=ST_fileSystem->getFileNameInfo(brfNameL2, filePath=filePath, extension=extension)
  ;
  ;    infoVarTOACh1=['refl_0_65um_nom', workDir+toaNameL2, toaNameL2, workDir, ext, 'input_data[1,*]/toa_red_avhrr']
  ;    infoVarTOACh2=['refl_0_86um_nom', workDir+toaNameL2, toaNameL2, workDir, ext, 'input_data[1,*]/toa_nir_avhrr']
  ;    infoVarTOACldM=['cloud_mask', workDir+toaNameL2, toaNameL2, workDir, ext, 'input_data[1,*]/cloud_mask']
  ;
  ;    infoVarTOC1Ch1=['SREFL_CH1', workDir+toc1NameL2, toc1NameL2, workDir, ext, 'input_data[1,*]/toc_red_avhrr']
  ;    infoVarTOC1Ch2=['SREFL_CH2', workDir+toc1NameL2, toc1NameL2, workDir, ext, 'input_data[1,*]/toc_nir_avhrr']
  ;
  ;    ;infoVarTOC2Ch1=['SREFL_CH1', sourceFile, fName, dir, ext, 'input_data[1,*]/toa_red_avhrr']
  ;    ;infoVarTOC2Ch2=['SREFL_CH2', sourceFile, fName, dir, ext, 'input_data[1,*]/toa_nir_avhrr']
  ;
  ;    infoVarBRFCh1=['BRF_BAND_1', workDir+brfNameL2, brfNameL2, workDir, ext, 'input_data[1,*]/brf_red_avhrr']
  ;    infoVarBRFCh2=['BRF_BAND_2', workDir+brfNameL2, brfNameL2, workDir, ext, 'input_data[1,*]/brf_nir_avhrr']
  ;
  ;    ; read toa red
  ;    ncdfread, infoVarTOACh1[1], infoVarTOACh1[0], red_toa, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;    noIdxs=where(red_toa eq red_fillvalue, count)
  ;    red_toaL2=1.*red_toa*red_slope+red_offset
  ;    if count ne 0 then red_toaL2[noIdxs]=!VALUES.F_NAN
  ;    ; read toa nir
  ;    ncdfread, infoVarTOACh2[1], infoVarTOACh2[0], nir_toa, nir_slope, nir_offset, dim, nir_fillvalue, ERROR=ERROR
  ;    noIdxs=where(nir_toa eq nir_fillvalue, count)
  ;    nir_toaL2=1.*nir_toa*nir_slope+nir_offset
  ;    if count ne 0 then nir_toaL2[noIdxs]=!VALUES.F_NAN
  ;    ;window, i*10+1, title='TOA - RED'
  ;    ;tvscl, congrid(red_toa, 3600/3, 1800/3), /NAN
  ;    ;window, i*10+2, title='TOA - NIR'
  ;    ;tvscl, congrid(nir_toa, 3600/3, 1800/3), /NAN
  ;
  ;    ; read toc red
  ;    ncdfread, infoVarTOC1Ch1[1], infoVarTOC1Ch1[0], red_toc, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;    noIdxs=where(red_toc eq red_fillvalue, count)
  ;    red_toc=1.*red_toc*red_slope+red_offset
  ;    if count ne 0 then red_toc[noIdxs]=!VALUES.F_NAN
  ;    red_tocL2=reverse(temporary(red_toc),2)
  ;    ; read toc nir
  ;    ncdfread, infoVarTOC1Ch2[1], infoVarTOC1Ch2[0], nir_toc, nir_slope, nir_offset, dim, nir_fillvalue, ERROR=ERROR
  ;    noIdxs=where(nir_toc eq nir_fillvalue, count)
  ;    nir_toc=1.*nir_toc*nir_slope+nir_offset
  ;    if count ne 0 then nir_toc[noIdxs]=!VALUES.F_NAN
  ;    nir_tocL2=reverse(temporary(nir_toc),2)
  ;    ;window, 3+i*10, title='TOC - RED ('+labels[i]+')'
  ;    ;tvscl, congrid(red_toc, 3600/3, 1800/3), /NAN
  ;    ;window, 4+i*10, title='TOC - NIR ('+labels[i]+')'
  ;    ;tvscl, congrid(nir_toc, 3600/3, 1800/3), /NAN
  ;
  ;    ; read brf red
  ;    ncdfread, infoVarBRFCh1[1], infoVarBRFCh1[0], red_brf, red_slope, red_offset, dim, red_fillvalue, ERROR=ERROR
  ;    noIdxs=where(red_brf eq red_fillvalue, count)
  ;    red_brfL2=1.*red_brf*red_slope+red_offset
  ;    if count ne 0 then red_brfL2[noIdxs]=!VALUES.F_NAN
  ;    ; read brf nir
  ;    ncdfread, infoVarBRFCh2[1], infoVarBRFCh2[0], nir_brf, nir_slope, nir_offset, dim, nir_fillvalue, ERROR=ERROR
  ;    noIdxs=where(nir_brf eq nir_fillvalue, count)
  ;    nir_brfL2=1.*nir_brf*nir_slope+nir_offset
  ;    if count ne 0 then nir_brfL2[noIdxs]=!VALUES.F_NAN
  ;    ;window, 5+i*10, title='BRF - RED ('+labels[i]+')'
  ;    ;tvscl, congrid(red_brf, 3600/3, 1800/3), /NAN
  ;    ;window, 6+10*i, title='BRF - NIR ('+labels[i]+')'
  ;    ;tvscl, congrid(nir_brf, 3600/3, 1800/3), /NAN
  ;    ; read cloud mask
  ;    ncdfread, infoVarTOACldM[1], infoVarTOACldM[0], cloud_mask, cloud_slope, cloud_offset, dim, cloud_fillvalue, ERROR=ERROR
  ;    noIdxs=where(cloud_mask eq cloud_fillvalue or cloud_mask eq 2 or cloud_mask eq 3, count)
  ;    cloud_maskL2=1.*cloud_mask; no slope, no offset
  ;    if count ne 0 then cloud_maskL2[noIdxs]=!VALUES.F_NAN
  ;
  ;    ; enlarge TOA to same resolution as Brf/Toc
  ;    red_toaL2=congrid(red_toaL2, 7200, 3600)
  ;    nir_toaL2=congrid(nir_toaL2, 7200, 3600)
  ;
  ;    ; remove NaN (from Red)
  ;    badIndexes=where(finite(red_brfL1) ne 1, count)
  ;    if count gt 0 then red_toa[badIndexes]=!VALUES.F_NAN
  ;
  ;    ; remove NaN (from NIR)
  ;    badIndexes=where(finite(nir_brfL1) ne 1, count)
  ;    if count gt 0 then nir_toa[badIndexes]=!VALUES.F_NAN
  ;    ;latIndexes=[1200,1800,2400]
  ;    ; Select an horizontal stripe
  ;    if n_elements(latPixelValue) eq 1 then latIndexes=latPixelValue else latIndexes=1800
  ;
  ;    ;for j=0, n_elements(latIndexes)-1 do begin
  ;    latIndex=latIndexes;[j]
  ;
  ;    testBrfRedLineL1=red_brfL1[*,latIndex]
  ;    testTocRedLineL1=red_tocL1[*,latIndex]
  ;    testToaRedLineL1=red_toaL1[*,latIndex]
  ;    testBrfNirLineL1=nir_brfL1[*,latIndex]
  ;    testTocNirLineL1=nir_tocL1[*,latIndex]
  ;    testToaNirLineL1=nir_toaL1[*,latIndex]
  ;
  ;    testBrfRedLineL2=red_brfL2[*,latIndex]
  ;    testTocRedLineL2=red_tocL2[*,latIndex]
  ;    testToaRedLineL2=red_toaL2[*,latIndex]
  ;    testBrfNirLineL2=nir_brfL2[*,latIndex]
  ;    testTocNirLineL2=nir_tocL2[*,latIndex]
  ;    testToaNirLineL2=nir_toaL2[*,latIndex]
  ;
  ;    ; remove NaN, use only SAME valid indexes for all the days/band...
  ;    if n_elements(validIdxs1) eq 0 then validIdxs1=where(finite(testBrfRedLineL1) eq 1, count1)
  ;    testBrfRedLineL1=testBrfRedLineL1[validIdxs1]
  ;    testTocRedLineL1=testTocRedLineL1[validIdxs1]
  ;    testToaRedLineL1=testToaRedLineL1[validIdxs1]/100 ;Normalize TOA from 0. to 1.
  ;
  ;    if n_elements(validIdxs2) eq 0 then validIdxs2=where(finite(testBrfNirLineL1) eq 1, count2)
  ;    testBrfNirLineL1=testBrfNirLineL1[validIdxs2]
  ;    testTocNirLineL1=testTocNirLineL1[validIdxs2]
  ;    testToaNirLineL1=testToaNirLineL1[validIdxs2]/100 ;Normalize TOA from 0. to 1.
  ;
  ;    ;      meanBRFL1=mean(testBrfNirLineL1, /NAN)
  ;    ;      meanTOCL1=mean(testTocNirLineL1, /NAN)
  ;    ;      meanTOAL1=mean(testToaNirLineL1, /NAN)
  ;
  ;    badIndexes=where(finite(red_brfL2) ne 1, count)
  ;    if count gt 0 then red_toaL2[badIndexes]=!VALUES.F_NAN
  ;
  ;    badIndexes=where(finite(nir_brfL2) ne 1, count)
  ;    if count gt 0 then nir_toaL2[badIndexes]=!VALUES.F_NAN
  ;
  ;    if n_elements(validIdxs1) eq 0 then validIdxs1=where(finite(testBrfRedLineL2) eq 1, count1)
  ;    testBrfRedLineL2=testBrfRedLineL2[validIdxs1]
  ;    testTocRedLineL2=testTocRedLineL2[validIdxs1]
  ;    testToaRedLineL2=testToaRedLineL2[validIdxs1]/100
  ;
  ;    if n_elements(validIdxs2) eq 0 then validIdxs2=where(finite(testBrfNirLineL2) eq 1, count2)
  ;    testBrfNirLineL2=testBrfNirLineL2[validIdxs2]
  ;    testTocNirLineL2=testTocNirLineL2[validIdxs2]
  ;    testToaNirLineL2=testToaNirLineL2[validIdxs2]/100
  ;
  ;    ; just check how many overthresholds we have... (Not remove them from dataset)
  ;    idxBrfL1=where(testBrfRedLineL1 gt 1., countBrfThL1)
  ;    idxTocL1=where(testTocRedLineL1 gt 1., countTocThL1)
  ;    idxToaL1=where(testToaRedLineL1 gt 1., countToaThL1)
  ;    idxBrfL2=where(testBrfRedLineL2 gt 1., countBrfThL2)
  ;    idxTocL2=where(testTocRedLineL2 gt 1., countTocThL2)
  ;    idxToaL2=where(testToaRedLineL2 gt 1., countToaThL2)
  ;
  ;    print, 'Over Threshold Brf/Toc/Toa (L1):', countBrfThL1, countTocThL1, countToaThL1
  ;    print, 'Over Threshold Brf/Toc/Toa (L2):', countBrfThL2, countTocThL2, countToaThL2
  ;
  ;    meanListRedToaL1[i]=(moment(testToaRedLineL1, /NAN))[0]
  ;    meanListRedTocL1[i]=(moment(testTocRedLineL1, /NAN))[0]
  ;    meanListRedBrfL1[i]=(moment(testBrfRedLineL1, /NAN))[0]
  ;
  ;    meanListNirToaL1[i]=(moment(testToaNirLineL1, /NAN))[0]
  ;    meanListNirTocL1[i]=(moment(testTocNirLineL1, /NAN))[0]
  ;    meanListNirBrfL1[i]=(moment(testBrfNirLineL1, /NAN))[0]
  ;
  ;    meanListRedToaL2[i]=(moment(testToaRedLineL2, /NAN))[0]
  ;    meanListRedTocL2[i]=(moment(testTocRedLineL2, /NAN))[0]
  ;    meanListRedBrfL2[i]=(moment(testBrfRedLineL2, /NAN))[0]
  ;
  ;    meanListNirToaL2[i]=(moment(testToaNirLineL2, /NAN))[0]
  ;    meanListNirTocL2[i]=(moment(testTocNirLineL2, /NAN))[0]
  ;    meanListNirBrfL2[i]=(moment(testBrfNirLineL2, /NAN))[0]
  ;    ;      meanBRFL2=mean(testBrfNirLineL2, /NAN)
  ;    ;      meanTOCL2=mean(testTocNirLineL2, /NAN)
  ;    ;      meanTOAL2=mean(testToaNirLineL2, /NAN)
  ;
  ;    ;window, 7+10*i, title='COMPARISON - RED ('+labels[i]+')'
  ;    ;cgplot, testBrfRedLine, color="black", background='Gray'
  ;    ;cgoplot, testTocRedLine, line_style=2, color="blue", symbol=2
  ;    ;cgoplot, testToaRedLine, line_style=4, color="red", symbol=4
  ;
  ;    ;window, 8+10*i, title='COMPARISON - NIR ('+labels[i]+')'
  ;    ;    winIndex++
  ;    ;    window, k, title='COMPARISON - NIR ('+labelsL1[i]+')'
  ;    ;    cgplot, testBrfNirLineL1, color='Pale Green', background=backcols[0], YRANGE=[0.0, 1.0]
  ;    ;    cgoplot, testTocNirLineL1, line_style=2, color='Dark Salmon', symbol=5
  ;    ;    cgoplot, testToaNirLineL1, line_style=4, color='Light Yellow', symbol=6
  ;    ;    cgplots, [0,n_elements(testBrfNirLineL1)], [meanBrfL1,meanBrfL1], color='Pale Green', linestyle=2, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testTocNirLineL1)], [meanTOCL1,meanTOCL1], color='Dark Salmon', linestyle=4, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testToaNirLineL1)], [meanTOAL1,meanTOAL1], color='Light Yellow', linestyle=6, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testBrfNirLineL2)], [meanBrfL2,meanBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testTocNirLineL2)], [meanTOCL2,meanTOCL2], color='Light Salmon', linestyle=4, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testToaNirLineL2)], [meanTOAL2,meanTOAL2], color='Beige', linestyle=6, thick=1.5, /DATA
  ;    ;    cgLegend, COLORS=['Pale Green', 'Dark Salmon', 'Light Yellow'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;    ;      titles=['BRF - '+string(meanBRFL1, format='(F5.3)'), 'TOC - '+string(meanTOCL1, format='(F5.3)'), 'TOA - '+string(meanTOAL1, format='(F5.3)')]
  ;
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - NIR Brf Vs Toc ('+labelsL1[i]+')'
  ;    ;    validIdx=where(finite(testBrfNirLineL1) eq 1 and finite(testTocNirLineL1) eq 1, cnt)
  ;    ;    cgscatter2d,  testBrfNirLineL1[validIdx], testTocNirLineL1[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[0]
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - RED Brf Vs Toc ('+labelsL1[i]+')'
  ;    ;    validIdx=where(finite(testBrfRedLineL1) eq 1 and finite(testTocRedLineL1) eq 1, cnt)
  ;    ;    cgscatter2d,  testBrfRedLineL1[validIdx], testTocRedLineL1[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[0]
  ;
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - NIR Toa Vs Toc ('+labelsL1[i]+')'
  ;    ;    validIdx=where(finite(testToaNirLineL1) eq 1 and finite(testTocNirLineL1) eq 1, cnt)
  ;    ;    cgscatter2d,  testToaNirLineL1[validIdx], testTocNirLineL1[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[0]
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - RED Toa Vs Toc ('+labelsL1[i]+')'
  ;    ;    validIdx=where(finite(testToaRedLineL1) eq 1 and finite(testTocRedLineL1) eq 1, cnt)
  ;    ;    cgscatter2d,  testToaRedLineL1[validIdx], testTocRedLineL1[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[0]
  ;    ;
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - NIR Toa Vs Brf ('+labelsL1[i]+')'
  ;    ;    validIdx=where(finite(testBrfNirLineL1) eq 1 and finite(testToaNirLineL1) eq 1, cnt)
  ;    ;    cgscatter2d,  testToaNirLineL1[validIdx], testBrfNirLineL1[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[0]
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - RED Toa Vs Brf ('+labelsL1[i]+')'
  ;    ;    validIdx=where(finite(testBrfRedLineL1) eq 1 and finite(testToaRedLineL1) eq 1, cnt)
  ;    ;    cgscatter2d,  testToaRedLineL1[validIdx], testBrfRedLineL1[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[0]
  ;
  ;    ;    winIndex++
  ;    ;    window, k, title='COMPARISON - NIR ('+labelsL2[i]+')'
  ;    ;    cgplot, testBrfNirLineL2, color='Spring Green', background='Gray', YRANGE=[0.0, 1.0]
  ;    ;    cgoplot, testTocNirLineL2, color='Light Salmon'
  ;    ;    cgoplot, testToaNirLineL2, color='Beige'
  ;    ;    cgplots, [0,n_elements(testBrfNirLineL1)], [meanBrfL1,meanBrfL1], color='Pale Green', linestyle=2, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testTocNirLineL1)], [meanTOCL1,meanTOCL1], color='Dark Salmon', linestyle=4, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testToaNirLineL1)], [meanTOAL1,meanTOAL1], color='Light Yellow', linestyle=6, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testBrfNirLineL2)], [meanBrfL2,meanBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testTocNirLineL2)], [meanTOCL2,meanTOCL2], color='Light Salmon', linestyle=4, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testToaNirLineL2)], [meanTOAL2,meanTOAL2], color='Beige', linestyle=6, thick=1.5, /DATA
  ;    ;    cgLegend, COLORS=['Spring Green', 'Light Salmon', 'Beige'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;    ;      titles=['BRF - '+string(meanBRFL2, format='(F5.3)'), 'TOC - '+string(meanTOCL2, format='(F5.3)'), 'TOA - '+string(meanTOAL2, format='(F5.3)')]
  ;
  ;    ; aggregate only nice pixel for these days
  ;    validIdx=where(finite(testToaNirLineL1) eq 1 and finite(testTocNirLineL1) eq 1 and finite(testBrfNirLineL1) eq 1 and $
  ;      finite(testToaNirLineL2) eq 1 and finite(testTocNirLineL2) eq 1 and finite(testBrfNirLineL2) eq 1, cnt)
  ;
  ;    if cnt eq 0 then continue
  ;    fullNirDataTocL1=[fullNirDataTocL1, reform(testTocNirLineL1[validIdx], cnt)]
  ;    fullNirDataToaL1=[fullNirDataToaL1, reform(testToaNirLineL1[validIdx], cnt)]
  ;    fullNirDataBrfL1=[fullNirDataBrfL1, reform(testBrfNirLineL1[validIdx], cnt)]
  ;
  ;    fullRedDataTocL1=[fullRedDataTocL1, reform(testTocRedLineL1[validIdx], cnt)]
  ;    fullRedDataToaL1=[fullRedDataToaL1, reform(testToaRedLineL1[validIdx], cnt)]
  ;    fullRedDataBrfL1=[fullRedDataBrfL1, reform(testBrfRedLineL1[validIdx], cnt)]
  ;
  ;    ;validIdx=where(finite(testToaNirLineL2) eq 1 and finite(testTocNirLineL2) eq 1 and finite(testBrfNirLineL2) eq 1, cnt)
  ;
  ;    fullNirDataTocL2=[fullNirDataTocL2, reform(testTocNirLineL2[validIdx], cnt)]
  ;    fullNirDataToaL2=[fullNirDataToaL2, reform(testToaNirLineL2[validIdx], cnt)]
  ;    fullNirDataBrfL2=[fullNirDataBrfL2, reform(testBrfNirLineL2[validIdx], cnt)]
  ;
  ;    fullRedDataTocL2=[fullRedDataTocL2, reform(testTocRedLineL2[validIdx], cnt)]
  ;    fullRedDataToaL2=[fullRedDataToaL2, reform(testToaRedLineL2[validIdx], cnt)]
  ;    fullRedDataBrfL2=[fullRedDataBrfL2, reform(testBrfRedLineL2[validIdx], cnt)]
  ;    ;endfor
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - NIR Brf Vs Toc ('+labelsL2[i]+')'
  ;    ;    validIdx=where(finite(testBrfNirLineL2) eq 1 and finite(testTocNirLineL2) eq 1, cnt)
  ;    ;    cgscatter2d,  testBrfNirLineL2[validIdx], testTocNirLineL2[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[1]
  ;
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - RED Brf Vs Toc ('+labelsL2[i]+')'
  ;    ;    validIdx=where(finite(testBrfRedLineL2) eq 1 and finite(testTocRedLineL2) eq 1, cnt)
  ;    ;    cgscatter2d,  testBrfRedLineL2[validIdx], testTocRedLineL2[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[1]
  ;
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - NIR Toa Vs Toc ('+labelsL2[i]+')'
  ;    ;    validIdx=where(finite(testToaNirLineL2) eq 1 and finite(testTocNirLineL2) eq 1, cnt)
  ;    ;    cgscatter2d,  testToaNirLineL2[validIdx], testTocNirLineL2[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[1]
  ;
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - RED Toa Vs Toc ('+labelsL2[i]+')'
  ;    ;    validIdx=where(finite(testToaRedLineL2) eq 1 and finite(testTocRedLineL2) eq 1, cnt)
  ;    ;    cgscatter2d,  testToaRedLineL2[validIdx], testTocRedLineL2[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[1]
  ;    ;
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - NIR Toa Vs Brf ('+labelsL2[i]+')'
  ;    ;    validIdx=where(finite(testToaNirLineL2) eq 1 and finite(testBrfNirLineL2) eq 1, cnt)
  ;    ;    cgscatter2d,  testToaNirLineL2[validIdx], testBrfNirLineL2[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[1]
  ;
  ;    ;    winIndex++
  ;    ;    window, k, title='SCATTER - RED Toa Vs Brf ('+labelsL2[i]+')'
  ;    ;    validIdx=where(finite(testToaRedLineL2) eq 1 and finite(testBrfRedLineL2) eq 1, cnt)
  ;    ;    cgscatter2d,  testToaRedLineL2[validIdx], testBrfRedLineL2[validIdx], xrange=[0., 1.], yrange=[0., 1.], background=backcols[1]
  ;
  ;    ;    winIndex++
  ;    ;    window, k, title='COMPARISON - NIR (diff)'
  ;    ;    diffBrf=testBrfNirLineL2-testBrfNirLineL1
  ;    ;    diffToc=testTocNirLineL2-testTocNirLineL1
  ;    ;    diffToa=testToaNirLineL2-testToaNirLineL1
  ;    ;    cgplot, diffBrf, color="black", background='Gray', YRANGE=[-0.5, +0.5]
  ;    ;    cgoplot, diffToc, line_style=2, color="blue", symbol=5
  ;    ;    cgoplot, diffToa, line_style=4, color="red", symbol=6
  ;    ;    cgplots, [0,n_elements(testBrfNirLineL2)], [meanBrfL1,meanBrfL1], color='black', linestyle=2, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testTocNirLineL2)], [meanTOCL1,meanTOCL1], color='blue', linestyle=4, thick=1.5, /DATA
  ;    ;    cgplots, [0,n_elements(testToaNirLineL2)], [meanTOAL1,meanTOAL1], color='red', linestyle=6, thick=1.5, /DATA
  ;    ;    cgLegend, COLORS=['black', 'blue', 'red'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;    ;      titles=['Diff BRF - '+string(meanBRFL2-meanBRFL1, format='(F7.4)'), 'Diff TOC - '+string(meanTOCL2-meanTOCL1, format='(F7.4)'), 'Diff TOA - '+string(meanTOAL2-meanTOAL1, format='(F7.4)')]
  ;
  ;    ;    lower=where(diffBrf le -0.1 , countLower)
  ;    ;    higher=where(diffBrf ge 0.1, countHigher)
  ;    ;    loadct,12
  ;    ;    winIndex++
  ;    ;    window, k, title='Histo diff Brf ('+labelsL2[i]+')'
  ;    ;    a=cghistogram(diffBrf, binsize=0.05, /NAN, min=-0.7, max=0.7)
  ;    ;    xAxisValues=string((findgen(n_elements(a))/20.)-0.7, format='(F5.2)')
  ;    ;    cgbarplot, [countLower,a,countHigher], background='Gray', BARNAMES=['tot <= -0.1', xAxisValues, 'tot >= 0.1'], charsize=0.8
  ;    ;    winIndex++
  ;    ;    window, k, title='Histo diff Toc ('+labelsL2[i]+')'
  ;    ;    a=cghistogram(diffToc, binsize=0.05, /NAN, min=-0.7, max=0.7)
  ;    ;    cgbarplot, a, [], background='Gray', BARNAMES=xAxisValues
  ;    ;    winIndex++
  ;    ;    window, k, title='Histo diff Toa ('+labelsL2[i]+')'
  ;    ;    a=cghistogram(diffToc, binsize=0.05, /NAN, min=-0.7, max=0.7)
  ;    ;    cgbarplot, a, background='Gray', BARNAMES=xAxisValues
  ;
  ;  endfor
  ;
  ;  fullData1=[meanListNirToaL2, meanListNirTocL2, meanListNirBrfL2, meanListNirToaL1, meanListNirTocL1, meanListNirBrfL1]
  ;  min1=min(fullData1, max=max1)
  ;  fullData2=[meanListRedToaL2, meanListRedTocL2, meanListRedBrfL2, meanListRedToaL1, meanListRedTocL1, meanListRedBrfL1]
  ;  min2=min(fullData2, max=max2)
  ;
  ;  diffRedBrf=fullnirDataBrfL2-fullRedDataBrfL1
  ;  diffRedToc=fullNirDataTocL2-fullRedDataTocL1
  ;  diffRedToa=fullNirDataToaL2-fullRedDataToaL1
  ;
  ;  diffNirBrf=fullNirDataBrfL2-fullNirDataBrfL1
  ;  diffNirToc=fullNirDataTocL2-fullNirDataTocL1
  ;  diffNirToa=fullNirDataToaL2-fullNirDataToaL1
  ;
  ;  ;okIndexes=where(abs(diffBrf) le 0.5)
  ;  ;diffBrf=diffBrf[okIndexes]
  ;  ;diffToc=diffToc[okIndexes]
  ;  ;diffToa=diffToa[okIndexes]
  ;
  ;  thick=1.3
  ;  ; Red - NL1
  ;
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Red-Brf NL1'
  ;  ;  cgHistoplot, fullRedDataBrfL1, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='GRN4', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Red-Toc NL1'
  ;  ;  cgHistoplot, fullRedDataTocL1, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='BLU4', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Red-Toa NL1'
  ;  ;  cgHistoplot, fullRedDataToaL1, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='RED4', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;
  ;  ;  ; red - NL2
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Red-Brf NL2'
  ;  ;  cgHistoplot, fullRedDataBrfL2, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='GRN8', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Red-Toc NL2'
  ;  ;  cgHistoplot, fullRedDataTocL2, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='BLU8', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Red-Toa NL2'
  ;  ;  cgHistoplot, fullRedDataToaL2, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='RED8', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;
  ;  ;  ; Red - Diff
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Red-Diff Brf'
  ;  ;  cgHistoplot, diffRedBrf, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='GRN1', /FillPolygon, DataColor='navy'
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Red-Diff Toc'
  ;  ;  cgHistoplot, diffRedToc, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='BLU1', /FillPolygon, DataColor='navy'
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Red-Diff Toa'
  ;  ;  cgHistoplot, diffRedToa, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='RED1', /FillPolygon, DataColor='navy'
  ;  ;
  ;  ;  ; Nir - NL1
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Nir-Brf NL1'
  ;  ;  cgHistoplot, fullnirDataBrfL1, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='GRN4', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Nir-Toc NL1'
  ;  ;  cgHistoplot, fullNirDataTocL1, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='BLU4', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Nir-Toa NL1'
  ;  ;  cgHistoplot, fullNirDataToaL1, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='RED4', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;
  ;  ;  ; Nir - NL2
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Nir-Brf NL2'
  ;  ;  cgHistoplot, fullNirDataBrfL2, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='GRN8', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Nir-Toc NL2'
  ;  ;  cgHistoplot, fullNirDataTocL2, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='BLU8', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Nir-Toa NL2'
  ;  ;  cgHistoplot, fullNirDataToaL2, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='RED8', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4
  ;  ;
  ;  ;  ; Nir - Diff
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Nir-Diff Brf'
  ;  ;  cgHistoplot, diffNirBrf, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='GRN1', /FillPolygon, DataColor='navy'
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Nir-Diff Toc'
  ;  ;  cgHistoplot, diffNirToc, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='BLU1', /FillPolygon, DataColor='navy'
  ;  ;  winIndex++
  ;  ;  window, k mod 33, title='Nir-Diff Toa'
  ;  ;  cgHistoplot, diffNirToa, /Frequency, /OProbability, ProbColor='black', $
  ;  ;    ProbThick=thick, PolyColor='RED1', /FillPolygon, DataColor='navy'
  ;
  ;  ;sigma=sqrt(brfStat[1])
  ;  ;  sigma=brfStat[1]
  ;  ;  smoothIndexes=where(diffBrf ge brfStat[0]-2*sigma and diffBrf le brfStat[0]+2*sigma, cnt)
  ;  ;  ;PRINT, 'Mean: ', brfStat[0] & PRINT, 'Variance: ', brfStat[1] & $
  ;  ;  ;  PRINT, 'Skewness: ', brfStat[2] & PRINT, 'Kurtosis: ', brfStat[3]
  ;  ; Some stats...
  ;  ; nir
  ;  ; NL1
  ;  brfNirStat=moment(fullNirDataBrfL1, /NAN)
  ;  meanNirBRFL1=brfNirStat[0]
  ;  tocNirStat=moment(fullNirDataTocL1, /NAN)
  ;  meanNirTOCL1=tocNirStat[0]
  ;  toaNirStat=moment(fullNirDataToaL1, /NAN)
  ;  meanNirTOAL1=toaNirStat[0]
  ;  ; NL2
  ;  meanNirBRFL2=mean(fullNirDataBrfL2, /NAN)
  ;  meanNirTOCL2=mean(fullNirDataTocL2, /NAN)
  ;  meanNirTOAL2=mean(fullNirDataToaL2, /NAN)
  ;
  ;  ; red...
  ;  brfRedStat=moment(fullRedDataBrfL1, /NAN)
  ;  meanredBRFL1=brfRedStat[0]
  ;  tocRedStat=moment(fullRedDataTocL1, /NAN)
  ;  meanRedTOCL1=tocRedStat[0]
  ;  toaRedStat=moment(fullRedDataToaL1, /NAN)
  ;  meanRedTOAL1=toaRedStat[0]
  ;  ; NL2
  ;  meanRedBRFL2=mean(fullRedDataBrfL2, /NAN)
  ;  meanRedTOCL2=mean(fullRedDataTocL2, /NAN)
  ;  meanRedTOAL2=mean(fullRedDataToaL2, /NAN)
  ;
  ;  ; Try to cut off "noise" values (too high or too low)
  ;  ;smoothIndexes=where(fullNirDataBrfL1 ge brfNirStat[0]-sqrt(brfNirStat[1]) and fullNirDataBrfL1 le brfNirStat[0]+sqrt(brfNirStat[1]), cnt)
  ;  smoothIndexes=indgen(n_elements(diffNirBrf))
  ;  ;
  ;  ;
  ;  ;  a0=where((fullDataBrfL1 ge 0) and (fullDataBrfL1 le 1) and finite((fullDataBrfL1) eq 1), count0)
  ;  ;  a1=where((fullDataBrfL2 ge 0) and (fullDataBrfL2 le 1) and finite((fullDataBrfL2) eq 1), count1)
  ;  ;  meanBRFL1Clean=mean(fullDataBrfL1[a0], /NAN)
  ;  ;  meanBRFL2Clean=mean(fullDataBrfL2[a1], /NAN)
  ;  ;  ;aa=where(testBrfNirLineL1 lt 0, count1)
  ;  ;  ;bb=where(testBrfNirLineL1 gt 0, count2)
  ;  ;  ;cc=where(finite(testBrfNirLineL1) eq 0, count3)
  ;  ;
  ;  ;meanNirBRFL1=mean(fullNirDataBrfL1, /NAN)
  ;  ;meanNirTOCL1=mean(fullNirDataTocL1, /NAN)
  ;  ;meanNirTOAL1=mean(fullNirDataToaL1, /NAN)
  ;
  ;  ;
  ;  ;  print, meanBRFL1, meanBRFL2
  ;  ;  print, meanBRFL1Clean, meanBRFL2Clean
  ;  ;
  ;  ;  ;meanBRFDiff=mean(diffBrf, /NAN)
  ;  ;  ;meanTOCDiff=mean(diffToc, /NAN)
  ;  ;  ;meanTOADiff=mean(diffToa, /NAN)
  ;  ;  meanNirBRFDiff=meanNirBRFL2-meanNirBRFL1
  ;  ;  meanNirTOCDiff=meanTOCL2-meanNirTOCL1
  ;  ;  meanNirTOADiff=meanTOAL2-meanNirTOAL1
  ;  ;
  ;  ; day by day red L1
  ;  NL2Style=2
  ;  NL1Style=0
  ;
  ;  Ns=['N'+string(noaa1, format='(I02)')+' '+string(year1, format='(I4)')+' '+string(month1, format='(I02)'), 'N'+string(noaa2, format='(I02)')+' '+string(year2, format='(I4)')+' '+string(month2, format='(I02)')]
  ;  nstyles=[0,2]
  ;
  ;  ;bcolors=['Orange', 'Spring Green', 'Sky Blue']
  ;  bcolors=['Red', 'Green', 'Blue']
  ;  bands=['Brf', 'Toc', 'Toa']
  ;
  ;  wLength=['Red','Nir']
  ;  symbols=[4,6]
  ;
  ;  n=0
  ;  outstyles=intarr(6)
  ;  outcolors=strarr(6)
  ;  outsymbols=intarr(6)
  ;  titles=strarr(6)
  ;
  ;  m=1 ; set nir
  ;  n=0
  ;  for j=0, 1 do begin
  ;    for l=0, 2 do begin
  ;      titles[n]=Ns[j] + ' ' + bands[l] + ' ' + wLength[m]
  ;      outstyles[n]=nstyles[j]
  ;      outcolors[n]=bcolors[l]
  ;      outsymbols[n]=symbols[m]
  ;      n++
  ;    endfor
  ;  endfor
  ;
  ;  winIndex++
  ;  window, winIndex, title=''
  ;  n=0
  ;  ;  cgplot, meanListRedBrfL1, linestyle=outstyles[n], color=outcolors[n], background='snow', YRANGE=[0.2, 0.8], $
  ;  ;    title=string(n_elements(meanListRedBrfL2), format='(I2)')+' days, NL1 Profile, y='+string(latIndexes[0], format='(I4)')
  ;  ;  cgoplot, meanListRedBrfL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[m]
  ;  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListRedTocL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=5
  ;  ;  cgoplot, meanListRedTocL1, linestyle=outstyles[n], color=outcolors[n];, symbol=5
  ;  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListRedToaL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  cgoplot, meanListRedToaL1, linestyle=outstyles[n], color=outcolors[n]
  ;  ;  print, titles[n]
  ;  ;  n++
  ;  cgplot, meanListNirBrfL1, linestyle=outstyles[n], color=outcolors[n], background='snow', YRANGE=[min1-min1/10, max1+max1/10], title=wLength[m]+'-'+string(filesToCompare, format='(I2)')+' days - DaybyDay mean trend, y='+string(latIndexes[0], format='(I4)')
  ;  cgoplot, meanListNirBrfL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  print, titles[n]
  ;  n++
  ;  cgoplot, meanListNirTocL1, linestyle=outstyles[n], color=outcolors[n]
  ;  cgoplot, meanListNirTocL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  print, titles[n]
  ;  n++
  ;  cgoplot, meanListNirToaL1, linestyle=outstyles[n], color=outcolors[n]
  ;  cgoplot, meanListNirToaL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListRedBrfL2, linestyle=outstyles[n], color=outcolors[n]
  ;  ;  cgoplot, meanListRedBrfL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListRedTocL2, linestyle=outstyles[n], color=outcolors[n]
  ;  ;  cgoplot, meanListRedTocL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListRedToaL2, linestyle=outstyles[n], color=outcolors[n]
  ;  ;  cgoplot, meanListRedToaL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  print, titles[n]
  ;  n++
  ;  cgoplot, meanListNirBrfL2, linestyle=outstyles[n], color=outcolors[n]
  ;  cgoplot, meanListNirBrfL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  print, titles[n]
  ;  n++
  ;  cgoplot, meanListNirTocL2, linestyle=outstyles[n], color=outcolors[n]
  ;  cgoplot, meanListNirTocL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  print, titles[n]
  ;  n++
  ;  cgoplot, meanListNirToaL2, linestyle=outstyles[n], color=outcolors[n]
  ;  cgoplot, meanListNirToaL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  print, titles[n]
  ;  ;cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedBRFL1,meanRedBRFL1], color='Pale Green', linestyle=0, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedTOCL1,meanRedTOCL1], color='Dark Salmon', linestyle=0, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedTOAL1,meanRedTOAL1], color='Sky Blue', linestyle=0, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedBrfL2,meanRedBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedTOCL2,meanRedTOCL2], color='Light Salmon', linestyle=2, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedTOAL2,meanRedTOAL2], color='Blue', linestyle=2, thick=1.5, /DATA
  ;  ;cgtext, 0.8, 0.9, 'NL1', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;
  ;  cgLegend, BACKGROUND='gray', BG_COLOR='snow', /BOX, LOCATION=[0.7,0.95], $
  ;    titles=titles, linestyles=outstyles, colors=outcolors, psym=outsymbols, THICK=1.5
  ;
  ;  m=0 ; set red
  ;  n=0
  ;  for j=0, 1 do begin
  ;    for l=0, 2 do begin
  ;      titles[n]=Ns[j] + ' ' + bands[l] + ' ' + wLength[m]
  ;      outstyles[n]=nstyles[j]
  ;      outcolors[n]=bcolors[l]
  ;      outsymbols[n]=symbols[m]
  ;      n++
  ;    endfor
  ;  endfor
  ;
  ;  winIndex++
  ;  window, winIndex,  title=''
  ;  n=0
  ;  cgplot, meanListRedBrfL1, linestyle=outstyles[n], color=outcolors[n], background='snow', YRANGE=[min2-min2/10, max2+max2/10], $
  ;    title=wLength[m]+'-'+string(filesToCompare, format='(I2)')+' days - DaybyDay mean trend, y='+string(latIndexes[0], format='(I4)')
  ;  cgoplot, meanListRedBrfL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[m]
  ;  print, titles[n]
  ;  n++
  ;  cgoplot, meanListRedTocL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=5
  ;  cgoplot, meanListRedTocL1, linestyle=outstyles[n], color=outcolors[n];, symbol=5
  ;  print, titles[n]
  ;  n++
  ;  cgoplot, meanListRedToaL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  cgoplot, meanListRedToaL1, linestyle=outstyles[n], color=outcolors[n]
  ;  print, titles[n]
  ;  n++
  ;  ;  cgplot, meanListNirBrfL1, linestyle=outstyles[n], color=outcolors[n], background='snow', YRANGE=[0.2, 0.8]
  ;  ;  cgoplot, meanListNirBrfL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListNirTocL1, linestyle=outstyles[n], color=outcolors[n]
  ;  ;  cgoplot, meanListNirTocL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListNirToaL1, linestyle=outstyles[n], color=outcolors[n]
  ;  ;  cgoplot, meanListNirToaL1, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  print, titles[n]
  ;  ; n++
  ;  cgoplot, meanListRedBrfL2, linestyle=outstyles[n], color=outcolors[n]
  ;  cgoplot, meanListRedBrfL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  print, titles[n]
  ;  n++
  ;  cgoplot, meanListRedTocL2, linestyle=outstyles[n], color=outcolors[n]
  ;  cgoplot, meanListRedTocL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  print, titles[n]
  ;  n++
  ;  cgoplot, meanListRedToaL2, linestyle=outstyles[n], color=outcolors[n]
  ;  cgoplot, meanListRedToaL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListNirBrfL2, linestyle=outstyles[n], color=outcolors[n]
  ;  ;  cgoplot, meanListNirBrfL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListNirTocL2, linestyle=outstyles[n], color=outcolors[n]
  ;  ;  cgoplot, meanListNirTocL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  print, titles[n]
  ;  ;  n++
  ;  ;  cgoplot, meanListNirToaL2, linestyle=outstyles[n], color=outcolors[n]
  ;  ;  cgoplot, meanListNirToaL2, linestyle=outstyles[n], color=outcolors[n], psym=outsymbols[n];, symbol=6
  ;  ;  print, titles[n]
  ;  ;  cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedBRFL1,meanRedBRFL1], color='Pale Green', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedTOCL1,meanRedTOCL1], color='Dark Salmon', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedTOAL1,meanRedTOAL1], color='Sky Blue', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedBrfL2,meanRedBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedTOCL2,meanRedTOCL2], color='Light Salmon', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(meanListRedBrfL1)], [meanRedTOAL2,meanRedTOAL2], color='Blue', linestyle=2, thick=1.5, /DATA
  ;  ;cgtext, 0.8, 0.9, 'NL1', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  cgLegend, BACKGROUND='gray', BG_COLOR='snow', /BOX, LOCATION=[0.7,0.95], $
  ;    titles=titles, linestyles=outstyles, colors=outcolors, psym=outsymbols, THICK=1.5
  ;
  ;  ;;;;;
  ;
  ;  ;cgtext, 0.8,0.2, 'NL2', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;  cgLegend, COLORS=['Spring Green', 'Light Salmon', 'Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.2], $
  ;  ;    titles=['NL2 BRF * '+string(meanRedBRFL2, format='(F5.3)'), 'TOC * '+string(meanRedTOCL2, format='(F5.3)'), 'TOA * '+string(meanRedTOAL2, format='(F5.3)')], linestyles=[2,2,2], THICK=1.5
  ;
  ;  ; day by day nir L1
  ;  ;  winIndex++
  ;  ;  window, winIndex,  title='NIR - Day by Day Means - (1st Nov to 6th Nov)'
  ;  ;  cgplot, meanListNirBrfL1, linestyle=0, color='Pale Green', background=backcols[0], YRANGE=[0.0, 1.0], title=string(n_elements(meanListRedBrfL2), format='(I2)')+' days, NL1 Profile, y='+string(latIndexes[0], format='(I4)')
  ;  ;  cgoplot, meanListNirTocL1, linestyle=0, color='Dark Salmon';, symbol=5
  ;  ;  cgoplot, meanListNirToaL1, linestyle=0, color='Sky Blue';, symbol=6
  ;  ;  ;cgplots, [0,n_elements(meanListNirBrfL1)], [meanNirBRFL1,meanNirBRFL1], color='Pale Green', linestyle=0, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListNirBrfL1)], [meanNirTOCL1,meanNirTOCL1], color='Dark Salmon', linestyle=0, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListNirBrfL1)], [meanNirTOAL1,meanNirTOAL1], color='Sky Blue', linestyle=0, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListNirBrfL1)], [meanNirBrfL2,meanNirBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListNirBrfL1)], [meanNirTOCL2,meanNirTOCL2], color='Light Salmon', linestyle=2, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListNirBrfL1)], [meanNirTOAL2,meanNirTOAL2], color='Blue', linestyle=2, thick=1.5, /DATA
  ;  ;  ;cgtext, 0.8, 0.9, 'NL1', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;  cgLegend, COLORS=['Pale Green', 'Dark Salmon', 'Sky Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;    titles=['NL1 BRF * '+string(meanNirBRFL1, format='(F5.3)'), 'TOC * '+string(meanNirTOCL1, format='(F5.3)'), 'TOA * '+string(meanNirTOAL1, format='(F5.3)')], linestyles=[0,0,0], THICK=1.5
  ;  ;  ;cgtext, 0.8,0.2, 'NL2', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;;  cgLegend, COLORS=['Spring Green', 'Light Salmon', 'Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.2], $
  ;  ;;    titles=['NL2 BRF * '+string(meanNirBRFL2, format='(F5.3)'), 'TOC * '+string(meanNirTOCL2, format='(F5.3)'), 'TOA * '+string(meanNirTOAL2, format='(F5.3)')], linestyles=[2,2,2], THICK=1.5
  ;
  ;  ; day by day red L2
  ;  ;  winIndex++
  ;  ;  window, winIndex,  title='RED - Day by Day Means - (L2th Nov to 16th Nov)'
  ;  ;  cgplot, meanListRedBrfL2, linestyle=0, color='Pale Green', background=backcols[0], YRANGE=[0.0, 1.0], title=string(n_elements(meanListRedBrfL2), format='(I2)')+' days, NL2 Profile, y='+string(latIndexes[0], format='(I4)')
  ;  ;  cgoplot, meanListRedTocL2, linestyle=0, color='Dark Salmon';, symbol=5
  ;  ;  cgoplot, meanListRedToaL2, linestyle=0, color='Sky Blue';, symbol=6
  ;  ;  ;cgplots, [0,n_elements(meanListRedBrfL2)], [meanRedBRFL1,meanRedBRFL1], color='Pale Green', linestyle=0, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListRedBrfL2)], [meanRedTOCL1,meanRedTOCL1], color='Dark Salmon', linestyle=0, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListRedBrfL2)], [meanRedTOAL1,meanRedTOAL1], color='Sky Blue', linestyle=0, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListRedBrfL2)], [meanRedBrfL2,meanRedBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListRedBrfL2)], [meanRedTOCL2,meanRedTOCL2], color='Light Salmon', linestyle=2, thick=1.5, /DATA
  ;  ;  ;cgplots, [0,n_elements(meanListRedBrfL2)], [meanRedTOAL2,meanRedTOAL2], color='Blue', linestyle=2, thick=1.5, /DATA
  ;  ;  ;cgtext, 0.8, 0.9, 'NL1', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;;  cgLegend, COLORS=['Spring Green', 'Light Salmon', 'Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;;    titles=['NL1 BRF * '+string(meanRedBRFL1, format='(F5.3)'), 'TOC * '+string(meanRedTOCL1, format='(F5.3)'), 'TOA * '+string(meanRedTOAL1, format='(F5.3)')], linestyles=[2,2,2], THICK=1.5
  ;  ;  ;cgtext, 0.8,0.2, 'NL2', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;  cgLegend, COLORS=['Pale Green', 'Dark Salmon', 'Sky Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;    titles=['NL2 BRF * '+string(meanRedBRFL2, format='(F5.3)'), 'TOC * '+string(meanRedTOCL2, format='(F5.3)'), 'TOA * '+string(meanRedTOAL2, format='(F5.3)')], linestyles=[0,0,0], THICK=1.5
  ;
  ;  ; day by day nir L2
  ;  ;  winIndex++
  ;  ;  window, winIndex,  title='NIR - Day by Day Means - (L2th Nov to 16th Nov)'
  ;  ;  cgplot, meanListNirBrfL2, linestyle=0, color='Pale Green', background=backcols[0], YRANGE=[0.0, 1.0], title=string(n_elements(meanListRedBrfL2), format='(I2)')+' days, NL2 Profile, y='+string(latIndexes[0], format='(I4)')
  ;  ;  cgoplot, meanListNirTocL2, linestyle=0, color='Dark Salmon';, symbol=5
  ;  ;  cgoplot, meanListNirToaL2, linestyle=0, color='Sky Blue';, symbol=6
  ;  ;  cgoplot, meanListRedBrfL2, linestyle=2, color='Spring Green';, background=backcols[0], YRANGE=[0.0, 1.0], title=string(n_elements(meanListRedBrfL2), format='(I2)')+' days, NL2 Profile, y='+string(latIndexes[0], format='(I4)')
  ;  ;  cgoplot, meanListRedTocL2, linestyle=2, color='Light Salmon';, symbol=5
  ;  ;  cgoplot, meanListRedToaL2, linestyle=2, color='Blue';, symbol=6
  ;  ;cgplots, [0,n_elements(meanListNirBrfL2)], [meanNirBRFL2,meanNirBRFL2], color='Pale Green', linestyle=0, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListNirBrfL2)], [meanNirTOCL2,meanNirTOCL2], color='Dark Salmon', linestyle=0, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListNirBrfL2)], [meanNirTOAL2,meanNirTOAL2], color='Sky Blue', linestyle=0, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListNirBrfL1)], [meanNirBrfL1,meanNirBrfL1], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListNirBrfL1)], [meanNirTOCL1,meanNirTOCL1], color='Light Salmon', linestyle=2, thick=1.5, /DATA
  ;  ;cgplots, [0,n_elements(meanListNirBrfL1)], [meanNirTOAL1,meanNirTOAL1], color='Blue', linestyle=2, thick=1.5, /DATA
  ;  ;cgtext, 0.8, 0.9, 'NL1', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;  cgLegend, COLORS=['Spring Green', 'Light Salmon', 'Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;    titles=['NL1 BRF * '+string(meanNirBRFL1, format='(F5.3)'), 'TOC * '+string(meanNirTOCL1, format='(F5.3)'), 'TOA * '+string(meanNirTOAL1, format='(F5.3)')], linestyles=[2,2,2], THICK=1.5
  ;  ;cgtext, 0.8,0.2, 'NL2', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;  cgLegend, COLORS=['Pale Green', 'Dark Salmon', 'Sky Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;    titles=['NL2 BRF * '+string(meanNirBRFL2, format='(F5.3)'), 'TOC * '+string(meanNirTOCL2, format='(F5.3)'), 'TOA * '+string(meanNirTOAL2, format='(F5.3)')], linestyles=[0,0,0], THICK=1.5
  ;  ;stop
  ;  ;;; full aggregation...
  ;  ; plot NL1 NIR related band (6-days-profile)
  ;  ;  winIndex++
  ;  ;  window, winIndex,  title='NIR - 6Days - (1st Nov to 6th Nov)'
  ;  ;  cgplot, fullNirDataBrfL1[smoothIndexes], linestyle=0, color='Pale Green', background=backcols[0], YRANGE=[0.0, 1.0], title=string(n_elements(fullNirDataBrfL1[smoothIndexes]), format='(I5)')+' valid pixels, NL1 Profile, y='+string(latIndexes[0], format='(I4)')
  ;  ;  cgoplot, fullNirDataTocL1[smoothIndexes], linestyle=0, color='Dark Salmon';, symbol=5
  ;  ;  cgoplot, fullNirDataToaL1[smoothIndexes], linestyle=0, color='Sky Blue';, symbol=6
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL1[smoothIndexes])], [meanNirBRFL1,meanNirBRFL1], color='Pale Green', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL1[smoothIndexes])], [meanNirTOCL1,meanNirTOCL1], color='Dark Salmon', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL1[smoothIndexes])], [meanNirTOAL1,meanNirTOAL1], color='Sky Blue', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL1[smoothIndexes])], [meanNirBrfL2,meanNirBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL1[smoothIndexes])], [meanNirTOCL2,meanNirTOCL2], color='Light Salmon', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL1[smoothIndexes])], [meanNirTOAL2,meanNirTOAL2], color='Blue', linestyle=2, thick=1.5, /DATA
  ;  ;  ;cgtext, 0.8, 0.9, 'NL1', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;  cgLegend, COLORS=['Pale Green', 'Dark Salmon', 'Sky Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;    titles=['NL1 BRF * '+string(meanNirBRFL1, format='(F5.3)'), 'TOC * '+string(meanNirTOCL1, format='(F5.3)'), 'TOA * '+string(meanNirTOAL1, format='(F5.3)')], linestyles=[0,0,0], THICK=1.5
  ;  ;  ;cgtext, 0.8,0.2, 'NL2', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;  cgLegend, COLORS=['Spring Green', 'Light Salmon', 'Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.2], $
  ;  ;    titles=['NL2 BRF * '+string(meanNirBRFL2, format='(F5.3)'), 'TOC * '+string(meanNirTOCL2, format='(F5.3)'), 'TOA * '+string(meanNirTOAL2, format='(F5.3)')], linestyles=[2,2,2], THICK=1.5
  ;  ;  ;
  ;  ;  ; plot NL2 NIR related band (6-days-profile)
  ;  ;  winIndex++
  ;  ;  window, winIndex,  title='NIR - 6Days (9th Nov to 15th Nov)'
  ;  ;  cgplot, fullNirDataBrfL2[smoothIndexes],linestyle=2, color='Spring Green', background=backcols[0], YRANGE=[0.0, 1.0], title=string(n_elements(fullNirDataBrfL2[smoothIndexes]), format='(I5)')+' valid pixels, NL2 Profile, y='+string(latIndexes[0], format='(I4)')
  ;  ;  cgoplot, fullNirDataTocL2[smoothIndexes], linestyle=2, color='Light Salmon';, symbol=5
  ;  ;  cgoplot, fullNirDataToaL2[smoothIndexes], linestyle=2, color='Blue';, symbol=6
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL2[smoothIndexes])], [meanNirBRFL1,meanNirBRFL1], color='Pale Green', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL2[smoothIndexes])], [meanNirTOCL1,meanNirTOCL1], color='Dark Salmon', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL2[smoothIndexes])], [meanNirTOAL1,meanNirTOAL1], color='Sky Blue', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL2[smoothIndexes])], [meanNirBrfL2,meanNirBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL2[smoothIndexes])], [meanNirTOCL2,meanNirTOCL2], color='Light Salmon', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullNirDataTocL2[smoothIndexes])], [meanNirTOAL2,meanNirTOAL2], color='Blue', linestyle=2, thick=1.5, /DATA
  ;  ;  cgLegend, COLORS=['Spring Green', 'Light Salmon', 'Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;    titles=['NL2 BRF * '+string(meanNirBrfL2, format='(F5.3)'), 'TOC * '+string(meanNirTOCL2, format='(F5.3)'), 'TOA * '+string(meanNirTOAL2, format='(F5.3)')], linestyles=[2,2,2], THICK=1.5
  ;  ;  cgLegend, COLORS=['Pale Green', 'Dark Salmon', 'Sky Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.2], $
  ;  ;    titles=['NL1 BRF * '+string(meanNirBRFL1, format='(F5.3)'), 'TOC * '+string(meanNirTOCL1, format='(F5.3)'), 'TOA * '+string(meanNirTOAL1, format='(F5.3)')], linestyles=[0,0,0], THICK=1.5
  ;  ;
  ;  ;  ; plot NL1 NIR related band (6-days-profile)
  ;  ;  winIndex++
  ;  ;  window, winIndex,  title='RED - 6Days - (1st Nov to 6th Nov)'
  ;  ;  cgplot, fullRedDataBrfL1[smoothIndexes], linestyle=0, color='Pale Green', background=backcols[0], YRANGE=[0.0, 1.0], title=string(n_elements(fullRedDataBrfL1[smoothIndexes]), format='(I5)')+' valid pixels, NL1 Profile, y='+string(latIndexes[0], format='(I4)')
  ;  ;  cgoplot, fullRedDataTocL1[smoothIndexes], linestyle=0, color='Dark Salmon';, symbol=5
  ;  ;  cgoplot, fullRedDataToaL1[smoothIndexes], linestyle=0, color='Sky Blue';, symbol=6
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL1[smoothIndexes])], [meanRedBRFL1,meanRedBRFL1], color='Pale Green', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL1[smoothIndexes])], [meanRedTOCL1,meanRedTOCL1], color='Dark Salmon', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL1[smoothIndexes])], [meanRedTOAL1,meanRedTOAL1], color='Sky Blue', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL1[smoothIndexes])], [meanRedBrfL2,meanRedBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL1[smoothIndexes])], [meanRedTOCL2,meanRedTOCL2], color='Light Salmon', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL1[smoothIndexes])], [meanRedTOAL2,meanRedTOAL2], color='Blue', linestyle=2, thick=1.5, /DATA
  ;  ;  ;cgtext, 0.8, 0.9, 'NL1', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;  cgLegend, COLORS=['Pale Green', 'Dark Salmon', 'Sky Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;    titles=['NL1 BRF * '+string(meanRedBRFL1, format='(F5.3)'), 'TOC * '+string(meanRedTOCL1, format='(F5.3)'), 'TOA * '+string(meanRedTOAL1, format='(F5.3)')], linestyles=[0,0,0], THICK=1.5
  ;  ;  ;cgtext, 0.8,0.2, 'NL2', /NORM, ALIGN=0.5, color='Black', charsize=1.6
  ;  ;  cgLegend, COLORS=['Spring Green', 'Light Salmon', 'Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.2], $
  ;  ;    titles=['NL2 BRF * '+string(meanRedBRFL2, format='(F5.3)'), 'TOC * '+string(meanRedTOCL2, format='(F5.3)'), 'TOA * '+string(meanRedTOAL2, format='(F5.3)')], linestyles=[2,2,2], THICK=1.5
  ;  ;
  ;  ;  smoothIndexes=indgen(n_elements(diffRedBrf))
  ;  ;
  ;  ;  winIndex++
  ;  ;  window, winIndex,  title='Red - 6Days (9th Nov to 15th Nov)'
  ;  ;  cgplot, fullRedDataBrfL2[smoothIndexes],linestyle=2, color='Spring Green', background=backcols[0], YRANGE=[0.0, 1.0], title=string(n_elements(fullRedDataBrfL2[smoothIndexes]), format='(I5)')+' valid pixels, NL2 Profile, y='+string(latIndexes[0], format='(I4)')
  ;  ;  cgoplot, fullRedDataTocL2[smoothIndexes], linestyle=2, color='Light Salmon';, symbol=5
  ;  ;  cgoplot, fullRedDataToaL2[smoothIndexes], linestyle=2, color='Blue';, symbol=6
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL2[smoothIndexes])], [meanRedBRFL1,meanRedBRFL1], color='Pale Green', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL2[smoothIndexes])], [meanRedTOCL1,meanRedTOCL1], color='Dark Salmon', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL2[smoothIndexes])], [meanRedTOAL1,meanRedTOAL1], color='Sky Blue', linestyle=0, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL2[smoothIndexes])], [meanRedBrfL2,meanRedBrfL2], color='Spring Green', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL2[smoothIndexes])], [meanRedTOCL2,meanRedTOCL2], color='Light Salmon', linestyle=2, thick=1.5, /DATA
  ;  ;  cgplots, [0,n_elements(fullRedDataTocL2[smoothIndexes])], [meanRedTOAL2,meanRedTOAL2], color='Blue', linestyle=2, thick=1.5, /DATA
  ;  ;  cgLegend, COLORS=['Spring Green', 'Light Salmon', 'Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;    titles=['NL2 BRF * '+string(meanRedBrfL2, format='(F5.3)'), 'TOC * '+string(meanRedTOCL2, format='(F5.3)'), 'TOA * '+string(meanRedTOAL2, format='(F5.3)')], linestyles=[2,2,2], THICK=1.5
  ;  ;  cgLegend, COLORS=['Pale Green', 'Dark Salmon', 'Sky Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.2], $
  ;  ;    titles=['NL1 BRF * '+string(meanRedBRFL1, format='(F5.3)'), 'TOC * '+string(meanRedTOCL1, format='(F5.3)'), 'TOA * '+string(meanRedTOAL1, format='(F5.3)')], linestyles=[0,0,0], THICK=1.5
  ;  ;
  ;  ;  ;!p.MULTI=[1,1]
  ;  ;  ;
  ;  ;  ;  winIndex++
  ;  ;  ;  window, winIndex,  title='NIR (Diff) - 6Days'
  ;  ;  ;  cgplot, diffBrf[smoothIndexes],linestyle=0, color='Green', background=backcols[0], YRANGE=[-0.6, 0.6], title='Only valid pixels (Difference Profile, y='+string(latIndexes[0], format='(I4)')+')'
  ;  ;  ;  cgoplot, diffToc[smoothIndexes], linestyle=0, color='Red', symbol=5
  ;  ;  ;  cgoplot, diffToa[smoothIndexes], linestyle=0, color='Cyan', symbol=6
  ;  ;  ;  cgplots, [0,n_elements(fullDataTocL2[smoothIndexes])], [0.,0.], color='Black', linestyle=1, thick=1.5, /DATA
  ;  ;  ;  cgplots, [0,n_elements(fullDataTocL2[smoothIndexes])], [meanBRFDiff,meanBRFDiff], color='Green', linestyle=2, thick=1.5, /DATA
  ;  ;  ;  cgplots, [0,n_elements(fullDataTocL2[smoothIndexes])], [meanTOCDiff,meanTOCDiff], color='Red', linestyle=2, thick=1.5, /DATA
  ;  ;  ;  cgplots, [0,n_elements(fullDataTocL2[smoothIndexes])], [meanTOADiff,meanTOADiff], color='Cyan', linestyle=2, thick=1.5, /DATA
  ;  ;  ;  cgLegend, COLORS=['Green', 'Red', 'Cyan'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.75,0.9], $
  ;  ;  ;    titles=['Diff BRF * '+string(meanBRFDiff, format='(F6.3)'), 'Diff TOC * '+string(meanTocDiff, format='(F6.3)'), 'Diff TOA * '+string(meanToaDiff, format='(F6.3)')], linestyles=[2,2,2], THICK=1.2
  ;  ;  ;
  ;  ;  ;  lower=where(diffBrf le -0.1 , countLower)
  ;  ;  ;  higher=where(diffBrf ge 0.1, countHigher)
  ;  ;  ;  loadct,12
  ;  ;  ;  winIndex++
  ;  ;  ;  a=cghistogram(diffBrf, binsize=0.05, /NAN, min=-0.7, max=0.7)
  ;  ;  ;  xAxisValues=string((findgen(n_elements(a))/20.)-0.7, format='(F5.2)')
  ;  ;  ;  window, winIndex,  title='Histo diff Brf'
  ;  ;  ;  cgbarplot, [countLower,a,countHigher], background='Gray', BARNAMES=['tot <= -0.1', xAxisValues, 'tot >= 0.1'], charsize=0.8
  ;  ;  ;
  ;  ;  ;  lower=where(diffToc le -0.1 , countLower)
  ;  ;  ;  higher=where(diffToc ge 0.1, countHigher)
  ;  ;  ;  loadct,12
  ;  ;  ;  winIndex++
  ;  ;  ;  a=cghistogram(diffToc, binsize=0.05, /NAN, min=-0.7, max=0.7)
  ;  ;  ;  xAxisValues=string((findgen(n_elements(a))/20.)-0.7, format='(F5.2)')
  ;  ;  ;  window, winIndex,  title='Histo diff Toc'
  ;  ;  ;  cgbarplot, [countLower,a,countHigher], background='Gray', BARNAMES=['tot <= -0.1', xAxisValues, 'tot >= 0.1'], charsize=0.8
  ;  ;  ;
  ;  ;  ;  lower=where(diffToa le -0.1 , countLower)
  ;  ;  ;  higher=where(diffToa ge 0.1, countHigher)
  ;  ;  ;  loadct,12
  ;  ;  ;  winIndex++
  ;  ;  ;  a=cghistogram(diffToa, binsize=0.05, /NAN, min=-0.7, max=0.7)
  ;  ;  ;  xAxisValues=string((findgen(n_elements(a))/20.)-0.7, format='(F5.2)')
  ;  ;  ;  window, winIndex,  title='Histo diff Toa'
  ;  ;  ;  cgbarplot, [countLower,a,countHigher], background='Gray', BARNAMES=['tot <= -0.1', xAxisValues, 'tot >= 0.1'], charsize=0.8
  ;  ;  ;
  ;  ;  ;  winIndex++
  ;  ;  ;  window, winIndex,  title='FULL DATA SCATTER - NIR Toa Vs Toc (NL1)'
  ;  ;  ;  cgscatter2d,  fullNirDataToaL1[1:*], fullNirDataTocL1[1:*], xrange=[0., 1.], yrange=[0., 1.], background=backcols[0]
  ;  ;  ;;
  ;  ;  ;  winIndex++
  ;  ;  ;  window, winIndex,  title='FULL DATA SCATTER - NIR Brf Vs Toc (NL1)'
  ;  ;  ;  cgscatter2d,  fullNirDataBrfL1[1:*], fullNirDataTocL1[1:*], xrange=[0., 1.], yrange=[0., 1.], background=backcols[0], XTitle='Brf', YTitle='Toc'
  ;  ;  ;
  ;  ;  ;  winIndex++
  ;  ;  ;  window, winIndex,  title='FULL DATA SCATTER - NIR Toa Vs Brf (NL1)'
  ;  ;  ;  cgscatter2d,  fullNirDataToaL1[1:*], fullNirDataBrfL1[1:*], xrange=[0., 1.], yrange=[0., 1.], background=backcols[0]
  ;  ;  ;
  ;  ;  ;  winIndex++
  ;  ;  ;  window, winIndex,  title='FULL DATA SCATTER - NIR Toa Vs Toc (NL2)'
  ;  ;  ;  cgscatter2d,  fullDataToaL2[1:*], fullDataTocL2[1:*], xrange=[0., 1.], yrange=[0., 1.], background=backcols[1]
  ;  ;  ;;
  ;  ;  ;  winIndex++
  ;  ;  ;  window, winIndex,  title='FULL DATA SCATTER - NIR Brf Vs Toc (NL2)'
  ;  ;  ;  cgscatter2d,  fullNirDataBrfL2[1:*], fullDataTocL2[1:*], xrange=[0., 1.], yrange=[0., 1.], background=backcols[1], XTitle='Brf', YTitle='Toc'
  ;  ;  ;;
  ;  ;  ;  winIndex++
  ;  ;  ;  window, winIndex,  title='FULL DATA SCATTER - NIR Toa Vs Brf (NL2)'
  ;  ;  ;  cgscatter2d,  fullDataToaL2[1:*], fullNirDataBrfL2[1:*], xrange=[0., 1.], yrange=[0., 1.], background=backcols[1]
  ;
  ;  ;return, 1

END