;testCompareFAPAR, 41, -96, 1999, 6, 14, /TO_PIX, /ten
pro testCompareFAPAR, latPos, lonPos, year, month, noaa, TO_PIX=TO_PIX, RESET=RESET, RED=RED, NIR=NIR, TEN=TEN;, sourceFile, confDir, year, month, day, sensor, missionCode, noaaCode, resolution, mainVar, outputBaseDir, tempDir, $
  ;testFile=testFile, OVERWRITE=OVERWRITE, HDF=HDF, NC=NC

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem
  COMMON winInfo, winIndex
  ; pixel_position=[2620, 1680]
  declareSingleTons
  if keyword_set(TO_PIX) then begin
    pixLatPos=fix((float(latPos)+90)/.05)
    pixLonPos=fix((float(lonPos)+180)/.05)
  endif else begin
    pixLatPos=fix(latPos)
    pixLonPos=fix(lonPos)
  endelse

  if n_elements(winIndex) eq 0 then winIndex=0
  workDir='E:\mariomi\Desktop\fapar_presentation'
  ;workDir='/home/mariomi/data/test/tc'
  workDir=ST_fileSystem->adjustDirSep(workDir, /ADD)

  ; close all windows...
  fixString='_900S900N1800W1800E_0005D_FPA_'
  dailyFaparFiles=file_search(workDir, 'AVH_'+string(year, format='(I4)')+string(month, format='(I02)')+'??_001D'+fixString+'N'+string(noaa, format='(I02)')+'.NC')
  ;monthly
  monthlyFaparFiles=file_search(workDir, 'AVH_'+string(year, format='(I4)')+string(month, format='(I02)')+'??_001M'+fixString+'*.NC')
  ;10 D
  if keyword_set(TEN) then monthlyFaparFiles=file_search(workDir, 'AVH_'+string(year, format='(I4)')+string(month, format='(I02)')+'??_010D'+fixString+'*.NC')
  days=n_elements(dailyFaparFiles)
  if keyword_set(RESET) then cgCleanup
  if keyword_set(ten) then days=10

  myDailyPixFapar=fltarr(days)
  myDailyPixSigma=fltarr(days)
  ext='NC'

  selDailyMain='FAPAR'
  selDailySigma='Sigma_FAPAR'
  tcMain='FAPAR'
  tcSigma='Dev_Temp_FAPAR'
  title='FAPAR'
  if keyword_set(RED) then begin
    selDailyMain='rectified_band_1'
    selDailySigma='Sigma_RECTIFIED_BAND_1'
    tcMain='rectified_red'
    tcSigma='Dev_Temp_Red'
    title='Red'
  endif
  if keyword_set(NIR) then begin
    selDailyMain='rectified_band_2'
    selDailySigma='Sigma_RECTIFIED_BAND_2'
    tcMain='rectified_nir'
    tcSigma='Dev_Temp_Nir'
    title='Nir'
  endif

  for i=0, days-1 do begin
    print, dailyFaparFiles[i]
    faparName=ST_fileSystem->getFileNameInfo(dailyFaparFiles[i], filePath=filePath, extension=extension)
    infoVarFPA=[selDailyMain, workDir+faparName, faparName, workDir, ext, 'input_data[1,*]/fapar']
    infoVarFPA_Sigma=[selDailySigma, workDir+faparName, faparName, workDir, ext, 'input_data[1,*]/sigma_fapar']

    ncdfread, infoVarFPA[1], infoVarFPA[0], fapar, fapar_slope, fapar_offset, dim, fapar_fillvalue, ERROR=ERROR
    noIdxs=where(fapar eq fapar_fillvalue, count)
    fapar=1.*fapar*fapar_slope+fapar_offset
    if count ne 0 then fapar[noIdxs]=!VALUES.F_NAN
    myDailyPixFapar[i]=fapar[pixLonPos, pixLatPos]

    ncdfread, infoVarFPA_Sigma[1], infoVarFPA_Sigma[0], sigma_fapar, sigma_fapar_slope, sigma_fapar_offset, dim, sigma_fapar_fillvalue, ERROR=ERROR
    noIdxs=where(sigma_fapar eq sigma_fapar_fillvalue, count)
    sigma_fapar=1.*sigma_fapar*sigma_fapar_slope+sigma_fapar_offset

    if count ne 0 then sigma_fapar[noIdxs]=!VALUES.F_NAN
    myDailyPixSigma[i]=sigma_fapar[pixLonPos, pixLatPos]

  endfor

  restore, filename='first_distance.sav'
  restore, filename='first_meandat.sav'
  restore, filename='first_std_mean.sav'
  offset=1440
  ; distance from call mean 3 computation
  myDailyPixDistance=reform(distance[*,pixLonPos-offset, pixLatPos])
  ; mean...
  myDailyPixmeandat=meandat.fapar[pixLonPos-offset, pixLatPos]
  ; std dev...
  myDailyPixstd_mean=std_mean.temp[pixLonPos-offset, pixLatPos]
  restore, 'new_distance.sav'
  restore, 'new_meandat.sav'
  restore, 'new_std_mean.sav'
  myDailyPixNewDistance=reform(distance[*,pixLonPos-offset, pixLatPos])
  myDailyPixNewmeandat=meandat.fapar[pixLonPos-offset, pixLatPos]
  myDailyPixNewstd_mean=std_mean.temp[pixLonPos-offset, pixLatPos]
  
  faparName=ST_fileSystem->getFileNameInfo(monthlyFaparFiles[0], filePath=filePath, extension=extension)

  infoVarFPA=[tcMain, workDir+faparName, faparName, workDir, ext, 'input_data[1,*]/fapar']
  infoVarFPA_dev_temp=[tcSigma, workDir+faparName, faparName, workDir, ext, 'input_data[1,*]/Dev_Temp_FAPAR']

  ncdfread, infoVarFPA[1], infoVarFPA[0], fapar, fapar_slope, fapar_offset, dim, fapar_fillvalue, ERROR=ERROR
  noIdxs=where(fapar eq fapar_fillvalue, count)
  fapar=1.*fapar*fapar_slope+fapar_offset

  ncdfread, infoVarFPA_dev_temp[1], infoVarFPA_dev_temp[0], dev_temp_fapar, dev_temp_fapar_slope, dev_temp_fapar_offset, dim, dev_temp_fapar_fillvalue, ERROR=ERROR
  noIdxs=where(dev_temp_fapar eq dev_temp_fapar_fillvalue, count)
  dev_temp_fapar=1.*dev_temp_fapar*dev_temp_fapar_slope+dev_temp_fapar_offset

  scr_dims=GET_SCREEN_SIZE()
  ;pixLonPos=3600 & pixLatPos=1800
  ;lonSurroundings=[(fix(pixLonPos)-25)>0:(fix(pixLonPos)+25)<(7200-1)]
  ;latSurroundings=[(fix(pixLatPos)-25)>0:(fix(pixLatPos)+25)<(3600-1)]


  ;window, 1, XSIZE=720, YSIZE=360
  tvfapar=fapar
  tvfapar[(fix(pixLonPos)-50)>0:(fix(pixLonPos)+50)<(7200-1), (fix(pixLatPos)-50)>0:(fix(pixLatPos)+50)<(3600-1)]=1
  ;tvscl, congrid(tvfapar, 720, 360)

  myMonthlyPixFapar=fapar[pixLonPos, pixLatPos]
  myMonthlyPixDevTemp=dev_temp_fapar[pixLonPos, pixLatPos]

  ;high_error = (data + cgScaleVector(RandomU(seed, N_Elements(data)), 3, 7)) < 35
  ;low_error = (data - cgScaleVector(RandomU(seed, N_Elements(data)), 2, 6)) > (-5)

  ;time=string(indgen(days)+1, format='(I02)')
  tick_names=string(indgen(days)+1, format='(I02)')
  time=float(indgen(days)+1)
  data=myDailyPixFapar
  ;ERR_YLow=(data-myDailyPixSigma)>0
  ;ERR_YHIGH=data+myDailyPixSigma
  ERR_YLow=(myDailyPixSigma)>0
  ERR_YHIGH=myDailyPixSigma

  idxToNan=where(data le 0 or finite(data) ne 1, zeroes)
  if zeroes gt 0 then begin
    data[idxToNan]=!VALUES.F_NAN
    ERR_YLow[idxToNan]=!VALUES.F_NAN
    ERR_YHIGH[idxToNan]=!VALUES.F_NAN
  endif

  ;  PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
  ;    PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
  ;  Mean:       66.7333
  ;  Variance:    7.06667
  ;  Skewness:   -0.0942851
  ;  Kurtosis:   -1.18258

  ;ERR_YLow=(-myDailyPixSigma)>0
  ;ERR_YHIGH=myDailyPixSigma
  fullStat=moment(data, /NAN)
  fullMean=fullStat[0]
  fullStdDev=sqrt(fullStat[1])

  maxx=max(data+ERR_YHIGH, /NAN)
  yrange=[0, maxx+maxx/10.]

  devErr=fltarr(days)
  highDevErr=devErr
  lowDevErr=devErr

  highDevErr[*]=myMonthlyPixFapar+myMonthlyPixDevTemp
  lowDevErr[*]=myMonthlyPixFapar-myMonthlyPixDevTemp

  ;cgtext, 'Lat Position':latPos
  ;cgtext, 'Lon Position':lonPos

  window, 1, XSIZE=float(scr_dims[0])/4*3, YSIZE=float(scr_dims[1])/4*3

  LatLabel='Lat:'+string(latPos, format='(f8.3)')+'deg'
  LonLabel='Lon:'+string(lonPos, format='(f8.3)')+'deg'
  MTitle='Daily ('+LatLabel+' '+LonLabel+')'
  XTitle='June 1999'
  Ytitle=Title

  ; data serie
  ;sub=[0,4,9,14,19,24,29]
  cgPlot, time, data, Title=MTitle, XTitle=xtitle, YTitle=ytitle, $
    Position=position, color='Red', YRANGE=YRANGE, /LOWER_ZERO

  ; tc mean+stddev filling
  cgColorFill, [time, Reverse(time), time[0]], $
    [highDevErr, Reverse(lowDevErr), highDevErr[0]], $
    Color='sky blue', /LINE_FILL, SPACING=0.15, ORIENT=45

  ; tc mean+stddev border
  cgPlots, [time, Reverse(time), time[0]], $
    [highDevErr, Reverse(lowDevErr), highDevErr[0]], $
    Color='sky blue'

  ; TC mean...
  cgOPlot, [min(time), max(time)], [myMonthlyPixFapar,myMonthlyPixFapar] , Title=title, XTitle=xtitle, YTitle=ytitle, $
    Position=position, linestyle=2, color='sky blue', thick=1.3

  ;; ***
  ; Full serie (without zero and Nan
  cgOPlot, time, data, Title=title, XTitle=xtitle, YTitle=ytitle, $
    Position=position, color='Red', /ERR_SHAPE, $
    ERR_YLow=ERR_YLow, ERR_YHIGH=ERR_YHIGH, psym=4, /LOWER_ZERO;, YRange=[-5, 35], YStyle=1

  ; Full serie (psym marker)
  cgOPlot, time, data, Title=title, XTitle=xtitle, YTitle=ytitle, $
    Position=position, linestyle=2, color='Red'

  ; Full serie mean
  cgOPlot, [min(time), max(time)], [fullMean,fullMean], $
    Position=position, linestyle=1, color='Red', thick=2.

  ; First cut-off
  firstMoment=moment(data, /NAN)
  firstMean=firstMoment[0]
  firstStdDev=sqrt(firstMoment[1])
  firstOut=where((data ge firstMean+firstStdDev or data le firstMean-firstStdDev) and finite(data) ne 0, countFirst)

  countSecond=0
  if countFirst gt 0 then begin
    data[firstOut]=!VALUES.F_NAN
    firstMoment=moment(data, /NAN)
    firstMean=firstMoment[0]
    firstStdDev=sqrt(firstMoment[1])
    ERR_YLow=(firstStdDev)>0
    ERR_YHIGH=firstStdDev
    secondOut=where((data ge firstMean+firstStdDev or data le firstMean-firstStdDev) and finite(data) ne 0, countSecond)
  endif

  devErr=fltarr(days)
  highDevErr=devErr
  lowDevErr=devErr

  highDevErr[*]=fullMean+fullStdDev
  lowDevErr[*]=fullMean-fullStdDev

  cgColorFill, [time, Reverse(time), time[0]], $
    [highDevErr, Reverse(lowDevErr), highDevErr[0]], $
    Color='Red', /LINE_FILL, SPACING=0.15, ORIENT=135

  ; Filter 1 serie (without zero and Nan)
  cgOPlot, time+0.15, data, Title=title, XTitle=xtitle, YTitle=ytitle, $
    Position=position, color='Green', psym=2;, /ERR_SHAPE, $
  ;ERR_YLow=ERR_YLow, ERR_YHIGH=ERR_YHIGH, psym=2, /LOWER_ZERO;, YRange=[-5, 35], YStyle=1

  ; Filter 1 serie (psym marker)
  cgOPlot, time+0.15, data, Title=title, XTitle=xtitle, YTitle=ytitle, $
    Position=position, linestyle=2, color='Green'

  ; Filter 1 mean
  cgOPlot, [min(time), max(time)], [firstMean,firstMean], $
    Position=position, linestyle=2, color='Green', thick=1.3

  ; Second cut-off
  secondMean=firstMean
  secondStdDev=firstStdDev
  if countSecond gt 0 then begin
    data[SecondOut]=!VALUES.F_NAN
    secondMoment=moment(data, /NAN)
    secondMean=secondMoment[0]
    secondStdDev=sqrt(secondMoment[1])
    ERR_YLow=(secondStdDev)>0
    ERR_YHIGH=secondStdDev
    ;secondOut=where(data ge firstMean+firstStdDev and data le firstMean-firstStdDev and finite(data) ne 0, countSecond)
  endif

  ; More TC Info
;  cgOPlot, myDailyPixDistance/max(myDailyPixDistance)*YRANGE[1], $
;    color='Pink', thick=2.0;, /LEGO
  
;  myDailyPixDistance=reform(distance[*,pixLonPos-offset, pixLatPos])
;  myDailyPixmeandat=meandat.fapar[pixLonPos-offset, pixLatPos]
;  myDailyPixstd_mean=std_mean.temp[pixLonPos-offset, pixLatPos]
;
;  myDailyPixNewDistance=reform(distance[*,pixLonPos-offset, pixLatPos])
;  myDailyPixNewmeandat=meandat.fapar[pixLonPos-offset, pixLatPos]
;  myDailyPixNewstd_mean=std_mean.temp[pixLonPos-offset, pixLatPos]

  ; 
  ; Filter 2 serie (without zero and Nan)
  ;cgOPlot, time+0.3, data, Title=title, XTitle=xtitle, YTitle=ytitle, $
  ;  Position=position, color='Blue', /ERR_SHAPE, $
  ;  ERR_YLow=ERR_YLow, ERR_YHIGH=ERR_YHIGH, psym=5, /LOWER_ZERO;, YRange=[-5, 35], YStyle=1

  ; Filter 2 serie (psym marker)
  ;cgOPlot, time+0.3, data, Title=title, XTitle=xtitle, YTitle=ytitle, $
  ;  Position=position, linestyle=2, color='Blue'

  ; Filter 2 mean
  ;cgOPlot, [min(time), max(time)], [secondMean,secondMean], $
  ;  Position=position, linestyle=3, color='Blue', thick=1.1

  cgLegend, COLORS=['Red', 'Green', 'Sky Blue'], BACKGROUND='snow', BG_COLOR='snow', /BOX, LOCATION=[0.7,0.9], $
    titles=['Original (mean + stddev)', 'After Outliers (mean)', 'TC computation']

  device, decompose=0
  loadct, 73
  tvlct,r,g,b, /get
  r[254]=0 & g[254]=0 & b[254]=255
  r[255]=0 & g[255]=0 & b[255]=255
  tvlct,r,g,b
  tvscl, congrid(tvfapar, 240, 120), 0

  ;myMonthlyPixFaparM=float(time)
  ;myMonthlyPixFaparM[*]=myMonthlyPixFapar
  ;  window, 2
  ;  cgHistoplot, fullRedDataBrfL1, /Frequency, /OProbability, ProbColor='black', $
  ;    ProbThick=thick, PolyColor='GRN4', /FillPolygon, DataColor='navy', BINSIZE=0.05, MININPUT=0.0, MAXINPUT=1.4


  print, '**'
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

END