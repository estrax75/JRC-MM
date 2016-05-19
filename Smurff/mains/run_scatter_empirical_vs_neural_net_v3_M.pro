PRO run_scatter_empirical_vs_neural_net_v3_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  yearList=request->getYearList()
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  
  SCATTERGRAPHICFORMAT=strupcase(mainApp->getKeyValue('OUT_GRAPHIC_FORMAT'))  ;NOTDEFINED
  if SCATTERGRAPHICFORMAT eq '' then begin
    SCATTERGRAPHICFORMAT='jpeg'
  endif
  SCATTERGRAPHICFORMAT='eps'
  
  monthList=request->getMonthList()
  
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByPriority(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getroiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
  inputDirs=request->getInputDir()
  inputDirs=strsplit(inputDirs, ';', /EXTRACT)
  outputDir=request->getoutputDir()
  inputFileFilter=request->getInputFileFilter()
  inputParameterList=request->getinputParameterList(NOTFOUND=NOTFOUND)
  numInPar=n_elements(inputParameterList)
  
  ;  readProcedureList='' & varList=''
  ;  for i=0, numInPar-1 do begin
  ;    pars=strsplit(inputParameterList[i], '$', /EXTRACT)
  ;    readProcedureList=[readProcedureList, pars[0]]
  ;    varList=[varList, pars[1]]
  ;  endfor
  ;  readProcedureList=readProcedureList[1:*] & varList=varList[1:*]
  
  parInfos=extractParameterStruct(inputParameterList[0])
  parInfos=replicate(parInfos, numInPar)
  for i=1, numInPar-1 do parInfos[i]=extractParameterStruct(inputParameterList[i])
  labelList=parInfos[*].label
  varList=parInfos[*].id
  intermediateElabList=parInfos[*].stat
  readFunctionList=parInfos[*].getFunction
  extractFlagList=parInfos[*].extractFlag
  
  overWriteFlag=request->getOverwriteResultFlag()
  deleteInputFlag=request->getDeleteInputFlag()
  periodType=request->getPeriodType()
  destRoiCode=request->getOutputRoi()
  
  if periodType ne 'M' then message, 'This run is designed for working with monthly (last biomap) data, you need to (copy and) custom it if you want a different interval period and/or data'
  if n_elements(inputParameterList) ne 2 then begin
    doLog, 'Scatter mode needs two (and only two) input parameters', level=4
    return
  endif
  
  nLoops=n_elements(yearList)*n_elements(monthList)*n_elements(roiCodeList)
  doLog, /STACK, callingRoutine=callingRoutine
  title='processing: '+callingRoutine
  progCount=1
  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  
  for i=0, n_elements(yearList)-1 do begin
    year=yearList[i]
    year_str=string(format='(I4)', year)
    physicals=mainApp->getPhysicalFromYear(year)
    physical=physicals[0]
    for j=0, n_elements(monthList)-1 do begin
      month=monthList[j]
      for k=0, n_elements(roiCodeList)-1 do begin
        refRoiCode=mainApp->getRoiRefRoiCodesByCodes(roiCodeList[k])
        roiInfo=mainApp->getROIInfoByCode(roiCodeList[k])
        if refRoiCode eq '' or refRoiCode eq 'N/A' then delIdlVar, refRoiCode;refRoi=mainApp->getROIInfoByCode(refRoiCode)
        pathSep = path_sep()
        firstDay = julday(month,1, year) - julday(1,1, year) + 1;
        howManyDays=julday((month ne 12) ? (month+1) : 1, 1, (month ne 12) ? year : (year+1)) - julday(month,1, year);
        lastDay=firstDay+howManyDays-1
        envifiles=strarr(howManyDays)
        NOTFOUND=0
        ;for day=firstDay, lastDay do begin
        doLog,'year: ', yearList[i], 'month: ', monthList[j]
        ;data1 = call_function(readProcedureList[0]+'_'+periodType, periodType, monthList[j], yearList[i], roiCodeList[k], roiArchiveList[k], inputDirs[0], outputDir, $
        data1 = call_function(readFunctionList[0]+'_'+periodType, periodType, monthList[j], yearList[i], roiCodeList[k], roiArchiveList[k], inputDirs[0], outputDir, $
          varList[0], NOTFOUND=NOTFOUND, refRoi=refRoiCode, GETCHLVAR='ALG_CHL' eq varList[0], /SETNAN)
        ;varList[0], destRoiCode, NOTFOUND=NOTFOUND, refRoi=refRoiCode, GETCHLVAR='ALG_CHL' eq varList[0])
        ;if ~keyword_set(NOTFOUND) then data2 = call_function(readProcedureList[1]+'_'+periodType, periodType, monthList[j], yearList[i], roiCodeList[k], roiArchiveList[k], inputDirs[1], outputDir, $
        if ~keyword_set(NOTFOUND) then data2 = call_function(readFunctionList[1]+'_'+periodType, periodType, monthList[j], yearList[i], roiCodeList[k], roiArchiveList[k], inputDirs[1], outputDir, $
          varList[1], NOTFOUND=NOTFOUND, refRoi=refRoiCode, GETCHLVAR='ALG_CHL' eq varList[1], /SETNAN) else continue
        ;varList[1], destRoiCode, NOTFOUND=NOTFOUND, refRoi=refRoiCode, GETCHLVAR='ALG_CHL' eq varList[1]) else continue
        if ~keyword_set(NOTFOUND) then begin
          xTitle='Chla (Empirical)'
          yTitle='Chla (Neural Net V3)'
          xy=size(data1, /DIM)
          maxValue1=max(data1, min=minValue1, /NAN)
          maxValue2=max(data2, min=minValue2, /NAN)
          maxT=max([maxValue1, maxValue2, minValue1, minValue2], min=minT)
          ;window, 1, xsize=xy[0], ysize=xy[1]
          ;tv, 255-bytscl(data1, max=maxT, min=minT)
          ;;erase, 'FFFFFF'xL
          ;;tv, 255b-bytscl(data1, /NAN)
          ;window, 2, xsize=xy[0], ysize=xy[1]
          ;;erase, 'FFFFFF'xL
          ;tv, 255-bytscl(data2, max=maxT, min=minT)
          ;;tv, 255b-bytscl(data2, /NAN)
          data1=reform(data1,n_elements(data1))
          data2=reform(data2,n_elements(data2))
          dataList=ptrarr(2)
          dataList[0]=ptr_new(data1, /NO_COPY)
          dataList[1]=ptr_new(data2, /NO_COPY)
          title=monthList[j]+' - '+year+' - '+roiInfo.displayName+' - '+'Empirical Vs ' + 'Neural Net V3';physical->getDisplayName()
          ;day_of_year_to_day_of_month, year, day, resMonth, resDay
          ;if periodType eq 'D' then title=strcompress(resDay)+' - '+monthList[j]+' - '+year+' - '+roiInfo.displayName+' - '+'JRC Vs ' + physical->getDisplayName() else title=monthList[j]+' - '+year+' - '+roiInfo.displayName+' - '+'JRC Vs ' + physical->getDisplayName()
          if 'ALG_CHL' eq strupcase(varList[0]) then outParCode=physicals->getParameterCodeChl() else outParCode=varList[0]
          res=call_function('do_single_scatter'+'_'+periodType ,dataList, $
            monthList[j], yearList[i], roiCodeList[k], roiArchiveList[k], inputDirs, inputFileFilter, outputDir, $
            title, xtitle, ytitle, $
            overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, day=day, outParCode=outParCode, $
            /LOGAXIS, /ONEONEFITLINE, /SIMPLEFIT, SCATTERGRAPHICFORMAT=SCATTERGRAPHICFORMAT, YRANGE=[0.01, 100.])
          ptr_free, dataList[0]
          ptr_free, dataList[1]
        endif else continue
        if keyword_set(NOTFOUND) then doLog,'skip file month: ', monthList[j], ' year: ', yearList[i], level=2
        ;endif else doLog,'No files'
        doLog,'**************', level=0
        if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
        progCount++
      ;delIdlVar, refRoi
      ;endfor
      endfor
      heap_gc
    endfor
  endfor
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
  
END