function computeMonthlyComplexCheckStatFromDaily, operator, roiNo, singleStatToApply, periodtype, inParCodes, inParNames, outParCode, outParName, yearList, monthList, statTimeSeriesData, reportTimeSeriesData, timeSeriesData, $
  foundTimeSeriesData, flagTypeStatIndexes, sigmaTypeStatIndexes, binThresholds, cvThresholds, yGraphicDisplayName, sensor, outputDir, utility

  ;
  numInPar=n_elements(inParCodes)
  monthlyBinsNumber=n_elements(yearList)*n_elements(monthList)
  flagStatNum=n_elements(flagTypeStatIndexes)
  sigmaStatNum=n_elements(sigmaTypeStatIndexes)

  binNum=n_elements(binThresholds)
  cvNum=n_elements(cvThresholds)
  yearNo=n_elements(yearList)
  monthNo=n_elements(monthList)

  ; This procedure create only 1 (one) specific computation. You can set more than one parameter
  reportMonthlyTimeSeriesData=fltarr(roiNo, 1,  monthlyBinsNumber, flagStatNum+sigmaStatNum)
  ;reportMonthlyTimeSeriesBinDataAbs=fltarr(roiNo, numInPar,  monthlyBinsNumber, binNum, usedStatNum, /NOZERO)
  reportMonthlyTimeSeriesBinDataFlagAbs=fltarr(roiNo, 1,  monthlyBinsNumber, binNum, flagStatNum)
  reportMonthlyTimeSeriesBinDataSigmaAbs=fltarr(roiNo, 1,  monthlyBinsNumber, cvNum);, 2)
  foundMonthlyTimeSeriesData=fltarr(roiNo, 1,  monthlyBinsNumber)

  if strlowcase(outParCode) eq 'chlor_a' then begin
    checkMean=1
    meanThreshold=0.1
    reportMonthlyTimeSeriesBinDataSigmaAbsExtra=reportMonthlyTimeSeriesBinDataSigmaAbs
  endif

  if strlowcase(outParCode) eq 'aot_865' then begin
    meanThreshold=0.1
    checkMean=1
    reportMonthlyTimeSeriesBinDataSigmaAbsExtra=reportMonthlyTimeSeriesBinDataSigmaAbs
  endif

  if outParName(outParCode) eq 'angstrom' then begin
    checkMean=1
    meanThreshold=1.0
    reportMonthlyTimeSeriesBinDataSigmaAbsExtra=reportMonthlyTimeSeriesBinDataSigmaAbs
  endif

  fullFileNameHBinFreqFlagAbs=strarr(flagStatNum, binNum)
  fullFileNameHBinFreqSigmaAbs=strarr(sigmaStatNum, cvNum, 2)

  histoMainTitlesBinFreqAbsFlag=strarr(flagStatNum, binNum)
  HistoMainTitlesBinFreqAbsSigma=strarr(sigmaStatNum, cvNum, 2)

  if keyword_set(checkMean) then begin
    fullFileNameHBinFreqSigmaAbsExtra=strarr(sigmaStatNum, cvNum)
    HistoMainTitlesBinFreqAbsSigmaExtra=strarr(sigmaStatNum, cvNum)
  endif

  fullFileName=operator->buildOperatorResultFileName(periodType, outParCode, monthList, yearList, sensor, '', outputDir, INTERVAL=INTERVAL, /FULLPATH)
  for statIndex=0, flagStatNum-1 do Begin
    for binIndex=0, binNum-1 do begin
      prefix='flagged_cases_pixs_le_'+strcompress(binThresholds[binIndex], /REMOVE)
      fullFileNameHBinFreqFlagAbs[statIndex, binIndex]=operator->buildOperatorResultFileName(periodType, prefix+outParCode, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
    endfor
  endfor

  for statIndex=0, sigmaStatNum-1 do Begin
    for cvIndex=0, cvNum-1 do begin
      prefix='homogeneus_cases_cv_le_'+string(format='(f3.1)', cvThresholds[cvIndex])
      parDesc=''
      for i=0, numInPar-2 do parDesc=parDesc+inParCodes[i]+'_'
      parDesc=parDesc+outParCode
      fullFileNameHBinFreqSigmaAbs[statIndex, cvIndex, 0]=operator->buildOperatorResultFileName(periodType, prefix+parDesc, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
      if keyword_set(checkMean) then begin
        prefixExtra='homogeneus_cases_cv_le_'+string(format='(f3.1)', cvThresholds[cvIndex])+'and_mean_le'+string(format='(f3.1)', meanThreshold)
        fullFileNameHBinFreqSigmaAbsExtra[statIndex, cvIndex, 0]=operator->buildOperatorResultFileName(periodType, prefixExtra+parDesc, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
      endif
      ;fullFileNameHBinFreqSigmaAbs[statIndex, cvIndex, 1]=operator->buildOperatorResultFileName(periodType, prefix+parName, monthList, yearList, 'cv'+singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
    endfor
  endfor

  ;outParameter is ALWAYS one (for each aggregated stats)
  parDataIndex=0
  leqSymbol='$\leq$ '
  ;andSymbol='$\and$ '
  andSymbol='&'
  for k=0, roiNo-1 do begin
    ;for l=0, numInPar-1 do begin
    dayCount=0
    timeIndex=0
    for m=0, yearNo-1 do begin
      year=yearList[m]
      for n=0, monthNo-1 do begin
        month=monthList[n]
        monthDays=utility->calcDayOfMonth([fix(year),fix(month),1,0])
        sTSD=statTimeSeriesData[k, parDataIndex, dayCount:dayCount+monthDays-1,0]
        if keyword_set(checkMean) then extrasTSD=timeSeriesData[k, parDataIndex, dayCount:dayCount+monthDays-1]
        rTSD=reportTimeSeriesData[k, parDataIndex, dayCount:dayCount+monthDays-1];,flagTypeStatIndexes[statIndex]]
        foundDays=foundTimeSeriesData[k, parDataIndex, dayCount:dayCount+monthDays-1]
        multipleCheck=''
        for jj=0, numInpar-2 do begin
          indexAsString=strcompress(jj, /REMOVE)
          stringToExec='sTSD'+indexAsString+'=statTimeSeriesData[k, '+indexAsString+', dayCount:dayCount+monthDays-1,0]'
          res=execute(stringToExec)
          stringToExec='rTSD'+indexAsString+'=reportTimeSeriesData[k, '+indexAsString+', dayCount:dayCount+monthDays-1]'
          res=execute(stringToExec)
          thisFilter='(sTSD'+indexAsString+' lt cvThresholds[cvCheckIndex])'
          thisFilter=thisFilter+' and '
          multipleCheck=multipleCheck+thisFilter
          insertTitle=''
        endfor
        ;endif
        for statIndex=0, flagStatNum-1 do begin
          for binIndex=0, binNum-1 do begin
            ;histoMainTitlesBinFreqAbsFlag[statIndex, binIndex]='Non Flagged Cases ('+yGraphicDisplayName+': F!DE!N!X '+'<='+strcompress(binThresholds[binIndex], /REMOVE)+')'
            if binThresholds[binIndex] eq 0 then symbol='= ' else symbol=leqSymbol;cgSymbol('leq')
            histoMainTitlesBinFreqAbsFlag[statIndex, binIndex]='Non Flagged Cases ('+yGraphicDisplayName+': F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+')'
            bin1=where(foundDays eq 1 and sTSD ne 0 and rTSD le binThresholds[binIndex], count)
            reportMonthlyTimeSeriesBinDataFlagAbs[k, parDataIndex, timeIndex,binIndex]=count;,statIndex]=count
          endfor
          foundMonthlyTimeSeriesData[k, parDataIndex, timeIndex]=total(foundDays, /NAN)
          doLog, 'files found:', dayCount, dayCount+monthDays-1, k, parDataIndex, '---->', total(foundDays, /NAN), LEVEL=1
        endfor
        for statIndex=0, sigmaStatNum-1 do begin
          for cvCheckIndex=0, cvNum-1 do begin
            histoMainTitlesBinFreqAbsSigma[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+')'
            ;histoMainTitlesBinFreqAbsSigmaExtra[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])++')'++'  mean value '+leqSymbol+string(format='(f3.1)', meanThreshold)
            histoMainTitlesBinFreqAbsSigmaExtra[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+' '+andSymbol+' '+'QC!Ds!N!X '+')'
            ;histoMainTitlesBinFreqAbsSigma[statIndex, cvCheckIndex, 1]='No flag check ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+')'
            ;cc=where(foundDays eq 1 and sTSD ne 0 and sTSD lt cvThresholds[cvCheckIndex] and rTSD eq 0, countCVDays)
            checkStringPart1='cc=where('
            checkStringPart2='sTSD ne 0 and sTSD lt cvThresholds[cvCheckIndex] and rTSD eq 0'
            checkStringPart3=', countCVDays)'
            res=execute(checkStringPart1+multipleCheck+checkStringPart2+checkStringPart3)
            reportMonthlyTimeSeriesBinDataSigmaAbs[k, parDataIndex, timeIndex, cvCheckIndex]=countCVDays
            if keyword_set(checkMean) then begin
              ;leqSymbol='$\leq$ '
              ;histoMainTitlesBinFreqAbsSigmaExtra[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+':'+'Mean '+leqSymbol+string(format='(f3.1)', meanThreshold)+' C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+')'
              dataMeanCheck='and extrasTSD le meanThreshold'
              ;checkStringPart3=', countExtraCVDays)'
              res=execute(checkStringPart1+multipleCheck+checkStringPart2+dataMeanCheck+checkStringPart3)
              reportMonthlyTimeSeriesBinDataSigmaAbsExtra[k, parDataIndex, timeIndex, cvCheckIndex]=countCVDays
            endif
            ;cc1=where(sTSD lt cvThresholds[cvCheckIndex] and foundDays eq 1, countCVTotDays)
            ;reportMonthlyTimeSeriesBinDataSigmaAbs[k, l, timeIndex, cvCheckIndex, 1]=countCVTotDays
          endfor
          ;reportMonthlyTimeSeriesData[k, l, timeIndex, 0]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,0])
          ;reportMonthlyTimeSeriesData[k, l, timeIndex, 1]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,1])
          ;reportMonthlyTimeSeriesData[k, l, timeIndex, 2]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,2])
          ;reportMonthlyTimeSeriesData[k, l, timeIndex, 3]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,3])
          ;reportMonthlyTimeSeriesData[k, l, timeIndex, 4]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,4])
        endfor
        if foundMonthlyTimeSeriesData[k,parDataIndex, timeIndex] eq 0 then begin
          foundMonthlyTimeSeriesData[k,parDataIndex, timeIndex, *]=!VALUES.F_NAN
          ;reportMonthlyTimeSeriesBinDataSigmaAbs[k, l, timeIndex, *, *]=!VALUES.F_NAN
          ;reportMonthlyTimeSeriesBinDataFlagAbs[k, l, timeIndex, *]=!VALUES.F_NAN
          reportMonthlyTimeSeriesBinDataSigmaAbs[k,parDataIndex, timeIndex, *]=!VALUES.F_NAN
          reportMonthlyTimeSeriesBinDataFlagAbs[k,parDataIndex, timeIndex, *]=!VALUES.F_NAN
        endif
        dayCount=dayCount+monthDays
        timeIndex++
      endfor
    endfor
    ;endfor
  endfor

  if keyword_set(checkMean) then begin
    monthlyData={reportMonthlyTimeSeriesBinDataSigmaAbs:reportMonthlyTimeSeriesBinDataSigmaAbs, $
      reportMonthlyTimeSeriesBinDataFlagAbs:reportMonthlyTimeSeriesBinDataFlagAbs, $
      foundMonthlyTimeSeriesData:foundMonthlyTimeSeriesData, fullFileNameHBinFreqFlagAbs:fullFileNameHBinFreqFlagAbs, $
      fullFileNameHBinFreqSigmaAbs:fullFileNameHBinFreqSigmaAbs, histoMainTitlesBinFreqAbsFlag:histoMainTitlesBinFreqAbsFlag, $
      HistoMainTitlesBinFreqAbsSigma:HistoMainTitlesBinFreqAbsSigma, reportMonthlyTimeSeriesBinDataSigmaAbsExtra:reportMonthlyTimeSeriesBinDataSigmaAbsExtra, $
      histoMainTitlesBinFreqAbsSigmaExtra:histoMainTitlesBinFreqAbsSigmaExtra, fullFileNameHBinFreqSigmaAbsExtra:fullFileNameHBinFreqSigmaAbsExtra}
  endif else begin
    monthlyData={reportMonthlyTimeSeriesBinDataSigmaAbs:reportMonthlyTimeSeriesBinDataSigmaAbs, $
      reportMonthlyTimeSeriesBinDataFlagAbs:reportMonthlyTimeSeriesBinDataFlagAbs, $
      foundMonthlyTimeSeriesData:foundMonthlyTimeSeriesData, fullFileNameHBinFreqFlagAbs:fullFileNameHBinFreqFlagAbs, $
      fullFileNameHBinFreqSigmaAbs:fullFileNameHBinFreqSigmaAbs, histoMainTitlesBinFreqAbsFlag:histoMainTitlesBinFreqAbsFlag, $
      HistoMainTitlesBinFreqAbsSigma:HistoMainTitlesBinFreqAbsSigma}
  endelse
  return, monthlyData

end