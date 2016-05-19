function computeMonthlyStatFromDaily, operator, roiNo, singleStatToApply, periodtype, parName, numInPar, yearList, monthList, statTimeSeriesData, reportTimeSeriesData, $
  foundTimeSeriesData, flagTypeStatIndexes, sigmaTypeStatIndexes, binThresholds, cvThresholds, yGraphicDisplayName, sensor, outputDir, utility

  ;
  monthlyBinsNumber=n_elements(yearList)*n_elements(monthList)
  flagStatNum=n_elements(flagTypeStatIndexes)
  sigmaStatNum=n_elements(sigmaTypeStatIndexes)
  
  binNum=n_elements(binThresholds)
  cvNum=n_elements(cvThresholds)
  yearNo=n_elements(yearList)
  monthNo=n_elements(monthList)
  
  reportMonthlyTimeSeriesData=fltarr(roiNo, numInPar,  monthlyBinsNumber, flagStatNum+sigmaStatNum)
  ;reportMonthlyTimeSeriesBinDataAbs=fltarr(roiNo, numInPar,  monthlyBinsNumber, binNum, usedStatNum, /NOZERO)
  reportMonthlyTimeSeriesBinDataFlagAbs=fltarr(roiNo, numInPar,  monthlyBinsNumber, binNum, flagStatNum)
  reportMonthlyTimeSeriesBinDataSigmaAbs=fltarr(roiNo, numInPar,  monthlyBinsNumber, cvNum);, 2)
  foundMonthlyTimeSeriesData=fltarr(roiNo, numInPar,  monthlyBinsNumber)
  
  fullFileNameHBinFreqFlagAbs=strarr(flagStatNum, binNum)
  fullFileNameHBinFreqSigmaAbs=strarr(sigmaStatNum, cvNum, 2)

  histoMainTitlesBinFreqAbsFlag=strarr(flagStatNum, binNum)
  HistoMainTitlesBinFreqAbsSigma=strarr(sigmaStatNum, cvNum, 2)
  
  fullFileName=operator->buildOperatorResultFileName(periodType, parName, monthList, yearList, sensor, '', outputDir, INTERVAL=INTERVAL, /FULLPATH)
  for statIndex=0, flagStatNum-1 do Begin
    for binIndex=0, binNum-1 do begin
      prefix='flagged_cases_pixs_le_'+strcompress(binThresholds[binIndex], /REMOVE)
      fullFileNameHBinFreqFlagAbs[statIndex, binIndex]=operator->buildOperatorResultFileName(periodType, prefix+parName, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
    endfor
  endfor
  
  for statIndex=0, sigmaStatNum-1 do Begin
    for cvIndex=0, cvNum-1 do begin
      prefix='homogeneus_cases_cv_le_'+string(format='(f3.1)', cvThresholds[cvIndex])
      fullFileNameHBinFreqSigmaAbs[statIndex, cvIndex, 0]=operator->buildOperatorResultFileName(periodType, prefix+parName, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
      ;fullFileNameHBinFreqSigmaAbs[statIndex, cvIndex, 1]=operator->buildOperatorResultFileName(periodType, prefix+parName, monthList, yearList, 'cv'+singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
    endfor
  endfor

  for k=0, roiNo-1 do begin
    for l=0, numInPar-1 do begin
      dayCount=0
      timeIndex=0
      for m=0, yearNo-1 do begin
        year=yearList[m]
        for n=0, monthNo-1 do begin
          month=monthList[n]
          monthDays=utility->calcDayOfMonth([fix(year),fix(month),1,0])
          sTSD=statTimeSeriesData[k, l, dayCount:dayCount+monthDays-1];,0]
          rTSD=reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1];,flagTypeStatIndexes[statIndex]]
          foundDays=foundTimeSeriesData[k, l, dayCount:dayCount+monthDays-1]
          for statIndex=0, flagStatNum-1 do begin
            for binIndex=0, binNum-1 do begin
              ;histoMainTitlesBinFreqAbsFlag[statIndex, binIndex]='Non Flagged Cases ('+yGraphicDisplayName+': F!DE!N!X '+'<='+strcompress(binThresholds[binIndex], /REMOVE)+')'
              if binThresholds[binIndex] eq 0 then symbol='= ' else symbol='$\leq$ ';cgSymbol('leq')
                histoMainTitlesBinFreqAbsFlag[statIndex, binIndex]='Non Flagged Cases ('+yGraphicDisplayName+': F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+')'
              bin1=where(foundDays eq 1 and sTSD ne 0 and rTSD le binThresholds[binIndex], count)
              reportMonthlyTimeSeriesBinDataFlagAbs[k, l, timeIndex,binIndex]=count;,statIndex]=count
            endfor
            foundMonthlyTimeSeriesData[k, l, timeIndex]=total(foundDays, /NAN)
            doLog, 'files found:', dayCount, dayCount+monthDays-1, k, l, '---->', total(foundDays, /NAN), LEVEL=1
          endfor
          for statIndex=0, sigmaStatNum-1 do begin
            for cvCheckIndex=0, cvNum-1 do begin
              histoMainTitlesBinFreqAbsSigma[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+')'
              ;histoMainTitlesBinFreqAbsSigma[statIndex, cvCheckIndex, 1]='No flag check ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+')'
              ;cc=where(foundDays eq 1 and sTSD ne 0 and sTSD lt cvThresholds[cvCheckIndex] and rTSD eq 0, countCVDays)
              cc=where(sTSD ne 0 and sTSD lt cvThresholds[cvCheckIndex] and rTSD eq 0, countCVDays)
              ;cc1=where(sTSD lt cvThresholds[cvCheckIndex] and foundDays eq 1, countCVTotDays)
              reportMonthlyTimeSeriesBinDataSigmaAbs[k, l, timeIndex, cvCheckIndex]=countCVDays
              ;reportMonthlyTimeSeriesBinDataSigmaAbs[k, l, timeIndex, cvCheckIndex, 1]=countCVTotDays
            endfor
            ;reportMonthlyTimeSeriesData[k, l, timeIndex, 0]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,0])
            ;reportMonthlyTimeSeriesData[k, l, timeIndex, 1]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,1])
            ;reportMonthlyTimeSeriesData[k, l, timeIndex, 2]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,2])
            ;reportMonthlyTimeSeriesData[k, l, timeIndex, 3]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,3])
            ;reportMonthlyTimeSeriesData[k, l, timeIndex, 4]=total(reportTimeSeriesData[k, l, dayCount:dayCount+monthDays-1,4])
          endfor
          if foundMonthlyTimeSeriesData[k, l, timeIndex] eq 0 then begin
            foundMonthlyTimeSeriesData[k, l, timeIndex, *]=!VALUES.F_NAN
            ;reportMonthlyTimeSeriesBinDataSigmaAbs[k, l, timeIndex, *, *]=!VALUES.F_NAN
            ;reportMonthlyTimeSeriesBinDataFlagAbs[k, l, timeIndex, *]=!VALUES.F_NAN
            reportMonthlyTimeSeriesBinDataSigmaAbs[k, l, timeIndex, *]=!VALUES.F_NAN
            reportMonthlyTimeSeriesBinDataFlagAbs[k, l, timeIndex, *]=!VALUES.F_NAN
          endif
          dayCount=dayCount+monthDays
          timeIndex++
        endfor
      endfor
    endfor
  endfor
  
  monthlyData={reportMonthlyTimeSeriesBinDataSigmaAbs:reportMonthlyTimeSeriesBinDataSigmaAbs, $
    reportMonthlyTimeSeriesBinDataFlagAbs:reportMonthlyTimeSeriesBinDataFlagAbs, $
    foundMonthlyTimeSeriesData:foundMonthlyTimeSeriesData, fullFileNameHBinFreqFlagAbs:fullFileNameHBinFreqFlagAbs, $
    fullFileNameHBinFreqSigmaAbs:fullFileNameHBinFreqSigmaAbs, histoMainTitlesBinFreqAbsFlag:histoMainTitlesBinFreqAbsFlag, $
    HistoMainTitlesBinFreqAbsSigma:HistoMainTitlesBinFreqAbsSigma}
  return, monthlyData
  
end