function computeMonthlyComplexFinalCheckStatFromDaily, operator, roiNo, singleStatToApply, periodtype, $
  cvInParCodes, cvInParNames, thrInParCodes, thrInParNames, $
  outParCode, outParName, $
  yearList, monthList, $
  cvTimeSeriesData, reportTimeSeriesData, meantimeSeriesData, foundTimeSeriesData, $
  flagTypeStatIndexes, sigmaTypeStatIndexes, $
  binThresholds, cvThresholds, $
  yGraphicDisplayName, sensor, outputDir, utility
  
  ;
  ;numInPar=n_elements(inParCodes)
  mDataPrefix='mTSD'
  rDataPrefix='rTSD'
  cvDataPrefix='cvTSD'
  
  cvInParNum=n_elements(cvInParCodes)
  thrInParNum=n_elements(thrInParCodes)
  
  monthlyBinsNumber=n_elements(yearList)*n_elements(monthList)
  flagStatNum=n_elements(flagTypeStatIndexes)
  sigmaStatNum=n_elements(sigmaTypeStatIndexes)
  
  binNum=n_elements(binThresholds) > 1
  cvNum=n_elements(cvThresholds)
  yearNo=n_elements(yearList)
  monthNo=n_elements(monthList)
  
  ; This procedure create only 1 (one) specific computation. You can set more than one parameter
  reportMonthlyTimeSeriesData=fltarr(roiNo, 1,  monthlyBinsNumber, flagStatNum+sigmaStatNum)
  ;reportMonthlyTimeSeriesBinDataAbs=fltarr(roiNo, numInPar,  monthlyBinsNumber, binNum, usedStatNum, /NOZERO)
  reportMonthlyTimeSeriesBinDataFlagAbs=fltarr(roiNo, 1,  monthlyBinsNumber, binNum, flagStatNum)
  reportMonthlyTimeSeriesBinDataSigmaAbs=fltarr(roiNo, 1,  monthlyBinsNumber, cvNum);, 2)
  reportMonthlyTimeSeriesBinDataSigmaAbsExtra=fltarr(roiNo, 1,  monthlyBinsNumber, cvNum);, 2)
  reportMonthlyTimeSeriesBinDataSigmaSingleAbsExtra=fltarr(roiNo, 1,  monthlyBinsNumber, cvNum, thrInParNum, 2)
  foundMonthlyTimeSeriesData=fltarr(roiNo, 1,  monthlyBinsNumber)
  
  meanThresholds=fltarr(thrInParNum)
  idx=where(strlowcase(thrInParCodes) eq 'chlor_a', count)
  if count eq 1 then meanThresholds[idx]=0.1
  idx=where(strlowcase(thrInParCodes) eq 'aot_865', count)
  if count eq 1 then meanThresholds[idx]=0.1
  idx=where(strlowcase(thrInParCodes) eq 'angstrom', count)
  if count eq 1 then meanThresholds[idx]=1
  checkMean=1
  
  fullFileNameHBinFreqFlagAbs=strarr(flagStatNum, binNum)
  fullFileNameHBinFreqSigmaAbs=strarr(sigmaStatNum, cvNum, 2)
  
  histoMainTitlesBinFreqAbsFlag=strarr(flagStatNum, binNum)
  HistoMainTitlesBinFreqAbsSigma=strarr(sigmaStatNum, cvNum, 2)
  
  if keyword_set(checkMean) then begin
    fullFileNameHBinFreqSigmaAbsExtra=strarr(sigmaStatNum, cvNum)
    HistoMainTitlesBinFreqAbsSigmaExtra=strarr(sigmaStatNum, cvNum)
    fullFileNameHBinFreqSigmaSingleAbsExtra=strarr(sigmaStatNum, cvNum, thrInParNum, 2)
    HistoMainTitlesBinFreqAbsSigmaSingleExtra=strarr(sigmaStatNum, cvNum, thrInParNum, 2)
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
      cvDesc=''
      for i=0, n_elements(cvInParCodes)-1 do cvDesc=cvDesc+cvInParCodes[i]+'_'
      parDesc=prefix+cvDesc
      parDesc=strmid(parDesc, 0, strlen(parDesc)-1)
      fullFileNameHBinFreqSigmaAbs[statIndex, cvIndex, 0]=operator->buildOperatorResultFileName(periodType, parDesc, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
      if keyword_set(checkMean) then begin
        prefixExtra='multiple_'
        for i=0, n_elements(thrInParCodes)-1 do begin
          thisDesc1=cvDesc+'and_'+thrInParCodes[i]+'_le_'+string(format='(f3.1)', meanThresholds[i])
          thisDesc2=thrInParCodes[i]+'_le_'+string(format='(f3.1)', meanThresholds[i])
          fullFileNameHBinFreqSigmaSingleAbsExtra[statIndex, cvIndex, i, 0]=operator->buildOperatorResultFileName(periodType, 'single_'+prefix+thisDesc1, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
          fullFileNameHBinFreqSigmaSingleAbsExtra[statIndex, cvIndex, i, 1]=operator->buildOperatorResultFileName(periodType, 'single_'+thisDesc2, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
        endfor
        fullFileNameHBinFreqSigmaAbsExtra[statIndex, cvIndex, 0]=operator->buildOperatorResultFileName(periodType, prefixExtra+parDesc, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
      endif
    endfor
  endfor
  
  ;outParameter is ALWAYS one (for each aggregated stats)
  parDataIndex=0
  leqSymbol='$\leq$ '
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
        foundDays=foundTimeSeriesData[k, parDataIndex, dayCount:dayCount+monthDays-1]
        
        ; data setting
        for jj=0, cvInParNum+thrInParNum-1 do begin
          indexAsString=strcompress(jj, /REMOVE)
          
          rTSDDataString=rDataPrefix+indexAsString
          cvTSDDataString=cvDataPrefix+indexAsString
          mTSDDataString=mDataPrefix+indexAsString
          
          stringToExec=cvTSDDataString+'=cvTimeSeriesData[k, '+indexAsString+', dayCount:dayCount+monthDays-1,0]'
          res=execute(stringToExec)
          
          stringToExec=rTSDDataString+'=reportTimeSeriesData[k, '+indexAsString+', dayCount:dayCount+monthDays-1]'
          res=execute(stringToExec)
          
          stringToExec=mTSDDataString+'=meanTimeSeriesData[k, '+indexAsString+', dayCount:dayCount+monthDays-1]'
          res=execute(stringToExec)
          
          res=execute('filterNan=where(~finite('+mTSDDataString+'), checkNan)')
          if checkNan gt 0 then res=execute(mTSDDataString+'[filterNan]=0')
          
        endfor
        
        ; cv setting
        cvCheck=''
        for jj=0, cvInParNum-1 do begin
          indexAsString=strcompress(jj, /REMOVE)
          cvTSDDataString=cvDataPrefix+indexAsString
          
          thisFilter='('+cvTSDDataString+' lt cvThresholds[cvCheckIndex])'
          thisFilter=thisFilter+' and '
          cvCheck=cvCheck+thisFilter
          
          thisFilter='('+cvTSDDataString+' ne 0)'
          thisFilter=thisFilter+' and '
          cvCheck=cvCheck+thisFilter
          
        endfor
        
        ; threshold setting
        thrCheck=''
        for jj=0, thrInParNum-1 do begin
          indexAsString=strcompress(jj+cvInParNum, /REMOVE)
          localIndexAsString=strcompress(jj, /REMOVE)
          
          mTSDDataString=mDataPrefix+indexAsString
          
          stringToExec=mTSDDataString+'=meanTimeSeriesData[k, '+indexAsString+', dayCount:dayCount+monthDays-1,0]'
          
          thisFilter='('+mTSDDataString+' le meanThresholds['+localIndexAsString+'])'
          thisFilter=thisFilter+' and '
          thrCheck=thrCheck+thisFilter
          
          thisFilter='('+mTSDDataString+' ne 0)'
          thisFilter=thisFilter+' and '
          thrCheck=thrCheck+thisFilter
          
        endfor
        ;endif
        for statIndex=0, flagStatNum-1 do begin
          for binIndex=0, binNum-1 do begin
            ;histoMainTitlesBinFreqAbsFlag[statIndex, binIndex]='Non Flagged Cases ('+yGraphicDisplayName+': F!DE!N!X '+'<='+strcompress(binThresholds[binIndex], /REMOVE)+')'
            if binThresholds[binIndex] eq 0 then symbol='= ' else symbol=leqSymbol;cgSymbol('leq')
            histoMainTitlesBinFreqAbsFlag[statIndex, binIndex]='Non Flagged Cases ('+yGraphicDisplayName+': F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+')'
            bin1=where(foundDays eq 1 and cvTSD0 ne 0 and rTSD0 le binThresholds[binIndex], count)
            reportMonthlyTimeSeriesBinDataFlagAbs[k, parDataIndex, timeIndex,binIndex]=count;,statIndex]=count
          endfor
          foundMonthlyTimeSeriesData[k, parDataIndex, timeIndex]=total(foundDays, /NAN)
          doLog, 'files found:', dayCount, dayCount+monthDays-1, k, parDataIndex, '---->', total(foundDays, /NAN), LEVEL=1
        endfor
        
        for statIndex=0, sigmaStatNum-1 do begin
          for cvCheckIndex=0, cvNum-1 do begin
            histoMainTitlesBinFreqAbsSigma[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+')'
            histoMainTitlesBinFreqAbsSigmaExtra[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+' '+andSymbol+' '+'QC!Ds!N!X '+')'
            
            checkStringPart1='cc=where('
            checkStringPart2='(cvTSD0 ne 0)'
            checkStringPart3='and (rTSD0 eq 0) and (rTSD1 eq 0) and (rTSD2 eq 0) and foundDays eq 1'
            checkStringPart4=', countCVDays'
            
            ; only cv
            res=execute(checkStringPart1+cvCheck+checkStringPart2+checkStringPart3+checkStringPart4+')')
            ; only cv AND thr
            res=execute(checkStringPart1+cvCheck+thrCheck+checkStringPart2+checkStringPart3+checkStringPart4+'2)')
            
            ; each single check... hard coded...
            for kk=0, thrInParNum-1 do begin
              indexAsString=strcompress(kk+cvInParNum, /REMOVE)
              localIndexAsString=strcompress(kk, /REMOVE)
              mTSDDataString=mDataPrefix+indexAsString
              
              title1='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])
              title2='Rrs multiple flagged ('
              title1=title1+' and '+thrInParNames[kk]+' '+leqSymbol+' '+string(format='(f3.1)', meanThresholds[kk])+')'
              title2=title2+thrInParNames[kk]+' '+leqSymbol+' '+string(format='(f3.1)', meanThresholds[kk])+')'
              histoMainTitlesBinFreqAbsSigmaSingleExtra[statIndex, cvCheckIndex, kk,0]=title1
              histoMainTitlesBinFreqAbsSigmaSingleExtra[statIndex, cvCheckIndex, kk,1]=title2
              
              thisCheck='('+mTSDDataString+' le meanThresholds['+localIndexAsString+'])'
              thisCheck=thisCheck+' and '
              thisCheck=thisCheck+'('+mTSDDataString+' ne 0) and '
              
              ;cv and thr and rrs flag
              res=execute(checkStringPart1+cvCheck+thisCheck+checkStringPart2+checkStringPart3+checkStringPart4+'3)')
              ;thr and rrs flag
              res=execute(checkStringPart1+thisCheck+checkStringPart2+checkStringPart3+checkStringPart4+'4)')
              reportMonthlyTimeSeriesBinDataSigmaSingleAbsExtra[k, parDataIndex, timeIndex, cvCheckIndex, kk, 0]=countCVDays3
              reportMonthlyTimeSeriesBinDataSigmaSingleAbsExtra[k, parDataIndex, timeIndex, cvCheckIndex, kk, 1]=countCVDays4
              doLog, 'single check',thrInParCodes[kk], countCVDays3, LEVEL=4
            endfor
            
            reportMonthlyTimeSeriesBinDataSigmaAbs[k, parDataIndex, timeIndex, cvCheckIndex]=countCVDays
            reportMonthlyTimeSeriesBinDataSigmaAbsExtra[k, parDataIndex, timeIndex, cvCheckIndex]=countCVDays2
          endfor
        endfor
        if foundMonthlyTimeSeriesData[k,parDataIndex, timeIndex] eq 0 then begin
          foundMonthlyTimeSeriesData[k,parDataIndex, timeIndex, *]=!VALUES.F_NAN
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
      histoMainTitlesBinFreqAbsSigmaExtra:histoMainTitlesBinFreqAbsSigmaExtra, fullFileNameHBinFreqSigmaAbsExtra:fullFileNameHBinFreqSigmaAbsExtra, $
      reportMonthlyTimeSeriesBinDataSigmaSingleAbsExtra:reportMonthlyTimeSeriesBinDataSigmaSingleAbsExtra, fullFileNameHBinFreqSigmaSingleAbsExtra:fullFileNameHBinFreqSigmaSingleAbsExtra, $
      HistoMainTitlesBinFreqAbsSigmaSingleExtra:HistoMainTitlesBinFreqAbsSigmaSingleExtra}
  endif else begin
    monthlyData={reportMonthlyTimeSeriesBinDataSigmaAbs:reportMonthlyTimeSeriesBinDataSigmaAbs, $
      reportMonthlyTimeSeriesBinDataFlagAbs:reportMonthlyTimeSeriesBinDataFlagAbs, $
      foundMonthlyTimeSeriesData:foundMonthlyTimeSeriesData, fullFileNameHBinFreqFlagAbs:fullFileNameHBinFreqFlagAbs, $
      fullFileNameHBinFreqSigmaAbs:fullFileNameHBinFreqSigmaAbs, histoMainTitlesBinFreqAbsFlag:histoMainTitlesBinFreqAbsFlag, $
      HistoMainTitlesBinFreqAbsSigma:HistoMainTitlesBinFreqAbsSigma}
  endelse
  return, monthlyData
  
end