function computeMonthlySimpleThresholdCheckStatFromDaily, operator, roiNo, singleStatToApply, periodtype, $
  thrInParCodes, thrInParNames, $
  outParCode, outParName, $
  yearList, monthList, $
  reportTimeSeriesData, meantimeSeriesData, foundTimeSeriesData, $
  flagTypeStatIndexes, $
  binThresholds, $
  yGraphicDisplayName, sensor, outputDir, utility, parIndexOffset=parIndexOffset

  ;
  ;numInPar=n_elements(inParCodes)
  mDataPrefix='mTSD'
  rDataPrefix='rTSD'

  thrInParNum=n_elements(thrInParCodes)

  monthlyBinsNumber=n_elements(yearList)*n_elements(monthList)
  flagStatNum=n_elements(flagTypeStatIndexes)
  sigmaStatNum=n_elements(sigmaTypeStatIndexes)

  binNum=n_elements(binThresholds) > 1
  yearNo=n_elements(yearList)
  monthNo=n_elements(monthList)

  ; This procedure create only 1 (one) specific computation. You can set more than one parameter
  foundMonthlyTimeSeriesData=fltarr(roiNo, 1,  monthlyBinsNumber)
  reportMonthlyTimeSeriesBinThresholdsAbs=fltarr(roiNo, 1,  monthlyBinsNumber, 2, thrInParNum)

  meanThresholds=fltarr(thrInParNum)
  idx=where(strlowcase(thrInParCodes) eq 'chlor_a', count)
  if count eq 1 then meanThresholds[idx]=0.1
  idx=where(strlowcase(thrInParCodes) eq 'aot_865', count)
  if count eq 1 then meanThresholds[idx]=0.1
  idx=where(strlowcase(thrInParCodes) eq 'angstrom', count)
  if count eq 1 then meanThresholds[idx]=1

  fullFileNameHBinFreqThreshold=strarr(flagStatNum, thrInParNum)
  histoMainTitlesHBinFreqThreshold=strarr(flagStatNum, thrInParNum)

  for statIndex=0, flagStatNum-1 do Begin
    prefix='single_'
    for i=0, n_elements(thrInParCodes)-1 do begin
      thisDesc=thrInParCodes[i]+'_le_'+string(format='(f3.1)', meanThresholds[i])
      fullFileNameHBinFreqThreshold[statIndex, i]=operator->buildOperatorResultFileName(periodType, prefix+thisDesc, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
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

        ; rrs presetting
        for jj=0, 2 do begin
          indexAsString=strcompress(jj, /REMOVE)
          rTSDDataString=rDataPrefix+indexAsString
          stringToExec=rTSDDataString+'=reportTimeSeriesData[k, '+indexAsString+', dayCount:dayCount+monthDays-1]'
          res=execute(stringToExec)
        endfor
        ; parameter setting
        for jj=3, 5 do begin
          indexAsString=strcompress(jj, /REMOVE)

          mTSDDataString=mDataPrefix+indexAsString

          stringToExec=mTSDDataString+'=meanTimeSeriesData[k, '+indexAsString+', dayCount:dayCount+monthDays-1]'
          res=execute(stringToExec)

          res=execute('filterNan=where(~finite('+mTSDDataString+'), checkNan)')
          if checkNan gt 0 then res=execute(mTSDDataString+'[filterNan]=0')

          checkStringPart1='cc=where('
          rrsFilter=' and (rTSD0 eq 0) and (rTSD1 eq 0) and (rTSD2 eq 0)'
          checkStringPart2='(rTSD0 eq 0)'
          checkStringPart3=' and (foundDays eq 1)'
          preCheck=checkStringPart1+checkStringPart2+checkStringPart3

          thisFilter=' and ('+mTSDDataString+' le meanThresholds['+indexAsString+'])'
          thisFilter=thisFilter+' and '
          thisFilter=thisFilter+'('+mTSDDataString+' ne 0)'

          checkStringFinal=', count'

          res=execute(preCheck+thisFilter+checkStringFinal+'Thrs)')
          res=execute(preCheck+rrsFilter+checkStringFinal+')')

          title='Under Threshold Cases ('+yGraphicDisplayName+thrInParNames[jj]+' '+leqSymbol+' '+string(format='(f3.1)', meanThresholds[jj])+')'
          histoMainTitlesHBinFreqThreshold[0, jj]=title
          reportMonthlyTimeSeriesBinThresholdsAbs[k, parDataIndex, timeIndex, 0, jj]=countThrs
          reportMonthlyTimeSeriesBinThresholdsAbs[k, parDataIndex, timeIndex, 1, jj]=count

        endfor

        dayCount=dayCount+monthDays
        timeIndex++
      endfor
    endfor

  endfor

  monthlyData={reportMonthlyTimeSeriesBinThresholdsAbs:reportMonthlyTimeSeriesBinThresholdsAbs, $
    fullFileNameHBinFreqThreshold:fullFileNameHBinFreqThreshold, $
    histoMainTitlesHBinFreqThreshold:histoMainTitlesHBinFreqThreshold}
  return, monthlyData

end