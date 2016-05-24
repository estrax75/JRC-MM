function computeMonthlySummaryFlagStatsFromDaily, operator, roiNo, singleStatToApply, periodtype, $
  cvInParCodes, cvInParNames, thrInParCodes, thrInParNames, trueFlagInCodes, trueFlagInNames, $
  outParCode, outParName, $
  yearList, monthList, $
  cvTimeSeriesData, reportTimeSeriesData, meanTimeSeriesData, flagTimeSeriesData, foundTimeSeriesData, $
  flagTypeStatIndexes, sigmaTypeStatIndexes, flagPos, $
  binThresholds, cvThresholds,$
  yGraphicDisplayName, sensor, outputDir, utility, DUPLICATE=DUPLICATE
  
  histoFolder='histoFinal'
  ;       statValue[0]=total(fTBTotal)
  ;       statValue[1]=total(fTBGlint)
  ;       statValue[2]=total(fTBHiSatZen)
  ;       statValue[3]=total(fTBIceCloud)
  ;numInPar=n_elements(inParCodes)
  mDataPrefix='mTSD'
  rDataPrefix='rTSD'
  cvDataPrefix='cvTSD'
  if flagPos ne -1 then flagDataPrefix='flagTSD'
  ;outParameter is ALWAYS one (for each aggregated stats)
  parDataIndex=0
  leqSymbol='$\leq$ '
    geqSymbol='$\geq$ '
    neqSymbol='$\neq$ '
    andSymbol='&'
  cvInParNum=n_elements(cvInParCodes)
  thrInParNum=n_elements(thrInParCodes)
  
  flagInfoNum=n_elements(trueFlagInCodes)
  if flagInfoNum gt 0 then begin
    checkTrueFlag=1
    startThrIndex=1
    extParIndex=[indgen(cvInParNum), indgen(thrInParNum-startThrIndex)+startThrIndex+cvInParNum]
  endif else begin
    checkTrueFlag=0
    startThrIndex=0
  endelse
  cvTimeSeriesData=cvTimeSeriesData[*,extParIndex,*]
  reportTimeSeriesData=reportTimeSeriesData[*,extParIndex,*]
  meanTimeSeriesData=meanTimeSeriesData[*,extParIndex,*]
  foundTimeSeriesData=foundTimeSeriesData[*,extParIndex,*]
  
  thrInParCodes=thrInParCodes[startThrIndex:*]
  thrInParNames=thrInParNames[startThrIndex:*]
  
  cvInParNum=n_elements(cvInParCodes)
  thrInParNum=n_elements(thrInParCodes)
  
  monthlyBinsNumber=n_elements(yearList)*n_elements(monthList)
  flagStatNum=n_elements(flagTypeStatIndexes)
  sigmaStatNum=n_elements(sigmaTypeStatIndexes)
  
  binNum=n_elements(binThresholds) > 1
  cvNum=n_elements(cvThresholds)
  yearNo=n_elements(yearList)
  monthNo=n_elements(monthList)
  
  foundMonthlyTimeSeriesData=fltarr(roiNo, 1,  monthlyBinsNumber)
  
  ; This procedure create only 1 (one) specific computation. You can set more than one parameter
  reportMonthlyTimeSeriesDataCheck1=fltarr(roiNo, 1,  monthlyBinsNumber, binNum)
  reportMonthlyTimeSeriesDataCheck2=fltarr(roiNo, 1,  monthlyBinsNumber, binNum)
  
  reportMonthlyTimeSeriesDataCheck3=fltarr(roiNo, 1,  monthlyBinsNumber, cvNum)
  reportMonthlyTimeSeriesDataCheck4=fltarr(roiNo, 1,  monthlyBinsNumber, cvNum)
  reportMonthlyTimeSeriesDataCheck5=fltarr(roiNo, 1,  monthlyBinsNumber, cvNum)
  
  reportMonthlyTimeSeriesDataCheck6=fltarr(roiNo, 1,  monthlyBinsNumber, cvNum, thrInParNum)
  
  reportMonthlyTimeSeriesDataCheck7=fltarr(roiNo, 1,  monthlyBinsNumber, binNum, flagInfoNum-1)
  
  
  meanThresholds=fltarr(thrInParNum)
  idx=where(strlowcase(thrInParCodes) eq 'chlor_a', count)
  if count eq 1 then meanThresholds[idx]=0.1
  idx=where(strlowcase(thrInParCodes) eq 'aot_865', count)
  if count eq 1 then meanThresholds[idx]=0.1
  idx=where(strlowcase(thrInParCodes) eq 'angstrom', count)
  if count eq 1 then meanThresholds[idx]=1
  checkMean=1
  
  fullFileNameHistoCheck1=strarr(flagStatNum, binNum)
  fullFileNameHistoCheck2=strarr(flagStatNum, binNum)
  
  fullFileNameHistoCheck3=strarr(flagStatNum, cvNum)
  fullFileNameHistoCheck4=strarr(flagStatNum, cvNum)
  fullFileNameHistoCheck5=strarr(flagStatNum, cvNum)
  
  fullFileNameHistoCheck6=strarr(flagStatNum, cvNum, thrInParNum)
  fullFileNameHistoCheck7=strarr(flagStatNum, binNum, flagInfoNum-1)
  
  histoMainTitlesCheck1=strarr(flagStatNum, binNum)
  histoMainTitlesCheck2=strarr(flagStatNum, binNum)
  
  histoMainTitlesCheck3=strarr(flagStatNum, cvNum)
  histoMainTitlesCheck4=strarr(flagStatNum, cvNum)
  histoMainTitlesCheck5=strarr(flagStatNum, cvNum)
  
  histoMainTitlesCheck6=strarr(flagStatNum, cvNum, thrInParNum)
  histoMainTitlesCheck7=strarr(flagStatNum, binNum, flagInfoNum-1)
  
  outParCode=strcompress(outParCode, /REMOVE)
  fullFileName=operator->buildOperatorResultFileName(periodType, outParCode, monthList, yearList, sensor, '', outputDir, INTERVAL=INTERVAL, /FULLPATH)
  for statIndex=0, flagStatNum-1 do Begin
    for binIndex=0, binNum-1 do begin
      if binThresholds[binIndex] eq 0 then symbol='= ' else symbol=leqSymbol
      prefix1='1_rrs_positives_values'
      fullFileNameHistoCheck1[statIndex, binIndex]=operator->buildOperatorResultFileName(periodType, prefix1, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+histoFolder, INTERVAL=INTERVAL, /FULLPATH)
      prefix7='7_flagged_cases_pixs_ne_'+strcompress(binThresholds[binIndex], /REMOVE)
      ;fullFileNameHistoCheck1[statIndex, binIndex]=operator->buildOperatorResultFileName(periodType, prefix+outParCode, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
      for jj=1, flagInfoNum-1 do begin
        if jj ne 0 then begin
          fileDesc='_'+trueFlagInNames[jj]
          ;histoMainTitlesCheck7[statIndex, binIndex, jj-1]='Non Flagged Cases ('+trueFlagInNames[jj]+': F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+')'
          histoMainTitlesCheck7[statIndex, binIndex, jj-1]='Flagged Cases ('+trueFlagInNames[jj]+': F!DE!N!X '+neqSymbol+strcompress(binThresholds[binIndex], /REMOVE)+')'
          fullFileNameHistoCheck7[statIndex, binIndex, jj-1]=operator->buildOperatorResultFileName(periodType, prefix7+fileDesc, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+histoFolder, INTERVAL=INTERVAL, /FULLPATH)
        endif
      endfor
    endfor
  endfor
  
  for statIndex=0, sigmaStatNum-1 do Begin
    for cvIndex=0, cvNum-1 do begin
      prefix='homogeneus_cases_cv_le_'+string(format='(f3.1)', cvThresholds[cvIndex])
      prefix3='3_'+prefix
      prefix4='4_'+prefix+'excl_flag_eq_0_'
      cvDesc=''
      for i=0, n_elements(cvInParCodes)-1 do cvDesc=cvDesc+cvInParCodes[i]+'_'
      parDesc=prefix3+cvDesc
      parDesc=strmid(parDesc, 0, strlen(parDesc)-1)
      fullFileNameHistoCheck3[statIndex, cvIndex, 0]=operator->buildOperatorResultFileName(periodType, parDesc, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+histoFolder, INTERVAL=INTERVAL, /FULLPATH)
      parDesc=prefix4+cvDesc
      parDesc=strmid(parDesc, 0, strlen(parDesc)-1)
      fullFileNameHistoCheck4[statIndex, cvIndex, 0]=operator->buildOperatorResultFileName(periodType, parDesc, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+histoFolder, INTERVAL=INTERVAL, /FULLPATH)
      if keyword_set(checkMean) then begin
        prefix5='5_multiple_qc_'
        for i=0, n_elements(thrInParCodes)-1 do begin
          thisDesc1=cvDesc+'and_'+thrInParCodes[i]+'_le_'+string(format='(f3.1)', meanThresholds[i])
          ;thisDesc2=thrInParCodes[i]+'_le_'+string(format='(f3.1)', meanThresholds[i])
          fullFileNameHistoCheck6[statIndex, cvIndex, i]=operator->buildOperatorResultFileName(periodType, '6_single_qc_'+prefix+thisDesc1, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+histoFolder, INTERVAL=INTERVAL, /FULLPATH)
          ;fullFileNameHBinFreqSigmaSingleAbsExtra[statIndex, cvIndex, i, 1]=operator->buildOperatorResultFileName(periodType, 'single_'+thisDesc2, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+'histo'+path_sep()+'abs', INTERVAL=INTERVAL, /FULLPATH)
        endfor
        fullFileNameHistoCheck5[statIndex, cvIndex]=operator->buildOperatorResultFileName(periodType, prefix5+parDesc, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+histoFolder, INTERVAL=INTERVAL, /FULLPATH)
      endif
    endfor
  endfor
  
  for k=0, roiNo-1 do begin
    ;for l=0, numInPar-1 do begin
    dayCount=0
    timeIndex=0
    ;subscribeDayCount=0
    ;for dd=0, keyword_set(DUPLICATE) do begin
    ;subscribeDayCount++
    for m=0, yearNo-1 do begin
      year=yearList[m]
      for n=0, monthNo-1 do begin
        month=monthList[n]
        monthDays=utility->calcDayOfMonth([fix(year),fix(month),1,0])
        if keyword_set(DUPLICATE) then begin
          monthDays=monthDays*2
        endif
        foundDays=foundTimeSeriesData[k, parDataIndex, dayCount:dayCount+monthDays-1]
        
        ; data setting
        for jj=0, flagInfoNum-1 do begin
          indexAsString=strcompress(jj, /REMOVE)
          flagTSDDataString=flagDataPrefix+indexAsString
          stringToExec=flagTSDDataString+'=flagTimeSeriesData[k, '+indexAsString+', dayCount:dayCount+monthDays-1]'
          res=execute(stringToExec)
        endfor
        zeroFlagsCondition=' and ('+flagDataPrefix+'0'+' eq 0) '
        
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
        foundMonthlyTimeSeriesData[k, parDataIndex, timeIndex]=total(foundDays, /NAN)
        
        checkStringPart1='cc=where('
        checkStringPart2='(cvTSD0 ne 0) and '
        checkStringPart3='(rTSD0 eq 0) and (rTSD1 eq 0) and (rTSD2 eq 0)'
        checkStringPart4=' and (foundDays eq 1), countDays'
        
        for statIndex=0, sigmaStatNum-1 do begin
          binIndex=0
          ; 1) Rrs positives
          
          rrsPositivesCheck=checkStringPart1+checkStringPart2+checkStringPart3+checkStringPart4+'RRS_POS)'
          res=execute(rrsPositivesCheck)
          ;symbol=geqSymbol
          histoMainTitlesCheck1[statIndex, binIndex]=yGraphicDisplayName+': Rrs > '+strcompress(binThresholds[binIndex], /REMOVE)
          ;histoMainTitlesCheck1[statIndex, binIndex]='Non Flagged Cases ('+yGraphicDisplayName+': F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+')'
          reportMonthlyTimeSeriesDataCheck1[k, parDataIndex, timeIndex,0]=countDaysRRS_POS
          print, countDaysRRS_POS
          
          ; 2) Rrs positives AND zero "true" flags condition
          rrsPositivesZeroFlagsCheck=checkStringPart1+checkStringPart3+zeroFlagsCondition+checkStringPart4+'RRS_POS_ZERO_FLAGS)'
          res=execute(rrsPositivesZeroFlagsCheck)
          print, countDaysRRS_POS_ZERO_FLAGS
          
          if binThresholds[binIndex] eq 0 then symbol='= ' else symbol=leqSymbol;cgSymbol('leq')
          ;histoMainTitlesCheck2[*,*]='('+trueFlagInNames[0]+': F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+andSymbol+'Rrs >'+strcompress(binThresholds[binIndex], /REMOVE)+')'
          histoMainTitlesCheck2[*,*]=yGraphicDisplayName+': Rrs > '+strcompress(binThresholds[binIndex], /REMOVE)+' '+andSymbol+' F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)
          ;histoMainTitlesCheck2[0,0]='Non Flagged Cases ('+trueFlagInNames[0]+': F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+')'
          prefix2='2_flag_attrib_exclusive_cases_pixs_eq_'+strcompress(binThresholds[binIndex], /REMOVE)
          fullFileNameHistoCheck2[*, *]=operator->buildOperatorResultFileName(periodType, prefix2, monthList, yearList, singleStatToApply, '', outputDir+path_sep()+histoFolder, INTERVAL=INTERVAL, /FULLPATH)
          reportMonthlyTimeSeriesDataCheck2[k, parDataIndex, timeIndex,0]=countDaysRRS_POS_ZERO_FLAGS
          
          for jj=1, flagInfoNum-1 do begin
            indexAsString=strcompress(jj, /REMOVE)
            flagTSDDataString=flagDataPrefix+indexAsString
            flagTSDDataString=+'('+flagTSDDataString+' ne 0)'
            stringToExec=checkStringPart1+flagTSDDataString+checkStringPart4+'SINGLE_FLAG)'
            res=execute(stringToExec)
            print, countDaysSINGLE_FLAG
            reportMonthlyTimeSeriesDataCheck7[k, parDataIndex, timeIndex,0,jj-1]=countDaysSINGLE_FLAG
          endfor
          
          globalCheck=''
          for cvCheckIndex=0, cvNum-1 do begin
            histoMainTitlesCheck3[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+')'
            histoMainTitlesCheck4[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+' '+andSymbol+' F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+')'
            histoMainTitlesCheck5[statIndex, cvCheckIndex, 0]='Homogeneus Cases ('+yGraphicDisplayName+': C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+' '+andSymbol+' F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+' '+andSymbol+' '+'QC!Ds!N!X '+')'
            
            ; each single check... hard coded...
            for kk=0, thrInParNum-1 do begin
              indexAsString=strcompress(kk+cvInParNum, /REMOVE)
              localIndexAsString=strcompress(kk, /REMOVE)
              mTSDDataString=mDataPrefix+indexAsString
              
              title1='Homogeneus Cases ('+yGraphicDisplayName+':'
              title1=title1+' C!Dv!N!X '+'< '+string(format='(f3.1)', cvThresholds[cvCheckIndex])+' '+andSymbol
              title1=title1+' '+thrInParNames[kk]+' '+leqSymbol+string(format='(f3.1)', meanThresholds[kk])+' '+andSymbol
              title1=title1+' F!DE!N!X '+symbol+strcompress(binThresholds[binIndex], /REMOVE)+')'
              ;title2='Homogeneus Cases ('
              ;title2=title2+thrInParNames[kk]+' '+leqSymbol+' '+string(format='(f3.1)', meanThresholds[kk])+')'
              histoMainTitlesCheck6[statIndex, cvCheckIndex, kk]=title1
              ;histoMainTitlesBinFreqAbsSigmaSingleExtra[statIndex, cvCheckIndex, kk,1]=title2
              
              thisCheck='('+mTSDDataString+' le meanThresholds['+localIndexAsString+'])'
              thisCheck=thisCheck+' and '
              thisCheck=thisCheck+'('+mTSDDataString+' ne 0) and '
              globalCheck=globalCheck+thisCheck
              
              ; 6a/b/c) Rrs positives and Rrs cv AND zero flags AND multiple pars thresholds
              execString=checkStringPart1+cvCheck+thisCheck+checkStringPart2+checkStringPart3+zeroFlagsCondition+checkStringPart4+'ALL_CHECK_SINGLE_THR)'
              res=execute(execString)
              print, countDaysALL_CHECK_SINGLE_THR
              
              reportMonthlyTimeSeriesDataCheck6[k, parDataIndex, timeIndex, cvCheckIndex, kk]=countDaysALL_CHECK_SINGLE_THR
              
              ;doLog, 'single check',thrInParCodes[kk], countDays3, LEVEL=4
            endfor
            ; 3) Rrs positives AND Rrs cv
            execString=checkStringPart1+cvCheck+checkStringPart2+checkStringPart3+checkStringPart4+'CV)'
            res=execute(execString)
            print, countDaysCV
            ; 4) Rrs positives and Rrs cv AND zero flags
            execString=checkStringPart1+cvCheck+checkStringPart2+checkStringPart3+zeroFlagsCondition+checkStringPart4+'RRS_CV_ZF)'
            res=execute(execString)
            print, countDaysRRS_CV_ZF
            ; 5) Rrs positives and Rrs cv AND zero flags AND multiple pars thresholds
            execString=checkStringPart1+cvCheck+thrCheck+checkStringPart2+checkStringPart3+zeroFlagsCondition+checkStringPart4+'RRS_CV_ZF_MUL_THR)'
            res=execute(execString)
            print, countDaysRRS_CV_ZF_MUL_THR
            
            reportMonthlyTimeSeriesDataCheck3[k, parDataIndex, timeIndex, cvCheckIndex]=countDaysCV
            reportMonthlyTimeSeriesDataCheck4[k, parDataIndex, timeIndex, cvCheckIndex]=countDaysRRS_CV_ZF
            reportMonthlyTimeSeriesDataCheck5[k, parDataIndex, timeIndex, cvCheckIndex]=countDaysRRS_CV_ZF_MUL_THR
          endfor
          ;res=execute(checkStringPart1+cvCheck+checkStringPart2+checkStringPart3+checkStringPart4+')')
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
  
  monthlyData={reportMonthlyTimeSeriesDataCheck1:reportMonthlyTimeSeriesDataCheck1, $
    fullFileNameHistoCheck1:fullFileNameHistoCheck1, $
    histoMainTitlesCheck1:histoMainTitlesCheck1, $
    reportMonthlyTimeSeriesDataCheck2:reportMonthlyTimeSeriesDataCheck2, $
    fullFileNameHistoCheck2:fullFileNameHistoCheck2, $
    histoMainTitlesCheck2:histoMainTitlesCheck2, $
    reportMonthlyTimeSeriesDataCheck3:reportMonthlyTimeSeriesDataCheck3, $
    fullFileNameHistoCheck3:fullFileNameHistoCheck3, $
    histoMainTitlesCheck3:histoMainTitlesCheck3, $
    reportMonthlyTimeSeriesDataCheck4:reportMonthlyTimeSeriesDataCheck4, $
    fullFileNameHistoCheck4:fullFileNameHistoCheck4, $
    histoMainTitlesCheck4:histoMainTitlesCheck4, $
    reportMonthlyTimeSeriesDataCheck5:reportMonthlyTimeSeriesDataCheck5, $
    fullFileNameHistoCheck5:fullFileNameHistoCheck5, $
    histoMainTitlesCheck5:histoMainTitlesCheck5, $
    reportMonthlyTimeSeriesDataCheck6:reportMonthlyTimeSeriesDataCheck6, $
    fullFileNameHistoCheck6:fullFileNameHistoCheck6, $
    histoMainTitlesCheck6:histoMainTitlesCheck6, $
    reportMonthlyTimeSeriesDataCheck7:reportMonthlyTimeSeriesDataCheck7, $
    fullFileNameHistoCheck7:fullFileNameHistoCheck7, $
    histoMainTitlesCheck7:histoMainTitlesCheck7, $
    foundMonthlyTimeSeriesData:foundMonthlyTimeSeriesData}
    
  return, monthlyData
  
end