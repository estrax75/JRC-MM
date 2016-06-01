PRO computeSingleSatelliteExtraction

  sensorList=['MODISA','SEAWIFS', 'VIIRS', 'MERIS']
  siteList=['NPO','MSea', 'CSea', 'ASea', 'NAO', 'LSea', 'EIO']
  ;statDir='/exports/local1/mariomi/application/smurff/extractStat/'
  statDir='E:\projects\idl\smurff\extractStat\'
  sourceDirPrefix='E:\projects\idl\smurff\data\global\calibration\'
  sourceDirPostfix='\timeseries\daily\1km\L2\raw\'
  summaryStats=replicate({sensor:'', site:'', numberOfAvailableFiles:0l, extractFiles:0l, numberOfSingleDays:0l, $
    numberOfPotentialDays:0l, alongTrackExclude: 0l, anyReasonExclude: 0l},$
    n_elements(sensorList)*n_elements(siteList))
  l=0
  for i=0, n_elements(sensorList)-1 do begin
    thisSensor=sensorList[i]
    for j=0, n_elements(siteList)-1 do begin
      thisSite=siteList[j]
      fileToRead=statDir+'satInfo'+'_'+thisSensor+'_'+thisSite+'.sav'
      summaryStats[l].site=thisSite
      summaryStats[l].sensor=thisSensor
      delIDLVar, satInfo
      fileName=file_Search(fileToread, FOLD_CASE=1)
      if fileName ne '' then restore, fileName else continue
      ;summaryStats={numberOfAvailableFiles:0l, extractFiles:0l, numberOfSingleDays:0l, numberOfPotentialDays:0l}
      
      inputFileNo=n_elements(satInfo[*])
      summaryStats[l].numberOfAvailableFiles=inputFileNo
      
      cc=where(satInfo[*].ASSIGNEDEXTRACTIONFILE ne '', count)
      summaryStats[l].extractFiles=count
      
      ;date correction & extra diagnostic infos
      destSat=replicate(getSatelliteInfoStructAdvanced(), inputFileNo)
      ;destSat[*].siteName=satInfo[*].siteName
      ;destSat[*].sensorName=satInfo[*].sensorName
      destSat[*].sourceFile=satInfo[*].sourceFile
      destSat[*].assignedExtractionFile=satInfo[*].assignedExtractionFile
      destSat[*].straightExtractionFile=satInfo[*].straightExtractionFile
      destSat[*].istatus=satInfo[*].istatus
      destSat[*].orbitNumber=satInfo[*].orbitNumber
      destSat[*].edgeEWflag=satInfo[*].edgeEWflag
      destSat[*].edgeNSflag=satInfo[*].edgeNSflag
      destSat[*].findPixelistatus=satInfo[*].findPixelistatus

      for k=0, count-1 do begin
        index=cc[k]
        thisFile=strlowcase(satInfo[index].ASSIGNEDEXTRACTIONFILE)
        print, thisFile 
        chlafile=thisFile
        tSens=strlowcase(thisSensor)
        tSite=strlowcase(thisSite)
        par='chlor_a'
        parPos=strpos(thisFile, par)
        prefix=strmid(thisFile,0,parPos)
        postfix=strmid(thisFile,parPos+strlen(par),strlen(thisFile)-parPos+strlen(par))
        
        flagFile=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'flag'+postfix
        chlafile=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'chlor_a'+postfix
        angFile=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'angstrom'+postfix
        aotFile=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'aot_865'+postfix
        rrs443File=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'rrs_443'+postfix
        rrs490File=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'rrs_490'+postfix
        rrs555File=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'rrs_555'+postfix

        check=file_info([flagFile, chlafile, angFile, aotFile, rrs443File, rrs490File, rrs555File])
        if check[0].size eq 0 then flagFile=file_Search(flagFile, FOLD_CASE=1)
        if check[1].size eq 0 then chlafile=file_Search(chlafile, FOLD_CASE=1)
        if check[2].size eq 0 then angFile=file_Search(angFile, FOLD_CASE=1)
        if check[3].size eq 0 then aotFile=file_Search(aotFile, FOLD_CASE=1)
        if check[4].size eq 0 then rrs443File=file_Search(rrs443File, FOLD_CASE=1)
        if check[5].size eq 0 then rrs490File=file_Search(rrs490File, FOLD_CASE=1)
        if check[6].size eq 0 then rrs555File=file_Search(rrs555File, FOLD_CASE=1)
        
        check=file_info([flagFile, chlafile, angFile, aotFile, rrs443File, rrs490File, rrs555File])
        if total(check.size) eq 0 then continue
        ; no files, no check...

        rightDate=strmid(postfix, 0, 7)
        dayofyear=strmid(rightDate, 4, 3)
        year=strmid(rightDate, 0, 4)
        
        dayMonth=extractDayInfo(dayofyear, year)
        destSat[index].dayofyear=long(dayofyear)
        destSat[index].day=dayMonth[0]
        destSat[index].month=dayMonth[1]
        destSat[index].year=long(year)

        restore, flagFile & flagData=data
        restore, angFile & angData=data
        restore, aotFile & aotData=data
        restore, chlafile & chlaData=data
        restore, rrs443File & rrs443Data=data
        restore, rrs490File & rrs490Data=data
        restore, rrs555File & rrs555Data=data
        
        flagToBit=flagIntToBin(flagData, NOFLAG=NOFLAG)
        destSat[index].flagsPixel=flagToBit
        if ~keyword_set(NOFLAG) then for m=0, n_elements(flagToBit)-1 do destSat[index].flagsTotal[m]=total(*(flagToBit[m]))
        
        matchUPStruct=computeMatchUp(flagToBit, angData, aotData, chlaData, rrs443Data,rrs490Data, rrs555Data)
        destSat[index].matchUpConditions=matchUPStruct.desc
        destSat[index].matchUpResults=matchUPStruct.values
        destSat[index].matchUp1Count=matchUPStruct.matchUp1Count
        destSat[index].nonFlaggedCount=matchUPStruct.nonFlaggedCount
        ;NbDayMax: potential number of days  ( ~365 * number of years)
        ;-> NbDays/NbDayMax , number of occurrences with 2 extractions for the same day.
        
        ;NbMatchups= number of potential match-ups (with criteria to define)
        ;-> NbMatchups/NbDays, NbMatchups/NbDayMax, NbMatchups/ number of years
        
        
        ;NbPixels = number of pixels extracted
        ;For all flags:
        ;flag=1 if raised for one pixel:
        ;Frequency of occurrence of the flag (%): 100 * sum_over_all_pixels_over_all_extractions / NbDays / NbPixels
        
      endfor
      advancedInfoFileName=statDir+'advSatInfo'+'_'+thisSensor+'_'+thisSite+'.sav'
      save, destSat, fileName=advancedInfoFileName
      validFiles=satInfo[cc].ASSIGNEDEXTRACTIONFILE
      
      singleFiles= validFiles[UNIQ(validFiles, SORT(validFiles))]
      
      uniqueDays=n_elements(singleFiles)
      ;5), 6a), 6b),6c), 7) --> during computation of the data
      summaryStats[l].numberOfSingleDays=uniqueDays
      
      alongTrackNo=where(satInfo[*].iStatus le 0, count)
      summaryStats[l].anyReasonExclude=count
      
      alongTrackNo=where(satInfo[*].iStatus le 0 and satInfo[*].edgeNSFlag eq 1, count)
      summaryStats[l].alongTrackExclude=count
      
      ;5), 6a), 6b),6c), 7) --> during computation of the data
      
      cc=where(satInfo[*].STRAIGHTEXTRACTIONFILE ne '', count)
      allFiles=satInfo[cc].STRAIGHTEXTRACTIONFILE
      allFiles= allFiles[UNIQ(allFiles, SORT(allFiles))]
      firstFile=allFiles[0]
      lastFile=allFiles[n_elements(allFiles)-1]
      
      print, firstFile, lastFile
      stringIdx=strpos(firstFile, thisSensor)
      firstYear=strmid(firstFile, stringIdx-15, 4)
      stringIdx=strpos(lastFile, thisSensor)
      lastYear=strmid(lastFile, stringIdx-15, 4)
      
      lastDay=julday(12, 31, long(lastYear))
      firstDay=julday(1, 1, long(firstYear))
      days=lastDay-firstDay+1
      summaryStats[l].numberOfPotentialDays=days
      l++
    endfor
  endfor
  idxs=where(summaryStats.sensor eq 'MODISA')
  totalongTrackExclude=total(summaryStats[idxs].alongTrackExclude)
  numberOfSingleDays=total(summaryStats[idxs].numberOfSingleDays)
  numberOfPotentialDays=total(summaryStats[idxs].numberOfPotentialDays)
  extractFiles=total(summaryStats[idxs].extractFiles)
  numberOfAvailableFiles=total(summaryStats[idxs].numberOfAvailableFiles)
  ;save, summaryStats, fileName=statDir+'summary.sav'
  
END
