PRO computeSingleSatelliteExtraction_v4

  ; run the procedure run_$sensor$_L2_extraction before calling this one
  ;sensorList=['MODISA','SEAWIFS', 'VIIRS', 'MERIS']
  sensorList=['MODISA', 'SEAWIFS']
  ;sensorList=['MERIS']
  ;sensorList=['MODISA']
  ;sensorList=['VIIRS', 'MERIS']
  ;siteList=['NPO','MSea', 'CSea', 'ASea', 'NAO', 'LSea', 'EIO']
  siteList=['SoS', 'BSea', 'SPG']
  ;siteList=['Msea']
  ;siteList=['SoS', 'BSea']
  ;siteList=['EIO', 'ASea']
  ;siteList=['NPO', 'MSea']
  ;statDir='/exports/local1/mariomi/application/smurff/extractStat/'
  ;statDir='E:\projects\idl\smurff\extractStat\'
  statDir='E:\projects\idl\smurff\save\'
  sourceDirPrefix='E:\projects\idl\smurff\data\global\calibration\'
  sourceDirPostfix='\timeseries\daily\1km\L2\raw\'
  ;statDir='/exports/local1/mariomi/application/smurff/save/'
  ;sourceDirPrefix='/exports/local1/mariomi/application/smurff/data/global/calibration/'
  ;sourceDirPostfix='/timeseries/daily/1km/L2/raw/'
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
      destSat[*].hour=satInfo[*].hour

      for k=0, count-1 do begin
        index=cc[k]
        thisFile=strlowcase(satInfo[index].ASSIGNEDEXTRACTIONFILE)
        ;print, thisFile 
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
        sunZenithFile=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'SUNZENITH'+postfix
        rrs443File=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'rrs_443'+postfix
        rrs490File=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'rrs_490'+postfix
        rrs555File=sourceDirPrefix+sensorList[i]+sourceDirPostfix+prefix+'rrs_555'+postfix

        check=file_info([flagFile, chlafile, angFile, aotFile, sunZenithFile, rrs443File, rrs490File, rrs555File])
        if check[0].size eq 0 then flagFile=file_Search(flagFile, FOLD_CASE=1)
        if check[1].size eq 0 then chlafile=file_Search(chlafile, FOLD_CASE=1)
        if check[2].size eq 0 then angFile=file_Search(angFile, FOLD_CASE=1)
        if check[3].size eq 0 then aotFile=file_Search(aotFile, FOLD_CASE=1)
        if check[4].size eq 0 then sunZenithFile=file_Search(sunZenithFile, FOLD_CASE=1)
        if check[5].size eq 0 then rrs443File=file_Search(rrs443File, FOLD_CASE=1)
        if check[6].size eq 0 then rrs490File=file_Search(rrs490File, FOLD_CASE=1)
        if check[7].size eq 0 then rrs555File=file_Search(rrs555File, FOLD_CASE=1)
        
        check=file_info([flagFile, chlafile, angFile, aotFile, sunZenithFile, rrs443File, rrs490File, rrs555File])
        if total(check.size) eq 0 then continue
        print, 'check in progress'
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
        restore, sunZenithFile & sunZenithData=data
        restore, chlafile & chlaData=data
        restore, rrs443File & rrs443Data=data
        restore, rrs490File & rrs490Data=data
        restore, rrs555File & rrs555Data=data
        
        flagToBit=flagIntToBin(flagData, NOFLAG=NOFLAG)
        destSat[index].flagsPixel=flagToBit
        if ~keyword_set(NOFLAG) then for m=0, n_elements(flagToBit)-1 do destSat[index].flagsTotal[m]=total(*(flagToBit[m]))
;        
;        ;
;        store-->chlor_a           6        1999
;        store-->kd_490           6        1999
;        store-->Rrs_412           6        1999
;        store-->Rrs_443           6        1999
;        store-->Rrs_490           6        1999
;        store-->Rrs_510           6        1999
;        store-->Rrs_555           6        1999
;        store-->Rrs_670           6        1999
;        store-->Aot_865           6        1999
;        store-->angstrom           6        1999
;        store-->ozone           6        1999
;        store-->zwind           6        1999
;        store-->mwind           6        1999
;        store-->water_vapor           6        1999
;        store-->pressure           6        1999
;        store-->humidity           6        1999
;        store-->solz           6        1999
;        store-->sola           6        1999
;        store-->senz           6        1999
;        store-->sena           6        1999
;        store-->flag           6        1999
;        ;
        matchUPStruct=computeMatchUp_v4(flagToBit, chlaData, angData, aotData,  sunZenithData, rrs443Data,rrs490Data, rrs555Data,flagData)
        destSat[index].matchUpConditions=matchUPStruct.desc
        destSat[index].plotTitles=matchUPStruct.plotTitles
        destSat[index].matchUpResults=matchUPStruct.values
        ;destSat[index].matchUp1Count=matchUPStruct.matchUp1Count
        destSat[index].nonFlaggedCount=matchUPStruct.nonFlaggedCount
        destSat[index].statDesc=matchUPStruct.statDesc
        destSat[index].angStat=matchUPStruct.angStat
        destSat[index].aotStat=matchUPStruct.aotStat
        destSat[index].chlaStat=matchUPStruct.chlaStat
        destSat[index].sunZenithStat=matchUPStruct.sunZenithStat
        ; new april 7th 2016
        destSat[index].ozoneStat=matchUPStruct.ozoneStat
        destSat[index].water_vaporStat=matchUPStruct.water_vaporStat
        destSat[index].pressureStat=matchUPStruct.pressureStat
        destSat[index].humidityStat=matchUPStruct.humidityStat
        destSat[index].solzStat=matchUPStruct.solzStat
        destSat[index].solaStat=matchUPStruct.solaStat
        destSat[index].senzStat=matchUPStruct.senzStat
        destSat[index].senaStat=matchUPStruct.senaStat


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
      print, advancedInfoFileName, ' ... write'
      
      ;modis npo 1708 findistatus=1
      ;modis npo 1707 istatus=1
      
      print, '1) -->', total(destSat[cc].matchUpResults[0])
      print, '2) -->', total(destSat[cc].matchUpResults[1])
      print, '3) -->', total(destSat[cc].matchUpResults[2])
      print, '4) -->', total(destSat[cc].matchUpResults[3])
      print, '5) -->', total(destSat[cc].matchUpResults[4])
      print, '6) -->', total(destSat[cc].matchUpResults[5])
      ; 

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
  a=dialog_message('diagnostic summary (step2): done')
  idxs=where(summaryStats.sensor eq 'MODISA')
  totalongTrackExclude=total(summaryStats[idxs].alongTrackExclude)
  numberOfSingleDays=total(summaryStats[idxs].numberOfSingleDays)
  numberOfPotentialDays=total(summaryStats[idxs].numberOfPotentialDays)
  extractFiles=total(summaryStats[idxs].extractFiles)
  numberOfAvailableFiles=total(summaryStats[idxs].numberOfAvailableFiles)
  ;save, summaryStats, fileName=statDir+'summary.sav'
  
END
