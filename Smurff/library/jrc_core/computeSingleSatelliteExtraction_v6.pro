PRO computeSingleSatelliteExtraction_v6

  ; run the procedure run_$sensor$_L2_extraction before calling this one
  ;sensorList=['MODISA','SEAWIFS', 'VIIRS', 'MERIS', 'JRCSEAWIFS']
  ;sensorList=['MERIS']
  ;siteList=['ASea']

  ;sensorList=['SEAWIFS']
  ;siteList=['LSea']

  ;sensorList=['MODISA']
  ;siteList=['MSea', 'SPG', 'EIO']

  sensorList=['JRCSEAWIFS']
  ;siteList=['LSea', 'NPO', 'CSea', 'EIO', 'ASea', 'MSea', 'NAO']

  ;sensorList=['MERIS']
  ;sensorList=['MODISA']
  ;sensorList=['VIIRS', 'MERIS']
  ;siteList=['NPO','MSea', 'CSea', 'ASea', 'NAO', 'LSea', 'EIO']
  ;siteList=['LSea']
  ;siteList=['Msea']
  siteList=['SoS', 'BSea']
  ;siteList=['MSea']
  ;siteList=['NPO', 'MSea']
  ;statDir='/exports/local1/mariomi/application/smurff/extractStat/'
  ;statDir='E:\projects\idl\smurff\extractStat\'
  ;statDir='E:\projects\idl\smurff\save\'
  ;sourceDirPrefix='E:\projects\idl\smurff\data\global\calibration\'
  ;sourceDirPostfix='\timeseries\daily\1km\L2\raw\'
  statDir='/net/joralla/exports/local1/mariomi/application/smurff/save/'
  ;statDir='/exports/local1/mariomi/application/smurff/save/'
  sourceDirPrefix='/net/joralla/exports/local1/mariomi/application/smurff/data/global/calibration/'
  ;sourceDirPrefix='/exports/local1/mariomi/application/smurff/data/global/calibration/'
  sourceDirPrefix='/net/joralla/exports/local1/mariomi/application/smurff/data/global/calibration/'
  sourceDirPostfix='/timeseries/daily/1km/L2/raw/'
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
        ;thisFile=strlowcase(satInfo[index].ASSIGNEDEXTRACTIONFILE)
        thisFile=satInfo[index].ASSIGNEDEXTRACTIONFILE
        ;print, thisFile 
        chlafile=thisFile
        tSens=strupcase(thisSensor)
        a1=strpos(strlowcase(thisFile), strlowcase(tSens))
        thisFile=strmid(thisfile,0,a1)+strupcase(tSens)+strmid(thisFile, a1+strlen(tSens), 100)
        tSite=strlowcase(thisSite)
        par='chlor_a'
        parPos=strpos(thisFile, par)
        prefix=strmid(thisFile,0,parPos)
        postfix=strmid(thisFile,parPos+strlen(par),strlen(thisFile)-parPos+strlen(par))
        
        ;parList=['flag', 'chlor_a', 'angstrom', 'aot_865', 'rrs_443', 'rrs_490', 'rrs_555', 'kd_490', 'Rrs_412', 'Rrs_510', 'Rrs_670', $
        ;  'ozone', 'zwind', 'mwind', 'water_vapor', 'pressure', 'humidity', 'solz', 'sola', 'senz', 'sena']
        parList=['flag', 'chlor_a', 'angstrom', 'Aot_865', 'Rrs_443', 'Rrs_490', 'Rrs_555', 'kd_490', 'Rrs_412', 'Rrs_510', 'Rrs_670', $
          'ozone', 'zwind', 'mwind', 'water_vapor', 'pressure', 'humidity', 'solz', 'sola', 'senz', 'sena']
        parNo=n_elements(parList)
        parFiles=strarr(parNo)
        sizes=0.
        for m=0, parNo-1 do begin
          ;parFiles[m]=sourceDirPrefix+strlowcase(sensorList[i])+sourceDirPostfix+prefix+parList[m]+postfix
          parFiles[M]=sourceDirPrefix+strlowcase(sensorList[I])+sourceDirPostfix+prefix+parList[M]+postfix
          check=file_info(parFiles[m])
          if check.size eq 0 then begin
            parFiles[m]=file_Search(parFiles[m], FOLD_CASE=1)
            check=file_info(parFiles[m])
          endif
          sizes=sizes+check.size
        endfor
        if sizes eq 0 then begin
          print, 'skip'
          continue
        endif
        print, 'check in progress'

; store as sqrt(m^2+z^2)
;        store-->zwind           6        1999
;        store-->mwind           6        1999
        
        ; no files, no check...

        rightDate=strmid(postfix, 0, 7)
        dayofyear=strmid(rightDate, 4, 3)
        year=strmid(rightDate, 0, 4)
        
        dayMonth=extractDayInfo(dayofyear, year)
        destSat[index].dayofyear=long(dayofyear)
        destSat[index].day=dayMonth[0]
        destSat[index].month=dayMonth[1]
        destSat[index].year=long(year)

        thisIdx=where(parList eq 'flag') & restore, parFiles[thisIdx] & flagData=data
        thisIdx=where(parList eq 'angstrom') & restore, parFiles[thisIdx] & angData=data
        thisIdx=where(parList eq 'Aot_865') & restore, parFiles[thisIdx] & aotData=data
        thisIdx=where(parList eq 'chlor_a') & restore, parFiles[thisIdx] & chlaData=data
        thisIdx=where(parList eq 'Rrs_443') & restore, parFiles[thisIdx] & rrs443Data=data
        thisIdx=where(parList eq 'Rrs_490') & restore, parFiles[thisIdx] & rrs490Data=data
        thisIdx=where(parList eq 'Rrs_555') & restore, parFiles[thisIdx] & rrs555Data=data

        thisIdx=where(parList eq 'kd_490') & restore, parFiles[thisIdx] & kd490Data=data
        thisIdx=where(parList eq 'Rrs_412') & restore, parFiles[thisIdx] & rrs412Data=data
        thisIdx=where(parList eq 'Rrs_510') & restore, parFiles[thisIdx] & rrs510Data=data
        thisIdx=where(parList eq 'Rrs_670') & restore, parFiles[thisIdx] & rrs670Data=data

        thisIdx=where(parList eq 'solz') & restore, parFiles[thisIdx] & solzData=data
        thisIdx=where(parList eq 'sola') & restore, parFiles[thisIdx] & solaData=data
        thisIdx=where(parList eq 'senz') & restore, parFiles[thisIdx] & senzData=data
        thisIdx=where(parList eq 'sena') & restore, parFiles[thisIdx] & senaData=data

        thisIdx=where(parList eq 'zwind') & restore, parFiles[thisIdx] & zWindData=data
        thisIdx=where(parList eq 'mwind') & restore, parFiles[thisIdx] & mWindData=data
        modWindData=sqrt(zWindData^2+mWindData^2)
        thisIdx=where(parList eq 'ozone') & restore, parFiles[thisIdx] & ozoneData=data
        thisIdx=where(parList eq 'water_vapor') & restore, parFiles[thisIdx] & water_vaporData=data
        thisIdx=where(parList eq 'pressure') & restore, parFiles[thisIdx] & pressureData=data
        thisIdx=where(parList eq 'humidity') & restore, parFiles[thisIdx] & humidityData=data
        
        flagToBit=flagIntToBin(flagData, NOFLAG=NOFLAG)
        destSat[index].flagsPixel=flagToBit
        if ~keyword_set(NOFLAG) then for m=0, n_elements(flagToBit)-1 do destSat[index].flagsTotal[m]=total(*(flagToBit[m]))

         matchUPStruct=computeMatchUp_v6(flagData, flagToBit, $
          rrs443Data,rrs490Data, rrs555Data, $
          chlaData, angData, aotData, $
          kd490Data, solzData, $
          rrs412Data,rrs510Data,rrs670Data, $
          solaData, senzData, senaData, $
          modWindData, $
          ozoneData, water_vaporData, pressureData, humidityData)

        destSat[index].matchUpConditions=matchUPStruct.desc
        destSat[index].plotTitles=matchUPStruct.plotTitles
        destSat[index].matchUpResults=matchUPStruct.values
        destSat[index].nonFlaggedCount=matchUPStruct.nonFlaggedCount
        destSat[index].statParameters=matchUPStruct.statParameters
        destSat[index].statDesc=matchUPStruct.statDesc
        destSat[index].statParameters=matchUPStruct.statParameters
        destSat[index].statMeans=matchUPStruct.statMeans
        destSat[index].statStdDev=matchUPStruct.statStdDev

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
