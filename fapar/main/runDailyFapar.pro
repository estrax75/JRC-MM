pro runDailyFapar, confDir, tempDir, startYear, endYear, startMonth, endMonth, missionIndex, $
  NC=NC, HDF=HDF, CSV=CSV, outputDir=outputDir, datadir=datadir, DISKSPACE=DISKSPACE, $
  OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, $
  PIXELS_PROCESS=PIXELS_PROCESS, coeffFile=coeffFile

  ;confDir='E:\mariomi\Documents\projects\LDTR\data\AVHRR\data'

  years=indgen(endYear-startYear+1)+startYear
  months=indgen(endMonth-startMonth+1)+startMonth

  ;sensor='AVHRR'
  ;sensor='AVH09C1'
  
  ;if keyword_set(PIXELS_PROCESS) then instrument='AVH09C1' else instrument='AVH'
  instrument='AVH'
  indicator='LAN'
  spatialResolution='0005D'
  level='L1'
  missionName='N'
  ;missionName='NOAA-N'

  ;years3=[2006]
  ;months2=indgen(12)+1
  ;months2=indgen(8)+4
  startDay=1
  ;monthDays=[30]

  utility=obj_new('Utility')

  for y=0, n_elements(years)-1 do begin
    thisYear=years[y]
    if n_elements(missionIndex) eq 0 then begin
      noaanumber=getAVHRRNOAANumber(thisYear, undef)
      if n_elements(noaanumber) gt 1 then noaanumber=noaanumber[MISSIONOVERLAPINDEX]
      noaanumber=noaanumber[0]
    endif else begin
      noaanumber=fix(missionIndex)
    endelse
    for m=0, n_elements(months)-1 do begin
      monthDays=utility->calcDayOfMonth([years[y],months[m],1,0])
      ;monthDays=6
      for d=startDay, monthDays do begin
        ;for d=0, n_elements(monthDays)-1 do begin
        thisDay=d
        ;thisDay=monthDays[d]
        thisYear=years[y]
        thisMonth=months[m]
        yearDay=utility->calcDayOfYear([thisYear,thisMonth,thisDay,0])+1
        print, 'working on', thisYear, thisMonth, thisDay, '...'
        if n_elements(datadir) eq 1 then sourceDir=dataDir else sourceDir=getsourcedir_by_year(thisYear) 
        if n_elements(outputDir) eq 0 then outputDir=sourceDir
        resFile=doDailyFapar(instrument, indicator, spatialResolution, level, missionName, mainVarName, noaanumber, thisyear, thismonth, thisday, $
          sourceDir, outputDir, tempdir, $
          NC=NC, HDF=HDF, CSV=CSV, $; /FIRST, 
          MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, $
          OVERWRITE=OVERWRITE, DISKSPACE=DISKSPACE, $
          PIXELS_PROCESS=PIXELS_PROCESS, coeffFile=coeffFile)
        if resFile eq -1 then print, thisyear, thismonth, thisday, ' skip (already exists or missing/corrupted source file)
        ;resFile=AVH01_merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
        ;resFile=merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
        print, '... done'
      endfor
    endfor
  endfor
  obj_destroy, utility

END