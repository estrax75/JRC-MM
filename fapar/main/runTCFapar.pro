pro runTCFapar, confDir, sourceDir, tempDir, outputDir, startYear, endYear, startMonth, endMonth, $
  MONTHLY=MONTHLY, FIVEDAYS=FIVEDAYS, TENDAYS=TENDAYS, SIXTEENDAYS=SIXTEENDAYS, ELAB_TC=ELAB_TC, ELAB_MEAN=ELAB_MEAN

  ;confDir='E:\mariomi\Documents\projects\LDTR\data\AVHRR\data'

  years=indgen(endYear-startYear+1)+startYear
  months=indgen(endMonth-startMonth+1)+startMonth

  ;sensor='AVHRR'
  sensor='AVH09C1'
  resolution='GEOG_0.05DEG'
  missionName='N'
  ;missionName='NOAA-N'
  mainVarName='FAPAR'

  ;years3=[2006]
  ;months2=indgen(12)+1
  ;months2=indgen(8)+4
  startDay=1
  ;monthDays=[30]

  utility=obj_new('Utility')

  for y=0, n_elements(years)-1 do begin
    for m=0, n_elements(months)-1 do begin
      thisYear=years[y]
      thisMonth=months[m]
      ;missionCode=getAVHRRNOAANumber(thisYear, undef)
      ;        testFile1=buildAvhrrLandFileName_D(sensor1, missionCode1, thisYear, thisMonth, thisDay, rootDir1, /full)
      ;        testFile2=buildAvhFileName_D(sensor2, missionCode2, thisYear, thisMonth, yearDay, rootDir2, /full, /JULDAY)
      ;        noaanumber=16
      ;        testFile3=buildAvhrrGlobFileName_D(sensor3, missionCode3, thisYear, thisMonth, yearDay, rootDir3, /full, /JULDAY)
      ;        testFile='GLOBAL_L3_GEOG_0.05DEG_001-001_03.NOAA-16.hdf'
      ;        if thisYear lt 2010 then begin
      ;          noaanumber=16
      ;          testFile3=buildAvhrrGlobFileName_D(sensor3, missionCode3, thisYear, thisMonth, yearDay, rootDir3, /full, /JULDAY)
      ;          testFile='GLOBAL_L3_GEOG_0.05DEG_001-001_03.NOAA-16.hdf'
      ;        endif
      ;        if thisYear eq 1999 then begin
      ;          testFile3=buildAvhrrGlobFileName1_D(sensor4, missionCode4, thisYear, thisMonth, yearDay, rootDir4, /full, /JULDAY)
      ;          noaanumber=16
      ;          testFile='GLOBAL_P17_GEOG_0.05DEG_182-182_99.NOAA-14.hdf'
      ;        endif
      ;        if thisYear eq 2004 then begin
      ;          testFile3=buildAvhrrGlobFileName1_D(sensor4, missionCode4, thisYear, thisMonth, yearDay, rootDir4, /full, /JULDAY)
      ;          noaanumber=16
      ;          testFile='GLOBAL_P17_GEOG_0.05DEG_182-182_99.NOAA-14.hdf'
      ;        endif
      ;        file1=file_search(testFile1, count=count1)
      ;        file2=file_search(testFile2, count=count2)
      ;        file3=file_search(testFile3, count=count3)
      ;        if count1 ne 1 then begin
      ;          file1=file_search(testFile1, count=count1, FOLD_CASE=1)
      ;          if count1 eq 0 then begin
      ;            print, 'AvhrrLand skip year/month/day', years[y], months[m], thisDay
      ;            ;continue
      ;          endif else begin
      ;            file1=file1[0]
      ;          endelse
      ;        endif
      ;        if count2 ne 1 then begin
      ;          file2=file_search(testFile2, count=count2, FOLD_CASE=1)
      ;          if count2 eq 0 then begin
      ;            print, 'Avh skip year/month/day', years[y], months[m], thisDay
      ;            continue
      ;          endif else begin
      ;            file2=file2[0]
      ;          endelse
      ;        endif
      ;        ;        if count2 ne 1 then begin
      ;        ;          file2=file_search(testFile2, count=count2, FOLD_CASE=1)
      ;        ;          if count2 ne 1 then begin
      ;        ;            print, 'Avh skip year/month/day', years[y], months[m], dayNum
      ;        ;            continue
      ;        ;          endif
      ;        ;        endif
      ;        if count3 ne 1 then begin
      ;          file3=file_search(testFile3, count=count3, FOLD_CASE=1)
      ;          if count3 eq 0 then begin
      ;            print, 'Glob deg 0.5 (NOAA xx) skip year/month/day', thisYear, thisMonth, thisDay
      ;            ;continue
      ;          endif else begin
      ;            file3=file3[0]
      ;          endelse
      ;        endif
      ;        ;        if count4 ne 1 then begin
      ;        ;          file4=file_search(testFile4, count=count3, FOLD_CASE=1)
      ;        ;          if count4 ne 1 then begin
      ;        ;            print, 'Glob deg 0.5 (P17 NOAA 14) skip year/month/day', years[y], months[m], dayNum
      ;        ;            continue
      ;        ;          endif
      ;        ;        endif
      ;        print, 'file1: ', file1
      ;        print, 'file2: ', file2
      ;        print, 'file3: ', file3

      resFile=doFaparComposite(sensor, resolution, missionName, confDir, sourceDir, tempDir, outputDir, thisYear, thisMonth, $
        MONTHLY=MONTHLY, TENDAYS=TENDAYS, SIXTEENDAYS=SIXTEENDAYS, FIVEDAYS=FIVEDAYS, $
        ELAB_TC=ELAB_TC, ELAB_MEAN=ELAB_MEAN)
      if resFile eq -1 then print, thisyear, thismonth, ' skip (already exists or missing/corrupted source file)
      ;resFile=AVH01_merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
      ;resFile=merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
      print, '... done'
    endfor
  endfor
  obj_destroy, utility

END