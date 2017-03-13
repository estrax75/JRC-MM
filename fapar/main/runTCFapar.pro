pro runTCFapar, confDir, sourceDir, tempDir, outputDir, startYear, endYear, startMonth, endMonth, level, platform, $
  TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, data_dir=data_dir, $
  SIGMA_WEIGHTED=SIGMA_WEIGHTED, CLOUDTYPELIST=CLOUDTYPELIST, APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, $
  PIXELS_PROCESS=PIXELS_PROCESS, IGNORE_MISSING_DATA=IGNORE_MISSING_DATA, OVERWRITE=OVERWRITE

  ;confDir='E:\mariomi\Documents\projects\LDTR\data\AVHRR\data'

  years=indgen(endYear-startYear+1)+startYear
  months=indgen(endMonth-startMonth+1)+startMonth

  indicator='LAN'
  spatialResolution='0005D'
  level='L2'
  missionName='N'
  ;outputDir='/space2/storage/projects/'

  ;years3=[2006]
  ;months2=indgen(12)+1
  ;months2=indgen(8)+4
  startDay=1
  ;monthDays=[30]

  utility=obj_new('Utility')
  if n_elements(MISSIONOVERLAPINDEX) eq 0 then MISSIONOVERLAPINDEX=0
  if n_elements(CLOUDTYPELIST) eq 0 then CLOUDTYPELIST=1  

  for y=0, n_elements(years)-1 do begin
    for m=0, n_elements(months)-1 do begin
      for ctIndex=0, n_elements(CLOUDTYPELIST)-1 do begin
        thisYear=years[y]
        thisMonth=months[m]
        if n_elements(missionIndex) eq 0 then begin
          noaanumber=getAVHRRNOAANumber(thisYear, undef)
          if n_elements(noaanumber) gt 1 then noaanumber=noaanumber[MISSIONOVERLAPINDEX]
          noaanumber=noaanumber[0]
        endif else begin
          noaanumber=fix(missionIndex)
        endelse
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

        sourceDir=getsourcedir_by_year(thisYear)
        if n_elements(outputDir) eq 0 then outputDir=sourceDir

        ;      resFile=doFaparComposite(sensor, resolution, missionName, level, confDir, sourceDir, tempDir, outputDir, thisYear, thisMonth, $
        ;        TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE)
        resFile=doTimeCompositeFapar_split(platform, indicator, spatialResolution, level, missionName, mainVarName, noaanumber, thisyear, thisMonth, day, $
          sourceDir, outputDir, tempdir, $
          NC=NC, HDF=HDF, CSV=CSV, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, $
          TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, data_dir=data_dir, $
          CLOUDTYPE=CLOUDTYPELIST[ctIndex], APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, SIGMA_WEIGHTED=SIGMA_WEIGHTED, $
          PIXELS_PROCESS=PIXELS_PROCESS, IGNORE_MISSING_DATA=IGNORE_MISSING_DATA, FIRST_LOOK=FIRST_LOOK, OVERWRITE=OVERWRITE)
        if resFile eq -1 then print, thisyear, thismonth, ' skip (already exists or missing/corrupted source file)
        ;resFile=AVH01_merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
        ;resFile=merge_BRFGlob(file1, file2, file3, confDir, thisYear, thisMonth, thisDay, noaanumber, operatorObj, fsObj, tempDir, testFile=testFile)
        print, '... done'
      endfor
    endfor
  endfor
  delidlvar, MISSIONOVERLAPINDEX
  obj_destroy, utility

END