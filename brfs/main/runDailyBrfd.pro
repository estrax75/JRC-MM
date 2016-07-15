;@/fapar/compile
;@/library/ncdf_tools/compile
;@//home/mariomi/IDLWorkspace85/Library/library/ncdf_tools/compile
;@/reader/compile
;@/colors/compile
;@/avhrr/compile
;@/main/fapar_uncertainties.pro
pro runDailyBrdf, confDir, sourceDir, tempDir, destDir, startYear, endYear, startMonth, endMonth, $
  OVERWRITE=OVERWRITE, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, $
  HDF=HDF, NC=NC

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  ;confDir='E:\mariomi\Documents\projects\LDTR\data\AVHRR\data'

  ; wrong 085 / 26.3
  ; wrong 095 / 5.4
  ; wrong 106 / 16.4
  ; missing 108 / 18.4
  ; wrong 159 / 8.6
  ; wrong 178 / 27.6
  ; wrong 271 / 28.9
  ; ?wrong 273 / 30.9
  ;years=[1999,2003]
  ;years1=[2004]
  years=indgen(endYear-startYear+1)+startYear
  months=indgen(endMonth-startMonth+1)+startMonth
  ;years3=[2006]
  ;months2=indgen(12)+1
  ;months2=indgen(8)+4
  startDay=1
  ;startDay=5

  ;months=[]
  ;months=[1,8]
  ;months=[8]
  ;monthDays=[30]
  ;months=[1,7,8]
  ;monthDays=[5,10,15,20,25]

  ;rootDir1='E:\mariomi\Documents\projects\LDTR\data\AVHRR\NOAA\surface_refl'
  ;rootDir1='/space3/storage/products/AVHRR_LDTR'
  ;sensor1='AVHRR-Land_v004'
  ;missionCode1='NOAA'

  ;rootDir2='E:\mariomi\Documents\projects\LDTR\data\AVHRR\AVH'
  ;rootDir2='/space3/storage/products/AVHRR_LDTR'
  mainVar='BRF'
  resolution='GEOG_0.05DEG'

  ;rootDir3='E:\mariomi\Documents\projects\LDTR\data\input\AVHRR'
  ;sensor3='GLOBAL_L3_GEOG_0.05DEG'
  ;missionCode3='NOAA-16' ;number coming from year info by mapping "table"

  ;rootDir4='E:\mariomi\Documents\projects\LDTR\data\input\BRDF'
  ;sensor4='GLOBAL_P17_GEOG_0.05DEG'
  ;missionCode4='NOAA-16'

  ;AVH09C1.A2003339.N16.004.2014112030717
  ;years=years2
  ;months=months2
  for y=0, n_elements(years)-1 do begin
    thisYear=years[y]
    originalNoaanumber=getAVHRRNOAANumber(thisYear, undef)
    ;stop
    noaanumber=originalNoaanumber
    if n_elements(noaanumber) gt 1 then noaanumber=originalNoaanumber[MISSIONOVERLAPINDEX]
    for m=0, n_elements(months)-1 do begin
      monthDays=ST_utils->calcDayOfMonth([years[y],months[m],1,0])
      for d=startDay, monthDays do begin
        ;for d=0, n_elements(monthDays)-1 do begin
        thisDay=d
        ;thisDay=monthDays[d]

        thisMonth=months[m]
        yearDay=ST_utils->calcDayOfYear([thisYear,thisMonth,thisDay,0])+1
        print, 'working on', thisYear, thisMonth, thisDay, '...'

        sensor='AVHRR-Land_v004_AVH09C1'
        missionCode='NOAA' ;number coming from year info by mapping "table"
        testFileLand=build_Land_Avh_FileName_D(sensor, missionCode, thisYear, thisMonth, thisDay, sourceDir[0], /full, MISSIONNUMBER=noaanumber, /YEARFOLDER)
        file=file_search(testFileLand, count=count)
        if count eq 1 then landstyle=1
        if count ne 1 then begin
          print, 'try (Land) overlap noaa number...'
          if n_elements(noaanumber) gt 1 then noaanumber=originalNoaanumber[1-MISSIONOVERLAPINDEX]
          testFileLand=build_Land_Avh_FileName_D(sensor, missionCode, thisYear, thisMonth, thisDay, sourceDir[0], /full, /JULDAY, MISSIONNUMBER=noaanumber, /YEARFOLDER)
          ;file=file_search(testFile, count=count, FOLD_CASE=1)
          file=file_search(testFileLand, count=count)
          if count eq 0 then begin
            print, 'AvhrrLand not found year/month/day (skip)', years[y], months[m], thisDay
            ;a=dialog_message(testFile, /INFO)
            continue
          endif else begin
            file=file[0]
            landStyle=1
          endelse
        endif

        if count ne 1 then begin
          ;testFile1=buildAvhrrLandFileName_D(sensor1, missionCode1, thisYear, thisMonth, thisDay, checkDir, /full)
          sensor='AVH09C1'
          missionCode='N' ;number coming from year info by mapping "table"
          testFileLDTR=build_LDTR_Avh_FileName_D(sensor, missionCode, thisYear, thisMonth, yearDay, sourceDir[1], /full, /JULDAY, MISSIONNUMBER=noaanumber, /YEARFOLDER)
          ;noaanumber=16
          ;testFile3=buildAvhrrGlobFileName_D(sensor3, missionCode3, thisYear, thisMonth, yearDay, rootDir3, /full, /JULDAY)
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
          ;file1=file_search(testFile1, count=count1)
          file=file_search(testFileLDTR, count=count)
          ;file3=file_search(testFile3, count=count3)
          if count ne 1 then begin
            print, 'try (LDTR) overlap noaa number...'
            if n_elements(noaanumber) gt 1 then noaanumber=originalNoaanumber[1-MISSIONOVERLAPINDEX]
            testFileLDTR=build_LDTR_Avh_FileName_D(sensor, missionCode, thisYear, thisMonth, yearDay, sourceDir[1], /full, /JULDAY, MISSIONNUMBER=noaanumber, /YEARFOLDER)
            ;file=file_search(testFile, count=count, FOLD_CASE=1)
            file=file_search(testFileLDTR, count=count)
            if count eq 0 then begin
              print, 'AvhrrLDTR not found year/month/day', years[y], months[m], thisDay
              ;a=dialog_message(testFile, /INFO)
              ;continue
            endif else begin
              file=file[0]
            endelse
            landStyle=0
          endif
        endif

        ;        if count2 ne 1 then begin
        ;          file2=file_search(testFile2, count=count2, FOLD_CASE=1)
        ;          if count2 eq 0 then begin
        ;            print, 'Avh skip year/month/day', years[y], months[m], thisDay
        ;            continue
        ;          endif else begin
        ;            file2=file2[0]
        ;          endelse
        ;        endif
        ;        if count2 ne 1 then begin
        ;          file2=file_search(testFile2, count=count2, FOLD_CASE=1)
        ;          if count2 ne 1 then begin
        ;            print, 'Avh skip year/month/day', years[y], months[m], dayNum
        ;            continue
        ;          endif
        ;        endif
        ;        if count3 ne 1 then begin
        ;          file3=file_search(testFile3, count=count3, FOLD_CASE=1)
        ;          if count3 eq 0 then begin
        ;            print, 'Glob deg 0.5 (NOAA xx) skip year/month/day', thisYear, thisMonth, thisDay
        ;            ;continue
        ;          endif else begin
        ;            file3=file3[0]
        ;          endelse
        ;        endif
        ;        if count4 ne 1 then begin
        ;          file4=file_search(testFile4, count=count3, FOLD_CASE=1)
        ;          if count4 ne 1 then begin
        ;            print, 'Glob deg 0.5 (P17 NOAA 14) skip year/month/day', years[y], months[m], dayNum
        ;            continue
        ;          endif
        ;        endif
        ;        print, 'file1: ', file1
        ;        print, 'file2: ', file2
        print, 'file: ', file
        ;resFile=doBrf(file, confDir, thisYear, thisMonth, thisDay, sensor, missionCode, noaanumber, resolution, mainVar, outputDir, tempDir, testFile=testFile, /OVERWRITE)
        if keyword_set(landStyle) then begin
          ;file=build_Land_Avh_FileName_D(sensor, missionCode, thisYear, thisMonth, yearDay, sourceDir[0], /full, /JULDAY, MISSIONNUMBER=noaanumber, /YEARFOLDER)
          resFile=doBrfLand(file, confDir, thisYear, thisMonth, thisDay, sensor, missionCode, noaanumber, resolution, mainVar, destDir, tempDir, $
            testFile=testFile, OVERWRITE=OVERWRITE, HDF=HDF, NC=NC)
        endif else begin
          ;file=build_LDTR_Avh_FileName_D(sensor, missionCode, thisYear, thisMonth, yearDay, sourceDir[1], /full, /JULDAY, MISSIONNUMBER=noaanumber, /YEARFOLDER)
          resFile=doBrfLDTR(file, confDir, thisYear, thisMonth, thisDay, sensor, missionCode, noaanumber, resolution, mainVar, destDir, tempDir, $
            testFile=testFile, OVERWRITE=OVERWRITE, HDF=HDF, NC=NC)
        endelse
        print, '... done'
      endfor
    endfor
  endfor

END