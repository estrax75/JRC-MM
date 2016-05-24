
; Procedures needed:
; .run /home/melinfr/OC/CAL/SWF/read_nc4_data

; .run /home/melinfr/OC/CAL/SWF/ExtractSubset

; .run /home/melinfr/OC/CAL/SWF/SWFInfo
; .run /home/melinfr/OC/CAL/SWF/FindPixel

; Execution
; .run /home/melinfr/OC/CAL/SWF/ExtractDataAEROC

;PRO ExtractData,k_site,option,n_side,in_dir,out_dir,root
PRO jrcswf_ExtractData, option, in_dir, out_dir, sensorFileFilter, parList, siteInfoStruct, operator, dType, yearList, sensor, $
  originalVarList=originalVarList, overwrite=overwrite, satInfo=satInfo


  ; Site chosen for extraction of data.

  sensor='JRCSEAWIFS'
  if n_elements(siteInfoStruct) eq 1 then begin
    sitelat = siteInfoStruct.lat;34.
    sitelon = siteInfoStruct.lon;25.
    siteFolder = siteInfoStruct.ext;"msea"
    ext=siteFolder
    n_side = siteInfoStruct.n_side;n_side
  endif



  ; Size of extraction (extraction of a square of n_side x n_side pixels) with n_side = 2n_e +1
  n_e = FIX ( (n_side-1)/2 + 0.1)

  ; type of the files to be read
  cd, curr=curr
  test1=in_dir+path_sep()+siteFolder
  test2=in_dir+path_sep()+strupcase(siteFolder)

  chk1=fileInfo(test1)
  chk2=fileInfo(test2)

  if chk1.exist then cd, test1
  if chk2.exist then cd, test2

  allFilenameList=file_search(sensorFileFilter, count=NbFiles, fold_case=1, /FULLY_QUALIFY););
  cd, curr
  delta_time = 200L
  if sitelon gt 150 and sitelon lt 180.2 then HOUR_SHIFT=12
  if sitelon gt -180.1 and sitelon lt -150 then HOUR_SHIFT=-12
  satInfo=replicate(getSatInfoStruct(), NbFiles)

  ;for j=0, n_elements(yearList)-1 do begin
  ;thisYear=YearList[j]
  ;checkList=where(strpos(allFilenameList, thisYear) ne -1, count)
  ;if count eq 0 then continue
  ;filenameList=allFilenameList[checkList]
  ;NbFiles=count

  ;print,'Number of files: ',NbFiles
  ; Delta time for LAC data: 166msec.

  ; Loop over the files.
  filenameList=allFilenameList
  FOR f=0,NbFiles-1 DO BEGIN
    ;FOR f=1,1 DO BEGIN

    ;print,filenameList[f]

    ; Define output

    sfname=strsplit(allFilenameList[f], path_sep(), /extract)
    sfname=sfname[n_elements(sfname)-1]
    startYear=strmid(sfname, 1, 4)
    startDay=strmid(sfname, 5, 3)
    testFileName=operator->buildOperatorResultFileName(dType, originalVarList[0], startday, startyear, sensor, ext, out_dir, $
      JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH, LOWCASE=LOWCASE)
    satInfo[f].straightExtractionFile=testFileName
    ;      if FILE_TEST(out_dir+path_sep()+testFileName) and ~keyword_set(overwrite) then begin
    ;        print, 'skip'
    ;        continue
    ;      endif
    ;      print, 'good'

    ifile = STRMID(STRCOMPRESS(filenameList[f],/REMOVE_ALL),STRLEN(in_dir), $
      STRLEN(STRCOMPRESS(filenameList[f],/REMOVE_ALL))-STRLEN(in_dir))

    ofile = STRMID(STRCOMPRESS(filenameList[f],/REMOVE_ALL),STRLEN(in_dir), $
      STRLEN(STRCOMPRESS(filenameList[f],/REMOVE_ALL))-STRLEN(in_dir))

    ofile = STRCOMPRESS(out_dir+ofile+"."+ext,/REMOVE_ALL)

    ; Get all SeaWiFS information useful for processing: time, location, size,...
    JRCSWFInfo,filenameList[f],SeaWiFSStruct, HOUR_SHIFT=HOUR_SHIFT

    SeaWiFSStruct.Source = ifile

    IF ( STRPOS(ifile,"LAC") GE 0 ) THEN ctype = "LAC" ELSE ctype = "GAC"
    SeaWiFSStruct.Type = ctype


    ; Find nearest pixel to searched location.
    jrcswf_FindPixel,SeaWiFSStruct,sitelat,sitelon,n_side,line,elem,istatus

    nline = SeaWiFSStruct.NumberLine
    nelem = SeaWiFSStruct.NumberPixel

    mm = LONARR(n_side)
    ; Check if the pixel is not close to edges and if the line are temporally contiguous.
    satInfo[f].sourceFile=filenameList[f]
    satInfo[f].findPixelistatus=istatus
    satInfo[f].edgeNSflag=(istatus eq 1) and (line LT n_e OR line GE nline-n_e) ; 
    satInfo[f].edgeEWflag=(istatus eq 1) and (elem LT n_e OR elem GE nelem-1-n_e) ; test 19 -->  =1 (istatus)
    ;satInfo[f].edgeNSflag=(line ge 0) and (line LE n_e OR line GE nline-n_e)
    ;satInfo[f].edgeEWflag=(elem ge 0) and (elem LE n_e OR elem GE nelem-1-n_e)
    satInfo[f].orbitnumber=SeaWiFSStruct.orbitnumber

    dataInfo=getDateAttrib(SeaWiFSStruct)
    satInfo[f].dayOfYear=dataInfo[0]
    satInfo[f].day=dataInfo[1]
    satInfo[f].month=dataInfo[2]
    satInfo[f].year=dataInfo[3]
    satInfo[f].hour=SeaWiFSStruct.starttime
    print, 'date conversion:', dataInfo, satInfo[f].hour 

    IF ( istatus EQ 1 ) THEN BEGIN

      IF ( line LE n_e OR line GE nline-n_e OR elem LE n_e OR elem GE nelem-n_e ) THEN BEGIN
        istatus = -1
      ENDIF ELSE BEGIN

        FOR l=0,n_side-1 DO mm[l] = SeaWiFSStruct.msec[line+l-n_e]

        FOR l=0,n_side-2 DO IF ( mm[l+1]-mm[l] GT delta_time ) THEN istatus = -1

      ENDELSE
    ENDIF

    satInfo[f].istatus=istatus
    IF ( SeaWiFSStruct.StartJDay NE SeaWiFSStruct.EndJDay ) THEN BEGIN

      IF ( SeaWiFSStruct.StartMillisec GT mm[0] ) THEN BEGIN
        SeaWiFSStruct.StartJDay = SeaWiFSStruct.EndJDay
        SeaWiFSStruct.StartJYear = SeaWiFSStruct.EndJYear
      ENDIF

      IF ( SeaWiFSStruct.EndMillisec LT mm[n_side-1] ) THEN BEGIN
        SeaWiFSStruct.EndJDay = SeaWiFSStruct.StartJDay
        SeaWiFSStruct.EndYear = SeaWiFSStruct.StartYear
      ENDIF

    ENDIF

    SeaWiFSStruct.StartMillisec = mm[0]
    SeaWiFSStruct.EndMillisec = mm[n_side-1]

    ; Go ahead only if a contiguous square of pixels is available around the site.
    IF ( istatus EQ 1 ) THEN BEGIN

      ; Get the geophysical values.
      jrcswf_SelectData,filenameList[f],SeaWiFSStruct,line,elem,n_side,SData,option

      SData.Latitude = sitelat
      SData.Longitude = sitelon

      FAIL=1
      parCheck=1
      testSensor=sensor
      for jj=0, n_elements(parList)-1 do begin
        ;fileName=operator->buildOperatorResultFileName(dType, originalVarList[jj], sdata.startday, sdata.startyear, sensor, ext, out_dir, $
        ;override wrong startDay from swf_SelectData (sdata.startday is day of month instead day of year)
        fileName=operator->buildOperatorResultFileName(dType, originalVarList[jj], dataInfo[0] , dataInfo[3], testSensor, ext, out_dir, $
          JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH, LOWCASE=LOWCASE)
        ;for cc=1, 9 do begin
        if testSensor eq sensor then begin
          checkExistence=where(satInfo[*].assignedExtractionFile eq fileName, countCheck)
          if countCheck ne 0 then testSensor=sensor+'_'+strcompress(1, /remove_all)
        endif
        ;testSensor=sensor+'_'+strcompress(cc, /remove_all)
        ;endfor
        operator->setLocalBandFromExtraction, sdata, parList[jj], FAIL=FAIL
        if keyword_set(parCheck) then satInfo[f].assignedExtractionFile=fileName
        DelIdlVar, parCheck
        ;WRITEFILE=1
        ;if FILE_TEST(out_dir+path_sep()+fileName) and ~keyword_set(overwrite) then begin
        ;  doLog,"File duplicated (previous elaboration or the pixel drops in different tiles)", LEVEL=4
        ;  doLog, 'skip-->', startday, sdata.startyear, LEVEL=4
        ;  WRITEFILE=0
        ;  duplicate++
        ;endif
        ;if ~keyword_set(FAIL) and WRITEFILE then begin
        doLog, 'store-->', originalVarList[jj], dataInfo[0], dataInfo[3], LEVEL=4
        fileName=operator->buildOperatorResultFileName(dType, originalVarList[jj], dataInfo[0], dataInfo[3], testSensor, ext, out_dir, $
          JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH, LOWCASE=LOWCASE)
        operator->writeResult, dataInfo[0], dataInfo[3], sensor, ext, archiveDir=out_dir, fileName=fileName
        ;if FILE_TEST(out_dir+path_sep()+fileName) then doLog, 'overwrite...', LEVEL=4
        ;doLog,"data found on: ", filenameList[f], ' assigned to result file: ',fileName, LEVEL=4
        ;doLog,"File not selected", LEVEL=4
        ;endif
      endfor
      ;saveFileName=out_dir+path_sep()+finalName
      ;save, /VARIABLES, SData, filename=saveFileName
    ENDIF ELSE BEGIN
      doLog,"File not selected", LEVEL=4
    ENDELSE


  ENDFOR
  ;endfor

END
