@../../library/core/structure_definition


; .run /home/melinfr/OC/CAL/MER/read_hdf_dataset
; .run /home/melinfr/OC/CAL/MER/GetMERName
; .run /home/melinfr/OC/CAL/MER/GetMERInfo

; .run /home/melinfr/OC/CAL/MER/FindPixel

; .run /home/melinfr/OC/CAL/MER/read_hdf_data

; .run /home/melinfr/OC/CAL/MER/ExtractSubset
; .run /home/melinfr/OC/CAL/MER/SelectData

; .run /home/melinfr/OC/CAL/MER/ExtractDataAEROC


;PRO mer_ExtractData,k_site,option,n_side,in_dir,out_dir,root, siteInfoStruct
PRO mer_ExtractData, option, in_dir, out_dir, sensorFileFilter, parList, siteInfoStruct, operator, dType, sensor, $
  originalVarList=originalVarList, overwrite=overwrite, satInfo=satInfo

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
  ;findvariable = STRCOMPRESS(in_dir+root,/REMOVE_ALL)

  ; List of files.
  ;filename=FINDFILE(findvariable, count=NbFiles)
  ;filenameList=file_search(varFilter, count=NbFiles, fold_case=1);
  filenameList=file_search(sensorFileFilter, count=NbFiles, fold_case=1, /FULLY_QUALIFY);
  cd, curr
  if sitelon gt 150 and sitelon lt 180.2 then HOUR_SHIFT=12
  if sitelon gt -180.1 and sitelon lt -150 then HOUR_SHIFT=-12
  satInfo=replicate(getSatInfoStruct(), NbFiles)

  ; Loop over the files.
  FOR f=0,NbFiles-1 DO BEGIN

    ;print,'-----------------------------'
    ;print,filenameList[f]

    sfname=strsplit(filenameList[f], path_sep(), /extract)
    sfname=sfname[n_elements(sfname)-1]
    startYear=strmid(sfname, 1, 4)
    startDay=strmid(sfname, 5, 3)
    testFileName=operator->buildOperatorResultFileName(dType, originalVarList[0], startday, startyear, sensor, ext, out_dir, $
      JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH, LOWCASE=LOWCASE)
    satInfo[f].straightExtractionFile=testFileName
    ;    if FILE_TEST(out_dir+path_sep()+testFileName) and ~keyword_set(overwrite) then begin
    ;      print, 'skip'
    ;      continue
    ;    endif
    ;    print, 'good'

    rootstr = STRMID(STRCOMPRESS(filenameList[f],/REMOVE_ALL),STRLEN(in_dir), $
      STRLEN(STRCOMPRESS(filenameList[f],/REMOVE_ALL))-STRLEN(in_dir)-7)

    ; Define output

    ofile = rootstr

    ofile = STRCOMPRESS(out_dir+ofile+".L2_LAC"+"."+ext,/REMOVE_ALL)

    ; Get all MERIS information useful for processing: time, location, size

    GetMERInfo,filenameList[f],DataStruct,1, HOUR_SHIFT=HOUR_SHIFT

    nline = DataStruct.NumberLine
    nelem = DataStruct.NumberPixel

    ; Find nearest pixel to searched location.
    mer_FindPixel,DataStruct,sitelat,sitelon,n_side,line,elem,istatus
    
    satInfo[f].sourceFile=filenameList[f]
    satInfo[f].findPixelistatus=istatus

    satInfo[f].edgeNSflag=(istatus eq 1) and (line LT n_e OR line GE nline-n_e) ; 
    satInfo[f].edgeEWflag=(istatus eq 1) and (elem LT n_e OR elem GE nelem-1-n_e) ; test 19 -->  =1 (istatus)

    satInfo[f].orbitnumber=DataStruct.orbitnumber
    
    dataInfo=getDateAttrib(DataStruct)
    satInfo[f].dayOfYear=dataInfo[0]
    satInfo[f].day=dataInfo[1]
    satInfo[f].month=dataInfo[2]
    satInfo[f].year=dataInfo[3]
    satInfo[f].hour=DataStruct.starthour
    print, 'date conversion:', dataInfo, satInfo[f].hour 
    

    ; Check if the pixel is not close to edges
    IF ( istatus EQ 1 AND ( line LE n_e OR line GE nline-n_e OR elem LE n_e OR elem GE nelem-1-n_e ) ) THEN istatus = -1
    satInfo[f].istatus=istatus

    IF ( istatus GT 0 ) THEN BEGIN

      ; Get the geophysial values.
      mer_SelectData,filenameList[f],DataStruct,line,elem,n_side,SData,option

      SData.Latitude = sitelat
      SData.Longitude = sitelon
      SData.Name = rootstr

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
        ; 20160408: check if wind -> store module (or adding an extra band)
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

END

 