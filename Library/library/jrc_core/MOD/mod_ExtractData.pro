
; .run /home/melinfr/OC/MOD/utilities/GetData
; .run /home/melinfr/OC/MOD/utilities/ReadMODISAGeo

; .run /home/melinfr/OC/MOD/utilities/FindPixel



; .run /home/melinfr/OC/MOD/utilities/read_hdf_data

; .run /home/melinfr/OC/MOD/utilities/ExtractSubset
; .run /home/melinfr/OC/MOD/extract/SelectData

; .run /home/melinfr/OC/MOD/extract/WriteData


; .run /home/melinfr/OC/MOD/extract/ExtractDataAEROC

PRO mod_ExtractData, option, in_dir, out_dir, sensorFileFilter, parList, siteInfoStruct, operator, dType, sensor, $
  originalVarList=originalVarList, overwrite=overwrite, satInfo=satInfo


  sensor='MODISA'
  if n_elements(siteInfoStruct) eq 1 then begin
    sitelat = siteInfoStruct.lat;34.
    sitelon = siteInfoStruct.lon;25.
    siteFolder = siteInfoStruct.ext;"msea"
    ext=siteFolder
    n_side = siteInfoStruct.n_side;n_side
  endif


  ; Site chosen for extraction of data.

  ; MSEA
  ;IF ( k_site EQ 1 ) THEN BEGIN
  ; sitelat = 34.
  ; sitelon = 25.
  ; ext = "msea"
  ;ENDIF


  ; Size of extraction (extraction of a square of n_side x n_side pixels) with n_side = 2n_e +1


  n_e = FIX ( (n_side-1)/2 + 0.1)


  ; type of the files to be read

  varFilter=in_dir+path_sep()+siteFolder+path_sep()+sensorFileFilter

  ;findvariable = STRCOMPRESS(in_dir+sensor+root,/REMOVE_ALL)


  ; List of files.
  filenameList=file_search(varFilter, count=NbFiles, fold_case=1);
  ;filename=FINDFILE(findvariable, count=NbFiles)



  doLog,'Number of files: ',NbFiles
  count=0
  duplicate=0
  discarded=0
  edgesType1=0l
  edgesType2=0l

  ; Loop over the files.
  if sitelon gt 150 and sitelon lt 180.2 then HOUR_SHIFT=12
  if sitelon gt -180.1 and sitelon lt -150 then HOUR_SHIFT=-12

  discardFileList=''
  satInfo=replicate(getSatInfoStruct(), NbFiles)
  
  FOR f=0,NbFiles-1 DO BEGIN


    ;print,'-----------------------------'
    doLog,filenameList[f]

    sfname=strsplit(filenameList[f], path_sep(), /extract)
    sfname=sfname[n_elements(sfname)-1]
    startYear=strmid(sfname, 1, 4)
    startDay=strmid(sfname, 5, 3)
    testFileName=operator->buildOperatorResultFileName(dType, originalVarList[0], startday, startyear, sensor, ext, out_dir, $
      JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH, LOWCASE=LOWCASE)
    satInfo[f].straightExtractionFile=testFileName
;    if FILE_TEST(out_dir+path_sep()+testFileName) and ~keyword_set(overwrite) then begin
;      doLog, 'skip'
;      continue
;    endif
;    doLog, 'good'



    ifile = STRMID(STRCOMPRESS(filenameList[f],/REMOVE_ALL),STRLEN(in_dir), $
      STRLEN(STRCOMPRESS(filenameList[f],/REMOVE_ALL))-STRLEN(in_dir))

    rootstr = STRMID(STRCOMPRESS(filenameList[f],/REMOVE_ALL),STRLEN(in_dir), $
      STRLEN(STRCOMPRESS(filenameList[f],/REMOVE_ALL))-STRLEN(in_dir)-7)



    ; Define geo file

    geofile = rootstr

    geofile = STRCOMPRESS(in_dir+geofile+".GEO",/REMOVE_ALL)

    ; Define output

    ofile = rootstr


    ofile = STRCOMPRESS(out_dir+ofile+".L2_LAC"+"."+ext,/REMOVE_ALL)


    ; Get all MODIS information useful for processing: time, location, size

    MODISInfo,filenameList[f],ModisInfo, HOUR_SHIFT=HOUR_SHIFT
    ;doLog, ModisInfo
    ;    print,'Read Geo'

    IF ( STRPOS(ifile,"LAC") GE 0 ) THEN ctype = "LAC" ELSE ctype = "GAC"
    MODISInfo.Type = ctype

    nline = ModisInfo.NumberLine
    nelem = ModisInfo.NumberPixel

    ; Find nearest pixel to searched location.
    mod_FindPixel,ModisInfo,sitelat,sitelon,n_side,line,elem,istatus

    
    ; Check if the pixel is not close to edges
    satInfo[f].sourceFile=filenameList[f]
    satInfo[f].findPixelistatus=istatus
    satInfo[f].edgeNSflag=(line ge 0) and (line LE n_e OR line GE nline-n_e)
    satInfo[f].edgeEWflag=(elem ge 0) and (elem LE n_e OR elem GE nelem-1-n_e)
    satInfo[f].orbitnumber=MODISInfo.orbitnumber
    
    dataInfo=getDateAttrib(MODISInfo)
    satInfo[f].dayOfYear=dataInfo[0]
    satInfo[f].day=dataInfo[1]
    satInfo[f].month=dataInfo[2]
    satInfo[f].year=dataInfo[3]

    IF ( istatus EQ 1 AND ( line LE n_e OR line GE nline-n_e OR elem LE n_e OR elem GE nelem-1-n_e ) ) THEN istatus = -1
    
    ;    elem = elem-ModisGeo.ExtractPixelOffset


    ;IF ( istatus EQ 1 AND elem GE nelem-1-n_e  ) THEN istatus = -1

    satInfo[f].istatus=istatus
    IF ( istatus GT 0 ) THEN BEGIN

      ; Get the geophysial values.
      mod_SelectData,filenameList[f],ModisInfo,line,elem,n_side,SData,option

      SData.Latitude = sitelat
      SData.Longitude = sitelon
      SData.Name = rootstr
      SData.Source = ifile

      FAIL=1
      parCheck=1
      for jj=0, n_elements(parList)-1 do begin
        fileName=operator->buildOperatorResultFileName(dType, originalVarList[jj], sdata.startday, sdata.startyear, sensor, ext, out_dir, $
          JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH, LOWCASE=LOWCASE)
        if keyword_set(parCheck) then satInfo[f].assignedExtractionFile=fileName
        DelIdlVar, parCheck
        operator->setLocalBandFromExtraction, sdata, parList[jj], FAIL=FAIL
        WRITEFILE=1
        ;if FILE_TEST(out_dir+path_sep()+fileName) and ~keyword_set(overwrite) then begin
        ;  doLog,"File duplicated (previous elaboration or the pixel drops in different tiles)", LEVEL=4
        ;  WRITEFILE=0
        ;  duplicate++
        ;endif
        if ~keyword_set(FAIL) and WRITEFILE then begin
          operator->writeResult, sdata.startday, sdata.startyear, sensor, ext, archiveDir=out_dir, fileName=fileName
          if FILE_TEST(out_dir+path_sep()+fileName) then doLog, 'overwrite...', LEVEL=4
          doLog,"data found on: ", filenameList[f], ' assigned to result file: ',fileName, LEVEL=4
          count++
        endif
      endfor
      ;saveFileName=out_dir+path_sep()+finalName
      ;save, /VARIABLES, SData, filename=saveFileName


      ; Write the various variables into a ascii file.
      ;     WriteData,ofile,SData,sensor,option
    ENDIF ELSE BEGIN
      doLog,"File not selected", LEVEL=4
      discarded++
    ENDELSE


  ENDFOR
  
END
