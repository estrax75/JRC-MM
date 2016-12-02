@..\wrapper\structure_definition
;FUNCTION L2Summary_Operator::importBand, dType, parCodes, date, year, roiCode, roiArchiveCode, archiveDir, NF=NF, $
;  day=day, targetMapInfo=targetMapInfo, report=report, DUPLICATE=DUPLICATE, extraDataInfo=extraDataInfo, ALL_DATA=ALL_DATA
;
;  if n_elements(report) eq 0 then report=getInvalidStruct()
;  data=-1 & i=0
;  ;for i=0, n_elements(parCodes)-1 do begin
;  sensor=self.sensorCode
;  ;savFilename = self->buildOperatorResultFileName(dType, parInfos[i].displayName, date, year, sensor, roiCode, archiveDir, JULDAY=dType eq 'D', /FULL, /LOWCASE)
;  if keyword_set(DUPLICATE) then testSensor=sensor+'_1' else testSensor=sensor
;  ;print, testsensor
;  ;print, testSensor
;  savFilename = self->buildOperatorResultFileName(dType, parCodes, date, year, sensor, roiArchiveCode, archiveDir, JULDAY=dType eq 'D', /FULL, /LOWCASE)
;  newIdx=where(fullSatelliteStat.year eq year and fullSatelliteStat.dayofyear eq date, ccc)
;  if count()
;  L2Op=self->getInternalOperator()
;  fakePar='chlor_a'
;  internalSavFilename = L2Op->buildOperatorResultFileName(dType, fakePar, date, year, testSensor, roiArchiveCode, archiveDir, JULDAY=dType eq 'D', /LOWCASE)
;  fullSatelliteStat=self->readSavVar(savFilename, FOUND=FOUND)
;  idx=where(strupcase(fullSatelliteStat.ASSIGNEDEXTRACTIONFILE) eq strupcase(internalSavFilename), count)
;  if count eq 0 then begin
;    ;use testSensor '_1' option only as second try...
;    internalSavFilename = L2Op->buildOperatorResultFileName(dType, fakePar, date, year, sensor , roiArchiveCode, archiveDir, JULDAY=dType eq 'D', /LOWCASE)
;    idx=where(strupcase(fullSatelliteStat.ASSIGNEDEXTRACTIONFILE) eq strupcase(internalSavFilename), count)
;    if count eq 2 and keyword_set(DUPLICATE) then begin
;      idx=idx[1]
;      count=1
;    endif else begin
;      idx=-1
;      count=0
;    endelse
;  endif
;
;  ; manage two files in a day...
;  ;print, 'date', 'year  ',date, year, duplicate
;  idx=idx[0]
;
;  FOUND=count gt 0
;  NF=1-FOUND
;  data=-1
;
;  errorIdx=where(fullSatelliteStat[*].iStatus eq 1 and fullSatelliteStat[*].ASSIGNEDEXTRACTIONFILE ne '', count)
;  ass=uniq(sort(fullSatelliteStat[*].ASSIGNEDEXTRACTIONFILE))
;  if keyword_set(FOUND) eq 1 then begin
;    doLog, 'FOUND-->', fullSatelliteStat[idx].sourcefile, '    ', fullSatelliteStat[idx].ASSIGNEDEXTRACTIONFILE, LEVEL=4
;
;    ;allAssigned=fullSatelliteStat[*].ASSIGNEDEXTRACTIONFILE
;    ;aa=uniq(allAssigned[sort(allAssigned)])
;    code=fix(parCodes)-1
;    if keyword_set(ALL_DATA) then data=fullSatelliteStat[idx].MATCHUPRESULTS[*] else data=fullSatelliteStat[idx].MATCHUPRESULTS[code]
;    if fullSatelliteStat[idx].iStatus ne 1 then begin
;      stop
;      data[*]=0
;    endif
;    ;extra data position is a good extraction...
;    data=[data, fullSatelliteStat[idx].iStatus]
;    checkVersion=where(strupcase(Tag_Names(fullSatelliteStat[idx])) eq strupcase('plottitles'), count)
;    if count eq 1 then extraDataInfo=fullSatelliteStat[idx].plottitles else extraDataInfo=fullSatelliteStat[idx].MATCHUPConditions
;  endif
;
;  return, data
;
;END

FUNCTION L2Summary_Operator::importBand, dType, parCodes, date, year, roiCode, roiArchiveCode, archiveDir, NF=NF, $
  day=day, targetMapInfo=targetMapInfo, report=report, DUPLICATE=DUPLICATE, extraDataInfo=extraDataInfo, ALL_DATA=ALL_DATA, RESET=RESET
  
  COMMON speed_up, fullSatelliteStat
  
  if keyword_set(RESET) then DelIdlVar, fullSatelliteStat
  if n_elements(report) eq 0 then report=getInvalidStruct()
  data=-1 & i=0
  ;for i=0, n_elements(parCodes)-1 do begin
  sensor=self.sensorCode
  ;savFilename = self->buildOperatorResultFileName(dType, parInfos[i].displayName, date, year, sensor, roiCode, archiveDir, JULDAY=dType eq 'D', /FULL, /LOWCASE)
  if keyword_set(DUPLICATE) then testSensor=sensor+'_1' else testSensor=sensor
  ;print, testsensor
  ;print, testSensor
  savFilename = self->buildOperatorResultFileName(dType, parCodes, date, year, sensor, roiArchiveCode, archiveDir, JULDAY=dType eq 'D', /FULL, /LOWCASE)
  if n_elements(fullSatelliteStat) eq 0 then begin
    fullSatelliteStat=self->readSavVar(savFilename, FOUND=FOUND)
    if ~FOUND then begin
      data=-1
      NF=1
      stop
      return, data
    endif
    issss=where(fullSatelliteStat[*].dayofyear ge 1 and fullSatelliteStat[*].dayofyear le 366 and fullSatelliteStat[*].year ge 2003 and fullSatelliteStat[*].year le 2007 and fullSatelliteStat[*].istatus eq 1 and fullSatelliteStat[*].ASSIGNEDEXTRACTIONFILE ne '', countttt)
    ;fullSatelliteStat[issss]
    doLog, 'total file expected:', countttt, LEVEL=4
  endif
  ;fullSatelliteStat=self->readSavVar(savFilename, FOUND=FOUND)
  idx=where((fullSatelliteStat[*].year eq year) and (fullSatelliteStat[*].dayofyear eq date) and (fullSatelliteStat[*].ASSIGNEDEXTRACTIONFILE ne '') and (fullSatelliteStat[*].istatus eq 1), count)
  ;if count eq 3 then stop
  if count eq 2 and keyword_set(DUPLICATE) then begin
    idx=idx[1]
    count=1
  endif else begin
    if count eq 1 and keyword_set(DUPLICATE) then begin
      idx=-1
      count=0
    endif
  endelse
  
  ; manage two files in a day...
  ;print, 'date', 'year  ',date, year, duplicate
  idx=idx[0]
  
  FOUND=count gt 0
  NF=1-FOUND
  data=-1
  
  ;errorIdx=where(fullSatelliteStat[*].iStatus eq 1 and fullSatelliteStat[*].ASSIGNEDEXTRACTIONFILE ne '', count)
  ;ass=uniq(sort(fullSatelliteStat[*].ASSIGNEDEXTRACTIONFILE))
  if keyword_set(FOUND) eq 1 then begin
    doLog, 'FOUND-->', fullSatelliteStat[idx].sourcefile, '    ', fullSatelliteStat[idx].ASSIGNEDEXTRACTIONFILE, LEVEL=4
    
    ;allAssigned=fullSatelliteStat[*].ASSIGNEDEXTRACTIONFILE
    ;aa=uniq(allAssigned[sort(allAssigned)])
    code=fix(parCodes)-1
    if keyword_set(ALL_DATA) then data=fullSatelliteStat[idx].MATCHUPRESULTS[*] else data=fullSatelliteStat[idx].MATCHUPRESULTS[code]
    data=[data, fullSatelliteStat[idx].iStatus]
    checkVersion=where(strupcase(Tag_Names(fullSatelliteStat[idx])) eq strupcase('plottitles'), count)
    if count eq 1 then extraDataInfo=fullSatelliteStat[idx].plottitles else extraDataInfo=fullSatelliteStat[idx].MATCHUPConditions
  endif
  
  return, data
  
END

FUNCTION L2Summary_Operator::readSavVar, fullFileName, FOUND=FOUND

  ERROR=0
  catch, error_status
  FOUND=0
  
  if error_status NE 0 THEN BEGIN
    catch, /CANCEL
    doLog, 'problem with : ', fullFileName, ' not found, not readable or with unexpected contents. Skip.', level=4
    FOUND=0
    return, -1
  endif
  
  testFile=file_test(fullFileName)
  if ~testFile then begin
    ;avoid linux / case sensitive filesystem, only when needed
    nfullFileName=file_Search(fullFileName, /fold_case)
    testFile=file_test(nfullFileName)
    if ~testFile then begin
      doLog, fullFileName, 'not found... skip', LEVEL=4
      return, 0
    endif
    fullFileName=nfullFileName
  endif
  
  restore, fullFileName;, /verbose
  if n_elements(destSat) eq 0 then begin
    doLog, fullFileName, 'unexpected contents... skip', LEVEL=4
    return, 0
  endif
  FOUND=1
  return, destSat
  
END

FUNCTION L2Summary_Operator::buildOperatorResultFileName, dType, displayName, month, year, sensor, roi, archiveDir, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH, LOWCASE=LOWCASE
  
  return, call_function('buildL2SummarySavFileName'+'_'+dType, $
    year, month, dType, roi, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, LOWCASE=LOWCASE)
  ;  if dType eq 'M' then return, buildBioMapVarFileName_M(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  ;  if dType eq 'D' then return, buildBioMapVarFileName_D(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  
END

FUNCTION L2Summary_Operator__define::doParCodeCorrection, parCodes

  parTest=strupcase(parCodes)
  parCheckIdx=where(parTest eq 'CHLOR_A', count)
  if count ge 1 then parTest[parCheckIdx]='CHLA'
  parCheckIdx=where(parTest eq 'RRS_555', count)
  if count ge 1 then parTest[parCheckIdx]='RRS_560'
  parCheckIdx=where(parTest eq 'ANGSTROM', count)
  if count ge 1 then parTest[parCheckIdx]='ANGSTROM_510'
  return, parTest
  
END

PRO L2Summary_Operator__define::CleanUp

  self->L2_Operator::Cleanup
  
END

FUNCTION L2Summary_Operator__define::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
  REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY, sensorCode=sensorCode
  
  if not (self -> L2_Operator :: init(application, workingDir, periodType, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  if ~keyword_set(sensorCode) then message, 'You must define sensor code'
  self.sensorCode=sensorCode
  return, 1
  
END

PRO L2Summary_Operator__Define

  Struct = { L2Summary_Operator , $
    Inherits L2_Operator $
  }
  
END