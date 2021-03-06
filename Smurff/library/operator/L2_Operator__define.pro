@..\wrapper\structure_definition
FUNCTION L2_Operator::doParCodeCorrection, parCodes

  parTest=strupcase(parCodes)
  parCheckIdx=where(parTest eq 'CHLOR_A', count)
  if count ge 1 then parTest[parCheckIdx]='CHLA'
  return, parTest

END

FUNCTION L2_Operator::getLocalBand

  b=*self.localBand
  return, b

END

PRO L2_Operator::setLocalBandFromExtraction, extractInfo, parCode, FAIL=FAIL

  FAIL=1
  data=extractInfo.data
  tagIdx=where(strupcase(tag_names(data)) eq strupcase(parCode), count)
  if count eq 1 then begin
    thisParValues=data.(tagIdx)
    if (n_elements(thisParValues) eq 1 and thisParValues[0] eq -9999) then begin
      doLog, 'par:',parCode,'... No values', LEVEL=4
      return
    endif
    self->setLocalBand, thisParValues, parCode, /SET_NAN
    FAIL=0
    return
  endif
  doLog, 'par:',parCode,'... Not a field of structure (check naming convention, sensor based)', LEVEL=4
  doLog, 'pars available:', tag_names(data), LEVEL=4
  doLog, 'Check and fill --doParCodeCorrection-- method of your operator', LEVEL=4

END

PRO L2_Operator::setLocalBand, band, bandName, SET_NAN=SET_NAN

  lBand=band
  if keyword_set(SET_NAN) then begin
    nanValue=float(self.app->getKeyValue('NAN_VALUE'))
    idxs=where(band eq nanValue, count)
    if count gt 1 then lBand[idxs]=!VALUES.F_NAN
  endif

  ptr_free, self.localBand
  self.localBand=ptr_new(lBand, /NO_COPY)
  self.localBandName=bandName

END

PRO L2_Operator::writeResult, date, year, sensor, roi, parCode, $
  archiveDir=archiveDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, JULDAY=JULDAY, INTERVAL=INTERVAL, $
  outScallingTypeList=outScallingTypeList, applyFormula=applyFormula, fileName=fileName

  COMMON countModis, voidData, totalData

  if n_elements(totalData) eq 0 then begin
    totalData=0l
    voiddata=0l
  endif
  totalData++
  data=self->getLocalBand()
  if keyword_set(archiveDir) then fullFileName=archiveDir+path_sep()+fileName else fullFileName=fileName

  aa=where(finite(data), c)
  if c eq 0 then voidData++
  doLog, 'void:',  voidData, ' on ', totalData, LEVEL=4
  ;save, data, fileName='/net/joralla/'+fullFileName
  save, data, fileName='/net/joralla'+path_sep()+fullFileName

END

FUNCTION L2_Operator::readSavVar, fullFileName, FOUND=FOUND

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

  restore, fullFileName
  if n_elements(data) eq 0 then begin
    doLog, fullFileName, 'unexpected contents... skip', LEVEL=4
    return, 0
  endif
  FOUND=1
  return, data

END

FUNCTION L2_Operator::importBand, dType, parCodes, date, year, roiCode, roiArchiveCode, archiveDir, NF=NF, $
  day=day, targetMapInfo=targetMapInfo, report=report, DUPLICATE=DUPLICATE

  ;dataMatrix=fltarr(targetMapInfo.mapPixelExtension, /NO)
  ;dataMatrix[*,*]=!VALUES.F_NAN

  if n_elements(report) eq 0 then report=getInvalidStruct()
  data=-1 & i=0
  ;for i=0, n_elements(parCodes)-1 do begin
  sensor=self.sensorCode
  ;savFilename = self->buildOperatorResultFileName(dType, parInfos[i].displayName, date, year, sensor, roiCode, archiveDir, JULDAY=dType eq 'D', /FULL, /LOWCASE)
  if keyword_set(DUPLICATE) then testSensor=sensor else testSensor=sensor+'_1'
  print, testSensor
  savFilename = self->buildOperatorResultFileName(dType, parCodes, date, year, testSensor, roiArchiveCode, archiveDir, JULDAY=dType eq 'D', /FULL, /LOWCASE)
  singleData=self->readSavVar(savFilename, FOUND=FOUND)

  report.found=0 & NF=1

  if keyword_set(FOUND) then begin
    ; force dimensions... (or perform a compatible data extraction...)
    report.found=1 & NF=0
    dims=sqrt(n_elements(singleData))
    dataMatrix=reform(singleData, dims, dims)
  endif else begin
    return, 0
  endelse

  sizeInfo=size(dataMatrix, /STR)
  mask=byte(dataMatrix)
  mask[*,*]=1b
  bandInfo={data:dataMatrix, mask:mask, nl:sizeInfo.dimensions[1], ns:sizeInfo.dimensions[1], dt:sizeInfo.type}

  report.found=1
  if ~keyword_set(NF) then begin
    ; raw check, no time... :-(
    validIdxs=where(bandInfo.data ne -9999 and bandInfo.data gt 0 and finite(bandInfo.data), count, complement=complement, ncomplement=ncomplement)
    report.expected=n_elements(bandInfo.data)
    report.found=1
    report.invalid_count=ncomplement
    report.SIGMA_FILTER=0
    report.VALID_COUNT=report.expected
    if ncomplement gt 0 then bandInfo.data[complement]=!VALUES.F_NAN
    data=bandInfo.data
  endif
  return, data

END

FUNCTION L2_Operator::buildOperatorResultFileName, dType, displayName, month, year, sensor, roi, archiveDir, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH, LOWCASE=LOWCASE

  return, call_function('buildL2SavFileName'+'_'+dType, $
    year, month, dType, roi, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, LOWCASE=LOWCASE)
  ;  if dType eq 'M' then return, buildBioMapVarFileName_M(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  ;  if dType eq 'D' then return, buildBioMapVarFileName_D(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)

END

PRO L2_Operator::CleanUp

  ptr_free, self.localBand
  self->GenericOperator::Cleanup

END

FUNCTION L2_Operator::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
  REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY, sensorCode=sensorCode

  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  if keyword_set(sensorCode) then self.sensorCode=sensorCode
  return, 1

END

PRO L2_Operator__Define

  Struct = { L2_Operator , $
    sensorCode: '', $
    localBand : ptr_new(), $
    localBandName : '', $
    Inherits GenericOperator $
  }

END