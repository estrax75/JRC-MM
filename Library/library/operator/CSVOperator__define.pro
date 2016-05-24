@..\wrapper\structure_definition
;FUNCTION CSVOperator::getBandsInfo, bandCodes
;
;  parInfos=replicate(getParameterInfoStruct(), n_elements(bandCodes))
;  for i=0, n_elements(bandCodes)-1 do begin
;    thisPar=self.app->getParameterByCode(bandCodes[i])
;    parInfos[i].bandName=thisPar.outputBandName
;    parInfos[i].shortName=thisPar.code
;    parInfos[i].longName=thisPar.outputBandName
;    parInfos[i].displayName=thisPar.outputBandName
;    parInfos[i].measureUnit=thisPar.measureUnit
;    parInfos[i].description=thisPar.description
;  endfor
;
;  return, parInfos
;
;END

FUNCTION CSVOperator::readCsvVar, fullFileName, header=header, dataColumnName=dataColumnName, lineIndex=lineIndex, lineIndexName=lineIndexName, lineIndexValue=lineIndexValue, FOUND=FOUND

  fs=self.app->getFileSystem()
  if n_elements(header) eq 1 then headerDiscardLines=1
  data=fs->readCSVFile(fullFileName, headerDiscardLines=headerDiscardLines,SEPARATOR=',',headerinfo=headerinfo)
  if n_elements(lineIndex) ne 1 then begin
    columnIndex=where(lineIndexName eq headerinfo, count)
    if count eq 1 then begin
      columnDataToSearch=data[columnIndex, *]
      lineIndex=where(columnDataToSearch eq lineIndexValue, count)
      if count eq 1 then begin
        dataIndex=where(dataColumnName eq headerInfo, count)
        if count eq 1 then value=data[dataIndex, lineIndex]
        FOUND=1
        return, float(reform(value))
      endif
    endif
  endif
  message, fullFileName, 'dataColumnName=', dataColumnName, 'data not found'
  
  
END

FUNCTION CSVOperator::importBand, dType, parCodes, month, year, roiCode, roiArchiveCode, archiveDir, NF=NF, $
    day=day, targetMapInfo=targetMapInfo, report=report, extraDataInfo=extraDataInfo

  dataMatrix=fltarr(targetMapInfo.mapPixelExtension, /NO)
  dataMatrix[*,*]=!VALUES.F_NAN

  if n_elements(report) eq 0 then report=getInvalidStruct()
  parInfos=self->getBandsInfo(parCodes)
  data=-1 & i=0
  ;for i=0, n_elements(parCodes)-1 do begin
  csvFilename = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roiCode, archiveDir, JULDAY=dType eq 'D', /FULL, /LOWCASE)
  singleData=self->readCsvVar(csvFilename, header=parInfos[i].bandName, dataColumnName=parInfos[i].bandName, $
    lineIndexName='Month', lineIndexValue=strcompress(string(fix(month), format='(I2)'), /REMOVE), FOUND=FOUND)
  
  if keyword_set(FOUND) then dataMatrix[*,*]=singleData
  NF=1-keyword_set(FOUND)
  
  sizeInfo=size(dataMatrix, /STR)
  mask=byte(dataMatrix)
  mask[*,*]=1b
  bandInfo={data:dataMatrix, mask:mask, mapInfo:targetMapInfo, nl:sizeInfo.dimensions[1], ns:sizeInfo.dimensions[1], dt:sizeInfo.type}

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
  ;self->importBandAsGeoNc, parInfos[i].bandName, parInfos[i].bandName, ncdfFilename, year, month, outputDir, $
  ;  refFileName=refFileName, extraMaskConditions=extraMaskConditions, $
  ;  applyFormula=applyFormula, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT
  ;endfor
  
END

FUNCTION CSVOperator::buildOperatorResultFileName, dType, displayName, month, year, sensor, roi, archiveDir, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH, LOWCASE=LOWCASE

  return, call_function('buildCsvVarFileName'+'_'+dType, $
    year, month, dType, roi, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, LOWCASE=LOWCASE)
  ;  if dType eq 'M' then return, buildBioMapVarFileName_M(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  ;  if dType eq 'D' then return, buildBioMapVarFileName_D(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  
END

PRO CSVOperator::CleanUp

  self->GenericOperator::Cleanup
  
END

FUNCTION CSVOperator::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
  REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
  
  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  return, 1
  
END

PRO CSVOperator__Define

  Struct = { CSVOperator , $
    Inherits GenericOperator $
  }
  
END