; Main call
FUNCTION TimeSeriesOperator::buildMaskConditions, parInfo, scallingType

 eps=self.app->getKeyValue('EPS')
 ;return, 'band eq -9999 or (band gt -'+eps+' and band le '+eps+')'
 return, 'band eq -9999'

END

FUNCTION TimeSeriesOperator::doComputation, dataPtrs, ignoreValue=ignoreValue, pixelMask=pixelMask, fileName=fileName, NO_DATA=NO_DATA

  NODATA=0
  utility=self.app->getUtility()
  tempDir=self.app->getKeyValue('TEMP_DIR')
  fullFileName=tempDir+path_sep()+utility->getSysTime(/FILECOMPATIBILITY)
  validIdxs=self->getCompatibleData(dataPtrs, hideIdxs=hideIdxs, hideCount=hideCount, validCount=validCount)
  for i=0, n_elements(dataPtrs)-1 do begin
    thisBand='band'+strcompress(i+1, /REMOVE_ALL)
    res=execute(thisBand+'=*dataPtrs['+strcompress(i, /REMOVE_ALL)+']')
  endfor  
  res=execute('resBand='+self.formula)
  if hideCount ne 0 then resBand[hideIdxs]=!VALUES.F_NAN

  self->createEnviFileFromBand, resBand, self.resultBandName, fullFileName, /MEMORY, ps=self.ps, mc=self.mc
  ;self->addBand, resBand, self.resultBandName, /MEMORY, /OVERWRITE 
  return, 1
  
end

FUNCTION TimeSeriesOperator::buildOperatorResultFileName, dType, resultBandName, month, year, sensor, roi, archivedir, JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH

  ;pars=*self.parameters
  ;if dType eq 'M' then return, buildCompareFileName_M(pars[0], pars[1], displayName, month, year, sensor, roi)
  ;if dType eq 'D' then return, buildCompareFileName_D(pars[0], pars[1], displayName, month, year, sensor, roi)
  ;if dType eq 'M' then return, buildCompareFileName_M(resultBandName, year, month, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  ;if dType eq 'D' then return, buildCompareFileName_D(resultBandName, year, month, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  return, call_function('buildTimeSeriesFileName'+'_'+dType, $
    year, month, dType, roi, sensor, resultBandName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
    
END

FUNCTION TimeSeriesOperator::getFileNameToExport, month, year, sensor, roi, $
    archiveDir=archiveDir, parInfos=parInfos, NONE=NONE, JULDAY=JULDAY, INTERVAL=INTERVAL
    
  dType=self->getPeriodType()
  ;bandToExport=self.app->getKeyValue('NAN_VALUE')
  NONE=0
  
  parInfos=self->getBandToExportInfoList()
  ncdfFilenames=strarr(n_elements(parInfos))
  for i=0, n_elements(parInfos)-1 do begin
    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roi, JULDAY=dType eq 'D', INTERVAL=INTERVAL)
  endfor
  return, ncdfFilenames
  
END

PRO TimeSeriesOperator::cleanMainFile

  self->removeBand, 'resBand'
;parInfo=self.app->getParameterByCode('kH')
;self->removeBand, parInfo.outputBandName
  
END

FUNCTION TimeSeriesOperator::getBandToExportInfoList

  selectedCode=self.resultBandName
  parInfos=replicate(getParameterInfoStruct(), 1)
  
  thisPar=self.app->getParameterByCode(selectedCode)
  parInfos[0].bandName=thisPar.outputBandName
  parInfos[0].shortName=thisPar.code
  parInfos[0].longName=thisPar.outputBandName
  parInfos[0].displayName=thisPar.outputBandName
  parInfos[0].measureUnit=thisPar.measureUnit
  parInfos[0].description=thisPar.description
  
  return, parInfos
  
END

FUNCTION TimeSeriesOperator::isTest

  return, self.mainFileName eq ''
  
END

FUNCTION TimeSeriesOperator::isEnviType

  return, self.isEnviType
  
END

PRO TimeSeriesOperator::cleanFileSystem

  self->removeMainFile
  
END

PRO TimeSeriesOperator::CleanUp

  self->cleanFileSystem
  self-> GenericOperator::Cleanup
  
END

FUNCTION TimeSeriesOperator::init, application, workingDir, periodType, resultBandName, $
    mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
    
  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  if n_elements(resultBandName) eq 1 then self.resultBandName= resultBandName else self.resultBandName='resultBand' 
  return, 1
  
END

PRO TimeSeriesOperator__Define

  Struct = { TimeSeriesOperator , $
    resultBandName: '', $
    Inherits GenericOperator $
    }
    
END