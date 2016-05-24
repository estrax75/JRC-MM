; Main call
FUNCTION CompareOperator::buildMaskConditions, parInfo, scallingType

 eps=self.app->getKeyValue('EPS')
 ;return, 'band eq -9999 or (band gt -'+eps+' and band le '+eps+')'
 return, 'band eq -9999'

END

FUNCTION CompareOperator::doComputation, dataPtrs, ignoreValue=ignoreValue, pixelMask=pixelMask, fileName=fileName, NO_DATA=NO_DATA

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
  ;if validCount ne 0 then stop
  self->createEnviFileFromBand, resBand, self.resultBandName, fullFileName, /MEMORY, ps=self.ps, mc=self.mc
  b1=band1 & band1[*]=!VALUES.F_NAN
  if validCount gt 0 then band1[validIdxs]=b1[validIdxs] 
  b2=band2 & band2[*]=!VALUES.F_NAN
  if validCount gt 0 then band2[validIdxs]=b2[validIdxs]

  band1=*dataPtrs[0]
  band2=*dataPtrs[1]
  self->addBand, band1, 'chl_reg', /MEMORY, /OVERWRITE 
  self->addBand, band2, 'chl_glob', /MEMORY, /OVERWRITE
  self->addBand, b1, 'chl_reg_match', /MEMORY, /OVERWRITE 
  self->addBand, b2, 'chl_glob_match', /MEMORY, /OVERWRITE
  return, 1
  
end

FUNCTION CompareOperator::buildOperatorResultFileName, dType, resultBandName, month, year, sensor, roi, archivedir, JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH

  ;pars=*self.parameters
  ;if dType eq 'M' then return, buildCompareFileName_M(pars[0], pars[1], displayName, month, year, sensor, roi)
  ;if dType eq 'D' then return, buildCompareFileName_D(pars[0], pars[1], displayName, month, year, sensor, roi)
  ;if dType eq 'M' then return, buildCompareFileName_M(resultBandName, year, month, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  ;if dType eq 'D' then return, buildCompareFileName_D(resultBandName, year, month, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  return, call_function('buildCompareFileName'+'_'+dType, $
    year, month, dType, roi, sensor, resultBandName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
    
END

FUNCTION CompareOperator::getFileNameToExport, month, year, sensor, roi, $
    archiveDir=archiveDir, parInfos=parInfos, NONE=NONE, JULDAY=JULDAY, INTERVAL=INTERVAL
    
  dType=self->getPeriodType()
  ;bandToExport=self.app->getKeyValue('NAN_VALUE')
  NONE=0
  
  parInfos=self->getBandToExportInfoList()
  parInfos=replicate(parInfos, 3)
  ncdfFilenames=strarr(n_elements(parInfos))
  parInfos[1]=parInfos[0] & parInfos[2]=parInfos[0] 
  parInfos[1].displayName='chl_reg'
  parInfos[2].displayName='chl_glob'
  parInfos[1].bandName='chl_reg'
  parInfos[2].bandName='chl_glob'
  for i=0, n_elements(parInfos)-1 do begin
    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roi, JULDAY=dType eq 'D', INTERVAL=INTERVAL)
  endfor
  return, ncdfFilenames
  
END

PRO CompareOperator::cleanMainFile

  self->removeBand, 'resBand'
;parInfo=self.app->getParameterByCode('kH')
;self->removeBand, parInfo.outputBandName
  
END

FUNCTION CompareOperator::getBandToExportInfoList

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

FUNCTION CompareOperator::isTest

  return, self.mainFileName eq ''
  
END

FUNCTION CompareOperator::isEnviType

  return, self.isEnviType
  
END

PRO CompareOperator::cleanFileSystem

  self->removeMainFile
  
END

PRO CompareOperator::CleanUp

  self->cleanFileSystem
  self-> GenericOperator::Cleanup
  
END

FUNCTION CompareOperator::init, application, workingDir, periodType, resultBandName, formula, $
    ps=ps, mc=mc, $
    mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
    
  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  if n_elements(formula) eq 1 then self.formula=formula
  if n_elements(ps) eq 2 then self.ps=ps
  if n_elements(mc) eq 4 then self.mc=mc
  if n_elements(resultBandName) eq 1 then self.resultBandName= resultBandName else self.resultBandName='resultBand' 
  return, 1
  
END

PRO CompareOperator__Define

  Struct = { CompareOperator , $
    formula: '', $
    ps: fltarr(2), $
    mc: fltarr(4), $
    resultBandName: '', $
    Inherits GenericOperator $
    }
    
END