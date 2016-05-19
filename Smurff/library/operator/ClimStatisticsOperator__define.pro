@..\wrapper\structure_definition
FUNCTION ClimStatisticsOperator::computeBinSize, data, ignoreValue=ignoreValue, bins=bins

  ;ignoreValue=float(self.app->getKeyValue('NAN_VALUE'))
  checkData=data
  if n_elements(ignoreValue) eq 1 then begin
    idxs=where(checkData eq ignoreValue, count)
    if count gt 0 then checkData[idxs]=!VALUES.F_NAN
  endif 
  maxvar=max(checkData, min=minvar, /NAN)
  checkData=0b
  if n_elements(bins) eq 1 then binSteps=bins else binSteps=10
  binSize=(maxvar-minvar)/binSteps
  return, binSize
  
END

FUNCTION ClimStatisticsOperator::getIntermediateElab

  if self.intermediateElab ne '' or self.intermediateElab ne 'N/A' then return, self.intermediateElab else return, ''
  
END

FUNCTION ClimStatisticsOperator::buildOperatorResultFileName, dType, displayName, month, year, FULLPATH=FULLPATH, NOROICODE=NOROICODE

  ;return, buildPSAVarFileName(dType, displayName, month, year)
  ;return, call_function('buildClimVarFileName'+'_'+dType, $
  ;    year, month, dType, self.roiCode, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
  return, call_function('buildClimVarFileName'+'_'+dType, $
    year, month, dType, self.roiArchiveCode, sensor, displayName, archivedir, NOROICODE=NOROICODE, $
    FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
    
END

PRO ClimStatisticsOperator::setAllYearFlag, allYearFlag

  self.allYearFlag=allYearFlag
  
END

FUNCTION ClimStatisticsOperator::getAllYearFlag

  return, self.allYearFlag
  
END

PRO ClimStatisticsOperator::setReadFunction, readFunction

  self.readFunction=readFunction
  
END

FUNCTION ClimStatisticsOperator::getReadFunction

  return, self.readFunction
  
END

PRO ClimStatisticsOperator::setDataDir, dataDir

  self.dataDir=dataDir
  
END

FUNCTION ClimStatisticsOperator::getDataDir

  return, self.dataDir
  
END

PRO ClimStatisticsOperator::setMonth, month

  self.month=month
  
END

FUNCTION ClimStatisticsOperator::getMonth

  return, self.month
  
END

FUNCTION ClimStatisticsOperator::buildAllYearList

  firstYear=fix(self.app->getKeyValue('FIRST_YEAR'))
  utility=self.app->getUtility()
  lastYear=fix(utility->getSysTime(/ONLY_YEAR))
  yearList=indgen(lastYear-firstYear+1)+firstYear
  return, yearList
  
END

PRO ClimStatisticsOperator::configYears, yearList=yearList, FOUND_ALL_YEARS=FOUND_ALL_YEARS

  ;set year list from a selected list or from first year to nowadays year (-1?)
  if n_elements(yearList) gt 0 then begin
    selectedYearList=yearList
  endif
  if keyword_set(FOUND_ALL_YEARS) then begin
    selectedYearList=self->buildAllYearList()
  endif
  self->setYearList, selectedYearList
  
END

FUNCTION ClimStatisticsOperator::getYearList

  if ptr_valid(self.yearList) then list = *self.yearList else list=-1
  return, list
  
END

PRO ClimStatisticsOperator::setYearList, list

  ptr_free, self.yearList
  self.yearList=ptr_new(list, /NO_COPY)
  
END

FUNCTION ClimStatisticsOperator::doArchiveComputation, archiveDir, filter, outputDir, COPY=COPY, fileName=fileName, EXTRACT=EXTRACT


  ;  if n_elements(lastValidFileName) ne 1 then begin
  ;    doLog, 'no files for month: ', self->getMonth(), level=2
  ;    doLog, 'skip', level=2
  ;    return, 0
  ;  endif

  years = self->getYearList()
  yearNo = n_elements(years)
  
  ignoreValue=float(self.app->getKeyValue('NAN_VALUE'))
  if self.parameterName eq 'ALG_CHL' then GETCHLVAR=1 else GETCHLVAR=0
  physicals=self.app->getPhysicalFromYear(years[0])
  physical=physicals[0]
  physPars=physical->getParametersList()
  nElemPhysPar=n_elements(physPars)
  if keyword_set(GETCHLVAR) then parName=physicals[0]->getParameterCodeChl() else parName=self.parameterName
  
  pars=self.app->getParameters()
  parInfo=pars->getElementByCode(parName)
  
  ;for i=0,  parNo-1 do statParInfo=self->buildStatParInfo(parInfos[i])
  statParInfo=self->buildStatParInfo(parInfo)
  fileNames=strarr(n_elements(statParInfo))
  ;for j=0, 3 do fileNames[j]=self->buildPPVarFileName(statParInfo[j].bandName, , 'CLIM')
  ;, GETCHLVAR='ALG_CHL' eq strupcase(self.parameterName)
  ;for j=0, 3 do fileNames[j]=self->buildOperatorResultFileName(self->getPeriodType(), statParInfo[j].bandName, self->getMonth(), year, FULLPATH=FULLPATH)
  if ~self.noRoiFlag then begin
    roiInfo=self.app->getRoiRelatedInfo(self.roiCode)
    if roiInfo.roiRefCode eq '' or roiInfo.roiRefCode eq 'N/A' then delIdlVar, roiRefCode else roiRefCode=roiInfo.roiRefCode;refRoi=mainApp->getROIInfoByCode(refRoiCode)
    roiArchiveCode=roiInfo.roiArchiveCode
  endif
  for j=0, 4 do fileNames[j]=self->buildOperatorResultFileName(self->getPeriodType(), statParInfo[j].shortName, self->getMonth(), year, FULLPATH=FULLPATH, NOROICODE=self.noRoiFlag)
  ;for j=0, 3 do fileNames[j]=self->buildOperatorResultFileName(self->getPeriodType(), self.parameterName, self->getMonth(), year, FULLPATH=FULLPATH)
  
  overWriteCheckList=self->overwriteAllowed(fileNames, outputDir)
  
  
  if total(overWriteCheckList) ne 0 then begin
    for j=0, n_elements(years)-1 do begin
      ;dataInfo=self->getNcdfDataValues(thisFileInfos.name, parInfos[i].bandName, /SET_NAN, /REVERSE)
      physicals=self.app->getPhysicalFromYear(years[j])
      physical=physicals[0]
      physPars=physical->getParametersList()
      nElemPhysPar=n_elements(physPars)
      ; New get right chl parameter (from year/sensor)
      if keyword_set(GETCHLVAR) then parName=physicals[0]->getParameterCodeChl() else parName=self.parameterName
      ; end new
      FOUND=0
      doLog, self.intermediateElab, LEVEL=4
      dataInfo=self->readDataFromFile(self.periodType, self.month, years[j], self.dataDir, self.readFunction, parName, $
        self.roiCode, self.roiArchiveCode, FOUND=FOUND, $
        refRoi=roiRefCode, outMapInfo=outMapInfo, elabName=self.intermediateElab, $
        ignoreValue=ignoreValue, fileName=fileName, EXTRACT=EXTRACT)
      if keyword_set(FOUND) then begin
        self->addDataSet, dataInfo.data, ignoreValue
        if n_elements(fileName) then lastValidFileName=fileName
        if n_elements(outMapInfo) then lastMapInfo=outMapInfo
      endif
    ;doLog, dataValues[18237], simpleSum[18237], simpleSquaredSum[18237], validPixels[18237]
    endfor
    if n_elements(lastValidFileName) ne 0 or n_elements(lastMapInfo) ne 0 then begin
      stats=self->doStats(ignoreValue=ignoreValue)
      if size(stats, /TYPE) eq 8 then begin
        ;vIdxs=where(stats.mean ne -9999, count)
        ;if count ne 0 then print, mean(stats.mean[vIdxs], /NAN)
        ;if overWriteCheckList[self->getVariancePosition()] and self->isToExport(self->getVarianceCode()) then self->exportBandAsGeoNc, self->getVarianceData(stats), statParInfo[self->getVariancePosition()], fileNames[self->getVariancePosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear' else self.app->logNoOverWriteFile, fileNames[self->getVariancePosition()]
        if overWriteCheckList[self->getVariancePosition()] and self->isToExport(self->getVarianceCode()) then begin
          varData=self->getVarianceData(stats)
          binSize=self->computeBinSize(varData, ignoreValue=float(self.app->getKeyValue('NAN_VALUE')))
          HISTOSTRUCT={ folder: getIntermediateDataDir(outputDir), $
            fname: '', $
            day: '', $
            month: '', $;            month: self.month, $
            year: '', $;            year: 'ALL-Variance', $
            cutvalue: -1, $
            binSize: binSize }
          self->exportBandAsGeoNc, varData, statParInfo[self->getVariancePosition()], fileNames[self->getVariancePosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear', DOHISTO=self.app->isTestMode(), HISTOSTRUCT=HISTOSTRUCT
          varData=0b
        endif else begin
          if self->isToExport(self->getVarianceCode()) then self.app->logNoOverWriteFile, fileNames[self->getVariancePosition()]
        endelse
        
        if overWriteCheckList[self->getStdDevPosition()] and self->isToExport(self->getStdDevCode()) then begin
          varData=self->getStdDevData(stats)
          binSize=self->computeBinSize(varData, ignoreValue=float(self.app->getKeyValue('NAN_VALUE')))
          HISTOSTRUCT={ folder: getIntermediateDataDir(outputDir), $
            fname: '', $
            day: '', $
            month: '', $;            month: self.month, $
            year: '', $;            year: 'ALL-StdDev', $
            cutvalue: -1, $
            binSize: binSize }
          self->exportBandAsGeoNc, varData, statParInfo[self->getStdDevPosition()], fileNames[self->getStdDevPosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear', DOHISTO=self.app->isTestMode(), HISTOSTRUCT=HISTOSTRUCT
          varData=0b
        endif else begin
          if self->isToExport(self->getStdDevCode()) then self.app->logNoOverWriteFile, fileNames[self->getStdDevPosition()]
        endelse
        
        if overWriteCheckList[self->getMeanPosition()] and self->isToExport(self->getMeanCode()) then begin
          varData=self->getMeanData(stats)
          binSize=self->computeBinSize(varData, ignoreValue=float(self.app->getKeyValue('NAN_VALUE')))
          HISTOSTRUCT={ folder: getIntermediateDataDir(outputDir), $
            fname: '', $
            day: '', $
            month: '', $;            month: self.month, $
            year: '', $;            year: 'ALL-Mean', $
            cutvalue: -1, $
            binSize: binSize }
          self->exportBandAsGeoNc, varData, statParInfo[self->getMeanPosition()], fileNames[self->getMeanPosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear', DOHISTO=self.app->isTestMode(), HISTOSTRUCT=HISTOSTRUCT
          varData=0b
        endif else begin
          if self->isToExport(self->getMeanCode()) then self.app->logNoOverWriteFile, fileNames[self->getMeanPosition()]
        endelse
        
        if overWriteCheckList[self->getValidPixelsPosition()] and self->isToExport(self->getValidPixelsCode()) then begin
          varData=self->getValidPixelsData(stats)
          binSize=self->computeBinSize(varData, ignoreValue=float(self.app->getKeyValue('NAN_VALUE')))
          HISTOSTRUCT={ folder: getIntermediateDataDir(outputDir), $
            fname: '', $
            day: '', $
            month: '', $;            month: self.month, $
            year: '', $;            year: 'ALL-ValidPixels', $
            cutvalue: -1, $
            binSize: binSize }
          self->exportBandAsGeoNc, varData, statParInfo[self->getValidPixelsPosition()], fileNames[self->getValidPixelsPosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear', DOHISTO=self.app->isTestMode(), HISTOSTRUCT=HISTOSTRUCT
          varData=0b
        endif else begin
          if self->isToExport(self->getValidPixelsCode()) then self.app->logNoOverWriteFile, fileNames[self->getValidPixelsPosition()]
        endelse
        
        if overWriteCheckList[self->getSumPosition()] and self->isToExport(self->getSumCode()) then begin
          varData=self->getSumData(stats)
          binSize=self->computeBinSize(varData, ignoreValue=float(self.app->getKeyValue('NAN_VALUE')))
          HISTOSTRUCT={ folder: getIntermediateDataDir(outputDir), $
            fname: '', $
            day: '', $
            month: '', $;            month: self.month, $
            year: '', $;            year: 'ALL-Sum', $
            cutvalue: -1, $
            binSize: binSize }
          self->exportBandAsGeoNc, varData, statParInfo[self->getSumPosition()], fileNames[self->getSumPosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear', DOHISTO=self.app->isTestMode(), HISTOSTRUCT=HISTOSTRUCT
          varData=0b
        endif else begin
          if self->isToExport(self->getSumCode()) then self.app->logNoOverWriteFile, fileNames[self->getSumPosition()]
        endelse
        
        ;        if overWriteCheckList[self->getVariancePosition()] and self->isToExport(self->getVarianceCode()) then self->exportBandAsGeoNc, self->getVarianceData(stats), statParInfo[self->getVariancePosition()], fileNames[self->getVariancePosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear' else self.app->logNoOverWriteFile, fileNames[self->getVariancePosition()]
        ;        if overWriteCheckList[self->getStdDevPosition()] and self->isToExport(self->getStdDevCode()) then self->exportBandAsGeoNc, self->getStdDevData(stats), statParInfo[self->getStdDevPosition()], fileNames[self->getStdDevPosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear' else self.app->logNoOverWriteFile, fileNames[self->getStdDevPosition()]
        ;        if overWriteCheckList[self->getMeanPosition()] and self->isToExport(self->getMeanCode()) then self->exportBandAsGeoNc, self->getMeanData(stats), statParInfo[self->getMeanPosition()], fileNames[self->getMeanPosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear' else self.app->logNoOverWriteFile, fileNames[self->getMeanPosition()]
        ;        if overWriteCheckList[self->getValidPixelsPosition()] and self->isToExport(self->getValidPixelsCode()) then self->exportBandAsGeoNc, self->getValidPixels(), statParInfo[self->getValidPixelsPosition()], fileNames[self->getValidPixelsPosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear' else self.app->logNoOverWriteFile, fileNames[self->getValidPixelsPosition()]
        ;        if overWriteCheckList[self->getSumPosition()] and self->isToExport(self->getSumCode()) then self->exportBandAsGeoNc, self->getSimpleSum(), statParInfo[self->getSumPosition()], fileNames[self->getSumPosition()], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName, mapInfo=lastMapInfo, scallingType='linear' else self.app->logNoOverWriteFile, fileNames[self->getSumPosition()]
        doLog, mean(stats.mean, /NAN), LEVEL=4
        delIdlVar, lastValidFileName
        delIdlVar, lastMapInfo
        self->reset
      endif else begin
        doLog, 'No data for month', self.month, LEVEL=4
      endelse
    endif else begin
      doLog, 'No data for month', self.month, LEVEL=4
    endelse
  endif else begin
    self.app->logNoOverWriteFile, fileNames
  endelse
;endfor
  
END

FUNCTION ClimStatisticsOperator::getBandToExportInfoList

  selectedCodes=self->getBandToExportList()
  parInfos=replicate(getParameterInfoStruct(), n_elements(selectedCodes))
  refParam=self.app->getParameterByCode(self.statBandCode)
  if self.parameterName eq '' and self.statBandCode ne '' then begin
    parameterName=refParam.name
    parameterMeasureUnit=refParam.measureUnit
    parameterDescription=refParam.description
  endif else begin
    parameterName=self.parameterName
    parameterMeasureUnit=self.parameterMeasureUnit
    parameterDescription=self.parameterDescription
  endelse
  ;parInfo=self.app->getParameterByCode(statBandCodes)
  for i=0, n_elements(selectedCodes)-1 do begin
    thisPar=self.app->getParameterByCode(selectedCodes[i])
    ;parInfo
    ;parInfos[i].bandName=parameterName+'_'+thisPar.outputBandName
    parInfos[i].bandName=thisPar.outputBandName
    parInfos[i].shortName=parameterName;+'_'+thisPar.code ; original parameter name without stats.
    parInfos[i].longName=parameterName+'_'+thisPar.outputBandName
    parInfos[i].displayName=thisPar.outputBandName
    if thisPar.measureUnit eq 'N/A' then parInfos[i].measureUnit=parameterMeasureUnit else parInfos[i].measureUnit='';thisPar.measureUnit
    parInfos[i].description=parameterDescription+'_'+thisPar.description
    
  ;    parInfos[i].bandName=self.parameterName+'_'+thisPar.outputBandName
  ;    parInfos[i].shortName=self.parameterName+'_'+thisPar.code
  ;    parInfos[i].longName=self.parameterName+'_'+thisPar.outputBandName
  ;    parInfos[i].displayName=thisPar.outputBandName
  ;    parInfos[i].measureUnit=self.parameterMeasureUnit;thisPar.measureUnit
  ;    parInfos[i].description=self.parameterDescription+'_'+thisPar.description
  endfor
  
  return, parInfos
  
END

FUNCTION ClimStatisticsOperator::getFileNameToExport, month, year, sensor, roi, archiveDir=archiveDir, parInfos=parInfos, NONE=NONE, JULDAY=JULDAY, INTERVAL=INTERVAL

  dType=self->getPeriodType()
  NONE=0
  
  parInfos=self->getBandToExportInfoList()
  ncdfFilenames=strarr(n_elements(parInfos))
  for i=0, n_elements(parInfos)-1 do begin
    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roi)
  endfor
  ;if ~keyword_set(overwriteFlag) then begin
  ;  ncdfFilenames=self->checkResultFileExistence(ncdfFilenames, archiveDir, idxs=idxs, NONE=NONE)
  ;  if ~keyword_set(NONE) then parInfos=parInfos[idxs]
  ;endif
  return, ncdfFilenames
  
END

PRO ClimStatisticsOperator::CleanUp

  ptr_free, self.yearList
  self-> BaseStatisticsOperator::Cleanup
  
END

FUNCTION ClimStatisticsOperator::init, application, workingDir, periodType, mainParameterName, $
    month, dataDir, readFunction, yearList, roiCode, roiArchiveCode, $
    allYearFlag=allYearFlag, maskBand=maskBand, geoBand=geoBand, fileName=fileName, $
    OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList, statBandCode=statBandCode, parameterName=parameterName, parameterDescription=parameterDescription, $
    parameterMeasureunit=parameterMeasureunit, ENVITYPE=ENVITYPE, noRoiFlag=noRoiFlag, intermediateElab=intermediateElab
    
  if not (self -> BaseStatisticsOperator :: init(application, workingDir, periodType, maskBand=maskBand, $
    geoBand=geoBand, fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList, $
    statBandCode=statBandCode, parameterName=parameterName, parameterDescription=parameterDescription, $
    parameterMeasureunit=parameterMeasureunit, ENVITYPE=ENVITYPE)) then return, 0
  self.month=month
  self.dataDir=dataDir
  self.readFunction=readFunction
  self.allYearFlag=keyword_set(allYearFlag)
  if n_elements(roiCode) eq 1 then self.roiCode=roiCode
  if n_elements(roiArchiveCode) eq 1 then self.roiArchiveCode=roiArchiveCode
  if n_elements(yearList) ne 0 then self.yearList=ptr_new(yearList)
  if n_elements(NOROIFLAG) ne 0 then self.noRoiFlag=NOROIFLAG
  if n_elements(intermediateElab) ne 0 then self.intermediateElab=intermediateElab
  return, 1
  
END

PRO ClimStatisticsOperator__Define

  Struct = { ClimStatisticsOperator , $
    Inherits BaseStatisticsOperator, $
    month: '', $
    dataDir: '', $
    readFunction: '', $
    allYearFlag: 0, $
    roiCode: '', $
    roiArchiveCode: '', $
    noRoiFlag: 0, $
    intermediateElab: '', $
    yearList: ptr_new() $
    }
    
END