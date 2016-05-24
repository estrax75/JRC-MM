FUNCTION BaseStatisticsOperator::isToExport, statCode

  bandsToExport=self->getBandToExportList()
  res=where(strupcase(statCode) eq strupcase(bandsToExport), count)
  return, count ne 0
  
END

FUNCTION BaseStatisticsOperator::getVariancePosition

  return, where(self.avStatistics eq self->getVarianceCode())
  
END

FUNCTION BaseStatisticsOperator::getVarianceData, statStruct

  return, statStruct.variance
  
END

FUNCTION BaseStatisticsOperator::getVarianceCode

  return, 'variance'
  
END

FUNCTION BaseStatisticsOperator::getStdDevPosition

  return, where(self.avStatistics eq self->getstdDevCode())
  
END

FUNCTION BaseStatisticsOperator::getStdDevData, statStruct

  return, statStruct.stdDev
  
END

FUNCTION BaseStatisticsOperator::getStdDevCode

  return, 'stdDev'
  
END

FUNCTION BaseStatisticsOperator::getMeanPosition

  return, where(self.avStatistics eq self->getMeanCode())
  
END

FUNCTION BaseStatisticsOperator::getMeanData, statStruct

  return, statStruct.mean
  
END

FUNCTION BaseStatisticsOperator::getMeanCode

  return, 'mean'
  
END

FUNCTION BaseStatisticsOperator::getValidPixelsPosition

  return, where(self.avStatistics eq self->getValidPixelsCode())
  
END

FUNCTION BaseStatisticsOperator::getValidPixelsData, statStruct

  return, float(statStruct.validPixels)
  
END

FUNCTION BaseStatisticsOperator::getValidPixelsCode

  return, 'validPixels'
  
END

FUNCTION BaseStatisticsOperator::getSumPosition

  return, where(self.avStatistics eq self->getSumCode())
  
END

FUNCTION BaseStatisticsOperator::getSumData, statStruct

  return, statStruct.sum
  
END

FUNCTION BaseStatisticsOperator::getSumCode

  return, 'sum'
  
END

FUNCTION BaseStatisticsOperator::getFileToOverwriteList, month, year, outputDir, sensor, roi, overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx, allFiles=allFiles

  ;return, self -> GenericOperator :: getFileToOverwriteList(month, year, outputDir, self->buildStatSensor(sensor), roi, overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx, allFiles=allFiles)
  return, self -> GenericOperator :: getFileToOverwriteList(month, year, outputDir, sensor, roi, overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx, allFiles=allFiles)
  
END

PRO BaseStatisticsOperator::writeAsNCDF, month, year, sensor, roi, $
    archiveDir=archiveDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, $
    refFileName=refFileName, JULDAY=JULDAY, INTERVAL=INTERVAL, applyFormula=applyFormula
    
  ;  self -> GenericOperator :: writeAsNCDF, month, year, self->buildStatSensor(sensor), roi, $
  ;  archiveDir=archiveDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, refFileName=refFileName, JULDAY=JULDAY, INTERVAL=INTERVAL
  self -> GenericOperator :: writeAsNCDF, month, year, sensor, roi, $
    archiveDir=archiveDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, refFileName=refFileName, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, applyFormula=applyFormula
    
END

;FUNCTION BaseStatisticsOperator::buildStatParameter, parameter
;
;  return, self->getStatBandCode()+'_'+parameter
;
;END

FUNCTION BaseStatisticsOperator::buildStatSensor, sensor

  ;return, self->getStatBandCode()+'_'+sensor
  return, self->getStatBandCode()+'_'+sensor
  
END

PRO BaseStatisticsOperator::setstatBandCode, value

  self.statBandCode=value
  
END

FUNCTION BaseStatisticsOperator::getstatBandCode

  return, self.statBandCode
  
END

PRO BaseStatisticsOperator::setparameterMeasureunit, value

  self.parameterMeasureunit=value
  
END

FUNCTION BaseStatisticsOperator::getparameterMeasureunit

  return, self.parameterMeasureunit
  
END

PRO BaseStatisticsOperator::setparameterDescription, value

  self.parameterDescription=value
  
END

FUNCTION BaseStatisticsOperator::getparameterDescription

  return, self.parameterDescription
  
END

PRO BaseStatisticsOperator::setParameterName, value

  self.parameterName=value
  
END

FUNCTION BaseStatisticsOperator::getParameterName

  return, self.parameterName
  
END

FUNCTION BaseStatisticsOperator::getBandToExportInfoList

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
    parInfos[i].shortName=parameterName+'_'+thisPar.code
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

PRO BaseStatisticsOperator::storeStatBand, NO_DATA=NO_DATA, ignoreValue=ignoreValue, refObj=refObj

  if obj_valid(refObj) then self->setMainFile, refObj->getMainfile(), /COPY, /OPEN else self->updateFid
  stats=self->doStats(NO_DATA=NO_DATA, ignoreValue=ignoreValue)
  if ~keyword_set(NO_DATA) then begin
    allBands=self->getBandNames()
    for i=0, n_elements(allBands)-2 do self->removeBand, allBands[i]
    
    if self->isToExport(self->getVarianceCode()) then self->addBand,  self->getVarianceData(stats), self->getVarianceCode(), /OVERWRITE, ignoreValue=ignoreValue
    if self->isToExport(self->getStdDevCode()) then self->addBand,  self->getStdDevData(stats), self->getStdDevCode(), /OVERWRITE, ignoreValue=ignoreValue
    if self->isToExport(self->getMeanCode()) then self->addBand,  self->getMeanData(stats), self->getMeanCode(), /OVERWRITE, ignoreValue=ignoreValue
    if self->isToExport(self->getValidPixelsCode())  then self->addBand,  self->getValidPixelsData(stats), self->getValidPixelsCode(), /OVERWRITE, ignoreValue=ignoreValue
    sum=float(self->getSimpleSum())
    if self->isToExport(self->getSumCode()) then self->addBand,  sum, self->getSumCode(), /OVERWRITE, ignoreValue=ignoreValue
  ;noDel=where(strupcase(allBands[i]) eq expBands, count)
  ;if (count ne 1) then self->removeBand,  allBands[i]
  endif
  
END

PRO BaseStatisticsOperator::setSimpleSquaredSum, values

  ptr_free, self.simpleSquaredSum
  temp=values
  self.simpleSquaredSum=ptr_new(temp, /NO_COPY)
  
END

FUNCTION BaseStatisticsOperator::getSimpleSquaredSum, NO_DATA=NO_DATA

  NO_DATA=1
  if ptr_valid(self.simpleSquaredSum) then begin
    temp=*self.simpleSquaredSum
    NO_DATA=0
  endif else temp=-1
  return, temp
  
END

PRO BaseStatisticsOperator::setValidPixels, values

  ptr_free, self.validPixels
  temp=values
  self.validPixels=ptr_new(temp, /NO_COPY)
  
END

FUNCTION BaseStatisticsOperator::getValidPixels, NO_DATA=NO_DATA

  NO_DATA=1
  if ptr_valid(self.validPixels) then begin
    temp=*self.validPixels
    NO_DATA=0
  endif else temp=-1
  ;cast to float cause _fillValue when export need to be float
  return, float(temp)
  
END

PRO BaseStatisticsOperator::setSimpleSum, values

  ptr_free, self.simpleSum
  temp=values
  self.simpleSum=ptr_new(temp, /NO_COPY)
  
END

FUNCTION BaseStatisticsOperator::getSimpleSum, NO_DATA=NO_DATA

  NO_DATA=1
  if ptr_valid(self.simpleSum) then begin
    temp=*self.simpleSum
    NO_DATA=0
  endif else temp=-1
  return, temp
  
END

PRO BaseStatisticsOperator::setOverWriteFlag, value

  self.overWriteFlag=value
  
END

FUNCTION BaseStatisticsOperator::getOverWriteFlag

  return, self.overWriteFlag
  
END

FUNCTION BaseStatisticsOperator::buildSourceFileNames, dir, lastValidFileName=lastValidFileName, lastValidParName=lastValidParName

  ; Set number of year to include in each climatological file
  years = self->getYearList()
  yearNo = n_elements(years)
  
  parInfo = self->getBandToExportinfoList()
  ;parNames = self->getBandToExportList()
  parNo = n_elements(parInfo)
  
  fileInfos = replicate({FILE_INFO},yearNo*parNo)
  k=-1
  
  for i = 0, yearNo-1 do begin
  
    for j=0, parNo-1 do begin
      k++
      fileName = self->buildPPVarFileName(parInfo[j].bandName, self->getMonth(), strcompress(years[i], /REMOVE_ALL))
      
      fullFileName = dir+path_sep()+fileName
      foundTest = file_search(fullFileName, COUNT=foundCount)
      if foundCount ne 1 then begin
        doLog, fullFileName + ' does not exist', level=2
        continue
      endif
      
      fileInfos[k] = file_info(fullFileName , /NOEXPAND_PATH )
      if NOT(fileInfos[k].exists and fileInfos[k].read and fileInfos[k].regular) then begin
        doLog, fullFileName + ' does not exist, is not readable or is not a file', level=2
        continue
      endif
      lastValidFileName=fullFileName
      lastValidParName=parInfo[j].bandName
    endfor
  endfor
  
  return, fileInfos
  
END

;PRO BaseStatisticsOperator::addDataSetFromFile, enviFileName, varName
;
;END

PRO BaseStatisticsOperator::addDataSet, dataValues, ignoreValue

  if n_elements(ignoreValue) eq 1 then begin
    idxs=where(dataValues eq ignoreValue, count)
    if count ne 0 then dataValues[idxs]=!VALUES.D_NAN
  ;if count ne 0 then dataValues[idxs]=0
  endif
  validIdxs=where(finite(dataValues) eq 1, count)
  ;doLog, 'dataValues[18237], simpleSum[18237], simpleSquaredSum[18237], validPixels[18237]'
  ;doLog, dataValues[18237], simpleSum[18237], simpleSquaredSum[18237], validPixels[18237]
  if count ge 1 then begin
    if ptr_valid(self.simpleSum) then begin
      simpleSum=self->getSimpleSum()
      validPixels=self->getValidPixels()
      simpleSquaredSum=self->getSimpleSquaredSum()
      simpleSum[validIdxs]=simpleSum[validIdxs]+dataValues[validIdxs]
      validPixels[validIdxs]=validPixels[validIdxs]+1
      simpleSquaredSum[validIdxs]=simpleSquaredSum[validIdxs]+(dataValues[validIdxs]*dataValues[validIdxs])
    ;        simpleSum=simpleSum+dataValues
    ;        validPixels=validPixels+1
    ;        simpleSquaredSum=simpleSquaredSum+(dataValues*dataValues)
    endif else begin
      simpleSum=dataValues
      simpleSquaredSum=dataValues
      validPixels=fix(dataValues)
      validPixels[*]=0
      simpleSum[*]=0 &  simpleSquaredSum[*]=0
      simpleSum[validIdxs]=dataValues[validIdxs]
      validPixels[validIdxs]=validPixels[validIdxs]+1
      simpleSquaredSum[validIdxs]=dataValues[validIdxs]*dataValues[validIdxs]
    endelse
    
    self->setSimpleSum, simpleSum
    self->setValidPixels, validPixels
    self->setSimpleSquaredSum, simpleSquaredSum
  endif
  
END

PRO BaseStatisticsOperator::reset

  ptr_free, self.simpleSum
  ptr_free, self.validPixels
  ptr_free, self.simpleSquaredSum
  
END

FUNCTION BaseStatisticsOperator::doStats, NO_DATA=NO_DATA, ignoreValue=ignoreValue

  NO_DATA=1
  validPixels=self->getValidPixels(NO_DATA=NO_DATA)
  simpleSum=self->getSimpleSum(NO_DATA=NO_DATA)
  simpleSquaredSum=self->getSimpleSquaredSum(NO_DATA=NO_DATA)
  
  if ~(NO_DATA) then begin
  
    myVariance=simpleSquaredSum
    myStdDev = myVariance
    myMean = myStdDev
    mySum= simpleSum
    if n_elements(ignoreValue) then nan=ignoreValue else nan=float(self.app->getKeyValue('NAN_VALUE'))
    
    validIdxs=where(validPixels ne 0, validCount, COMPLEMENT=nonValidIdxs, NCOMPLEMENT=nonValidCount)
    if nonValidCount ne 0 then begin
      myVariance[nonValidIdxs]=nan
      myStdDev[nonValidIdxs]=nan
      myMean[nonValidIdxs]=nan
      mySum[nonValidIdxs]=nan
    endif
    if validCount ne 0 then begin
      myVariance[validIdxs]= (simpleSquaredSum[validIdxs] - simpleSum[validIdxs] * simpleSum[validIdxs] / validPixels[validIdxs]) / (validPixels[validIdxs] - 1)
      myStdDev[validIdxs] = sqrt(myVariance[validIdxs])
      myMean[validIdxs] = simpleSum[validIdxs] / validPixels[validIdxs]
    endif
    NO_DATA=0
    myMean=self.app->applyRangeConditions(self.parameterName, myMean, self.parameterName, ignoreValue=ignoreValue)
    ;myStdDev=self.app->applyRangeConditions(self.parameterName, myStdDev, self.parameterName, ignoreValue=ignoreValue)
    ;myVariance=self.app->applyRangeConditions(self.parameterName, myVariance, self.parameterName, ignoreValue=ignoreValue)
    return, {variance:myVariance, stdDev:myStdDev, mean:myMean, validPixels:*self.validPixels, sum:mySum}
  endif
  return, -1
  
  
END

FUNCTION BaseStatisticsOperator::doArchiveComputation, archiveDir, filter, outputDir, COPY=COPY, fileName=fileName

  fileInfos=self->buildSourceFileNames(archiveDir, lastValidFileName=lastValidFileName, lastValidParName=lastValidParName)
  
  if n_elements(lastValidFileName) ne 1 then begin
    doLog, 'no files for month: ', self->getMonth(), level=2
    doLog, 'skip', level=2
    return, 0
  endif
  
  years = self->getYearList()
  yearNo = n_elements(years)
  
  parInfos = self->getBandToExportInfoList()
  parNo = n_elements(parInfos)
  
  dataInfo=self->getNcdfDataValues(lastValidFileName, lastValidParName)
  data=dataInfo.data
  simpleSum=data
  
  k=0
  nan=float(self.app->getKeyValue('NAN_VALUE'))
  
  fileNames=self.avStatistics
  for i=0,  parNo-1 do begin
    statParInfo=self->buildStatParInfo(parInfos[i])
    for j=0, n_elements(fileNames)-1 do fileNames[j]=self->buildPPVarFileName(statParInfo[j].bandName, self->getMonth(), 'CLIM')
    overWriteCheckList=self->overwriteAllowed(fileNames, outputDir)
    if total(overWriteCheckList) ne 0 then begin
      simpleSum[*]=0.
      simpleSquaredSum=simpleSum
      validPixels=fix(simpleSum)
      for j=0, n_elements(years)-1 do begin
        thisFileInfos=fileInfos[j*parNo+i]
        if thisFileInfos.read then begin
          dataInfo=self->getNcdfDataValues(thisFileInfos.name, parInfos[i].bandName, /SET_NAN, /REVERSE)
          dataValues=dataInfo.data
          self->addDataSet, dataValues
          doLog, dataValues[18237], simpleSum[18237], simpleSquaredSum[18237], validPixels[18237]
        endif
      endfor
      stats=self->doStats()
      ;myVariance= (simpleSquaredSum - simpleSum * simpleSum / validPixels) / (validPixels - 1)
      ;myStdDev = sqrt(myVariance)
      ;myMean = simpleSum / validPixels
      if overWriteCheckList[0] then self->exportBandAsGeoNc, stats.variance, statParInfo[0], fileNames[0], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName else self.app->logNoOverWriteFile, fileNames[0]
      if overWriteCheckList[1] then self->exportBandAsGeoNc, stats.stdDev, statParInfo[1], fileNames[1], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName else self.app->logNoOverWriteFile, fileNames[1]
      if overWriteCheckList[2] then self->exportBandAsGeoNc, stats.mean, statParInfo[2], fileNames[2], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName else self.app->logNoOverWriteFile, fileNames[2]
      if overWriteCheckList[3] then self->exportBandAsGeoNc, self->getValidPixels(), statParInfo[3], fileNames[3], 'CLIM', self->getMonth(), outputDir, refFileName=lastValidFileName else self.app->logNoOverWriteFile, fileNames[3]
      self->reset
    endif else begin
      self.app->logNoOverWriteFile, fileNames
    endelse
  endfor
  
END

FUNCTION BaseStatisticsOperator::overwriteAllowed, fileName, outputDir

  if self.overWriteFlag then return, replicate(1b, n_elements(fileName))
  fInfo=file_info(outputDir+path_sep()+fileName)
  idxs=where(fInfo.exists eq 1)
  return, 1-(fInfo.exists)
;if fInfo.exists then return, 0 else return, 1
  
END

FUNCTION BaseStatisticsOperator::buildStatParInfo, sourceParInfo

  stats=self.avStatistics
  
  statsMoU=strarr(n_elements(self.avStatistics))
  statsMoU[0]=sourceParInfo.measureUnit;+'^-2'
  statsMoU[1]=sourceParInfo.measureUnit
  statsMoU[2]=sourceParInfo.measureUnit
  statsMoU[3]=''
  statsMoU[4]=sourceParInfo.measureUnit
  
  statInfos=replicate(getParameterInfoStruct(), n_elements(stats))
  for i=0, n_elements(statInfos)-1 do begin
    statInfos[i].bandName=sourceParInfo.inputBandName+'_'+stats[i]
    statInfos[i].shortName=sourceParInfo.inputBandName+'_'+stats[i]
    statInfos[i].longName=sourceParInfo.inputBandName+'_'+stats[i]
    statInfos[i].displayName=sourceParInfo.displayName+'_'+stats[i]
    statInfos[i].measureUnit=statsMoU[i]
    statInfos[i].description=sourceParInfo.description+'('+stats[i]+')'
  endfor
  
  return, statInfos
  
END

;PRO BaseStatisticsOperator::writeResult, month, year, archiveDir
;
;  prevLog=self.log
;  self.log=1
;  self->writeAsNCDF, month, year, archiveDir
;  self.log=prevLog
;
;END

PRO BaseStatisticsOperator::CleanUp

  ;ptr_free, self.applyToBandList
  ptr_free, self.simpleSum
  ptr_free, self.validPixels
  ptr_free, self.simpleSquaredSum
  self-> GenericOperator::Cleanup
  
END

FUNCTION BaseStatisticsOperator::init, application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, fileName=fileName, $
    OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList, parameterName=parameterName, parameterDescription=parameterDescription, $
    parameterMeasureunit=parameterMeasureunit, statBandCode=statBandCode, ENVITYPE=ENVITYPE
    
  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, fileName=fileName, $
    OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList, ENVITYPE=ENVITYPE)) then return, 0
  if n_elements(parameterName) eq 1 then self.parameterName=parameterName
  if n_elements(parameterDescription) eq 1 then self.parameterDescription=parameterDescription
  if n_elements(parameterMeasureunit) eq 1 then self.parameterMeasureunit=parameterMeasureunit
  if n_elements(statBandCode) eq 1 then self.statBandCode=statBandCode
  self.avStatistics=[self->getVarianceCode(), self->getStdDevCode(), self->getMeanCode(),self->getValidPixelsCode(), self->getSumCode()]
  if n_elements(bandToExportList) eq 0 then  self->setBandToExportList, self.avStatistics
  return, 1
  
END

PRO BaseStatisticsOperator__Define

  Struct = { BaseStatisticsOperator , $
    ;applyToBandList: ptr_new(), $
    parameterName: '', $
    parameterDescription: '', $
    parameterMeasureunit: '', $
    statBandCode: '', $
    overWriteFlag: 0b, $
    simpleSum: ptr_new(), $
    validPixels: ptr_new(), $
    simpleSquaredSum: ptr_new(), $
    avStatistics: strarr(5), $
    Inherits GenericOperator $
    }
    
END