;FUNCTION SmurffApplication::isSensor
;
;    physicals=app->getPhysicalFromYear(year)
;    goOver=1
;    for sIdx=0, n_elements(physicals)-1 do begin
;      thisSensorCode=physical->getCode()
;      if sensorCode eq thisSensorCode or sensorCode eq 'ALL' then begin
;        goOver=0
;        continue
;      endif
;    endfor
;
;END

PRO SmurffApplication::addFilterLine, line

 
 fullFileName=self.fileSystem->getTempDir(/WITH)+self.filterFile
 line=self.utility->getSysTime()+'**'+line
 openw, destUnit, fullFileName, /APPEND, /GET_LUN
 printf, destUnit, line
 close, destUnit & free_lun, destUnit 

END

FUNCTION SmurffApplication::getROIExtractInfoByCodes, roiCodes

 return, self.rois->getExtractInfoByCodes(roiCodes)
  
END

FUNCTION SmurffApplication::getROIGeoInfoByCodes, roiCodes

  return, self.rois->getGeoInfoByCodes(roiCodes)
  
END

FUNCTION SmurffApplication::getRoiRelatedInfo, roiCode

  roiArchiveCode=self.rois->getArchiveRoiCodesSelectedByCodes(roiCode)
  roiRefCode=self.rois->getRefRoiCodesByCodes(roiCode)
  return, {roiArchiveCode:roiArchiveCode, roiRefCode:roiRefCode}
  
END

FUNCTION SmurffApplication::isTrue, var

  return, self.utility->isTrue(var)
  
END

FUNCTION SmurffApplication::isTestMode

  return, self.utility->isTrue(self->getKeyValue('TEST_MODE'))
  
END

PRO SmurffApplication::doLog, string1, string2, string3, string4, string5, string6, string7, string8, string9, level=level

  if n_elements(level) ne 1 then logLevel=1 else logLevel=level
  
  if self.logLevel lt logLevel then begin
  
    if n_elements(string9) ne 0 then begin
      print, string1, string2, string3, string4, string5, string6, string7, string8, string9
      return
    endif
    if n_elements(string8) ne 0 then begin
      print, string1, string2, string3, string4, string5, string6, string7, string8
      return
    endif
    if n_elements(string7) ne 0 then begin
      print, string1, string2, string3, string4, string5, string6, string7
      return
    endif
    if n_elements(string6) ne 0 then begin
      print, string1, string2, string3, string4, string5, string6
      return
    endif
    if n_elements(string5) ne 0 then begin
      print, string1, string2, string3, string4, string5
      return
    endif
    if n_elements(string4) ne 0 then begin
      print, string1, string2, string3, string4
      return
    endif
    if n_elements(string3) ne 0 then begin
      print, string1, string2, string3
      return
    endif
    if n_elements(string2) ne 0 then begin
      print, string1, string2
      return
    endif
    if n_elements(string1) ne 0 then begin
      print, string1
      return
    endif
  endif
  
END

PRO SmurffApplication::setLogLevel, value

  self.logLevel=value
  
END

FUNCTION SmurffApplication::getLogLevel

  return, self.logLevel
  
END

PRO SmurffApplication::logNoOverWriteFile, fileName

  prevLog=self->getLogLevel()
  self->setLogLevel, 1
  howMany=n_elements(fileName)
  if howMany eq 1 then doLog,'Skip result for: ', fileName, '. It already exists in file system.', level=3
  if howMany lt 1 then doLog,'All results already exist: nothing to do...', level=3
  if howMany gt 1 then begin
    labels=''
    for i=0, howMany-1 do labels=labels+fileName[i]+';'
    labels=strmid(labels, 0, strlen(labels)-1)
    doLog,'Skip result for: ', labels, '. They already exist in file system.', level=3
  endif
  self->setLogLevel, prevLog
  
END

PRO SmurffApplication::logRemoveFile, fileName

  prevLog=self->getLogLevel()
  self->setLog, 1
  doLog,'remove: '+fileName+'*'+' from file system, as requested.', level=3
  self->setLogLevel, prevLog
  
END

FUNCTION SmurffApplication::getReturnCharacter

  return, string(10B)
  
END

FUNCTION SmurffApplication::getLangCatLabelFromKey, key, placeHolders=placeHolders

  text=self.languageCatalog->Query(key)
  placeHolderMark='%%'
  if n_elements(placeHolders) ne 0 then begin
    lastPos=0
    newString=text
    for i=1, (8 < (n_elements(placeHolders)-1))+1 do begin
      thisPH=placeHolderMark+strcompress(i,/REMOVE)
      resStringPos=strpos(text, thisPH, lastPos)
      while (resStringPos ne -1) do begin
        preFix=strmid(newString, lastPos, resStringPos-lastPos)
        postFix=strmid(newString,resStringPos+strlen(thisPH), strlen(newString)-resStringPos)
        ;for j=0, n_elements(resString)-1 do newString=newString+resString[j]+placeHolders[i]
        newString=preFix+placeHolders[i-1]+postFix
        lastPos=resStringPos
        resStringPos=strpos(newString, thisPH, lastPos)
      endwhile
    endfor
    text=newString
  endif
  return, text
  
  
END

PRO SmurffApplication::runImageConvertFromFile, fileNames

  for i=0, n_elements(fileNames)-1 do begin
    request=self->loadImageConvert(fileNames[i])
    self->execImageConvert, request, /NODISPLAY
  endfor
  
END

FUNCTION SmurffApplication::loadImageConvert, fileName

  if n_elements(fileName) eq 1 then begin
    request=obj_new('ImageConvert')
    res=request->fillDataFromXMLFile(fileName)
    return, request
  endif
  return, -1
  
END

FUNCTION SmurffApplication::loadRequest, fileName

  if n_elements(fileName) eq 1 then begin
    request=obj_new('Request')
    res=request->fillDataFromXMLFile(fileName)
    return, request
  endif
  return, -1
  
END

PRO  SmurffApplication::runRequestFromFile, fileNames

  for i=0, n_elements(fileNames)-1 do begin
    request=self->loadRequest(fileNames[i])
    self->execRequest, request, /NODISPLAY
  endfor
  
END

FUNCTION SmurffApplication::getHomeDir

  return, self.fileSystem->getHomeDir()
  
END

FUNCTION SmurffApplication::getOperator, typeCode, fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList

  if typeCode eq 'PP' then return, obj_new('PPOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList)
  if typeCode eq 'CLIM' then return, obj_new('BaseStatisticsOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList)
  if typeCode eq 'PSA' then return, obj_new('PSAOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList)
  if typeCode eq 'OXYRISK' then return, obj_new('OxyRiskOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList)
  if typeCode eq 'OCEANSAT2' then return, obj_new('OceanSat2Operator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList)
  if typeCode eq 'BIOMAPEMIS' then return, obj_new('BiomapEmisOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList)
  if typeCode eq 'EREGIONALEMIS' then return, obj_new('ERegionalEmisOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList)
  if typeCode eq 'COMPAREEMIS' then return, obj_new('BiomapEmisOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList);obj_new('CompareEmisOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList)
  if typeCode eq 'GLOBOPERATOR' then return, obj_new('GlobOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList);obj_new('CompareEmisOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList)
  message, 'operator called: ',  typeCode, ' not available. Check spelling.'
  
END

FUNCTION SmurffApplication::buildTargetMapInfoFromRoi, roiCode, checkTMI=checkTMI, preserveResolution=preserveResolution

  roiInfo=self->getROIInfoByCode(roiCode)
  targetMapInfo={mapWindowBoundary:float([roiInfo.lonMin, roiInfo.lonMax, roiInfo.latMax, roiInfo.latMin]), $
    mapPixelExtension:fix([roiInfo.nbRows,roiInfo.nbLines]), $
    mapPixelResolution:[abs(float(roiInfo.lonMax)-float(roiInfo.lonMin))/float(roiInfo.nbRows), abs(float(roiInfo.latMax)-float(roiInfo.latMin))/float(roiInfo.nbLines)], $
    preserveResolution:keyword_set(preserveResolution)}
  ;print, targetMapInfo.(0), checkTMI.(0)
  ;print, targetMapInfo.(1), checkTMI.(1)
  ;print, targetMapInfo.(2), checkTMI.(2)
  return, targetMapInfo
  
END

FUNCTION SmurffApplication::getResolutionStruct

  return, self.resolutions->getListStructDef()
  
END

FUNCTION SmurffApplication::getRoiStruct

  return, self.rois->getListStructDef()
  
END

FUNCTION SmurffApplication::getPhysicalDataStruct

  return, self.physicalDataRuns->getListStructDef()
  
END

FUNCTION SmurffApplication::getPhysicalParameterStruct

  return, self.physicalParameterTypes->getListStructDef()
  
END

FUNCTION SmurffApplication::getYearStruct

  return, self.years->getListStructDef()
  
END

FUNCTION SmurffApplication::getMonthStruct

  return, self.months->getListStructDef()
  
END

FUNCTION SmurffApplication::getRunStruct

  return, self.runs->getListStructDef()
  
END

FUNCTION SmurffApplication::getParameterStruct

  return, self.parameters->getListStructDef()
  
END

FUNCTION SmurffApplication::getSatelliteInfoStruct

  return, self.satelliteInfos->getListStructDef()
  
END

FUNCTION SmurffApplication::getPhysicalInfoStruct

  return, self.physicalInfos->getListStructDef()
  
END

FUNCTION SmurffApplication::getModelInfoStruct

  return, self.modelInfos->getListStructDef()
  
END

FUNCTION SmurffApplication::getGlobInfoStruct

  return, self.globInfos->getListStructDef()
  
END

FUNCTION SmurffApplication::getDaacInfoStruct

  return, self.daacInfos->getListStructDef()
  
END

FUNCTION SmurffApplication::getQaaInfoStruct

  return, self.qaaInfos->getListStructDef()
  
END

PRO SmurffApplication::removeEnviFile, fullFileName

  self.fileSystem->removeEnviFile, fullFileName
  
END

FUNCTION SmurffApplication::enviFileExists, fullFileName

  return, self.fileSystem->enviFileExists(fullFileName)
  
END

FUNCTION SmurffApplication::getParameterByCode, code

  return, self.parameters->getElementByCode(code)
  
END

FUNCTION SmurffApplication::getParameterByOutputBandName, code, EXISTS=EXISTS

  return, self.parameters->getElementByOutputBandName(code, EXISTS=EXISTS)
  
END

PRO SmurffApplication::copyEnviFile, fullSourceFileName, fullDestFileName, TEMP=TEMP, newFileName=newFileName

  self.fileSystem->copyEnviFile, fullSourceFileName, fullDestFileName, TEMP=TEMP, newFileName=newFileName
  
END

FUNCTION SmurffApplication::applyRangeConditions, bandInfo, bandValues, bandVarName, ignoreValue=ignoreValue, SETNAN=SETNAN, NORANGE=NORANGE

  type=size(bandInfo, /TYPE)
  if type eq 7 then parCode=bandInfo
  if type eq 8 then parCode=bandInfo.bandName
  ;par=self.parameters->getElementByOutputBandName(parCode, EXISTS=EXISTS)
  par=self.parameters->getElementByCode(parCode, EXISTS=EXISTS)
  if keyword_set(EXISTS) then bandValues=self.parameters->applyRangeConditions(parCode, bandValues, bandVarName, ignoreValue=ignoreValue, SETNAN=SETNAN, NORANGE=NORANGE) else doLog, 'Miss Parameter: ', parCode, 'in parameter list. No value range restrictions applied.'
  ;if keyword_set(EXISTS) then bandValues=self.parameters->applyRangeConditions(parCode, bandValues, bandVarName, ignoreValue=ignoreValue, SETNAN=SETNAN)
  return, bandValues
  
END

FUNCTION SmurffApplication::getParameters

  return, self.parameters
  
END

FUNCTION SmurffApplication::getDaacFromYear, year

  yearInfo=self.years->getElementByCode(year)
  return, self.daacInfos->getDaac(yearInfo.daacCode)
  
END

FUNCTION SmurffApplication::getGlobFromYear, year, GLOBTYPE=GLOBTYPE

  yearInfo=self.years->getElementByCode(year)
  if n_elements(GLOBTYPE) eq 0 then GLOBTYPE='globOldSeaWiFSCode'
  doLog, 'GLOBTYPE', GLOBTYPE, LEVEL=4
  checkGlob=(where(strupcase(tag_names(yearInfo)) eq strupcase(GLOBTYPE)))[0]
  doLog, 'checkGlob', checkGlob, LEVEL=4
  globTag=yearInfo.(checkGlob)
  doLog, 'globTag', globTag, LEVEL=4
  ;tagName=yearInfo.[checkGlob]
  return, self.globInfos->getGlob(globTag)
  ;return, self.globInfos->getGlob(yearInfo.globCode)
  
END

FUNCTION SmurffApplication::getROIInfoByCode, code

  return, self.rois->getInfoByCode(code)
  
END

FUNCTION SmurffApplication::getRoiOverlapPriorityByCodes, codes

  return, self.rois->getOverlapPriorityByCodes(codes)
  
END

FUNCTION SmurffApplication::getRoiRefRoiCodesByCodes, codes

  return, self.rois->getRefRoiCodesByCodes(codes)
  
END

FUNCTION SmurffApplication::getROIColorDefinitionByCodes, codes

  return, self.rois->getColorDefinitionByCodes(codes)
  
END

FUNCTION SmurffApplication::getRoiDisplayNamesByCodes, codes

  return, self.rois->getDisplayNamesByCodes(codes)
  
END

FUNCTION SmurffApplication::getPhysicalFromCode, code

  return, self.physicalInfos->getPhysical(code)
  
END

FUNCTION SmurffApplication::getModelFromCode, code

  return, self.modelInfos->getModel(code)
  
END

FUNCTION SmurffApplication::getModelFromYear, year

  yearInfo=self.years->getElementByCode(year)
  return, self.modelInfos->getModel(yearInfo.modelCode)
  
END

FUNCTION SmurffApplication::getPhysicalFromYear, year

  yearInfo=self.years->getElementByCode(year)
  codes=strsplit(yearInfo.physicalCode, ';', /EXTRACT)
  noCodes=n_elements(codes)
  phys=objarr(noCodes)
  for i=0, noCodes-1 do phys[i]=self.physicalInfos->getPhysical(codes[i])
  return, phys
  
END

FUNCTION SmurffApplication::getSatelliteFromYear, year

  yearInfo=self.years->getElementByCode(year)
  return, self.satelliteInfos->getSatellite(yearInfo.satelliteCode)
  
END

FUNCTION SmurffApplication::getParameterInfoFromYearCode, year

  yearInfo=self.years->getElementByCode(year)
  parList=self.parameters->getParametersList(yearInfo.parametersFileCode)
  return,  parList
;yearInfo.parameter
;self.parameters
  
END

FUNCTION SmurffApplication::getSatelliteInfoFromYearCode, year

  yearInfo=self.years->getElementByCode(year)
  satelliteType=self.satelliteInfos->getParametersList(yearInfo.satelliteFileCode)
  satInfo=self.satelliteInfos->getSatellite(satelliteType)
  return, satInfo
  
END

FUNCTION SmurffApplication::getConfigParameterSymbol

  return, '%'
  
END

FUNCTION SmurffApplication::getConfigParameterListSeparatorSymbol

  return, ';'
  
END

FUNCTION SmurffApplication::getConfigParameterListInfo, testVar, ISLIST=ISLIST

  ;sep=self->getConfigParameterListSeparatorSymbol
  check=strsplit(testVar, self.application->getConfigParameterListSeparatorSymbol(), /EXTRACT, /PRESERVE)
  ISLIST=n_elements(check) gt 1
  return, check
  
END

FUNCTION SmurffApplication::getOutputFolder

  runCode=self.runSelectionDisplay->getRunCodeSelected()
  outputDir=self.runs->getOutputFolderNameByRunCode(runCode)
  outputDir=outputDir[0]
  return, outputDir
  
END

FUNCTION SmurffApplication::getUtility

  return, self.utility
  
END

FUNCTION SmurffApplication::getSelectedYearList

  return, self.runSelectionDisplay->getYearCodesSelected()
  
END

FUNCTION SmurffApplication::getSelectedMonthList

  return, self.runSelectionDisplay->getMonthCodesSelected()
  
END

FUNCTION SmurffApplication::getRunnableFileList, year, month, NO_SELECTION=NO_SELECTION

  ;ToDo: filter selected file list by single month/single year, user selections
  selCodes=self.runSelectionDisplay->getSourceFileSelectedCodes()
  fList=self.physicalDataRuns->getRunnableFileList(selCodes, year, month, NO_SELECTION=NO_SELECTION)
  if keyword_set(NO_SELECTION) then return, -1 else return,  fList
  
END

FUNCTION SmurffApplication::createControlFile, baseName

  controFileName=self.fileSystem->convertCompatibleFileName(baseName)+'_'
  controFileName=controFileName+self.utility->getSysTime(/FILECOMP)
  controFileName=controFileName+'.ctl'
  ;self.fileSystem->addSuffix()
  return, controFileName
  
END

FUNCTION SmurffApplication::orderList, list, sortArray=sortArray, NUMBER=NUMBER, REVERSE=REVERSE

  doLog, 'before', list
  if keyword_set(NUMBER) then orderedList=fix(list) else orderedList=list
  order=sort(orderedList)
  if keyword_set(REVERSE) then sortArray=reverse(order) else sortArray=order
  ;sortArray=order
  orderedList=list[sortArray]
  doLog, 'after', orderedList
  return, orderedList
  
END

FUNCTION SmurffApplication::orderRoisByPriority, roiList, sortArray=sortArray

  priority=self->getRoiOverlapPriorityByCodes(roiList)
  doLog, 'before', roiList
  order=sort(fix(priority))
  sortArray=reverse(order)
  ;sortArray=order
  roiList=roiList[sortArray]
  doLog, 'after', roiList
  return, roiList
  
END

FUNCTION SmurffApplication::orderRoisByDisplayName, roiList, sortArray=sortArray

  displayNames=self->getRoiDisplayNamesByCodes(roiList)
  doLog, 'before', roiList
  order=sort(displayNames)
  ;sortArray=reverse(order)
  sortArray=order
  roiList=roiList[sortArray]
  doLog, 'after', roiList
  return, roiList
  
END

FUNCTION SmurffApplication::buildImageConverter, statType, outputDir, $
    lookUpTableInfo=lookUpTableInfo, exportSize=exportSize
    
    
  mapdir=self->getKeyValue('IMG_MASK_DIR')
  ;extraColorMapFile=self->getKeyValue('IDL_EXTRA_COLORMAP_FILE')
  colorTableDir=self->getKeyValue('IDL_COLOR_TABLE_DIR')
  if n_elements(lookUpTableInfo) eq 1 then begin
    infos=strsplit(lookUpTableInfo, ';', /EXTRACT)
    exportLutMode=infos[0]
    if strupcase(infos[1]) ne 'AUTO' then lookUpTableCode=infos[1] 
    exportLutFileName=infos[2]
  endif
;  if n_elements(exportSize) eq 1 then begin
;    infos=strlowcase(strsplit(exportSize, ';', /EXTRACT))
;    avTypes=['s','i','m','l']
;    infoImages=intarr(4)
;    for i=0, n_elements(avTypes)-1 do begin
;      res=where(avTypes[i] eq infos, count)
;      infoImages[i]=(count gt 0) ? 1 : 0
;    endfor
;  endif
  
  ;imgConv=obj_new('ImageConverter', colorMapInfo=self.colorMapInfo, roiInfo=self.rois, mapDir=mapDir, colorTableDir=colorTableDir, statType=statType, outputDir=outputDir, lookUpTableCode=lookUpTableCode)
  imgConv=obj_new('ImageConverter', colorMapInfo=self.colorMapInfo, roiInfo=self.rois, mapDir=mapDir, $
    colorTableDir=colorTableDir, statType=statType, outputDir=outputDir, $
    lookUpTableCode=lookUpTableCode, exportLutMode=exportLutMode, exportLutFileName=exportLutFileName, $
    exportSize=exportSize)
  return, imgConv
  
END

PRO SmurffApplication::execImageConvert, request, NODISPLAY=NODISPLAY

  ;if self->checkRequest(/NODISPLAYCHECK) then begin

  fileFilter=request->getInputDir()
  fileFilter=fileFilter+path_sep()+request->getInputFileFilter()
  ;'E:\data\mariomi\application\smurff\data\convert\input\*.hdf'
  filelist = file_search(fileFilter, count=count);
  ;mapdir = 'E:\data\mariomi\application\smurff\input\masks'
  mapdir=self->getKeyValue('IMG_MASK_DIR')
  colorTableDir=self->getKeyValue('IDL_COLOR_TABLE_DIR')
  ;colorMapFile=self->getKeyValue('IMG_MASK_DIR')
  
  ;imgConv=obj_new('ImageConverter', colorMapFileName, roiFileName, colorMapInfo=colorMapInfo, roiInfo=roiInfo, mapDir=mapDir)
  ;lut=request->getLookUpTableCode()
  ;if lut ne '' then lookuptablecode=lut
  
  imgConv=self->buildImageConverter(request->getStatType(), request->getOutputDir(), $
    lookuptableInfo=request->getLookUpTableInfo(), exportSize=request->getExportSize())
  ;imgConv=obj_new('ImageConverter', colorMapInfo=self.colorMapInfo, roiInfo=self.rois,mapDir=mapDir, extraColorMapFile=extraColorMapFile, statType=request->getStatType(), outputDir=request->getOutputDir())
    
  ;imgConv->writeDemoColor
  param=self->getParameterByOutputBandName(request->getVariableCode())
  ;imgConv->processFiles, filelist, request->getVariableCode(), param.name, param.measureUnit
  if request->isCropSet() then cropLimits=strsplit(request->getCropLimits(), ';', /EXTRACT) 
  imgConv->processFiles, filelist, request->getVariableCode(), param.name, param.measureUnit, cropLimits=cropLimits
  obj_destroy, imgConv
  
  return
  
END

PRO SmurffApplication::execRequest, request, NODISPLAY=NODISPLAY

  ;if self->checkRequest(/NODISPLAY) then begin
  if self->checkRequest(/NODISPLAY) then begin
    if n_elements(request) eq 0 then begin
      self.runSelectionDisplay=self.runSelectionView->getInfo()
      request=self->buildRequest(self.runSelectionDisplay)
    endif; else begin
    ;endelse
    request=self->overwriteRunRelated(request, request->getRunCode(), ERROR=ERROR)
    if ~keyword_set(ERROR) then self->doElaboration, request, NODISPLAY=NODISPLAY else doLog,'Bad request', level=2
  endif else begin
    doLog,'Bad request', level=2
  ;aa=self.view->dialogMessage(self->getLangCatLabelFromKey('DialogMessage:CheckRqs'), title=self->getLangCatLabelFromKey('DialogMessageTitle:Request'), /error)
  endelse
  
END

FUNCTION SmurffApplication::overwriteRunRelated, request, runCode, ERROR=ERROR

  util=self.utility
  ERROR=0
  catch, error_status
  ;doLog,systime()
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    errMsg=dialog_message(['Error occured with this Request, saved request not compatible with environment.', 'Check log file (you need to close application).','Hint: make sure that request run code is in your run.xml file'], /ERROR)
    return, request
  endif
  
  runCommand=self.runs->getCommandByRunCode(runCode)
  runCommandType=self.runs->getCommandTypeByRunCode(runCode)
  outputDir=self.runs->getOutputFolderNameByRunCode(runCode)
  inputDir=self.runs->getInputFolderNameByRunCode(runCode)
  inputFileFilter=self.runs->getInputFilterByRunCode(runCode)
  runDisplayName=self.runs->getDisplayNameByRunCode(runCode)
  runOutputParametersList=self.runs->getOutputParametersByRunCode(runCode)
  runInputParametersList=self.runs->getInputParametersByRunCode(runCode)
  runOutputRoi=self.runs->getOutputRoiByRunCode(runCode)
  
  request->setRunCommand, runCommand
  request->setRunCommandType, runCommandType
  request->setInputDir, inputDir
  request->setInputFileFilter, inputFileFilter
  request->setInputFileList, sourceFileList
  ;if runOutputParametersList ne 'N/A' then request->setBandToExportList, util->stringListToArray(runOutputParametersList, sep=';')
  if runOutputParametersList ne 'N/A' then request->setOutputParameterList, util->stringListToArray(runOutputParametersList, sep=';')
  if runInputParametersList ne 'N/A' then request->setInputParameterList, util->stringListToArray(runInputParametersList, sep=';')
  if runOutputRoi ne 'N/A' then request->setOutputRoi, runOutputRoi
  request->setOutputDir, outputDir
  return, request
  
END

FUNCTION SmurffApplication::buildRequest, runSelectionDisplay

  ; convert gui data in a 'Request' object
  ; save on a Request object data needed to perform a job.
  ; then "DoElaboration" with the request as parameter.
  ;get procedure to run from

  util=self.utility
  ;if n_elements(location) eq 0 then location=[0.,0.,1.,1.]
  if n_elements(runSelectionDisplay) eq 0 then runSelectionDisplay=self.runSelectionDisplay
  request=obj_new('Request')
  
  ; add months and years
  runCode=runSelectionDisplay->getRunCodeSelected()
  sourceFileList=runSelectionDisplay->getSourceFileNameSelected()
  
  allYearFlag=runSelectionDisplay->getAllYearFlag()
  allMonthFlag=runSelectionDisplay->getAllMonthFlag()
  allRoiFlag=runSelectionDisplay->getAllRoiFlag()
  allResolutionFlag=runSelectionDisplay->getAllResolutionFlag()
  
  yearList=runSelectionDisplay->getYearCodesSelected()
  monthList=runSelectionDisplay->getMonthCodesSelected()
  roiList=runSelectionDisplay->getRoiCodesSelected()
  resolutionList=runSelectionDisplay->getResolutionCodesSelected()
  
  deleteInputFlag=runSelectionDisplay->getDeleteInputFlag()
  overwriteResultFlag=runSelectionDisplay->getOverwriteResultFlag()
  
  if strcompress(roiList[0], /REMOVE) ne '-1' then roiArchiveList=self.rois->getArchiveRoiCodesSelectedByCodes(roiList) else delidlvar, roiList
  ; overwrite when load...
  runCommand=self.runs->getCommandByRunCode(runCode)
  runCommandType=self.runs->getCommandTypeByRunCode(runCode)
  ;outputDir=self.runs->getOutputFolderNameByRunCode(runCode)
  ;inputDir=self.runs->getInputFolderNameByRunCode(runCode)
  inputFileFilter=self.runs->getInputFilterByRunCode(runCode)
  runDisplayName=self.runs->getDisplayNameByRunCode(runCode)
  periodType=self.runs->getPeriodTypeByRunCode(runCode)
  runOutputParametersList=self.runs->getOutputParametersByRunCode(runCode)
  runOutputRoi=self.runs->getOutputRoiByRunCode(runCode)
  runInputParametersList=self.runs->getInputParametersByRunCode(runCode)
  
  request->setAllMonthFlag, allMonthFlag
  request->setAllYearFlag, allYearFlag
  request->setAllRoiFlag, allRoiFlag
  request->setAllResolutionFlag, allResolutionFlag
  
  request->setRoiList, roiList
  request->setroiArchiveList, roiArchiveList
  request->setYearList, yearList
  request->setMonthList, monthList
  request->setResolutionList, resolutionList
  request->setRunCommand, runCommand
  request->setRunCommandType, runCommandType
  request->setPeriodType, periodType
  ;request->setRunControlFile, runControlFile
  request->setRunCode, runCode
  ;request->setInputDir, inputDir
  request->setInputFileFilter, inputFileFilter
  request->setInputFileList, sourceFileList
  if runInputParametersList ne 'N/A' then request->setInputParameterList, util->stringListToArray(runInputParametersList, sep=';')
  if runOutputParametersList ne 'N/A' then request->setOutputParameterList, util->stringListToArray(runOutputParametersList, sep=';')
  if runOutputRoi ne 'N/A' then request->setOutputRoi, runOutputRoi
  ;if runOutputParametersList ne 'N/A' then request->setBandToExportList, util->stringListToArray(runOutputParametersList, sep=';')
  ;request->setOutputDir, outputDir
  
  request->setDeleteInputFlag, deleteInputFlag
  request->setOverwriteResultFlag, overwriteResultFlag
  
  tempDir=self.fileSystem->getTempDir(/WITH)
  fileName=tempDir+self.utility->getSysTime(/FILECOMPATIBILITY)+self.fileSystem->getRequestExtension()
  doLog,'fileName', level=0
  doLog,fileName, level=0
  doLog,'********', level=0
  ;request->xmlWriteStructList, fileName
  ;request->restore, fileName
  return, request
  
END

FUNCTION SmurffApplication::getButtonLabelFont

  mapEntry=self.mainMapInfo->getEntryByKey('ButtonLabelFont')
  if obj_valid(mapEntry) then return, mapEntry->getValue() else return, self.utility->getButtonLabelFont()
  
END

FUNCTION SmurffApplication::getButtonMediumFont

  mapEntry=self.mainMapInfo->getEntryByKey('ButtonMediumFont')
  if obj_valid(mapEntry) then return, mapEntry->getValue() else return, self.utility->getButtonMediumFont()
  
END

FUNCTION SmurffApplication::getTitleTextFont

  mapEntry=self.mainMapInfo->getEntryByKey('TitleTextFont')
  if obj_valid(mapEntry) then return, mapEntry->getValue() else return, self.utility->getTitleTextFont()
  
END

FUNCTION SmurffApplication::getStandardLabelFont

  mapEntry=self.mainMapInfo->getEntryByKey('StandardLabelFont')
  if obj_valid(mapEntry) then return, mapEntry->getValue() else return, self.utility->getStandardLabelFont()
  
END

FUNCTION SmurffApplication::getStandardTextFont

  mapEntry=self.mainMapInfo->getEntryByKey('StandardTextFont')
  if obj_valid(mapEntry) then return, mapEntry->getValue() else return, self.utility->getStandardTextFont()
  
END

PRO SmurffApplication::setRunSelectionDisplay, runSelectionDisplay

  self.runSelectionDisplay=runSelectionDisplay
  
END

FUNCTION SmurffApplication::getRunSelectionDisplay

  return, self.runSelectionDisplay
  
END

PRO SmurffApplication::initModeDisplay, modeList

  codes=modeList->GetCodeList()
  self.modeDisplay->setNames, modeList->getDisplayNameList()
  self.modeDisplay->setCodes, codes
  self.modeDisplay->setDescriptions, modeList->getDescriptionList()
  self.modeDisplay->setSelection, 0
  
END

FUNCTION SmurffApplication::getFileSystem

  return, self.fileSystem
  
END

FUNCTION SmurffApplication::loadInitFileData

  return, self.fileSystem->loadInitFileData()
  
END

FUNCTION SmurffApplication::getApplicationName

  return, 'JRC - Procas - Smurff'
  
END

FUNCTION SmurffApplication::getApplicationShortName

  return, 'JRC_Procas_Smurff'
  
END

PRO SmurffApplication::closeApplication

  obj_destroy, self
  
END

PRO SmurffApplication::exitRequest

  exitMessage=self.mgr->getLangCatLabelFromKey('DialogMessageTitle:ExitConfirm')
  if self->dialogMessage(exitMessage, /QUESTION) eq 'Yes' then self->closeApplication
  
END

PRO SmurffApplication::updateRunSelectionDisplayInfo, runSelectionDisplayInfo

  obj_destroy, self.runSelectionDisplay
  self.runSelectionDisplay=entityDisplayInfo
  self.mainView->updateInfo
  self.executeOk=1
;runSelectionDisplayInfo->printStream
  
END

FUNCTION SmurffApplication::getScreenSize

  return, self.mainView->getScreenSize()
  
END

PRO SmurffApplication::disable

;self.mainView->disable

END

PRO SmurffApplication::displayRunSelectionGUI

  if obj_valid(self.lastView) then begin
    self.lastView->show
  endif else begin
    self.runSelectionView=obj_new('RunSelectionGUI', self.runSelectionDisplay->Clone(/DEEP), self)
    self.runSelectionView->realize
    self.lastView=self.runSelectionView
    self.mainView=self.runSelectionView
  endelse
  self->disable
  
END

PRO SmurffApplication::initViewConfiguration

  self->initRunSelectionDisplay
  
END

PRO  SmurffApplication::initConfigFileDisplay

END

PRO  SmurffApplication::initRunSelectionDisplay

  self.runSelectionDisplay=obj_new('RunSelectionDisplayInfo', self.rois, self.years, self.runs, self.resolutions, self.months, self.physicalDataRuns)
  
END

FUNCTION SmurffApplication::getKeyValue, key, NOTDEFINED=NOTDEFINED

  NOTDEFINED=0
  mapEntry=self.mainMapInfo->getEntryByKey(key)
  if obj_valid(mapEntry) then begin
    return, mapEntry->getValue()
  endif else begin
    doLog, 'symbol <'+key+'> not defined. Check config.xml file', LEVEL=3
    NOTDEFINED=1
    return, ''
  endelse
  
END

FUNCTION SmurffApplication::getMainMapInfoEntryByKey, key

  return, self.mainMapInfo->getEntryByKey(key)
  
END

FUNCTION SmurffApplication::getColorMapInfo, parameterKey, basinKey

  mapEntry=self.colorMapInfo->getEntryByKey(parameterKey, basinKey)
  if obj_valid(mapEntry) then return, mapEntry else message, 'color table for <'+parameterKey+'/'+basinKey+'> not defined. Check colorMap.xml file'
  
END

PRO  SmurffApplication::setMainMapInfo, mapInfo

  self.mainMapInfo=mapInfo
  
END

PRO  SmurffApplication::setColorMapInfo, colorMapInfo

  self.colorMapInfo=colorMapInfo
  
END

PRO  SmurffApplication::setConstantMapInfo, mapInfo

  self.constantMapInfo=mapDataEntries
  
END

FUNCTION SmurffApplication::getConstant, key

  mapEntry=self.mainMapInfo->getEntryByKey(key)
  if obj_valid(mapEntry) then return, mapEntry->getValue() else message, 'symbol <'+entryKey+'> not defined. Check constant.ini file'
  
END

FUNCTION SmurffApplication::getMatLabLocation, fileName

  entryKey='MATLAB_LOCATION'
  mapEntry=self.mainMapInfo->getEntryByKey(entryKey)
  if obj_valid(mapEntry) then return, mapEntry->getValue() else message, 'symbol <'+entryKey+'> not defined. Check init.ini file'
  
END

;apply delegation
PRO SmurffApplication::writeRecordStream, unit, recordList

  self.fileSystem->writeRecordStream, unit, recordList
  
END

;apply delegation
FUNCTION SmurffApplication::readRecordStream, unit, columns=columns

  return, self.fileSystem->readRecordStream(unit, columns=columns)
  
END

PRO SmurffApplication::startJournaling

  journalFullFileName=self.fileSystem->getLogDir(/WITH)
  journalFullFileName=journalFullFileName+self.utility->getSysTime(/FILECOMPATIBILITY)+'.log'
  journal, journalFullFileName
  
END

PRO SmurffApplication::initLanguage, mapDataEntries

  mapEntry=mapDataEntries->getEntryByKey('LANGUAGE_KEY')
  if obj_valid(mapEntry) then languageKey=mapEntry->getValue()
  
  mapEntry=mapDataEntries->getEntryByKey('LANGUAGE_PATH')
  if obj_valid(mapEntry) then languagePath=mapEntry->getValue() else languagePath=self.fileSystem->getResourceDir()
  
  mapEntry=mapDataEntries->getEntryByKey('LANGUAGE_FILE')
  if obj_valid(mapEntry) then languageFile=mapEntry->getValue()
  
  self.languageCatalog = OBJ_NEW( 'IDLffLangCat', languageKey, $
    APP_NAME=self->getApplicationShortName(), $
    APP_PATH=languagePath, $
    FILENAME=languageFile, /VERBOSE )
    
END

PRO SmurffApplication::startUp, BATCHMODE=BATCHMODE

  self->startJournaling
  confDir=self.fileSystem->getConfigurationDir(/WITH)
  resourceDir=self.fileSystem->getResourceDir(/WITH)
  ;mapDataEntries=self.fileSystem->loadInitFileData()
  mapDataEntries=self.fileSystem->loadInitXMLFileData()
  colorMapDataEntries=self.fileSystem->loadColorMapXMLFileData()
  self->initLanguage, mapDataEntries
  self->setMainMapInfo, mapDataEntries
  self->setColorMapInfo, colorMapDataEntries
  lookUpEntry=mapDataEntries->getEntryByKey('STARTUP_LOOKUP')
  if ~obj_valid(lookUpEntry) then doLookUp=1 else doLookUp=self.utility->isTrue((lookUpEntry->getValue()))
  if doLookUp then self.fileSystem->lookUpSystemData
  logEntry=mapDataEntries->getEntryByKey('LOG_LEVEL')
  ;0 No log, 1 warning, 2 error, 3 all log
  if ~obj_valid(logEntry) then logLevel=1 else logLevel=logEntry->getValue()
  self->setLogLevel, logLevel
  ; Load Standard Resources first (fixed)
  ;extraParameterNames=[''] & extraParameterValues=['']
  ;for i=0, n_elements(parameterName)-1 do begin
  for i=0, mapDataEntries->length()-1 do begin
    entry=mapDataEntries->getEntryByIndex(i)
    ;if strmid(thisPar, 0, 4) eq 'TXT_' then varname=parameterValue[i] else fileName=confDir+parameterValue[i]
    case strupCase(entry->getKey()) of
      'STARTUP_LOOKUP' : ;do nothing
      'RUN_FILE' : if self.runs->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong RUN_FILE'
      'YEAR_FILE' : if self.years->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong YEAR_FILE'
      'SATELLITE_FILE' : if self.satelliteInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong SATELLITE_FILE'
      'QAA_FILE' : if self.qaaInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong QAA_FILE'
      'PHYSICAL_FILE' : if self.physicalInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong PHYSICAL_FILE'
      'MODEL_FILE' : if self.modelInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong MODEL_FILE'
      'DAAC_FILE' : if self.daacInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong DAAC_FILE'
      'GLOB_FILE' : if self.globInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong GLOB_FILE'
;      'GLOB_OLDSEAWIFS_FILE' : if self.globInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong GLOB_OLDSEAWIFS_FILE'
;      'GLOB_SEAWIFS_FILE' : if self.globInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong GLOB_SEAWIFS_FILE'
;      'GLOB_MERIS_FILE' : if self.globInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong GLOB_MERIS_FILE'
;      'GLOB_MODISA_FILE' : if self.globInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong GLOB_MODISA_FILE'
;      'GLOB_VIIRS_FILE' : if self.globInfos->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong GLOB_VIIRS_FILE'
      'PARAMETER_FILE' : if self.parameters->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong PARAMETER_FILE'
      'MONTH_FILE' : if self.months->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong MONTH_FILE'
      'PHYSICALDATARUN_FILE' : if self.physicalDataRuns->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong PHYSICALDATARUN_FILE'
      'ROI_FILE' : if self.rois->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong ROI_FILE'
      'RESOLUTION_FILE' : if self.resolutions->fillDataFromXMLFile(confDir+entry->getValue()) ne 1 then message, 'wrong RESOLUTION_FILE'
      'TXT_VERSION_DATE' : self.versionDate=entry->getValue() ; save
      'TXT_VERSION_CODE' : self.versionCode=entry->getValue() ; save
      'BROWSER_LOCATION' : doLog,self->getBrowserLocation(), level=0 ;do nothing, move to virtual attribute using mapEntry list self->setBrowserLocation, entry->getValue() ; save location of browser application
      'WORKSHEET_LOCATION' : doLog,self->getWorkSheetLocation(), level=0 ;do nothing, move to virtual attribute using mapEntry list self->setWorkSheetLocation, entry->getValue() ; save location of worksheet reader
      'DOCREADER_LOCATION' : doLog,self->getDocReaderLocation(), level=0 ;do nothing, move to virtual attribute using mapEntry list self->setDocReaderLocation, entry->getValue() ; Save location of doc reader
      'PDFREADER_LOCATION' : doLog,self->getPdfReaderLocation(), level=0 ;do nothing, move to virtual attribute using mapEntry list self->setPdfReaderLocation, entry->getValue() ; Save location of pdf reader
      'MATLAB_LOCATION' : doLog,self->getMatLabLocation(), level=0 ;do nothing, move to virtual attribute using mapEntry list self->setPdfReaderLocation, entry->getValue() ; Save location of pdf reader
    else :begin
  ;doLog,'unexpected variable: Key<'+entry->getKey()+'>; Value<'+entry->getValue()+'>'
  ;extraParameterNames=[extraParameterNames, thisPar]
  ;extraParameterValues=[extraParameterValues, parameterValue[i]]
  end
endcase
endfor
if ~(keyword_set(startUp)) then self->initViewConfiguration
;executeXMLFile

END

; MM 11/11/11
; here end new code

FUNCTION SmurffApplication::getScaleInfo

  return, self.scaleInfo
  
END

PRO SmurffApplication::setScaleInfo, value

  self.scaleInfo=value
  
END

FUNCTION SmurffApplication::getSplashLogoImage

  image=read_bmp(self.fileSystemMgr->getSplashLogoFileName())
  dims=size(image, /DIM)
  ; trueImage=bytarr(dims[0], dims[1], 3)
  ; trueImage[*,*,0]=r & trueImage[*,*,1]=g & trueImage[*,*,2]=b
  return, image
  
END

FUNCTION SmurffApplication::getLogoImage, true=true

  image=read_bmp(self.fileSystemMgr->getLogoFileName(),/RGB)
  dims=size(image, /DIM)
  ; trueImage=bytarr(dims[0], dims[1], 3)
  ; trueImage[*,*,0]=r & trueImage[*,*,1]=g & trueImage[*,*,2]=b
  true=(where(dims eq 3))[0]
  return, image
  
END

FUNCTION SmurffApplication::getVersionDate

  return, self.versionDate
  
END

FUNCTION SmurffApplication::getVersionCode

  return, self.versionCode
  
END

; *****************************************************
; main view related methods
; *****************************************************
FUNCTION SmurffApplication::getMainView

  return, self.mainView
  
END

PRO SmurffApplication::setMainView, view

  self.mainView=view
  
END
FUNCTION SmurffApplication::dialogMessage, textMessage, title=title, info=info, error=error, question=question

  return, self.mainView->dialogMessage(textMessage, title=title, INFO=INFO, ERROR=ERROR, QUESTION=QUESTION)
  
END

PRO SmurffApplication::show

  self.mainView->show
  
END

PRO SmurffApplication::enable

;self.mainView->enable

END

PRO SmurffApplication::display

  self->displayRunSelectionGUI
  
END
; *****************************************************
; child views related methods
; *****************************************************
; get from views (data and analysis) multiple choice info related
;FUNCTION SmurffApplication::getMultipleChoiceInfo

PRO SmurffApplication::saveRequest

  fs=self->getFileSystem()
  ;filter=['*'+fs->getRequestExtension()]
  if (self.fileSystem->isOSUnix()) then fix_filter='*'+strmid(fs->getRequestExtension(),1,4) else fix_filter='*'+fs->getRequestExtension()
  if (self.fileSystem->isOSUnix()) then filter='*'+strmid(fs->getRequestExtension(),1,4) else filter='*'+fs->getRequestExtension()
  if (self.fileSystem->isOSUnix()) then default_extension=strmid(fs->getRequestExtension(),1,4) else default_extension=fs->getRequestExtension()
  requestSelectionFile=dialog_pickfile(DEFAULT_EXTENSION=default_extension, $
    DIALOG_PARENT=self.mainView->getTopBase(), $
    FILTER=filter, FIX_FILTER=fix_filter, $
    GET_PATH=path, PATH=fs->getSaveDir(), $
    TITLE='Save request', /OVERWRITE_PROMPT, /WRITE)
  ;important update contents from GUI BEFORE save a request
  if requestSelectionFile ne '' then begin
    requestSelectionFile=fs->removeFileExtension(requestSelectionFile)
    requestSelectionFile=fs->addFileExtension(requestSelectionFile, fs->getRequestExtension(), extensionSep='')
    self.runSelectionDisplay=self.runSelectionView->getInfo()
    request=self->buildRequest(self.runSelectionDisplay)
    if obj_valid(request) and requestSelectionFile ne '' then begin
      request->xmlWriteStructList, requestSelectionFile
      discard=self->dialogMessage(self->getLangCatLabelFromKey('DialogMessage:RequestSaved', placeHolders=[requestSelectionFile]), $
        title=self->getLangCatLabelFromKey('DialogMessageTitle:RequestSaved'), /INFO)
    endif
  endif
  
END

PRO SmurffApplication::convertToImage

  fs=self->getFileSystem()
  if (self.fileSystem->isOSUnix()) then fix_filter='*'+strmid(fs->getXMLExtension(),1,4) else fix_filter='*'+fs->getXMLExtension()
  if (self.fileSystem->isOSUnix()) then filter='*'+strmid(fs->getXMLExtension(),1,4) else filter=['*'+fs->getXMLExtension()]
  if (self.fileSystem->isOSUnix()) then default_extension=strmid(fs->getXMLExtension(),1,4) else default_extension=fs->getXMLExtension()
  xmlImageFile=dialog_pickfile(DEFAULT_EXTENSION=default_extension, $
    DIALOG_PARENT=self.mainView->getTopBase(), $
    FILTER=filter, FIX_FILTER=fix_filter, $
    GET_PATH=path, PATH=fs->getSaveDir(), $
    TITLE='XML convert to image file', /READ, /MUST_EXIST)
  if xmlImageFile ne '' then begin
    request=self->loadImageConvert(xmlImageFile)
    if obj_valid(request) then begin
      discard=self->dialogMessage(self->getLangCatLabelFromKey('DialogMessage:RequestRestoreFound', placeHolders=[xmlImageFile]), $
        title=self->getLangCatLabelFromKey('DialogMessageTitle::RequestRestoreFound'), /INFO)
      self->execImageConvert, request, /NODISPLAY
      discard=self->dialogMessage(self->getLangCatLabelFromKey('DialogMessage:RequestRestoreExecute', placeHolders=[xmlImageFile]), $
        title=self->getLangCatLabelFromKey('DialogMessageTitle::RequestRestoreExecute'), /INFO)
      return
    endif
    discard=self->dialogMessage(self->getLangCatLabelFromKey('DialogMessage:RequestRestoreFailed', placeHolders=[xmlImageFile]), $
      title=self->getLangCatLabelFromKey('DialogMessageTitle::RequestRestoreFailed'), /INFO)
  endif
  
END

PRO SmurffApplication::restoreRequest

  fs=self->getFileSystem()
  if (self.fileSystem->isOSUnix()) then fix_filter='*'+strmid(fs->getRequestExtension(),1,4) else fix_filter='*'+fs->getRequestExtension()
  if (self.fileSystem->isOSUnix()) then filter='*'+strmid(fs->getRequestExtension(),1,4) else filter=['*'+fs->getRequestExtension()]
  if (self.fileSystem->isOSUnix()) then default_extension=strmid(fs->getRequestExtension(),1,4) else default_extension=fs->getRequestExtension()
  requestSelectionFile=dialog_pickfile(DEFAULT_EXTENSION=default_extension, $
    DIALOG_PARENT=self.mainView->getTopBase(), $
    FILTER=filter, FIX_FILTER=fix_filter, $
    GET_PATH=path, PATH=fs->getSaveDir(), $
    TITLE='Restore request', /READ, /MUST_EXIST)
  if requestSelectionFile ne '' then begin
    request=self->loadRequest(requestSelectionFile)
    if obj_valid(request) then begin
      discard=self->dialogMessage(self->getLangCatLabelFromKey('DialogMessage:RequestRestoreFound', placeHolders=[requestSelectionFile]), $
        title=self->getLangCatLabelFromKey('DialogMessageTitle::RequestRestoreFound'), /INFO)
      self->execRequest, request, /NODISPLAY
      discard=self->dialogMessage(self->getLangCatLabelFromKey('DialogMessage:RequestRestoreExecute', placeHolders=[requestSelectionFile]), $
        title=self->getLangCatLabelFromKey('DialogMessageTitle::RequestRestoreExecute'), /INFO)
      return
    endif
    discard=self->dialogMessage(self->getLangCatLabelFromKey('DialogMessage:RequestRestoreFailed', placeHolders=[requestSelectionFile]), $
      title=self->getLangCatLabelFromKey('DialogMessageTitle::RequestRestoreFailed'), /INFO)
  endif
  
END

;PRO SmurffApplication::setBlockWindowControl, OFF=OFF
;
;  self.lastview=obj_new()
;; if keyword_set(OFF) then self.openViewCheck=0 else self.openViewCheck=1
;
;END

; *****************************************************
; core methods
; *****************************************************

FUNCTION SmurffApplication::checkRequest, multipleUserChoices=multipleUserChoices, NODISPLAY=NODISPLAY

  if self.runSelectionDisplay->checkIntegrity(self.mainView) ne 1 then return, 0
  return, 1b
  
END

PRO SmurffApplication::runMode, runMode, request, NODISPLAY=NODISPLAY

  case runMode of
    'SPAWN':self->runSpawnMode, request
    ;'IDL':self->runIDLMode, request->getRunCommand()+" "+request->getRunControlFile()
    'IDL':self->runIDLMode, request, NODISPLAY=NODISPLAY
    'MATLAB':self->runMatlabMode, request
    else: message, 'run type not available'
  endcase
  
END

PRO SmurffApplication::runMatlabMode, runString

  spawn, request->getRunCommand()+" "+self->createControlFile(request->getRunCode())
  
END

PRO SmurffApplication::runSpawnMode, runString

  spawn, request->getRunCommand()+" "+self->createControlFile(request->getRunCode())
  
END

PRO SmurffApplication::runIDLMode, request, NODISPLAY=NODISPLAY

  call_procedure, request->getRunCommand(), request, NODISPLAY=NODISPLAY
  
END

PRO SmurffApplication::doElaboration, request, NODISPLAY=NODISPLAY

  ;get data (fill result)
  ;  ERROR=0
  ;  catch, error_status
  ;  ;doLog,systime()
  ;  if error_status NE 0 THEN BEGIN
  ;    ERROR=1
  ;    catch, /CANCEL
  ;    errMsg=dialog_message(['Error occured with this Analysis/Data selection.', 'Check log file (you need to close application).','Hint: make sure that parameter is selected before stations'], /ERROR)
  ;    return
  ;  endif

  if ~keyword_set(NODISPLAY) then widget_control, /HOURGLASS
  ;startTime=systime(/SECONDS)
  ;endTime=systime(/SECONDS)
  ;doLog,'-->', endTime-startTime
  self->runMode, request->getRunCommandType(), request, NODISPLAY=NODISPLAY
  if ~keyword_set(NODISPLAY) then widget_control, HOURGLASS=0
  title=self->getLangCatLabelFromKey('DialogMessageTitle:JobFinished')
  message=self->getLangCatLabelFromKey('DialogMessage:JobFinished', placeHolders=request->getOutputDir())
  if obj_valid(self.runSelectionView) then res=self.runSelectionView->dialogMessage(message, $
    title=title, $
    /INFO) else self->logMessage, [Title, message]
  obj_destroy, request
  
END

PRO SmurffApplication::logMessage, text, level=level

  for i=0, n_elements(text)-1 do doLog,text[i], level=level
  
END

;****************************************************************************************
; start up methods
;****************************************************************************************

FUNCTION SmurffApplication::init, root=root

  if not self -> Object::init() then return , 0
  ;self.fileSystem=obj_New('SmurffileSystem', obj_class(self), self, applicationRoot='E:\data\mariomi\application\smurff')
  ;self.fileSystem=obj_New('SmurffFileSystem', obj_class(self), self, applicationRoot=root, /MUSTEXISTS)
  self.fileSystem=obj_New('SmurffFileSystem', 'SMURFF', self, applicationRoot=root, /MUSTEXISTS)
  if ~obj_valid(self.fileSystem) then return, 0
  self.utility=obj_new('Utility')
  ;self.mainConfig=obj_new('FMMainConfig')
  self.runs=obj_new('Run', self)
  self.rois=obj_new('Roi', self)
  self.months=obj_new('Month', self)
  self.years=obj_new('Year', self)
  self.parameters=obj_new('Parameter', self)
  self.satelliteInfos=obj_new('SatelliteInfo', self)
  self.qaaInfos=obj_new('QaaInfo', self)
  self.physicalInfos=obj_new('PhysicalInfo', self)
  self.modelInfos=obj_new('ModelInfo', self)
  self.daacInfos=obj_new('DaacInfo', self)
  self.globInfos=obj_new('GlobInfo', self)
  ;self.globInfos=objarr(obj_new('GlobInfo', self), obj_new('GlobInfo', self), obj_new('GlobInfo', self), obj_new('GlobInfo', self), obj_new('GlobInfo', self))
  self.resolutions=obj_new('Resolution', self)
  self.physicalDataRuns=obj_new('PhysicalDataRun', self)
  self.filterFile='filter.txt'
  
  ;self.entityDisplay=obj_new('EntityDisplayInfo')
  return, 1
  
END

PRO SmurffApplication::cleanUp

  obj_destroy, self.fileSystem
  obj_destroy, self.utility
  obj_destroy, self.rois
  obj_destroy, self.runs
  obj_destroy, self.months
  obj_destroy, self.years
  obj_destroy, self.resolutions
  obj_destroy, self.physicalDataRuns
  obj_destroy, self.parameters
  obj_destroy, self.satelliteInfos
  self -> Object :: cleanUp
  journal
  envi_batch_exit
  
END
;****************************************************************************************
; constructor/destructor
;****************************************************************************************
PRO SmurffApplication__Define

  Struct = { SmurffApplication , $
    mainView : obj_new(), $
    runSelectionView: obj_new(), $
    runSelectionDisplay: obj_new(), $
    modeDisplay: obj_new(), $
    configurationDisplay: obj_new(), $
    mainConfig : obj_new(), $
    resolutions : obj_new(), $
    rois : obj_new(), $
    physicalDataRuns : obj_new(), $
    physicalParameterTypes : obj_new(), $
    years : obj_new(), $
    months : obj_new(), $
    runs : obj_new(), $
    parameters : obj_new(), $
    satelliteInfos : obj_new(), $
    physicalInfos : obj_new(), $
    modelInfos : obj_new(), $
    daacInfos : obj_new(), $
    globInfos : obj_new(), $
    qaaInfos : obj_new(), $
    lastView: obj_new(), $
    versionDate : '', $
    versionCode : '', $
    mainMapInfo : obj_new(), $
    colorMapInfo : obj_new(), $
    languageCatalog: obj_new(), $
    fileSystem: obj_new(), $
    utility: obj_new(), $
    logLevel: 0, $
    filterFile: '', $
    Inherits Object $
    }
    
END

;****************************************************************************************
