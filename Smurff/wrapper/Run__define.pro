FUNCTION Run::getCommandTypeByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  commandTypeList=self->getcommandTypes()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    commandType=commandTypeList[idx]
    return, commandType
  endif
  NOTFOUND=1
  return, commandTypeList[0]
  
END

FUNCTION Run::getOutputParametersByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  outputParametersList=self->getOutputParameters()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    outputParameters=outputParametersList[idx]
    return, outputParameters
  endif
  NOTFOUND=1
  return, outputParametersList[0]
  
END

FUNCTION Run::getInputParametersByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  inputParametersList=self->getInputParameters()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    inputParameters=inputParametersList[idx]
    return, inputParameters
  endif
  NOTFOUND=1
  return, inputParametersList[0]
  
END

FUNCTION Run::getOutputRoiByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  outputRoiList=self->getOutputRois()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    outputRoi=outputRoiList[idx]
    return, outputRoi
  endif
  NOTFOUND=1
  return, outputRoiList[0]
  
END

FUNCTION Run::getDisplayNameByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  displayNameList=self->getDisplayNames()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    displayName=displayNameList[idx]
    return, displayName
  endif
  NOTFOUND=1
  return, displayNameList[0]
  
END

FUNCTION Run::getCommandByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  commandList=self->getCommands()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    command=commandList[idx]
    return, command
  endif
  NOTFOUND=1
  return, commandList[0]
  
END

FUNCTION Run::getDescriptionByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  descList=self->getDescriptions()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    desc=descList[idx]
    return, desc
  endif
  NOTFOUND=1
  return, descList[0]
  
END

FUNCTION Run::getPeriodTypeByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  periodTypeList=self->getPeriodTypes()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    periodType=periodTypeList[idx]
    return, periodType
  endif
  NOTFOUND=1
  return, periodTypeList[0]
  
END

FUNCTION Run::getUseSelectionByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  useSelectionList=self->getUseSelections()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    useSelection=useSelectionList[idx]
    return, useSelection
  endif
  NOTFOUND=1
  return, useSelection[0]
  
END

FUNCTION Run::buildFullFieldList, simpleList

  baseStruct=self->getListStructDef()
  nElem=n_elements(simpleList)
  list=replicate(baseStruct, nElem)
  for i=0, nElem-1 do begin
    list[i].code=strcompress(index, /REMOVE); & code=structElement.code
    list[i].displayName=simpleList[i].displayName
    list[i].command=simpleList[i].command
    list[i].commandType=simpleList[i].commandType
    list[i].periodType='M'
    list[i].inputFolder=simpleList[i].inputFolder
    list[i].inputFilter=simpleList[i].inputFilter
    list[i].inputParameters=simpleList[i].inputParameters
    list[i].outputFolder=simpleList[i].outputFolder
    list[i].outputFilter=simpleList[i].outputFilter
    list[i].outputParameters=simpleList[i].outputParameters
    list[i].useSelection=simpleList[i].useSelection
    list[i].description=simpleList[i].description
  endfor
  return, list
  
END

FUNCTION  Run::getVersion

  return, '1.0'
  
END

FUNCTION Run::getFolderContents, folderName, NOPATH=NOPATH

  fs=obj_new('FileSystem', /STAND_ALONE)
  ; 20160412 MM: speed up gui
  files=['Speed Up', 'Test']
  ;files=fs->searchFiles(folderName, /ONLYFILES, /RELATIVE)
  obj_destroy, fs
  return, files
; files=file_search(folderName, /TEST_REGULAR)
; for i=0, n_elements(files)-1 do files[i]=strsplit(files[i], path_sep(), /EXTRACT
; return, files
  
END

FUNCTION Run::getOutputFolderNameByRunCode, WITHFILTER=WITHFILTER, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  outputFolderList=self->getOutputFolders()
  outputFilterList=self->getOutputFilters()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    folderName=outputFolderList[idx]
    if strmid(folderName, strlen(folderName)-1,1) eq path_sep() then slash='' else slash=path_sep()
    if keyword_set(WITHFILTER) then return, folderName+slash+outputFilterList[idx] else return, folderName
  endif
  NOTFOUND=1
  return, outputFolderList[0]
  
END

FUNCTION Run::getOutputFilterByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  outputFilterList=self->getOutputFilters()
  idx=where(code eq codeList, count)
  if count eq 1 then return, outputFilterList[idx]
  NOTFOUND=1
  return, outputFilterList[0]
  
END

;FUNCTION Run::getFolderInfo, folderName
;
;
;END

FUNCTION Run::getInputFolderByRunCode, code, WITHFILTER=WITHFILTER, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  inputFolderList=self->getInputFolders()
  inputFilterList=self->getInputFilters()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    folderNames=inputFolderList[idx]
    ;check multiple folders
    folderNames=self.application->getConfigParameterListInfo(ISLIST)
    ;env parameter folder name
    for i=0, n_elements(folderNames) do folderInfo=self->getFolderInfo()
    if strmid(folderName, strlen(folderName)-1,1) eq path_sep() then slash='' else slash=path_sep()
    if keyword_set(WITHFILTER) then return, folderName+slash+inputFilterList[idx] else return, folderName
  endif
  NOTFOUND=1
  return, inputFolderList[0]
  
END

FUNCTION Run::getInputFolderNameByRunCode, code, WITHFILTER=WITHFILTER, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  inputFolderList=self->getInputFolders()
  inputFilterList=self->getInputFilters()
  idx=where(code eq codeList, count)
  if count eq 1 then begin
    folderName=inputFolderList[idx]
    if strmid(folderName, strlen(folderName)-1,1) eq path_sep() then slash='' else slash=path_sep()
    if keyword_set(WITHFILTER) then return, folderName+slash+inputFilterList[idx] else return, folderName
  endif
  NOTFOUND=1
  return, inputFolderList[0]
  
END

FUNCTION Run::getInputFilterByRunCode, code, NOTFOUND=NOTFOUND

  NOTFOUND=0
  codeList=self->getCodes()
  inputFilterList=self->getInputFilters()
  idx=where(code eq codeList, count)
  if count eq 1 then return, inputFilterList[idx]
  NOTFOUND=1
  return, inputFilterList[0]
  
END

FUNCTION Run::buildRecordFromElement, index, structElement

  code=strcompress(index, /REMOVE) & code=structElement.code
  displayName=structElement.displayName
  command=structElement.command
  commandType=structElement.commandType
  periodType=structElement.periodType
  inputFolder=structElement.inputFolder
  inputFilter=structElement.inputFilter
  inputParameters=structElement.inputParameters
  outputFolder=structElement.outputFolder
  outputFilter=structElement.outputFilter
  outputParameters=structElement.outputParameters
  useSelection=structElement.useSelection
  description=structElement.description
  record=[code, displayName, command, commandType, periodType, inputFolder, inputFilter, inputParameters, outputFolder, outputFilter, outputParameters, useSelection, description]
  return, record
  
END

FUNCTION Run::getUseSelections

  thisList=self->getList()
  list=thisList[*].useSelection
  return, list
  
END

FUNCTION Run::getCodes

  thisList=self->getList()
  list=thisList[*].code
  return, list
  
END

FUNCTION Run::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION Run::getCommands

  thisList=self->getList()
  return, thisList[*].command
  
END

FUNCTION Run::getCommandTypes

  thisList=self->getList()
  return, thisList[*].commandType
  
END

FUNCTION Run::getPeriodTypes

  thisList=self->getList()
  return, thisList[*].periodType
  
END

FUNCTION Run::getInputFolders

  thisList=self->getList()
  return, thisList[*].inputFolder
  
END

FUNCTION Run::getInputFilters

  thisList=self->getList()
  return, thisList[*].inputFilter
  
END

FUNCTION Run::getInputParameters

  thisList=self->getList()
  return, thisList[*].inputParameters
  
END

FUNCTION Run::getOutputFolders

  thisList=*self.list
  return, thisList[*].outputFolder
  
END

FUNCTION Run::getOutputFilters

  thisList=self->getList()
  return, thisList[*].outputFilter
  
END

FUNCTION Run::getOutputParameters

  thisList=self->getList()
  return, thisList[*].outputParameters
  
END

FUNCTION Run::getOutputRois

  thisList=self->getList()
  return, thisList[*].outputRoi
  
END

FUNCTION Run::getDescriptions

  thisList=self->getList()
  return, thisList[*].description
  
END

FUNCTION Run::getListStructDef

  struct = { code:0,$
    displayName:'', $
    command:'', $
    commandType:'', $
    periodType:'', $
    inputfolder:'', $
    inputfilter:'', $
    inputParameters:'', $
    outputFolder:'', $
    outputFilter:'', $
    outputParameters:'', $
    outputRoi:'', $
    useSelection:'', $
    description: '' $
    }
    
  return, struct
  
END

FUNCTION Run::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fix(fieldsInfo[0])
    thisStruct.displayName=fieldsInfo[1]
    thisStruct.command=fieldsInfo[2]
    thisStruct.commandType=fieldsInfo[3]
    thisStruct.periodType=fieldsInfo[4]
    thisStruct.inputFolder=fieldsInfo[5]
    thisStruct.inputFilter=fieldsInfo[6]
    thisStruct.inputParameters=fieldsInfo[7]
    thisStruct.outputFolder=fieldsInfo[8]
    thisStruct.outputFilter=fieldsInfo[9]
    thisStruct.outputParameters=fieldsInfo[10]
    thisStruct.outputRoi=fieldsInfo[11]
    thisStruct.useSelection=fieldsInfo[12]
    thisStruct.description=fieldsInfo[13]
  endif
  return, thisStruct
  
END

FUNCTION Run::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

PRO Run::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=self->getList()
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** displayName:<', thisList[i].displayName,'>'
    print, '**** command:<', thisList[i].command,'>'
    print, '**** commandType:<', thisList[i].commandType,'>'
    print, '**** periodType:<', thisList[i].periodType,'>'
    print, '**** inputFolder:<', thisList[i].inputFolder,'>'
    print, '**** inputFilter:<', thisList[i].inputFilter,'>'
    print, '**** inputParameters:<', thisList[i].inputParameters,'>'
    print, '**** outputFolder:<', thisList[i].outputFolder,'>'
    print, '**** outputFilter:<', thisList[i].outputFilter,'>'
    print, '**** outputParameters:<', thisList[i].outputParameters,'>'
    print, '**** outputRoi:<', thisList[i].outputRoi,'>'
    print, '**** useSelection:<', thisList[i].useSelection,'>'
    print, '**** description:<', thisList[i].description,'>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

FUNCTION Run::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO Run::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO Run__Define

  Struct = { Run , $
    Inherits ConfigurableData $
    }
    
END