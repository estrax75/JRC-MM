FUNCTION PhysicalDataRun::getRunnableFileList, codes, year, month, NO_SELECTION=NO_SELECTION

 NO_SELECTION=0
 nElem=n_elements(codes)
 check=intarr(nElem)
 allCodes=self->getCodes()
 for i=0, nElem-1 do begin
  check[i]=where(allCodes eq codes[i])
 endfor
 selList=(self->getList())[check]
 finalIdxs=where(selList.year eq year and selList.month eq month, finds)
 print, 'found (#, indexes-->', finds, '/', finalIdxs
 if finds gt 0 then return,  selList[finalIdxs].fileName
 NO_SELECTION=1
 return, -1
 
  
END

FUNCTION PhysicalDataRun::getFilesByRunCodes, codeList, NAME_ORDERED=NAME_ORDERED, OUTSORTSUB=OUTSORTSUB

  NOTFOUND=1
  allFiles=self->getFilenames()
  allCodes=self->getCodes()
  nElem=n_elements(codeList)
  fileList=strarr(nElem)
  for i=0, nElem-1 do begin
    checkIdx=where(codeList[i] eq allCodes, count)
    if count ne 0 then fileList[i]=allFiles[checkIdx[0]]
    ;allFiles[(where(codeList[i] eq allCodes))
    ;fileList[i]=allFiles[(where(codeList[i] eq allCodes))[0]]
  endfor
  if keyword_set(NAME_ORDERED) then begin
    OUTSORTSUB=sort(fileList)
    fileList=fileList[OUTSORTSUB]
  endif
  return, fileList
  
END

;FUNCTION PhysicalDataRun::buildFullFieldList, simpleList
;
;  baseStruct=self->getListStructDef()
;  nElem=n_elements(simpleList)
;  list=replicate(baseStruct, nElem)
;  for i=0, nElem-1 do begin
;    list[i].code=strcompress(i, /REMOVE)
;    list[i].displayName=simpleList[i].filename
;    list[i].type=simpleList[i].type
;    list[i].roi=simpleList[i].roi
;    list[i].resolution=simpleList[i].resolution
;    list[i].extraInfo=simpleList[i].extraInfo
;    list[i].unknown=simpleList[i].unknown
;    list[i].year=simpleList[i].year
;    list[i].month=simpleList[i].month
;    list[i].execDate=simpleList[i].execDate
;    list[i].fileName=simpleList[i].filename
;    list[i].description='Descr of: '+simpleList[i].filename
;  endfor
;  return, list
;  
;END

;FUNCTION  PhysicalDataRun::getVersion
;
;  return, '1.0'
;  
;END

FUNCTION PhysicalDataRun::filterByROI, roiCodes, runCodes, NOTFOUND=NOTFOUND

  NOTFOUND=1
  allCodes=self->getCodes()
  if n_elements(runCodes) eq 0 then begin
    runCodes=allCodes
    runIndexes=indgen(n_elements(runCodes))
  endif else begin
    runIndexes=intarr(n_elements(runCodes))
    for i=0, n_elements(runCodes)-1 do runIndexes[i]=(where(runCodes[i] eq allCodes))[0]
  endelse
  
  allRoiCodes=self->getRois()
  selectedRoiCodes=allRoiCodes[runIndexes]
  roiIndexes=['-1']
  for i=0, n_elements(roiCodes)-1 do begin
    idxs=where(roiCodes[i] eq selectedRoiCodes, count)
    if count gt 0 then roiIndexes=[roiIndexes, idxs]
  endfor
  if n_elements(roiIndexes) eq 1 then return, ['-1']
  NOTFOUND=0
  foundCodes=runCodes[roiIndexes[1:*]]
  ;print, (self->getFilenames())[roiIndexes[1:*]]
  return, foundCodes
;foundCodes=roiCodes[0]
;  foundIndexes=[-1]
;  for i=0, n_elements(roiCodes)-1 do begin
;    idxs=where(roiCodes[i] eq allRoiCodes, count)
;    if count gt 0 then foundIndexes=[foundIndexes, idxs]
;  endfor
;  if n_elements(foundIndexes) gt 1 then begin
;
;    for i=0, n_elements()
;    return, internalCodes[foundIndexes]
;  endif
;  NOTFOUND=1
;  return, [-1]
  
END

FUNCTION PhysicalDataRun::filterByYear, yearCodes, runCodes, NOTFOUND=NOTFOUND

  NOTFOUND=1
  allCodes=self->getCodes()
  if n_elements(runCodes) eq 0 then begin
    runCodes=allCodes
    runIndexes=indgen(n_elements(runCodes))
  endif else begin
    runIndexes=intarr(n_elements(runCodes))
    for i=0, n_elements(runCodes)-1 do runIndexes[i]=(where(runCodes[i] eq allCodes))[0]
  endelse
  
  allYearCodes=self->getYears()
  selectedYearCodes=allYearCodes[runIndexes]
  yearIndexes=['-1']
  for i=0, n_elements(yearCodes)-1 do begin
    idxs=where(yearCodes[i] eq selectedYearCodes, count)
    if count gt 0 then yearIndexes=[yearIndexes, idxs]
  endfor
  if n_elements(yearIndexes) eq 1 then return, ['-1']
  NOTFOUND=0
  foundCodes=runCodes[yearIndexes[1:*]]
  ;print, (self->getFilenames())[yearIndexes[1:*]]
  return, foundCodes
  
END

FUNCTION PhysicalDataRun::filterByResolution, resolutionCodes, runCodes, NOTFOUND=NOTFOUND

  NOTFOUND=1
  allCodes=self->getCodes()
  if n_elements(runCodes) eq 0 then begin
    runCodes=allCodes
    runIndexes=indgen(n_elements(runCodes))
  endif else begin
    runIndexes=intarr(n_elements(runCodes))
    for i=0, n_elements(runCodes)-1 do runIndexes[i]=(where(runCodes[i] eq allCodes))[0]
  endelse
  
  allResolutionCodes=self->getResolutions()
  selectedResolutionCodes=allResolutionCodes[runIndexes]
  resolutionIndexes=['-1']
  for i=0, n_elements(resolutionCodes)-1 do begin
    idxs=where(resolutionCodes[i] eq selectedResolutionCodes, count)
    if count gt 0 then resolutionIndexes=[resolutionIndexes, idxs]
  endfor
  if n_elements(resolutionIndexes) eq 1 then return, ['-1']
  NOTFOUND=0
  foundCodes=runCodes[resolutionIndexes[1:*]]
  ;print, (self->getFilenames())[resolutionIndexes[1:*]]
  return, foundCodes
  
END

FUNCTION PhysicalDataRun::filterByMonth, monthCodes, runCodes, NOTFOUND=NOTFOUND

  NOTFOUND=1
  allCodes=self->getCodes()
  if n_elements(runCodes) eq 0 then begin
    runCodes=allCodes
    runIndexes=indgen(n_elements(runCodes))
  endif else begin
    runIndexes=intarr(n_elements(runCodes))
    for i=0, n_elements(runCodes)-1 do runIndexes[i]=(where(runCodes[i] eq allCodes))[0]
  endelse
  
  allMonthCodes=self->getMonths()
  selectedMonthCodes=allMonthCodes[runIndexes]
  monthIndexes=['-1']
  for i=0, n_elements(monthCodes)-1 do begin
    idxs=where(monthCodes[i] eq selectedMonthCodes, count)
    if count gt 0 then monthIndexes=[monthIndexes, idxs]
  endfor
  if n_elements(monthIndexes) eq 1 then return, ['-1']
  NOTFOUND=0
  foundCodes=runCodes[monthIndexes[1:*]]
  ;print, (self->getFilenames())[monthIndexes[1:*]]
  return, foundCodes
  
END

FUNCTION PhysicalDataRun::buildRecordFromElement, index, structElement

  code=strcompress(index, /REMOVE) & code=structElement.code
  displayName=structElement.filename
  type=structElement.type
  roi=structElement.roi
  resolution=structElement.resolution
  extraInfo=structElement.extraInfo
  unknown=structElement.unknown
  year=structElement.year
  month=structElement.month
  execDate=structElement.execDate
  fileName=structElement.filename
  descr='Descr of: '+structElement.filename
  record=[code, displayName, type, roi, resolution, extraInfo, unknown, year, month, execDate, fileName, descr]
  return, record
  
END

;FUNCTION PhysicalDataRun::buildListStruct, fieldsInfo, NOFILL=NOFILL
;
;  thisStruct=self->getListStructDef()
;  if ~keyword_set(NOFILL) gt 0 then begin
;    thisStruct.code=fieldsInfo[0]
;    thisStruct.displayName=fieldsInfo[1]
;    thisStruct.type=fieldsInfo[2]
;    thisStruct.roi=fieldsInfo[3]
;    thisStruct.resolution=fieldsInfo[4]
;    thisStruct.extrainfo=fieldsInfo[5]
;    thisStruct.unknown=fieldsInfo[6]
;    thisStruct.year=fieldsInfo[7]
;    thisStruct.month=fieldsInfo[8]
;    thisStruct.execdate=fieldsInfo[9]
;    thisStruct.filename=fieldsInfo[10]
;    thisStruct.description=fieldsInfo[11]
;  endif
;  return, thisStruct
;  
;END

FUNCTION PhysicalDataRun::getListStructDef

  struct = {  code:'',$
    displayName:'', $
    type:'', $
    roi:'', $
    resolution:'', $
    extrainfo:'', $
    unknown:'', $
    year:'', $
    month:'', $
    execdate:'', $
    filename:'', $
    description: '' $
    }
    
  return, struct
  
END

FUNCTION PhysicalDataRun::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.displayName=fieldsInfo[1]
    thisStruct.type=fieldsInfo[2]
    thisStruct.roi=fieldsInfo[3]
    thisStruct.resolution=fieldsInfo[4]
    thisStruct.extrainfo=fieldsInfo[5]
    thisStruct.unknown=fieldsInfo[6]
    thisStruct.year=fieldsInfo[7]
    thisStruct.month=fieldsInfo[8]
    thisStruct.execdate=fieldsInfo[9]
    thisStruct.filename=fieldsInfo[10]
    thisStruct.description=fieldsInfo[11]
  endif
  return, thisStruct
  
END

FUNCTION PhysicalDataRun::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

FUNCTION PhysicalDataRun::getNamesByCodes, codes

  allNames=self->getDisplayNames()
  allCodes=self->getCodes()
  howManyCodes=n_elements(codes)
  idxs=intarr(howManyCodes)
  for i=0, howManyCodes-1 do idxs[i]=(where(codes[i] eq allCodes))[0]
  return, allNames[idxs]
  
END

FUNCTION PhysicalDataRun::getMeasureUnitsByCodes, codes

  allMU=self->getMeasureUnits()
  allCodes=self->getCodes()
  howManyCodes=n_elements(codes)
  idxs=intarr(howManyCodes)
  for i=0, howManyCodes-1 do idxs[i]=(where(codes[i] eq allCodes))[0]
  return, allMU[idxs]
  
END

FUNCTION PhysicalDataRun::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

;FUNCTION PhysicalDataRun::getTypes
;
;  thisList=self->getList()
;  return, thisList[*].type
;  
;END

FUNCTION PhysicalDataRun::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION PhysicalDataRun::getDescriptions

  thisList=self->getList()
  return, thisList[*].description
  
END
FUNCTION PhysicalDataRun::getTypes

  thisList=self->getList()
  return, thisList[*].type
  
END
FUNCTION PhysicalDataRun::getRois

  thisList=self->getList()
  return, thisList[*].roi
  
END
FUNCTION PhysicalDataRun::getResolutions

  thisList=self->getList()
  return, thisList[*].resolution
  
END
FUNCTION PhysicalDataRun::getExtraInfos

  thisList=self->getList()
  return, thisList[*].extrainfo
  
END

FUNCTION PhysicalDataRun::getUnknowns

  thisList=self->getList()
  return, thisList[*].unknown
  
END

FUNCTION PhysicalDataRun::getYears

  thisList=self->getList()
  return, thisList[*].year
  
END

FUNCTION PhysicalDataRun::getMonths

  thisList=self->getList()
  return, thisList[*].month
  
END

FUNCTION PhysicalDataRun::getExecdates

  thisList=self->getList()
  return, thisList[*].execdate
  
END

FUNCTION PhysicalDataRun::getFilenames

  thisList=self->getList()
  return, thisList[*].filename
  
END

PRO PhysicalDataRun::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=*self.list
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** type:<', thisList[i].type,'>'
    print, '**** displayName:<', thisList[i].displayName,'>'
    print, '**** measureUnit:<', thisList[i].measureUnit,'>'
    print, '**** description:<', thisList[i].description,'>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

FUNCTION PhysicalDataRun::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO PhysicalDataRun::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO PhysicalDataRun__Define

  Struct = { PhysicalDataRun , $
    Inherits ConfigurableData $
    }
    
END