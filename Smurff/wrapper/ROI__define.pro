FUNCTION ROI::getNumberOfElements

  thisList=self->getList()
  return, n_elements(thisList)
  
END

FUNCTION ROI::getInfoByCode, code

  cmpList=self->getCodes()
  idx=where(code[0] eq cmpList, count)
  thisList=self->getList()
  if count eq 1 then return, thisList[idx[0]] else return , {void:1}
  
END

FUNCTION ROI::getIndexesOfCodes, codes, runSelected, NOSELECTION=NOSELECTION

  if n_elements(runSelected) ne 1 then allCodes=self->getCodes() else allCodes=self->getCodesByRun(runSelected, NOSELECTION=NOSELECTION)
  if ~keyword_set(NOSELECTION) then begin
    nElem=n_elements(codes)
    idxs=intarr(nElem)
    for i=0, nElem-1 do idxs[i]=(where(codes[i] eq allCodes))[0]
    return, idxs
  endif
  
END

FUNCTION ROI::buildFullFieldList, simpleList

  baseStruct=self->getListStructDef()
  nElem=n_elements(simpleList)
  list=replicate(baseStruct, nElem)
  for i=0, nElem-1 do begin
    list[i].code=simpleList[i]
    list[i].archiveCode=simpleList[i]
    list[i].rangeTypeCode=0
    list[i].displayName=simpleList[i]
    list[i].runList='N/A'
    list[i].colorDefinition='N/A'
    
    list[i].mapProjection='N/A'
    list[i].refRoiCode='N/A'
    list[i].overlapPriority='N/A'
    list[i].nbRows='N/A'
    list[i].nbLines='N/A'
    list[i].factorList='N/A'
    list[i].centerLon='N/A'
    list[i].centerLat='N/A'
    list[i].lonMin='N/A'
    list[i].latMin='N/A'
    list[i].lonMax='N/A'
    list[i].latMax='N/A'
    list[i].iMonth='N/A'
    list[i].i8Day='N/A'
    list[i].iDay='N/A'
    list[i].iProd='N/A'
    
    list[i].prodMO0='N/A'
    list[i].prod8D0='N/A'
    list[i].prodDAY0='N/A'
    list[i].prodMO1='N/A'
    list[i].prod8D1='N/A'
    list[i].prodDAY1='N/A'
    list[i].prodMO2='N/A'
    list[i].prod8D2='N/A'
    list[i].prodDAY2='N/A'
    list[i].prodMO3='N/A'
    list[i].prod8D3='N/A'
    list[i].prodDAY3='N/A'
    list[i].prodMOp='N/A'
    list[i].prod8Dp='N/A'
    list[i].prodDAYp='N/A'
    
    list[i].description='Descr of: '+simpleList[i]
  endfor
  return, list
  
END

FUNCTION  ROI::getVersion

  return, '1.0'
  
END

FUNCTION ROI::buildRecordFromElement, index, structElement

  code=structElement
  archiveCode=structElement
  rangeTypeCode=0
  displayName=structElement
  runList='N/A'
  
  colorDefinition='N/A'
  mapProjection='N/A'
  refRoiCode='N/A'
  overlapPriority='N/A'
  nbRows='N/A'
  nbLines='N/A'
  factorList='N/A'
  
  centerLon='N/A'
  centerLat='N/A'
  lonMin='N/A'
  latMin='N/A'
  lonMax='N/A'
  latMax='N/A'
  
  iMonth='N/A'
  i8Day='N/A'
  iDay='N/A'
  iProd='N/A'
  
  prodMO0='N/A'
  prod8D0='N/A'
  prodDAY0='N/A'
  prodMO1='N/A'
  prod8D1='N/A'
  prodDAY1='N/A'
  prodMO2='N/A'
  prod8D2='N/A'
  prodDAY2='N/A'
  prodMO3='N/A'
  prod8D3='N/A'
  prodDAY3='N/A'
  prodMOp='N/A'
  prod8Dp='N/A'
  prodDAYp='N/A'
  
  description='Descr of: '+structElement
  
  recordList=[code, archiveCode, displayName, runList, colorDefinition, mapProjection, refRoiCode, overlapPriority, nbRows, nbLines, factorList, centerLon, centerLat, $
    lonMin, latMin, lonMax, latMax, iMonth, i8Day, iDay, iProd, $
    prodMO0,prod8D0,prodDAY0,prodMO1,prod8D1,prodDAY1,prodMO2,prod8D2,prodDAY2,prodMO3,prod8D3,prodDAY3,prodMOp,prod8Dp,prodDAYp,$
    description]
    
  return, recordList
  
END

FUNCTION ROI::getListStructDef

  struct = {  code: '', $
    archiveCode: '', $
    rangeTypeCode: '', $
    displayName: '', $
    runList: '', $
    colorDefinition: '', $
    mapProjection: '', $
    refRoiCode: '', $
    overlapPriority: '', $
    nbRows: '', $
    nbLines: '', $
    factorList: '', $
    centerLon: '', $
    centerLat: '', $
    lonMin: '', $
    latMin: '', $
    lonMax: '', $
    latMax: '', $
    iMonth: '', $
    i8Day: '', $
    iDay: '', $
    iProd: '', $
    prodMO0: '', $
    prod8D0: '', $
    prodDAY0: '', $
    prodMO1: '', $
    prod8D1: '', $
    prodDAY1: '', $
    prodMO2: '', $
    prod8D2: '', $
    prodDAY2: '', $
    prodMO3: '', $
    prod8D3: '', $
    prodDAY3: '', $
    prodMOp: '', $
    prod8Dp: '', $
    prodDAYp: '', $
    description: '' $
    }
    
  return, struct
  
END

FUNCTION ROI::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    tags=tag_names(thisStruct)
    for i=0, n_elements(tags)-1 do thisStruct.(i)=fieldsInfo[1] 
  endif
  return, thisStruct
  
END

FUNCTION ROI::getRefRoiCodesByArchiveCodes, codes

  nElem=n_elements(codes)
  refRoiCodes=self->getRefRoiCodes()
  list=make_array(nElem, TYPE=size(refRoiCodes, /TYPE))
  cmpList=self->getArchiveCodes()
  for i=0, nElem-1 do begin
    idx=(where(cmpList eq codes[i]))[0]
    list[i]=refRoiCodes[idx]
  endfor
  return, list

END

FUNCTION ROI::getOverlapPriorityByArchiveCodes, codes

  nElem=n_elements(codes)
  OverlapPrioritys=self->getOverlapPrioritys()
  list=make_array(nElem, TYPE=size(OverlapPrioritys, /TYPE))
  cmpList=self->getAllArchiveCodes()
  for i=0, nElem-1 do begin
    idx=(where(cmpList eq codes[i]))[0]
    list[i]=OverlapPrioritys[idx]
  endfor
  return, list
  
END

FUNCTION ROI::getExtractInfoByCodes, codes

  nElem=n_elements(codes)
  extractInfo=getExtractInfoStruct()
  nElem=n_elements(codes)
  extractInfos=replicate(extractInfo, nElem)
  for i=0, nElem-1 do begin
    info=self->getInfoByCode(codes[i])
    extractInfos[i].lat=info.centerLat & extractInfos[i].lon=info.centerLon 
    dirName=(strsplit(info.archiveCode, '_', /EXTRACT))[0]
    extractInfos[i].ext=dirName
    extractInfos[i].N_side=long(info.nbrows) > long(info.nblines)
  endfor
  return, extractInfos
  
END

FUNCTION ROI::getGeoInfoByCodes, codes

  nElem=n_elements(codes)
  geoStruct={centerLat: 0., centerLon: 0., latMin: 0., latMax: 0., lonMin: 0., lonMax: 0., latPixExtension:0l, lonPixExtension:0l}
  nElem=n_elements(codes)
  geoStructs=replicate(geoStruct, nElem)
  for i=0, nElem-1 do begin
    info=self->getInfoByCode(codes)
    geoStructs[i].centerLat=info.centerLat & geoStructs[i].centerLon=info.centerLon
    geoStructs[i].latMin=info.latMin & geoStructs[i].latMax=info.latMax
    geoStructs[i].lonMin=info.lonMin & geoStructs[i].lonMax=info.lonMax
    geoStructs[i].latPixExtension=info.nbrows & geoStructs[i].lonPixExtension=info.nblines
  endfor
  return, geoStructs
  
END

FUNCTION ROI::getOverlapPriorityByCodes, codes

  nElem=n_elements(codes)
  OverlapPrioritys=self->getOverlapPrioritys()
  list=make_array(nElem, TYPE=size(OverlapPrioritys, /TYPE))
  cmpList=self->getAllCodes()
  if codes[0] eq 'null' and n_elements(codes) eq 1 then return, list[0]
  for i=0, nElem-1 do begin
    idx=(where(cmpList eq codes[i]))[0]
    list[i]=OverlapPrioritys[idx]
  endfor
  return, list
  
END

FUNCTION ROI::getRefRoiCodesByCodes, codes

  nElem=n_elements(codes)
  refRoiCodes=self->getRefRoiCodes()
  list=make_array(nElem, TYPE=size(refRoiCodes, /TYPE))
  cmpList=self->getAllCodes()
  for i=0, nElem-1 do begin
    idx=(where(cmpList eq codes[i]))[0]
    list[i]=refRoiCodes[idx]
  endfor
  return, list
  
END

FUNCTION ROI::getColorDefinitionByCodes, codes

  nElem=n_elements(codes)
  colorDefinitions=self->getColorDefinitions()
  list=make_array(nElem, TYPE=size(colorDefinitions, /TYPE))
  cmpList=self->getAllCodes()
  for i=0, nElem-1 do begin
    idx=(where(cmpList eq codes[i]))[0]
    list[i]=colorDefinitions[idx]
  endfor
  return, list
  
END

FUNCTION ROI::getDisplayNamesByCodes, codes


  nElem=n_elements(codes)
  displayNames=self->getDisplayNames()
  list=make_array(nElem, TYPE=size(displayNames, /TYPE))


  cmpList=self->getAllCodes()
  for i=0, nElem-1 do begin
    idx=(where(cmpList eq codes[i]))[0]
    list[i]=displayNames[idx]

  endfor
  return, list
  
END

FUNCTION ROI::getArchiveRoiCodesSelectedByCodes, codes

  nElem=n_elements(codes)
  archiveCodes=self->getArchiveCodes()
  list=make_array(nElem, TYPE=size(archiveCodes, /TYPE))
  cmpList=self->getCodes()
  for i=0, nElem-1 do begin
    idx=(where(cmpList eq codes[i]))[0]
    list[i]=archiveCodes[idx]
  endfor
  return, list
  
END

FUNCTION ROI::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

FUNCTION ROI::getCodesByRun, runCodeSelected, NOSELECTION=NOSELECTION

  thisList=self->getList()
  codes=thisList[*].code
  runCodes=thisList[*].runList
  resCodes=['']
  NOSELECTION=1
  for i=0, n_elements(runCodes)-1 do begin
    if strupcase(runCodes[i]) ne 'ALL' then begin
      runs=strsplit(runCodes[i], ';', /EXTRACT)
      idx=where(strtrim(runCodeSelected, 1) eq runs, count)
      if count ne 0 then resCodes=[resCodes, codes[i]]
    endif else begin
      resCodes=[resCodes, codes[i]]
    endelse
  endfor
  if n_elements(resCodes) eq 1 then return, [-1]
  NOSELECTION=0
  return, resCodes[1:*]
  
END

FUNCTION ROI::getAllCodes

  return, self->getCodes()
  
END

FUNCTION ROI::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

FUNCTION ROI::getArchiveCodes

  thisList=self->getList()
  return, thisList[*].archiveCode
  
END

FUNCTION ROI::getRangeTypeCodes

  thisList=self->getList()
  return, thisList[*].rangeTypeCode
  
END

FUNCTION ROI::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName

END

FUNCTION ROI::getRunList

  thisList=self->getList()
  return, thisList[*].runList
  
END

FUNCTION ROI::getMapProjections

  thisList=self->getList()
  return, thisList[*].mapProjection
  
END

FUNCTION ROI::getColorDefinitions

  thisList=self->getList()
  return, thisList[*].colorDefinition
  
END

FUNCTION ROI::getRefRoiCodes

  thisList=self->getList()
  return, thisList[*].refRoiCode
  
END

FUNCTION ROI::getOverlapPrioritys ;syntax error, I know...

  thisList=self->getList()
  return, thisList[*].overlapPriority
  
END

FUNCTION ROI::getNbRowss

  thisList=self->getList()
  return, thisList[*].nbRows
  
END

FUNCTION ROI::getNbLiness

  thisList=self->getList()
  return, thisList[*].nbLines
  
END

FUNCTION ROI::getFactorLists

  thisList=self->getList()
  return, thisList[*].factorList
  
END

FUNCTION ROI::getCenterLons

  thisList=self->getList()
  return, thisList[*].centerLon
  
END

FUNCTION ROI::getCenterLats

  thisList=self->getList()
  return, thisList[*].centerLat
  
END

FUNCTION ROI::getLonMins

  thisList=self->getList()
  return, thisList[*].lonMin
  
END

FUNCTION ROI::getLatMins

  thisList=self->getList()
  return, thisList[*].latMin
  
END

FUNCTION ROI::getLonMaxs

  thisList=self->getList()
  return, thisList[*].lonMax
  
END

FUNCTION ROI::getLatMaxs

  thisList=self->getList()
  return, thisList[*].latMax
  
END

FUNCTION ROI::getIMonths

  thisList=self->getList()
  return, thisList[*].iMonth
  
END

FUNCTION ROI::getIDays

  thisList=self->getList()
  return, thisList[*].iDay
  
END

FUNCTION ROI::getIProds

  thisList=self->getList()
  return, thisList[*].iProd
  
END
FUNCTION ROI::getProdMO0s

  thisList=self->getList()
  return, thisList[*].prodMO0
  
END
FUNCTION ROI::getProd8D0s

  thisList=self->getList()
  return, thisList[*].prod8D0
  
END
FUNCTION ROI::getProdDAY0s

  thisList=self->getList()
  return, thisList[*].prodDAY0
  
END
FUNCTION ROI::getProdMO1s

  thisList=self->getList()
  return, thisList[*].prodMO1
  
END
FUNCTION ROI::getProd8D1s

  thisList=self->getList()
  return, thisList[*].prod8D1
  
END
FUNCTION ROI::getProdDAY1s

  thisList=self->getList()
  return, thisList[*].prodDAY1
  
END
FUNCTION ROI::getProdMO2s

  thisList=self->getList()
  return, thisList[*].prodMO2
  
END
FUNCTION ROI::getProd8D2s

  thisList=self->getList()
  return, thisList[*].prod8D2
  
END

FUNCTION ROI::getProdDAY2s

  thisList=self->getList()
  return, thisList[*].prodDAY2
  
END
FUNCTION ROI::getProdMO3s

  thisList=self->getList()
  return, thisList[*].prodMO3
  
END
FUNCTION ROI::getProd8D3s

  thisList=self->getList()
  return, thisList[*].prod8D3
  
END
FUNCTION ROI::getProdDAY3s

  thisList=self->getList()
  return, thisList[*].prodDAY3
  
END

FUNCTION ROI::getProdMOps

  thisList=self->getList()
  return, thisList[*].prodMOp
  
END

FUNCTION ROI::getProd8Dps

  thisList=self->getList()
  return, thisList[*].prod8Dp
  
END

FUNCTION ROI::getProdDAYps

  thisList=self->getList()
  return, thisList[*].prodDAYp
  
END

FUNCTION ROI::getDescriptions

  thisList=self->getList()
  return, thisList[*].description
  
END

PRO ROI::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=*self.list
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** archivecode:<', thisList[i].archivecode,'>'
    print, '**** rangeTypeCode:<', thisList[i].rangeTypeCode,'>'
    print, '**** displayName:<', thisList[i].displayName,'>'
    print, '**** runList:<', thisList[i].runList,'>'
    print, '**** colorDefinition:<', thisList[i].colorDefinition,'>'
    print, '**** mapProjection:<', thisList[i].mapProjection,'>'
    print, '**** refRoiCode:<', thisList[i].refRoiCode,'>'
    print, '**** overlapPriority:<', thisList[i].overlapPriority,'>'
    print, '**** nbRows:<', thisList[i].nbRows,'>'
    print, '**** nbLines:<', thisList[i].nbLines,'>'
    print, '**** factorList:<', thisList[i].factorList,'>'
    print, '**** centerLon:<', thisList[i].centerLon,'>'
    print, '**** centerLat:<', thisList[i].centerLat,'>'
    print, '**** lonMin:<', thisList[i].lonMin,'>'
    print, '**** latMin:<', thisList[i].latMin,'>'
    print, '**** lonMax:<', thisList[i].lonMax,'>'
    print, '**** latMax:<', thisList[i].latMax,'>'
    print, '**** iMonth:<', thisList[i].iMonth,'>'
    print, '**** i8Day:<', thisList[i].i8Day,'>'
    print, '**** iDay:<', thisList[i].iDay,'>'
    print, '**** iProd:<', thisList[i].iProd,'>'
    print, '**** prodMO0:<', thisList[i].prodMO0,'>'
    print, '**** prod8D0:<', thisList[i].prod8D0,'>'
    print, '**** prodDAY0:<', thisList[i].prodDAY0,'>'
    print, '**** prodMO1:<', thisList[i].prodMO1,'>'
    print, '**** prod8D1:<', thisList[i].prod8D1,'>'
    print, '**** prodDAY1:<', thisList[i].prodDAY1,'>'
    print, '**** prodMO2:<', thisList[i].prodMO2,'>'
    print, '**** prod8D2:<', thisList[i].prod8D2,'>'
    print, '**** prodDAY2:<', thisList[i].prodDAY2,'>'
    print, '**** prodMO3:<', thisList[i].prodMO3,'>'
    print, '**** prod8D3:<', thisList[i].prod8D3,'>'
    print, '**** prodDAY3:<', thisList[i].prodDAY3,'>'
    print, '**** prodMOp:<', thisList[i].prodMOp,'>'
    print, '**** prod8Dp:<', thisList[i].prod8Dp,'>'
    print, '**** prodDAYp:<', thisList[i].prodDAYp,'>'
    print, '**** description:<', thisList[i].description,'>'
    print, '**'
  endfor
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

FUNCTION ROI::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO ROI::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO ROI__Define

  Struct = { ROI , $
    Inherits ConfigurableData $
    }
    
END