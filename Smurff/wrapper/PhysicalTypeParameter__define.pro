FUNCTION PhysicalDataRun::buildFullFieldList, simpleList

  baseStruct=self->getListStructDef()
  nElem=n_elements(simpleList)
  list=replicate(baseStruct, nElem)
  for i=0, nElem-1 do begin
    list[i].displayName=simpleList[i].filename
    list[i].type=simpleList[i].type
    list[i].roi=simpleList[i].roi
    list[i].resolution=simpleList[i].resolution
    list[i].extraInfo=simpleList[i].extraInfo
    list[i].unknown=simpleList[i].unknown
    list[i].year=simpleList[i].year
    list[i].month=simpleList[i].month
    list[i].execDate=simpleList[i].execDate
    list[i].fileName=simpleList[i].filename
    list[i].descr='Descr of: '+simpleList[i].filename
  endfor
  return, list
  
END

FUNCTION  PhysicalDataRun::getVersion

  return, '1.0'
  
END

FUNCTION PhysicalTypeParameter::buildRecordFromElement, index, structElement

  return, self->ConfigurableData::buildRecordFromElement(index, structElement)
  
END

FUNCTION PhysicalTypeParameter::getListStructDef

  struct = {  code:'',$
    typeCode:'', $
    displayName:'', $
    measureUnit:'', $
    description: '' $
    }
    
  return, struct
  
END

FUNCTION PhysicalTypeParameter::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.typeCode=fieldsInfo[1]
    thisStruct.displayName=fieldsInfo[2]
    thisStruct.measureUnit=fieldsInfo[3]
    thisStruct.description=fieldsInfo[4]
  endif
  return, thisStruct
  
  
END

FUNCTION PhysicalTypeParameter::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

FUNCTION PhysicalTypeParameter::getNamesByCodes, codes

  allNames=self->getDisplayNames()
  allCodes=self->getCodes()
  howManyCodes=n_elements(codes)
  idxs=intarr(howManyCodes)
  for i=0, howManyCodes-1 do idxs[i]=(where(codes[i] eq allCodes))[0]
  return, allNames[idxs]
  
END

FUNCTION PhysicalTypeParameter::getMeasureUnitsByCodes, codes

  allMU=self->getMeasureUnits()
  allCodes=self->getCodes()
  howManyCodes=n_elements(codes)
  idxs=intarr(howManyCodes)
  for i=0, howManyCodes-1 do idxs[i]=(where(codes[i] eq allCodes))[0]
  return, allMU[idxs]
  
END

FUNCTION PhysicalTypeParameter::getTypeCodes

  thisList=self->getList()
  return, thisList[*].typeCode
  
END

FUNCTION PhysicalTypeParameter::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION PhysicalTypeParameter::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

FUNCTION PhysicalTypeParameter::getDescriptions

  thisList=self->getList()
  return, thisList[*].description
  
END

FUNCTION PhysicalTypeParameter::getMeasureUnits

  thisList=self->getList()
  return, thisList[*].measureUnit
  
END

PRO PhysicalTypeParameter::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=*self.list
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** typeCode:<', thisList[i].typeCode,'>'
    print, '**** displayName:<', thisList[i].displayName,'>'
    print, '**** measureUnit:<', thisList[i].measureUnit,'>'
    print, '**** description:<', thisList[i].description,'>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

FUNCTION PhysicalTypeParameter::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO PhysicalTypeParameter::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO PhysicalTypeParameter__Define

  Struct = { PhysicalTypeParameter , $
    Inherits ConfigurableData $
    }
    
END