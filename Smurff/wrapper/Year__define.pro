FUNCTION Year::getElementByCode, code

 allCodes=self->getCodes()
 idx=(where(code eq allCodes))[0]
 list=self->getList()
 return, list[idx]
 
END

FUNCTION Year::buildFullFieldList, simpleList

  baseStruct=self->getListStructDef()
  nElem=n_elements(simpleList)
  list=replicate(baseStruct, nElem)
  for i=0, nElem-1 do begin
    list[i].code=simpleList[i]
    list[i].modelCode=simpleList[i]
    list[i].physicalCode=simpleList[i]
    list[i].satellitecode=simpleList[i]
    list[i].daaccode=simpleList[i]
    list[i].globOldSeaWiFSCode=simpleList[i]
    list[i].globSeaWiFSCode=simpleList[i]
    list[i].globMerisCode=simpleList[i]
    list[i].globModisACode=simpleList[i]
    list[i].globViirsCode=simpleList[i]
    ;list[i].globcode=simpleList[i]
    list[i].displayName=simpleList[i]
    list[i].description='Descr of: '+simpleList[i]
  endfor
  return, list
  
END

;FUNCTION Year::buildFullFieldList, simpleList
;
;  baseStruct=self->getListStructDef()
;  nElem=n_elements(simpleList)
;  list=replicate(baseStruct, nElem)
;  for i=0, nElem-1 do begin
;    list[i].code=simpleList[i]
;    list[i].modelCode=simpleList[i]
;    list[i].physicalCode=simpleList[i]
;    list[i].satellitecode=simpleList[i]
;    list[i].daaccode=simpleList[i]
;    list[i].globcode=simpleList[i]
;    list[i].displayName=simpleList[i]
;    list[i].description='Descr of: '+simpleList[i]
;  endfor
;  return, list
;  
;END

FUNCTION  Year::getVersion

  return, '1.0'
  
END

;globOldSeaWiFSCode:'', $
;  globSeaWiFSCode:'', $
;  globMerisCode:'', $
;  globModisACode:'', $
;  globViirsCode:'', $

FUNCTION Year::buildRecordFromElement, index, structElement

  code=structElement
  displayName=structElement
  modelCode=''
  physicalCode=''
  satelliteCode=''
  daacCode=''
  globOldSeaWiFSCode=''
  globSeaWiFSCode=''
  globMerisCode=''
  globModisACode=''
  globViirsCode=''
  ;globCode=''
  description='Descr of: '+structElement
  ;record=[code, displayName, physicalCode, satelliteCode, daacCode, globCode, descr]
  record=[code, displayName, physicalCode, satelliteCode, daacCode, globOldSeaWiFSCode, globSeaWiFSCode, globMerisCode, globModisACode, globViirsCode, descr]

  return, record
  
END

;FUNCTION Year::buildRecordFromElement, index, structElement
;
;  code=structElement
;  displayName=structElement
;  modelCode=''
;  physicalCode=''
;  satelliteCode=''
;  daacCode=''
;  globCode=''
;  description='Descr of: '+structElement
;  record=[code, displayName, physicalCode, satelliteCode, daacCode, globCode, descr]
;  return, record
;  
;END

FUNCTION Year::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION Year::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

FUNCTION Year::getDescriptions

  thisList=self->getList()
  return, thisList[*].description
  
END

FUNCTION Year::getModelCodes

  thisList=self->getList()
  return, thisList[*].modelCode
  
END

FUNCTION Year::getPhysicalCodes

  thisList=self->getList()
  return, thisList[*].physicalCode
  
END

FUNCTION Year::getSatelliteCodes

  thisList=self->getList()
  return, thisList[*].satelliteCode
  
END

FUNCTION Year::getDaacCodes

  thisList=self->getList()
  return, thisList[*].daacCode
  
END

;FUNCTION Year::getGlobCodes
;
;  thisList=self->getList()
;  return, thisList[*].globCode
;  
;END

FUNCTION Year::globViirsCodes

  thisList=self->getList()
  return, thisList[*].globViirsCode
  
END

FUNCTION Year::globModisACodes

  thisList=self->getList()
  return, thisList[*].globModisACode
  
END

FUNCTION Year::globMerisCodes

  thisList=self->getList()
  return, thisList[*].globMerisCode
  
END

FUNCTION Year::globSeaWiFSCodes

  thisList=self->getList()
  return, thisList[*].globSeaWiFSCode
  
END

FUNCTION Year::globOldSeaWiFSCodes

  thisList=self->getList()
  return, thisList[*].globOldSeaWiFSCode
  
END

;globOldSeaWiFSCode:'', $
;  globSeaWiFSCode:'', $
;  globMerisCode:'', $
;  globModisACode:'', $
;  globViirsCode:'', $

PRO Year::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=self->getList()
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** displayName:<', thisList[i].displayName,'>'
    print, '**** modelCode:<', thisList[i].modelCode,'>'
    print, '**** physicalCode:<', thisList[i].physicalCode,'>'
    print, '**** satelliteCode:<', thisList[i].satelliteCode,'>'
    print, '**** daacCode:<', thisList[i].daacCode,'>'
    ;print, '**** globCode:<', thisList[i].globCode,'>'
    print, '**** globOldSeaWiFSCode:<', thisList[i].globOldSeaWiFSCode,'>'
    print, '**** globSeaWiFSCode:<', thisList[i].globSeaWiFSCode,'>'
    print, '**** globMerisCode:<', thisList[i].globMerisCode,'>'
    print, '**** globModisACode:<', thisList[i].globModisACode,'>'
    print, '**** globViirsCode:<', thisList[i].globViirsCode,'>'
    print, '**** description:<', thisList[i].description,'>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

;PRO Year::streamPrint
;
;  print, '***********************'
;  print, '**Start of<',OBJ_CLASS(self),'>**'
;  
;  print, '**** fileName:<', self.fileName,'>'
;  thisList=self->getList()
;  for i=0, n_elements(thisList)-1 do begin
;    print, '**element n.<', strcompress(i, /REMOVE),'>'
;    print, '**** code:<', thisList[i].code,'>'
;    print, '**** displayName:<', thisList[i].displayName,'>'
;    print, '**** modelCode:<', thisList[i].modelCode,'>'
;    print, '**** physicalCode:<', thisList[i].physicalCode,'>'
;    print, '**** satelliteCode:<', thisList[i].satelliteCode,'>'
;    print, '**** daacCode:<', thisList[i].daacCode,'>'
;    print, '**** globCode:<', thisList[i].globCode,'>'
;    print, '**** description:<', thisList[i].description,'>'
;    print, '**'
;  endfor
;  
;  print, '***********************'
;  print, '**End of:<',OBJ_CLASS(self),'>**'
;  
;END

FUNCTION Year::getListStructDef

  struct = { code:'',$
    displayName:'', $
    modelCode:'', $
    physicalCode:'', $
    satelliteCode:'', $
    daacCode:'', $
    globOldSeaWiFSCode:'', $
    globSeaWiFSCode:'', $
    globMerisCode:'', $
    globModisACode:'', $
    globViirsCode:'', $
    ;globCode:'', $
    description: '' $
    }
    
  return, struct
  
END

;FUNCTION Year::getListStructDef
;
;  struct = { code:'',$
;    displayName:'', $
;    modelCode:'', $
;    physicalCode:'', $
;    satelliteCode:'', $
;    daacCode:'', $
;    globCode:'', $
;    description: '' $
;  }
;  
;  return, struct
;  
;END

;globOldSeaWiFSCode:'', $
;  globSeaWiFSCode:'', $
;  globMerisCode:'', $
;  globModisACode:'', $
;  globViirsCode:'', $

FUNCTION Year::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.displayName=fieldsInfo[1]
    thisStruct.modelCode=fieldsInfo[2]
    thisStruct.physicalCode=fieldsInfo[3]
    thisStruct.satelliteCode=fieldsInfo[4]
    thisStruct.daacCode=fieldsInfo[5]
    ;thisStruct.globCode=fieldsInfo[6]
    thisStruct.globOldSeaWiFSCode=fieldsInfo[6]
    thisStruct.globSeaWiFSCode=fieldsInfo[7]
    thisStruct.globMerisCode=fieldsInfo[8]
    thisStruct.globModisACode=fieldsInfo[9]
    thisStruct.globViirsCode=fieldsInfo[10]
    thisStruct.description=fieldsInfo[11]
  endif
  return, thisStruct
  
END

;FUNCTION Year::buildListStruct, fieldsInfo, NOFILL=NOFILL
;
;  thisStruct=self->getListStructDef()
;  if ~keyword_set(NOFILL) gt 0 then begin
;    thisStruct.code=fieldsInfo[0]
;    thisStruct.displayName=fieldsInfo[1]
;    thisStruct.modelCode=fieldsInfo[2]
;    thisStruct.physicalCode=fieldsInfo[3]
;    thisStruct.satelliteCode=fieldsInfo[4]
;    thisStruct.daacCode=fieldsInfo[5]
;    thisStruct.globCode=fieldsInfo[6]
;    thisStruct.description=fieldsInfo[7]
;  endif
;  return, thisStruct
;  
;END

FUNCTION Year::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

FUNCTION Year::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO Year::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO Year__Define

  Struct = { Year , $
    Inherits ConfigurableData $
    }
    
END