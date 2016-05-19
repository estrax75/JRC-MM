FUNCTION Month::buildFullFieldList, simpleList

  baseStruct=self->getListStructDef()
  nElem=n_elements(simpleList)
  list=replicate(baseStruct, nElem)
  for i=0, nElem-1 do begin
    list[i].code=simpleList[i]
    list[i].displayName=simpleList[i]
    list[i].description='Descr of: '+simpleList[i]
  endfor
  return, list
  
END

FUNCTION Month::getVersion

  return, '1.0'
  
END

PRO Month::fillDataFromFile, fileName

  self->ConfigurableData::fillDataFromFile, fileName
  
END

FUNCTION Month::buildRecordFromElement, index, structElement

  code=structElement
  displayName=structElement
  descr='Descr of: '+structElement
  record=[code, displayName, descr]
  return, record
  
END

FUNCTION Month::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

FUNCTION Month::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION Month::getDescriptions

  thisList=self->getList()
  return, thisList[*].description
  
END

FUNCTION Month::getListStructDef

  struct = { code:'',$
    displayName:'', $
    description: '' $
    }
    
  return, struct
  
END

FUNCTION Month::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.displayName=fieldsInfo[1]
    thisStruct.description=fieldsInfo[2]
  endif
  return, thisStruct
  
  
END

FUNCTION Month::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

PRO Month::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=*self.list
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** displayName:<', thisList[i].displayName,'>'
    print, '**** description:<', thisList[i].description,'>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

FUNCTION Month::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO Month::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO Month__Define

  Struct = { Month , $
    Inherits ConfigurableData $
    }
    
END