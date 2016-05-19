FUNCTION Resolution::buildFullFieldList, simpleList

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

FUNCTION  Resolution::getVersion

  return, '1.0'
  
END

FUNCTION Resolution::buildRecordFromElement, index, structElement

  code=structElement
  displayName=structElement
  descr='Descr of: '+structElement
  recordList=[code, displayName, descr]
  return, recordList
  
END

FUNCTION Resolution::getListStructDef

  struct = {  code:'',$
    displayName:'', $
    description: '' $
    }
    
  return, struct
  
END

FUNCTION Resolution::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.displayName=fieldsInfo[1]
    thisStruct.description=fieldsInfo[2]
  endif
  return, thisStruct
  
  
END

FUNCTION Resolution::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

FUNCTION Resolution::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION Resolution::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

FUNCTION Resolution::getDescriptions

  thisList=self->getList()
  return, thisList[*].description
  
END

PRO Resolution::streamPrint

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

FUNCTION Resolution::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO Resolution::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO Resolution__Define

  Struct = { Resolution , $
    Inherits ConfigurableData $
    }
    
END