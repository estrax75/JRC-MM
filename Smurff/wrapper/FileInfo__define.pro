FUNCTION FileInfo::buildRecordFromElement, index, structElement

  code=structElement
  displayName=structElement
  descr='Descr of: '+structElement
  record=[code, displayName, descr]
  return, record
  
END

FUNCTION FileInfo::getDisplayNames

  thisList=*self.list
  return, thisList[*].displayName
  
END

FUNCTION FileInfo::getCodes

  thisList=*self.list
  return, thisList[*].code
  
END

FUNCTION FileInfo::getDescriptions

  thisList=*self.list
  return, thisList[*].description
  
END

FUNCTION FileInfo::getListStructDef

  struct = { code:'',$
    displayName:'', $
    description: '' $
    }
    
  return, struct
  
END

FUNCTION FileInfo::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.displayName=fieldsInfo[1]
    thisStruct.description=fieldsInfo[2]
  endif
  return, thisStruct
  
  
END

FUNCTION FileInfo::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

PRO FileInfo::streamPrint

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

FUNCTION FileInfo::init, folderName, filter=filter

  ; fill list of files (optionally selected by filter)
  if keyword_set(filter) then self.filter=filter else self.filter='*.*'
  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO FileInfo::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO FileInfo__Define

  Struct = { FileInfo , $
    filter: '', $
    Inherits ConfigurableData $
    }
    
END