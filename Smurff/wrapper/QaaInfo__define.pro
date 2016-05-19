FUNCTION QaaInfo::getQaa, qaaCode

  qaa=obj_new('Qaa')
  idx=self->getIndexByCode(qaaCode)
  fNames=self->getFullFileNames()
  res = qaa->fillDataFromXMLFile(fNames[idx])
  return, qaa
  
END

PRO QaaInfo::buildForApp

  classNum=8
  for i=0, classNum-1 do begin
    qaa=obj_new('Qaa')
    qaa->setDisplayName, 'Qaa Class '+strtrim(i+1,1)
    code = string(format='(I03', i)
    qaa->setCode, code
    qaa->setDescription, 'qaa class '+code+'data file'
    qaa->setGList, [-0.08945, 0.1247]
    qaa->setA440List, [0.2, -1.4, -1.8]
    qaa->setA555List, [0.2, -0.01]
    qaa->setYList, [2.2, 1.0, -1.2, -0.9]
    qaa->setZetaList, [0.71, 0.06, 0.8]
    qaa->setS, 0.015
    qaaFileName='E:\data\mariomi\application\oxyrisk\resource\class'+code+'_qaa.xml'
    modis->xmlWriteStructList, qaaFileName
    obj_destroy, modis
  endfor
  
END

PRO QaaInfo::testForApp

  classNum=8
  for i=0, classNum-1 do begin
    qaa=obj_new('qaa')
    code = string(format='(I03', i)
    qaaFileName='E:\data\mariomi\application\oxyrisk\resource\class'+code+'_qaa.xml'
    qaa->parse, qaaFileName
    qaaFileName->streamPrint
    obj_destroy, qaa
  endfor
  
END

FUNCTION QaaInfo::getFullElement, code, INDEX=INDEX

  element=self->getListStructDef()
  if n_elements(code) ne 1 then elementIndex=INDEX else elementIndex=self->getIndexByCode(code)
  thisList=self->getList()
  element.code=thisList.code[elementIndex]
  element.displayname=thisList.displayname[elementIndex]
  element.fullfilename=thisList.fullfilename[elementIndex]
  
END

FUNCTION QaaInfo::getIndexByCode, code

  codes=self->getCodes()
  idx=(where(code eq codes))[0]
  return, idx
  
END

FUNCTION QaaInfo::buildFullFieldList, simpleList

  baseStruct=self->getListStructDef()
  nElem=n_elements(simpleList)
  list=replicate(baseStruct, nElem)
  for i=0, nElem-1 do begin
    list[i].code=simpleList[i]
    list[i].displayname=simpleList[i]
    list[i].fullfilename=simpleList[i]
  endfor
  return, list
  
END

FUNCTION  QaaInfo::getVersion

  return, '1.0'
  
END

FUNCTION QaaInfo::buildRecordFromElement, index, structElement

  code=structElement
  displayName=structElement
  fullfilename=''
  record=[code, displayName, fullfilename]
  return, record
  
END

FUNCTION QaaInfo::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION QaaInfo::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

FUNCTION QaaInfo::getFullFileNames

  thisList=self->getList()
  return, thisList[*].fullfilename
  
END

PRO QaaInfo::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=self->getList()
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** displayName:<', thisList[i].displayName,'>'
    print, '**** fullfilename:<', thisList[i].fullfilename,'>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

FUNCTION QaaInfo::getListStructDef

  struct = { code:'',$
    displayName:'', $
    fullfilename: '' $
    }
    
  return, struct
  
END

FUNCTION QaaInfo::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.displayName=fieldsInfo[1]
    thisStruct.fullfilename=fieldsInfo[2]
  endif
  return, thisStruct
  
END

FUNCTION QaaInfo::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

FUNCTION QaaInfo::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO QaaInfo::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO QaaInfo__Define

  Struct = { QaaInfo , $
    Inherits ConfigurableData $
    }
    
END