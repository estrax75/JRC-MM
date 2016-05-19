FUNCTION ModelInfo::getModel, modelCode

  model=obj_new('Model')
  idx=self->getIndexByCode(modelCode)
  fNames=self->getFullFileNames()
  res = model->fillDataFromXMLFile(fNames[idx])
  return, model  

END

PRO ModelInfo::buildForApp

  psa=obj_new('model')
  psa->setArchiveroot, 'E:\data\mariomi\application\oxyrisk\data\input\model_psa'
  psa->setDisplayname, 'PSAModel'
  psa->setCode, 'psa'
  psa->setFileType, 'PSA'
  psa->setParametersList, ['depmx', 'depth', 'msigm', 'tmx', 'tbot', 'sbot', 'umx', 'ubot', 'vmx', 'vbot', 'bfri']
  psa->setDescription, 'psa data files'
  psafileName='E:\data\mariomi\application\oxyrisk\resource\psa_model.xml'
  psa->xmlWriteStructList, psafileName
  obj_destroy, psa
  
END

PRO ModelInfo::testForApp

;  modis=obj_new('satellite')
;  modisfileName='E:\data\mariomi\application\oxyrisk\configuration\modis_satellite.xml'
;  modis->parse, modisfileName
;  modis->streamPrint
;
;  obj_destroy, modis
;  
;  seawifs=obj_new('satellite')
;  seawifsName='E:\data\mariomi\application\oxyrisk\configuration\seawifs_satellite.xml'
;  seawifs->ParseFile, seawifsfileName
;  seawifs->streamPrint
;  obj_destroy, seawifs
  
END

FUNCTION ModelInfo::getFullElement, code, INDEX=INDEX

 element=self->getListStructDef()
 if n_elements(code) ne 1 then elementIndex=INDEX else elementIndex=self->getIndexByCode(code) 
 thisList=self->getList()
 element.code=thisList.code[elementIndex]
 element.displayname=thisList.displayname[elementIndex]
 element.fullfilename=thisList.fullfilename[elementIndex]

END

FUNCTION ModelInfo::getIndexByCode, code

 codes=self->getCodes()
 idx=(where(code eq codes))[0]
 return, idx

END

FUNCTION ModelInfo::buildFullFieldList, simpleList

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

FUNCTION  ModelInfo::getVersion

  return, '1.0'
  
END

FUNCTION ModelInfo::buildRecordFromElement, index, structElement

  code=structElement
  displayName=structElement
  fullfilename=''
  record=[code, displayName, fullfilename]
  return, record
  
END

FUNCTION ModelInfo::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION ModelInfo::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

FUNCTION ModelInfo::getFullFileNames

  thisList=self->getList()
  return, thisList[*].fullfilename
  
END

PRO ModelInfo::streamPrint

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

FUNCTION ModelInfo::getListStructDef

  struct = { code:'',$
    displayName:'', $
    fullfilename: '' $
    }
    
  return, struct
  
END

FUNCTION ModelInfo::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.displayName=fieldsInfo[1]
    thisStruct.fullfilename=fieldsInfo[2]
  endif
  return, thisStruct
  
END

FUNCTION ModelInfo::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

FUNCTION ModelInfo::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO ModelInfo::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO ModelInfo__Define

  Struct = { ModelInfo , $
    Inherits ConfigurableData $
    }
    
END