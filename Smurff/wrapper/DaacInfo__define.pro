FUNCTION DaacInfo::getDaac, daacCode

  daac=obj_new('DaacPar')
  idx=self->getIndexByCode(daacCode)
  fNames=self->getFullFileNames()
  res = daac->fillDataFromXMLFile(fNames[idx])
  return, daac  

END

PRO DaacInfo::buildForApp

  daacPar=obj_new('Daac')
  daacPar->setArchiveRoot, 'E:\data\mariomi\application\oxyrisk\input\seawifs'
  daacPar->setDisplayName, 'modis'
  daacPar->setCode, 'modis'
  daacPar->setFileType, 'DAAC'
  daacPar->setParametersList, ['l3m_data']
  daacPar->setDescription, 'daac par data files'
  daacParfileName='E:\data\mariomi\application\oxyrisk\resource\daac.xml'
  daacPar->xmlWriteStructList, daacParfileName
  obj_destroy, daacPar

  daacPar2=obj_new('Daac')
  daacPar2->setArchiveRoot, 'E:\data\mariomi\application\oxyrisk\input\seawifs'
  daacPar2->setDisplayName, 'modis'
  daacPar2->setCode, 'modis'
  daacPar2->setFileType, 'DAAC'
  daacPar2->setParametersList, ['l3m_data']
  daacPar2->setDescription, 'daac par data files'
  daacParfileName='E:\data\mariomi\application\oxyrisk\resource\daac.xml'
  daacPar2->xmlWriteStructList, daacParfileName
  obj_destroy, daacPar2
  
END

PRO DaacInfo::testForApp

  daac=obj_new('daac')
  daacfileName='E:\data\mariomi\application\oxyrisk\configuration\daac.xml'
  daac->parse, modisfileName
  daac->streamPrint

  obj_destroy, daac
  
  daac1=obj_new('daac')
  daac1fileName='E:\data\mariomi\application\oxyrisk\configuration\daac1.xml'
  daac1->ParseFile, daac1fileName
  daac1->streamPrint
  obj_destroy, daac1
  
END

FUNCTION DaacInfo::getFullElement, code, INDEX=INDEX

 element=self->getListStructDef()
 if n_elements(code) ne 1 then elementIndex=INDEX else elementIndex=self->getIndexByCode(code) 
 thisList=self->getList()
 element.code=thisList.code[elementIndex]
 element.displayname=thisList.displayname[elementIndex]
 element.fullfilename=thisList.fullfilename[elementIndex]

END

FUNCTION DaacInfo::getIndexByCode, code

 codes=self->getCodes()
 idx=(where(code eq codes))[0]
 return, idx

END

FUNCTION DaacInfo::buildFullFieldList, simpleList

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

FUNCTION  DaacInfo::getVersion

  return, '1.0'
  
END

FUNCTION DaacInfo::buildRecordFromElement, index, structElement

  code=structElement
  displayName=structElement
  fullfilename=''
  record=[code, displayName, fullfilename]
  return, record
  
END

FUNCTION DaacInfo::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION DaacInfo::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

FUNCTION DaacInfo::getFullFileNames

  thisList=self->getList()
  return, thisList[*].fullfilename
  
END

PRO DaacInfo::streamPrint

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

FUNCTION DaacInfo::getListStructDef

  struct = { code:'',$
    displayName:'', $
    fullfilename: '' $
    }
    
  return, struct
  
END

FUNCTION DaacInfo::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.displayName=fieldsInfo[1]
    thisStruct.fullfilename=fieldsInfo[2]
  endif
  return, thisStruct
  
END

FUNCTION DaacInfo::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

FUNCTION DaacInfo::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO DaacInfo::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO DaacInfo__Define

  Struct = { DaacInfo , $
    Inherits ConfigurableData $
    }
    
END