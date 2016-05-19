;interface implementation
PRO ConfigurationMap::startElement, URI, local, strName, attr, value

  doLog, '<'+strName+'>', level=0
  mainElement=self->xmlGetStructName()
  elementName=strlowcase(self->getElementClass())
  self.currentTagName=''
  CASE strlowcase(strName) OF
    elementName:begin
    self.currentTagName=''
    self.currentElementEntry=self->getElementObject()
  end
  mainElement: self.currentTagName=''
  else: begin
    self.currentTagName=strName
  end
ENDCASE

END

PRO ConfigurationMap::characters, data

  doLog, data, level=0
  if self.currentTagName ne '' then self->updateCurrentElementAttrib, data
  
END

PRO ConfigurationMap::updateCurrentElementAttrib, tagValue

  call_method, 'set'+self.currentTagName, self.currentElementEntry,  tagValue
  
END

PRO ConfigurationMap::endElement, URI, local, strName

  doLog,'*****', level=0
  doLog,'</'+strName+'>', level=0
  class=self->xmlGetStructName()
  elementName=strlowcase(self->getElementClass())
  doLog,elementName, level=0
  doLog,'*****', level=0
  CASE strlowcase(strName) OF
    class:;do nothing
    elementName:self->addEntry, self.currentElementEntry
    else:;do nothing
  endcase
  
END

FUNCTION ConfigurationMap::buildListStruct, fieldsInfo, NOFILL=NOFILL

  message, "Implements this"
  
END

PRO ConfigurationMap::updateCurrentXmlElement, tagIndex, tagValue

  message, "Implements this"
  
END

PRO ConfigurationMap::xmlWriteDataContents, unit, struct

  message, "Implements this"
  
END

FUNCTION ConfigurationMap::getListStructDef

  message, "Implements this"
  
END

FUNCTION ConfigurationMap::listFieldsExpectedNumber

  message, "Implements this on upper level"
  
END

FUNCTION ConfigurationMap::xmlGetRootElement

  return, obj_class(self)
  
END

FUNCTION ConfigurationMap::buildFullFieldList, list

  message, 'Implements this'
  return, 0
  
END

FUNCTION  ConfigurationMap::getVersion

  return, '1.0'
  
END

;**************************************************************
;Base upon map
;**************************************************************
PRO ConfigurationMap::buildMapEntryFromXMLFile, fileName

  res=self->fillDataFromXMLFile(fileName)
  
END

FUNCTION ConfigurationMap::length

  return, self.counter
;if ptr_valid(self.mapEntries) then return, n_elements(*self.mapEntries) else return, 0
  
END

PRO ConfigurationMap::removeEntryByIndex, index

  obj_destroy, (*self.mapEntries)[index]
  self->rebuldEntries
  
END

PRO ConfigurationMap::rebuldEntries, EXPAND=EXPAND

  if keyword_set(EXAPND) then newMapEntries=objarr(n_elements((*self.mapEntries)*2)) else newMapEntries=objarr(100)
  
  for i=0, self.counter-1 do begin
    if obj_valid(*self.mapEntries[index]) then begin
      newMapEntries[j]=(*self.mapEntries)[index]
      j++
    endif
  endfor
  self.counter=j
  ptr_free, self.mapEntries
  self.mapEntries=ptr_new(entries, /NO_COPY)
  
END

PRO ConfigurationMap::removeEntryByKey, key

  for i=0, self.counter-1 do begin
    if (*self.mapEntries)[index].equals(key) then begin
      obj_destroy, (*self.mapEntries)[index]
      self->rebuldEntries
      break
    endif
  endfor
  
  
END

FUNCTION ConfigurationMap::getElementObject

  return, obj_new(self->getElementClass())
  
END

FUNCTION ConfigurationMap::getElementClass

  return, 'MapEntry'
  
END

PRO ConfigurationMap::addEntry, elementEntry

  if self.counter ge n_elements((*self.mapEntries)) then self->rebuldEntries, /EXPAND
  (*self.mapEntries)[self.counter]=elementEntry
  self.counter++
  
END

FUNCTION ConfigurationMap::getEntryByIndex, index

  return, (*self.mapEntries)[index]
  
END

FUNCTION ConfigurationMap::getEntryByKey, key

  for i=0, self.counter-1 do begin
    ;print, (*self.mapEntries)[i]->getKey()
    ;print, (*self.mapEntries)[i]->getValue()
    if (*self.mapEntries)[i]->equals(key) then return, (*self.mapEntries)[i]
  endfor
  return, obj_new()
  
END

FUNCTION ConfigurationMap::init

  if not self -> XMLHelper :: init() then return , 0
  self.counter=0
  entries=objarr(200)
  self.mapEntries=ptr_new(entries, /NO_COPY)
  return , 1
  
END

PRO ConfigurationMap::cleanUp

  if ptr_valid(self.mapEntries) then begin
    mapE=*self.mapEntries
    for i=0, n_elements(mapE)-1 do obj_destroy, mapE[i]
    ptr_free, self.mapEntries
  endif
  obj_destroy, self.currentElementEntry
  
  self -> XMLHelper::Cleanup
  
END

;****************************************************************************************

PRO ConfigurationMap__Define

  Struct = { ConfigurationMap , $
    mapEntries: ptr_new(), $
    counter: 0, $
    currentElementEntry: obj_new(), $
    currentTagName: '', $
    Inherits XMLHelper $
    }
    
END

;****************************************************************************************
