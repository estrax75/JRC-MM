;interface section
FUNCTION ConfigurableData::listFieldsExpectedNumber

  message, "Implements this on upper level"
  
END

FUNCTION ConfigurableData::buildRecordFromElement, i, structElement

  message, "Implements this on upper level"
  
END

FUNCTION ConfigurableData::getListStructDef

  message, "Implements this on upper level"
  
END

FUNCTION ConfigurableData::buildFullFieldList, list

 message, 'Implements this on upper level'
 return, 0

END

FUNCTION  ConfigurableData::getVersion

  message, "Implements this on upper level"
  
END

PRO ConfigurableData::writeXmlFile, fileName, structList, WRITE=WRITE

 self->xmlWriteStructList, fileName, structList

END
; write xml related
FUNCTION ConfigurableData::XmlGetRootElement

 return, self->XmlGetStructName()+'catalog'

END

PRO ConfigurableData::characters, data

  ;doLog,data
  ; init struct array
  if self.tagIndex eq -999 then self->setList, replicate(self->getListStructDef(), fix(data))
  ; OR fill specific element
  if self.tagIndex ge 0 then self->updateCurrentXmlElement, self.tagIndex, data
  
END
; internal management
PRO ConfigurableData::setList,  structArray

  ptr_free, self.list
  self.list=ptr_new(structArray, /NO_COPY)
  
END

FUNCTION ConfigurableData::getList

  return, *self.list
  
END

PRO ConfigurableData::XmlWriteDataContents, unit, struct

  if n_elements(struct) eq 0 then list=self->getList() else list=self->buildFullFieldList(struct)
  nElem=n_elements(list)
  printf, unit, self->XmlGetOpenElementTag(self->XmlGetRootElement())
  tags=tag_names(list[0])
  self->XmlWriteSimpleFullElement, unit, 'numberOfElements', nElem
  xmlStructName=self->XmlGetStructName()
  for i=0, nElem-1 do begin
    printf, unit, self->XmlGetOpenElementTag(xmlStructName)
    for j=0, n_elements(tags)-1 do self->XmlWriteSimpleFullElement, unit, strlowcase(tags[j]), list[i].(j)
    printf, unit, self->XmlGetCloseElementTag(xmlStructName)
  endfor
  printf, unit, self->XmlGetCloseElementTag(self->XmlGetRootElement())
  
END

PRO ConfigurableData::updateCurrentXmlElement, tagIndex, tagValue

  ;(*self.xmlList)[self.currentXMLIndex].(tagIndex)=tagValue
  (*self.list)[self.currentXMLIndex].(tagIndex)=tagValue
  ;prevLogFlag=self.application->getLogLevel()
  ;self.application->setLogLevel, 0
  doLog, 'array pos:', level=0
  doLog, self.currentXMLIndex, level=0
  doLog, 'tag pos:', level=0
  doLog, (*self.list)[self.currentXMLIndex].(tagIndex), level=0
  ;self.application->setLogLevel, prevLogFlag
  
END

FUNCTION ConfigurableData::getIndexesOfCodes, codes

  allCodes=self->getCodes()
  nElem=n_elements(codes)
  idxs=intarr(nElem)
  for i=0, nElem-1 do idxs[i]=(where(codes[i] eq allCodes))[0]
  return, idxs
  
END

PRO ConfigurableData::writeStructList, unit, fileName, structList, WRITE=WRITE

  if keyword_set(WRITE) then openw, unit, filename
  ;Code,DisplayName,LatStart,LonStart,LatEnd,LonEnd,RegularFlag,Description
  
  structListNumbers=n_elements(structList)
  
  header=self->getFileHeader()
  if keyword_set(WRITE) then printf, unit, header else doLog,header, level=3
  for i=0, structListNumbers-1 do begin
    ;code=strcompress(i, /REMOVE)
    recordList=self->buildRecordFromElement(i, structList[i])
    if keyword_set(WRITE) then self.application->writeRecordStream, unit, recordList else doLog,recordList
  endfor
  if keyword_set(WRITE) then close, unit else doLog,'************EOF***************', level=3
  
END

PRO ConfigurableData::writeXMLStructList, unit, fileName, structList, WRITE=WRITE

  openw, unit, filename, /GET_LUN 
  ;if keyword_set(WRITE) then openw, unit, filename
  voidLine=''
  
  printf, unit, self->XmlGetMainHeader()
  
  printf, unit, self->XmlGetCommentOpenTag()
  printf, unit, 'Put here comments'
  printf, unit, self->XmlGetCommentCloseTag()
  
  printf, unit, voidLine

  self->XmlWriteDocTypeSection, unit
  
  self->XmlWriteDataContents, unit, recordList
  close, unit & free_lun, unit

END

; txt file section
PRO ConfigurableData::fillDataFromFile, fileName

  self.textFileName=fileName
  
  ;  ERROR=0
  ;  catch, error_status
  ;
  ;  if error_status NE 0 THEN BEGIN
  ;    ERROR=1
  ;    catch, /CANCEL
  ;    close, /all
  ;    errMsg=dialog_message('problem with file: <'+fileName+'> check contents, existence or read permission.', /ERROR)
  ;    return
  ;  endif
  
  openr, unit, fileName, /GET_LUN
  
  bufferString=''
  i=0
  structList=self->buildListStruct(/NOFILL)
  while not(eof(unit)) do begin
    info=self.application->readRecordStream(unit, columns=columns)
    i++
    if columns eq self->listFieldsExpectedNumber() then begin
      thisStruct=self->buildListStruct(info)
      structList=[structList, thisStruct]
    endif else begin
      doLog,'Bad or comment conf file at line', i, bufferString, level=3
    endelse
  endwhile
  
  close, unit & free_lun, unit
  self->setList, structList[1:*]
  
  self->writeXmlFile, dialog_pickfile(file=obj_class(self)+'.xml')
  
;self.list=ptr_new(structList[1:*], /NO_COPY)
  
END

FUNCTION ConfigurableData::getFileHeader

  tagNames=TAG_NAMES(self->getListStructDef())
  header='['''+obj_class(self)+''':'
  for i=0, n_elements(tagNames)-1 do header=header+tagNames[i]+';'
  return, strmid(header, 0, strlen(header)-1)+']'
  
END

PRO ConfigurableData::CleanUp

  ;self.applicationFS=obj_new()
  self.application=obj_new()
  ptr_free, self.list
  self-> XMLHelper::cleanup
  
END

FUNCTION ConfigurableData::init, application, filename, mode=mode

  if not (self -> XMLHelper :: init()) then return, 0
  
  ;if n_elements( applicationFS ) eq 1 then self.applicationFS = applicationFS
  if n_elements( application ) eq 1 then self.application = application
  if n_elements( fileName ) eq 1 then self.textFilename = filename
  return, 1
  
END

PRO ConfigurableData__Define

  Struct = { ConfigurableData , $
    application: obj_new(), $
    textFileName : '', $
    list: ptr_new(), $
    Inherits XMLHelper $
    }
    
END