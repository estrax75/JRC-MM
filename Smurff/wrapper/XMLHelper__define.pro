;interface
FUNCTION XMLHelper::buildListStruct, fieldsInfo, NOFILL=NOFILL

  message, "Implements this on upper level"
  
END

PRO XMLHelper::updateCurrentXmlElement, tagIndex, tagValue

  message, "Implements this on upper level"
  
END

PRO XMLHelper::xmlWriteDataContents, unit, struct

  message, "Implements this on upper level"
  
END

PRO XMLHelper::characters, data

  message, "Implements this on upper level"
  
END

FUNCTION XMLHelper::getListStructDef

  message, "Implements this on upper level"
  
END

FUNCTION XMLHelper::listFieldsExpectedNumber

  message, "Implements this on upper level"
  
END

FUNCTION XMLHelper::xmlGetRootElement

  message, 'Implements this on upper level'
  return, 0
  
END

FUNCTION XMLHelper::buildFullFieldList, list

  message, 'Implements this on upper level'
  return, 0
  
END

FUNCTION  XMLHelper::getVersion

  message, "Implements this on upper level"
  
END

;xml elements related methods
FUNCTION XMLHelper::getXMLVersion

  return, '1.0'
  
END

FUNCTION XMLHelper::getNonSerializedAttributes

  if ptr_valid(self.nonSerializedAttributes) then list=*self.nonSerializedAttributes
  return, list
  
END

PRO XMLHelper::setNonSerializedAttributes, list

  ptr_free, self.nonSerializedAttributes
  self.nonSerializedAttributes=ptr_new(list, /NO_COPY)
  
END

PRO XMLHelper::addNonSerializedAttributes, list

  if ptr_valid(self.nonSerializedAttributes) then begin
    previousList=self->getNonSerializedAttributes()
    list=[list,previousList]
  endif
  self->setNonSerializedAttributes, list
  
END

;get class/struct name
FUNCTION XMLHelper::xmlGetStructName

  return, strlowcase(obj_class(self))
  
END
;writer start
FUNCTION XMLHelper::xmlGetCommentCloseTag

  return, '-->'
  
END

FUNCTION XMLHelper::xmlGetCommentOpenTag

  return, '<!--'
  
END

FUNCTION XMLHelper::xmlGetMainHeader

  return, '<?xml version="'+strcompress(self->getXMLVersion(), /REMOVE_ALL)+'" ?>'
  ;return, '<?xml version="'+strcompress(self->getVersion(), /REMOVE_ALL)+'" ?>'
  
END

FUNCTION XMLHelper::xmlGetDocTypeCloseTag, unit, elementName

  return, ']>'
  
END

FUNCTION XMLHelper::xmlGetNumberOfElementsElement

  return, '<!ELEMENT numberOfElements (#PCDATA)>'
  
END

;FUNCTION XMLHelper::xmlGetDocTypeSimpleElement, elementName
;
;  return, '<!ELEMENT '+elementName + ' (#PCDATA)>'
;  
;END

FUNCTION XMLHelper::xmlGetDocTypeOpenComplexElement, elementName

  return, '<!ELEMENT '+elementName+ ' ('
  
END

FUNCTION XMLHelper::xmlGetDocTypeCloseComplexElement, elementName

  return, ')>'
  
END

PRO XMLHelper::closeXmlDocType

  printf, ']>'
  
END

;write a tag that contains a list of values OR a simple value
PRO XMLHelper::xmlWriteTagElement, unit, tagInfo

  if tagInfo.type eq 10 then begin
    if ptr_valid(self.(tagInfo.index)) then begin
      list=*self.(tagInfo.index)
      typeCode=size(list[0], /TYPE)
    endif else begin
      list='null'
      typeCode=size(list, /TYPE)
    endelse
    ;typeCode=size(list[0], /TYPE)
    self->xmlWriteValueListFullElement, unit, strlowcase(tagInfo.name), typeCode, list
  endif else begin
    typeCode=size(self.(tagInfo.index), /TYPE)
    self->xmlWriteTypedFullElement, unit, strlowcase(tagInfo.name), typeCode, self.(tagInfo.index)
  endelse
  
END

;write a tag that contains a list of values
PRO XMLHelper::xmlWriteValueListFullElement, unit, tagName, tagType, valueList

  buffer=self->xmlGetOpenElementTag(tagName)
  printf, unit, buffer
  nElem=n_elements(valueList)
  self->xmlWriteSimpleFullElement, unit, 'typeCode', tagType
  self->xmlWriteSimpleFullElement, unit, 'numberOfElements', n_elements(valueList)
  for i=0, nElem-1 do self->xmlWriteSimpleFullElement, unit, 'value', valueList[i]
  buffer=self->xmlGetCloseElementTag(tagName)
  printf, unit, buffer
  
END

;convert a value to string
FUNCTION XMLHelper::xmlGetElementValue, value

  if size(value, /TYPE) eq 1 then return, strcompress(fix(value), /REMOVE)
  if size(value, /TYPE) ne 7 then return, strcompress(value, /REMOVE) else return, value
  
END
;open tag ELEMENT encapsulated (by a name)
FUNCTION XMLHelper::xmlGetOpenElementTag, element

  return, '<'+element+'>'
  
END

;close tag ELEMENT encapsulated (by a name)
FUNCTION XMLHelper::xmlGetCloseElementTag, element

  return, '</'+element+'>'
  
END

;tag ELEMENT in DOC TYPE section (main element)
FUNCTION XMLHelper::xmlGetMainDocTypeElement

  return, '<!ELEMENT ' + self->xmlGetRootElement()+' ('+self->xmlGetStructName() + ' )*>'
  
END

;open tag DOCTYPE in DOC TYPE section
FUNCTION XMLHelper::xmlGetDocTypeOpenTag

  return, '<!DOCTYPE '
  
END
;write file methods (maybe you have to overwrite in super class)...
PRO XMLHelper::xmlWriteStructList, fileName, structList

  openw, unit, filename, /GET_LUN
  voidLine=''
  
  printf, unit, self->xmlGetMainHeader()
  
  printf, unit, self->xmlGetCommentOpenTag()
  printf, unit, 'Put here comments'
  printf, unit, self->xmlGetCommentCloseTag()
  
  printf, unit, voidLine
  
  self->xmlWriteDocTypeSection, unit
  
  self->xmlWriteDataContents, unit, structList
  close, unit & free_lun, unit
  
END

;DOCTYPE section
;PRO XMLHelper::xmlWriteDocType, unit
;
;  self->writeXmlDocTypeOpenTag, unit
;  self->writeXmlDocTypeContents, unit
;  self->writeXmlDocTypeCloseTag, unit
;  
;END
;main tag DOCTYPE in DOC TYPE section
FUNCTION XMLHelper::xmlGetDocTypeName

  return, self->xmlGetRootElement()+' ['
  
END
FUNCTION XMLHelper::xmlGetStructDefinitionElement

  buffer=self->xmlGetDocTypeOpenComplexElement(self->xmlGetStructName())
  strDef=self->buildListStruct(/NOFILL)
  tags=tag_names(strDef)
  for i=0, n_tags(strDef)-2 do buffer=buffer+strlowcase(tags[i])+','
  buffer=buffer+strlowcase(tags[i])
  buffer=buffer+self->xmlGetDocTypeCloseComplexElement()
  return, buffer
  
END

FUNCTION XMLHelper::xmlGetDocTypeValueListElement, elementName, MULTIPLE=MULTIPLE

  elementTag='<!ELEMENT '+elementName+ ' ('
  typeCodeTag= 'typeCode,'
  fullElementTag=elementTag+typeCodeTag
  star='' & numberOfElementsTag=''
  if keyword_set(MULTIPLE) then begin
    star='*'
    numberOfElementsTag= 'numberOfElements,'
  endif; else begin
  ;    return, self->xmlGetDocTypeSimpleElement(elementName, MULTIPLE=MULTIPLE)
  ;  endelse
  fullElementTag=fullElementTag+numberOfElementsTag+'value'+star+')>'
  return, fullElementTag
  
END

;FUNCTION XMLHelper::xmlGetDocTypeValueListElement, elementName, MULTIPLE=MULTIPLE
;
;  elementTag='<!ELEMENT '+elementName+ ' ('
;  typeCodeTag= 'typeCode,'
;  fullElementTag=elementTag+typeCodeTag
;  ;star=''
;  ;numberOfElementsTag=''
;  value='value'
;  if keyword_set(MULTIPLE) then begin
;    star='*'
;    numberOfElementsTag= 'numberOfElements,'
;    value='valueList'
;  endif; else begin
;  ;    return, self->xmlGetDocTypeSimpleElement(elementName, MULTIPLE=MULTIPLE)
;  ;  endelse
;  fullElementTag=fullElementTag+value+')>'
;  return, fullElementTag
;
;
;END

FUNCTION XMLHelper::xmlGetDocTypeSimpleElement, elementName, MULTIPLE=MULTIPLE

  if keyword_set(MULTIPLE) then star='*' else star=''
  return, '<!ELEMENT '+elementName + ' (#PCDATA)'+star+'>'
  
END

PRO XMLHelper::xmlWriteStructTagsDefinition, unit

  strDef=self->buildListStruct(/NOFILL)
  tags=tag_names(strDef)
  for i=0, n_elements(tags)-1 do begin
    thisElem=self->xmlGetDocTypeSimpleElement(strlowcase(tags[i]))
    printf, unit, thisElem
  endfor
  
END

PRO XMLHelper::xmlWriteDocType, unit

  self->writeXmlDocTypeOpenTag, unit
  self->writeXmlDocTypeContents, unit
  self->writeXmlDocTypeCloseTag, unit
  
END

PRO XMLHelper::xmlWriteTypedFullElement, unit, element, typeCode, value

  printf, unit, self->xmlGetOpenElementTag(element)
  ;  fullElement=self->xmlGetOpenElementTag('typeCode')
  ;  fullElement=fullElement+self->xmlGetElementValue(typeCode)
  ;  fullElement=fullElement+self->xmlGetCloseElementTag('typeCode')
  ;  printf, unit, fullElement
  self->xmlWriteSimpleFullElement, unit, 'typeCode', typeCode
  self->xmlWriteSimpleFullElement, unit, 'value', value
  ;  fullElement=self->xmlGetOpenElementTag('value')
  ;  fullElement=fullElement+self->xmlGetElementValue(value)
  ;  fullElement=fullElement+self->xmlGetCloseElementTag('value')
  ;  printf, unit, fullElement
  printf, unit, self->xmlGetCloseElementTag(element)
  
END

PRO XMLHelper::xmlWriteSimpleFullElement, unit, element, value

  fullElement=self->xmlGetOpenElementTag(element)
  fullElement=fullElement+self->xmlGetElementValue(value)
  fullElement=fullElement+self->xmlGetCloseElementTag(element)
  printf, unit, fullElement
  
END

PRO XMLHelper::xmlWriteDocTypeSection, unit

  buffer=self->xmlGetDocTypeOpenTag()
  buffer=buffer+self->xmlGetDocTypeName()
  printf, unit, buffer
  
  buffer=self->xmlGetMainDocTypeElement()
  printf, unit, buffer
  
  buffer=self->xmlGetNumberOfElementsElement()
  printf, unit, buffer
  
  buffer=self->xmlGetStructDefinitionElement()
  printf, unit, buffer
  
  self->xmlWriteStructTagsDefinition, unit
  
  buffer=self->xmlGetDocTypeCloseTag()
  printf, unit, buffer
  
END
;writer end

;parser start
FUNCTION XMLHelper::fillDataFromXMLFile, fileName

;  ERROR=0
;  catch, error_status
;  if error_status NE 0 THEN BEGIN
;    ERROR=1
;    catch, /CANCEL
;    close, /all
;    message, 'Error occured during parsing', /INFORMATIONAL
;    ;errMsg=dialog_message(['Error occured with this Analysis/Data selection.', 'Check log file (you need to close application).','Hint: make sure that parameter is selected before stations'], /ERROR)
;    return, 0
;  endif
  check=file_info(filename)
  if check.size eq 0 then filename='/net/joralla'+filename
  self->parseFile, filename
  return, 1
  
END

PRO XMLHelper::startElement, URI, local, strName, attr, value

  doLog, '<'+strName+'>', level=0
  catalog=strupcase(self->xmlGetRootElement())
  class=strupcase(self->xmlGetStructName())
  CASE strupcase(strName) OF
    self.numberOfElements:self.tagIndex=-999
    catalog: self.tagIndex=-1
    class: self.tagIndex=-1
    else: self.tagIndex=where(tag_names(self->getListStructDef()) eq strupcase(strName))
  ENDCASE
  doLog, strName, self.tagIndex, level=0
  
END

PRO XMLHelper::endElement, URI, local, strName

  doLog, '</'+strName+'>', level=0
  if strName eq 'daacParInfo' then stop 
  class=strupcase(self->xmlGetStructName())
  CASE strupcase(strName) OF
    class: self.currentXMLIndex++
    else: ; do nothing
  ENDCASE
  
  
END

PRO XMLHelper::startDocument

  self.tagIndex=-1
  
END

PRO XMLHelper::updateCurrentXmlValue, value

  doLog, 'store...', level=0
  
  doLog, 'type code'+strcompress(self.currentXmlTypeCode), level=0
  doLog, 'value'+strcompress(value), level=0
  if self.isCurrentXmlAList then begin
    ;if self.logFlag then doLog,"element stored as list element"
    doLog, 'list element', value, level=0
    (*self.currentXmlList)[self.currentXmlIndex]=value
    doLog, "****************", level=0
  endif else begin
    doLog, 'single element', strcompress(value), level=0
    newValue=(make_array(1, type=self.currentXmlTypeCode))
    newValue[0]=value
    self.(self.tagIndex)=newValue[0]
    doLog, "element stored as single element", level=0
    doLog, newValue, level=0
    doLog, "****************", level=0
  endelse
  
END

PRO XMLHelper::initCurrentXmlList, length

  doLog, 'list of ', length, ' elements', level=0
  ptr_free, self.currentXmlList
  self.currentXmlList=ptr_new(strarr(long(length)), /NO_COPY)
  
END

PRO XMLHelper::updateCurrentXmlTypeCodeElement, data

  doLog, 'typeCode: ', data, level=0
  self.currentXmlTypeCode=long(data)
  
END

PRO XMLHelper::storeCurrentXmlList

  list=self->castType(*self.currentXmlList)
  ptr_free, self.currentXmlList
  ptr_free, self.(self.tagIndex)
  self.(self.tagIndex)=ptr_new(list, /NO_COPY)
  
END
;parser end

FUNCTION XMLHelper::extractAttribs, attrib, ALL=ALL, XML=XML

  ; Check if "field" is a valid field name. If it is,
  ; return the value of the field. If not, return -1.

  ; Get the name of the object class.

  ;thisClass = Obj_Class(self)
  thisClass = Obj_Class("XMLHelper")
  
  ; Create a local structure of this type.
  
  ok = Execute('thisStruct = {' + thisClass + '}')
  
  structFields = Tag_Names(thisStruct)
  if ~(keyword_set(ALL)) then begin
    ; Make sure "field" is a string variable.
  
    s = Size(field)
    
    IF s[s[0]+1] NE 7 THEN Message, "Field variable must be a string."
    ; Find the field identifier (index) in the local structure.
    
    index = WHERE(structFields EQ StrUpCase(field), count)
    
    ; Extract and return the field if it is found.
    
    if count eq 1 then begin
      return, self.index(0)
    endif else begin
      message, 'Can not find field "' + field + $
        '" in structure.', /Informational
      return, -1
    endelse
  endif
  
  if keyword_set(XML) then nonSerList=strupcase(self->getNonSerializedAttributes()) else nonSerList=['']
  nElem=n_elements(structFields)
  info=replicate({index:0,name:'',type:0}, nElem)
  for i=0, nElem-1 do begin
    idx=where(nonSerList eq structFields[i], count)
    if count ne 1 then begin
      info[i].index=i
      info[i].name=structFields[i]
      info[i].type=size(thisStruct.(i), /TYPE)
    endif
  endfor
  return, info
  
END

FUNCTION XMLHelper::castType, variable

  destVar=make_array(n_elements(variable), TYPE=self.currentXmlTypeCode)
  destVar[*]=variable
  return, destVar
  
END

PRO XMLHelper::CleanUp

  ptr_free, self.currentXmlList
  ptr_free, self.nonSerializedAttributes
  self-> IDLffXMLSAX::cleanup
  
END

FUNCTION XMLHelper::init, logFlag

  if not (self -> IDLffXMLSAX :: init()) then return, 0
  self.numberOfElements=strupcase('numberOfElements')
  attribs=self->xmlHelper::extractAttribs(/ALL)
  attribsName=attribs.name
  if n_elements(logFlag) eq 1 then self.logFlag=logFlag 
  self->xmlHelper::addNonSerializedAttributes, attribsName
  return, 1
  
END

PRO XMLHelper__Define

  Struct = { XMLHelper , $
    tagIndex: 0, $
    currentXmlIndex: 0, $
    currentXmlTypeCode: 0, $
    isCurrentXmlAList: 0b, $
    currentXmlList: ptr_new(), $
    nonSerializedAttributes: ptr_new(), $
    numberOfElements: '', $
    Inherits IDLffXMLSAX $
    }
    
END