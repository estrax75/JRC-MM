FUNCTION SimpleXML::extractAttribs, field, ALL=ALL, XML=XML

  ; Check if "field" is a valid field name. If it is,
  ; return the value of the field. If not, return -1.

  ; Get the name of the object class.

  ;thisClass = Obj_Class(self)
  thisClass = Obj_Class(self.className)
  
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
  
  if keyword_set(XML) then nonSerAttribs=strupcase(self->getNonSerializedAttributes()) else nonSerAttribs=['']
  nElem=n_elements(structFields)
  info=replicate({index:-1,name:'',type:0}, nElem)
  k=0
  for i=0, nElem-1 do begin
    idx=where(nonSerAttribs eq structFields[i], count)
    info[i].index=-1
    if count ne 1 then begin
      info[k].index=i
      info[k].name=structFields[i]
      info[k].type=size(thisStruct.(i), /TYPE)
      k++
    endif
  endfor
  info=info[where(info.index ne -1)]
  return, info
  
END

FUNCTION SimpleXML::xmlGetMainDocTypeElement

  return, '<!ELEMENT ' + self->xmlGetRootElement()+'>'
  
END

FUNCTION SimpleXML::xmlGetRootElement

  return, self->xmlGetStructName()
  
END

PRO SimpleXML::xmlWriteDocTypeSection, unit

  buffer=self->xmlGetDocTypeOpenTag()
  buffer=buffer+self->xmlGetDocTypeName()
  printf, unit, buffer
  
  buffer=self->xmlGetStructDefinitionElement()
  ;buffer=self->xmlGetDocTypeOpenComplexElement, self->xmlGetRootElement()
  
  printf, unit, buffer
  
  ;  buffer=self->xmlGetNumberOfElementsElement()
  ;  printf, unit, buffer
  
  self->xmlWriteStructTagsDefinition, unit
  
  buffer=self->xmlGetDocTypeCloseTag()
  printf, unit, buffer
  
END

PRO SimpleXML::xmlWriteDataContents, unit, struct

  attribs=self->extractAttribs(/ALL, /XML)
  nElem=n_elements(attribs)
  printf, unit, self->xmlGetOpenElementTag(self->xmlGetStructName())
  for i=0, nElem-1 do begin
    self->xmlWriteTagElement, unit, attribs[i]
  endfor
  printf, unit, self->xmlGetCloseElementTag(self->xmlGetStructName())
  
END

PRO SimpleXML::xmlWriteStructTagsDefinition, unit

  attribs=self->extractAttribs(/ALL, /XML)
  for i=0, n_elements(attribs)-1 do begin
    thisElem=self->xmlGetDocTypeValueListElement(strlowcase(attribs[i].name), MULTIPLE=attribs[i].type eq 10)
    printf, unit, thisElem
  endfor
  idx=where(attribs.type eq 10, count)
  if count gt 0 then printf, unit, self->xmlGetNumberOfElementsElement()
  printf, unit, self->xmlGetDocTypeSimpleElement('typeCode')
  printf, unit, self->xmlGetDocTypeSimpleElement('value')
  
END

FUNCTION SimpleXML::xmlGetStructDefinitionElement

  buffer=self->xmlGetDocTypeOpenComplexElement(self->xmlGetStructName())
  attribs=self->extractAttribs(/ALL, /XML)
  tags=attribs.name
  for i=0, n_elements(tags)-2 do buffer=buffer+strlowcase(tags[i])+','
  buffer=buffer+strlowcase(tags[i])
  buffer=buffer+self->xmlGetDocTypeCloseComplexElement()
  return, buffer
  
END

;xml parser start
PRO SimpleXML::characters, data

  doLog, data, LEVEL=0
  doLog, self.currentXMLIndex, LEVEL=0
  if self.currentXMLIndex eq -999 then self->updateCurrentXmlElement, self.tagIndex, data
  if self.currentXMLIndex eq -998 then self->updateCurrentXmlTypeCodeElement, data
  if self.currentXMLIndex eq -1 then self->initCurrentXmlList, data
  if self.currentXMLIndex ge 0 then self->updateCurrentXmlValue, data
  
END

PRO SimpleXML::startElement, URI, local, strName, attr, value

  doLog,'<'+strName+'>', LEVEL=0
  mainElement=strupcase(self->xmlGetStructName())
  ;attribs=self->xmlGetStructName()
  CASE strupcase(strName) OF
    ;-1 list elements tag
    self.numberOfElements:begin
    self.currentXMLIndex=-1; do nothing self.tagIndex=-999
    self.isCurrentXmlAList=1
  end
  strupcase('typeCode'):self.currentXMLIndex=-998
  ;next list element tag
  strupcase('value'):begin
  self.currentXMLIndex=(self.currentXMLIndex lt 0) ? 0 : (self.currentXMLIndex+1)
  doLog, 'self.currentXMLIndex-->', self.currentXMLIndex, LEVEL=0
end
mainElement: self.tagIndex=-1
else: begin
  self.isCurrentXmlAList=0
  attribs=self->extractAttribs(/ALL, /XML)
  idx=where(strupcase(attribs.name) eq strupcase(strName))
  self.tagIndex=attribs[idx].index
  doLog, '-->'+strName, self.tagIndex, LEVEL=0
;-999 simple elements tag
;self.currentXMLIndex=-999
end
ENDCASE

END

PRO SimpleXML::endElement, URI, local, strName

  doLog, '</'+strName+'>', LEVEL=0
  class=strupcase(self->xmlGetStructName())
  CASE strupcase(strName) OF
    class: ;self.currentXMLIndex++
    strupcase('typeCode'):; do nothing
    strupcase('value'):;do nothing
    self.numberOfElements:;do nothing
    else: begin
      doLog, 'end element, do post processing...', LEVEL=0
      if self.isCurrentXmlAList then begin
        doLog,'Store list', LEVEL=0
        attribs=self->extractAttribs(/ALL, /XML)
        idx=where(strupcase(attribs.name) eq strupcase(strName))
        self.tagIndex=attribs[idx].index
        self->storeCurrentXmlList
      endif
    end
  ENDCASE
  
END

PRO SimpleXML::startDocument

  self.tagIndex=-1
  
END
;xml parser end
;FUNCTION SimpleXML::extractAttribs, field, ALL=ALL, XML=XML
;
;  message, "you have to implement in child class"
;  ; Check if "field" is a valid field name. If it is,
;  ; return the value of the field. If not, return -1.
;
;  ; Get the name of the object class.
;
;  ;thisClass = Obj_Class(self)
;  thisClass = Obj_Class("RunSelectionDisplayInfo")
;
;  ; Create a local structure of this type.
;
;  ok = Execute('thisStruct = {' + thisClass + '}')
;
;  structFields = Tag_Names(thisStruct)
;  if ~(keyword_set(ALL)) then begin
;    ; Make sure "field" is a string variable.
;
;    s = Size(field)
;
;    IF s[s[0]+1] NE 7 THEN Message, "Field variable must be a string."
;    ; Find the field identifier (index) in the local structure.
;
;    index = WHERE(structFields EQ StrUpCase(field), count)
;
;    ; Extract and return the field if it is found.
;
;    if count eq 1 then begin
;      return, self.index(0)
;    endif else begin
;      message, 'Can not find field "' + field + $
;        '" in structure.', /Informational
;      return, -1
;    endelse
;  endif
;
;  if keyword_set(XML) then nonSerAttribs=strupcase(self->getNonSerializedAttributes()) else nonSerAttribs=['']
;  nElem=n_elements(structFields)
;  info=replicate({index:-1,name:'',type:0}, nElem)
;  k=0
;  for i=0, nElem-1 do begin
;    idx=where(nonSerAttribs eq structFields[i], count)
;    info[i].index=-1
;    if count ne 1 then begin
;      info[k].index=i
;      info[k].name=structFields[i]
;      info[k].type=size(thisStruct.(i), /TYPE)
;      k++
;    endif
;  endfor
;  info=info[where(info.index ne -1)]
;  return, info
;
;END

FUNCTION SimpleXML::init, className, nonSerAttribs

  if not self -> XMLHelper :: init() then return , 0
  ;xml stuff...
  if n_elements(nonSerAttribs) ne 0 then self->addNonSerializedAttributes, nonSerAttribs
  self.className=className
  return , 1
  
END

PRO SimpleXML::cleanUp

  ptr_free, self.nonSerializedAttributes
  self -> XMLHelper::cleanUp
  
END

;****************************************************************************************

PRO SimpleXML__Define

  Struct = { SimpleXML , $
    className: '', $
    Inherits XMLHelper $
    }
    
END

;****************************************************************************************
