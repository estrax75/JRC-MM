PRO MapEntry::setValue, value

  self.value=value
  
END

;PRO MapEntry::setKey, key
;
;  self.key=key
;  
;END

PRO MapEntry::setDisplayname, displayname

  self.displayname=displayname
  
END

PRO MapEntry::setType, type

  self.type=type
  
END

PRO MapEntry::setIsArray, isArray

  if strlowcase(isArray) eq 'true' then flag=1 else flag=0
  self.isArray=flag
  
END

PRO MapEntry::setNumberOfElements, numberOfElements

  self.numberOfElements=fix(numberOfElements)
  
END

;PRO MapEntry::setSection, section
;
;  self.section=section
;  
;END

PRO MapEntry::setDescription, description

  self.description=description
  
END

PRO MapEntry::setAttrib, index, value

  self.(index)=value
  
END

FUNCTION MapEntry::extractAttribs, attrib, ALL=ALL, XML=XML

  ; Check if "field" is a valid field name. If it is,
  ; return the value of the field. If not, return -1.

  ; Get the name of the object class.

  ;thisClass = Obj_Class(self)
  thisClass = Obj_Class('MapEntry')
  
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

;**************************************************************
;From MapEntry in Java
;**************************************************************

FUNCTION MapEntry::equals, key, section

  if n_elements(section) ne 0 then sectionCheck=self.key eq section else sectionCheck=1
  if (key eq self.key) and (sectionCheck) then return, 1
  return, 0
  
END

PRO MapEntry::setSection, value

  if n_elements(value) eq 1 then self.section=value
  
END

PRO MapEntry::setKey, value

  self.key=value
  
END

FUNCTION MapEntry::getSection

  return, self.section
  
END

FUNCTION MapEntry::getKey

  return, self.key
  
END

;FUNCTION MapEntry::getStringValue
;
;  return, self.value
;  
;END

FUNCTION MapEntry::getType

  return, self.type
  
END

FUNCTION MapEntry::getValue

  return, self.value
  
END

FUNCTION MapEntry::getTypeName

  test=make_array(1, TYPE=self.type)
  return, size(test, /TNAME)
  
END

PRO MapEntry::set, key, value, section

  self->setKey, key
  self->setValue, value
  self->setSection, section
  
END

FUNCTION MapEntry::init, key, value, section

  if not self -> Object :: init() then return , 0
  if n_elements(key) eq 1 then self->set, key, value, section
  return , 1
  
END

PRO MapEntry::cleanUp

  self -> Object::cleanUp
  
END

;****************************************************************************************

PRO MapEntry__Define

  Struct = { MapEntry , $
    key: '',$
    displayname: '',$
    value: '',$
    type: '',$
    isArray: 0,$
    numberOfElements: 0,$
    section: '',$
    description: '',$
    Inherits Object $
    }
    
END

;****************************************************************************************
