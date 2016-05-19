PRO ColorScaleInfo::setKey, key

  self.key=key
  
END

PRO ColorScaleInfoEntry::setScaleName, scaleName

  self.scaleName=scaleName
  
END

PRO ColorScaleInfoEntry::setDisplayName, displayname

  self.displayname=displayname
  
END

PRO ColorScaleInfoEntry::setParameterCode, pCode

  self.parameterCode=pCode
  
END

PRO ColorScaleInfoEntry::setRangeTypeCode, rType

  self.rangeTypeCode=rType
  
END

PRO ColorScaleInfoEntry::setValue, value

  self.value=value
  
END

PRO ColorScaleInfoEntry::setValueList, value

  self.valueList=value
  
END

PRO ColorScaleInfoEntry::setVLow, vlow

  self.vLow=vlow
  
END

PRO ColorScaleInfoEntry::setVHigh, vhigh

  self.vHigh=vhigh
  
END

PRO ColorScaleInfoEntry::setVScale, vscale

  self.vScale=vscale
  
END

PRO ColorScaleInfoEntry::setCTableIdx, ctable

  self.cTableIdx=ctable
  
END

PRO ColorScaleInfoEntry::setCTableFileName, fileName

  self.cTableFileName=fileName
  
END

PRO ColorScaleInfoEntry::setBadV, badv

  self.badV=badv
  
END

PRO ColorScaleInfoEntry::setUnit, unit

  self.unit=unit
  
END

PRO ColorScaleInfoEntry::setDescription, description

  self.description=description
  
END

PRO ColorScaleInfoEntry::setNumberOfElements, numberOfElements

  self.numberOfElements=fix(numberOfElements)
  
END

PRO ColorScaleInfoEntry::setAttrib, index, value

  self.(index)=value
  
END

FUNCTION ColorScaleInfoEntry::extractAttribs, attrib, ALL=ALL, XML=XML

  ; Check if "field" is a valid field name. If it is,
  ; return the value of the field. If not, return -1.

  ; Get the name of the object class.

  ;thisClass = Obj_Class(self)
  thisClass = Obj_Class('ColorScaleInfoEntry')
  
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
;From ColorScaleInfoEntry in Java
;**************************************************************

FUNCTION ColorScaleInfoEntry::equals, keyCode

  if (keyCode eq self.key) then return, 1
  return, 0
  
END

FUNCTION ColorScaleInfoEntry::equalsByKeys, parameterCode, rangeTypeCode

  if (parameterCode eq self.parameterCode) and (rangeTypeCode eq self.rangeTypeCode) then return, 1
  return, 0
  
END

PRO ColorScaleInfoEntry::setKey, value

  self.key=value
  
END

FUNCTION ColorScaleInfoEntry::getSection

  return, self.section
  
END

FUNCTION ColorScaleInfoEntry::getKey

  return, self.key
  
END

;FUNCTION ColorScaleInfoEntry::getStringValue
;
;  return, self.value
;  
;END

FUNCTION ColorScaleInfoEntry::getType

  return, self.type
  
END

FUNCTION ColorScaleInfoEntry::getCTableIdx

  return, self.cTableIdx
  
END

FUNCTION ColorScaleInfoEntry::getCTableFileName

  return, self.cTableFileName
  
END

FUNCTION ColorScaleInfoEntry::getValue

  return, self.value
  
END

FUNCTION ColorScaleInfoEntry::getValueList

  return, self.valueList
  
END

FUNCTION ColorScaleInfoEntry::getTypeName

  test=make_array(1, TYPE=self.type)
  return, size(test, /TNAME)
  
END

FUNCTION ColorScaleInfoEntry::init

  if not self -> Object :: init() then return , 0
  return , 1
  
END

PRO ColorScaleInfoEntry::cleanUp

  self -> Object::cleanUp
  
END

;****************************************************************************************

PRO ColorScaleInfoEntry__Define

  Struct = { ColorScaleInfoEntry , $
    key: '',$
    scaleName: '',$
    displayName: '',$
    parameterCode: '', $
    rangeTypeCode: '', $
    valueList: '',$
    value: '',$
    vLow: 0.0, $
    vHigh: 0.0, $
    vScale: 0, $
    cTableIdx: 0, $
    cTableFilename: '', $
    bAdv: 0.0, $
    unit: '', $
    description: '', $
    Inherits Object $
    }
    
END

;****************************************************************************************
