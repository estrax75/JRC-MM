FUNCTION Object::extractField, field, ALL=ALL

  ; Check if "field" is a valid field name. If it is,
  ; return the value of the field. If not, return -1.

  ; Get the name of the object class.

  thisClass = Obj_Class(self)
  
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
  nElem=n_elements(structFields)
  info=replicate({index:0,name:'',type:0}, nElem)
  for i=0, nElem-1 do begin
    info[i].index=i
    info[i].name=structFields[i]
    info[i].type=size(self.index(i), /TYPE)
  endfor
  return, info
end
;****************************************************************************************

function Object :: clone

  return , obj_new()
  
end

;****************************************************************************************

function Object :: getClass , superclass = superclass , count = count

  return , obj_class( self , superclass = superclass , count = count )
  
end

;****************************************************************************************

function Object :: isA , class

  if ( size( class ) )[ 0 ] eq 0 and ( size( class ) )[ ( size( class ) )[ 0 ] + 1 ] eq 7 then return , obj_isa( self , class ) else return , 0
  
end

;****************************************************************************************

pro Object :: cleanUp

  self = 0b
  
end

;****************************************************************************************

function Object :: init, LOG=LOG

  return , 1
  
end

;****************************************************************************************

pro Object__Define

  Struct = { Object , $
    idObject : 0ll $
    }
    
end

;****************************************************************************************
