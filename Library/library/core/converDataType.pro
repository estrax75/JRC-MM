;  BYTE=bandDataType[v] eq 1, SHORT=bandDataType[v] eq 2, $
;    LONG=bandDataType[v] eq 3, $
;    UBYTE=bandDataType[v] eq 16, USHORT=bandDataType[v] eq 12, $
;    ULONG=bandDataType[v] eq 13, $
;    FLOAT=bandDataType[v] eq 4, DOUBLE=bandDataType[v] eq 5, $
;    STRING=bandDataType[v] eq 7, UINT64=bandDataType[v] eq 14, $

;function convertDataType, sourceValue, destDataType, GET_NAME=GET_NAME
;
;  if keyword_set(GET_NAME) then begin
;    case destDataType of
;      1: res='int8'
;      5: res='float64'
;      4: res='float32'
;      2: res='int16'
;      3: res='int32'
;      7: res='str'
;      12: res='uint16'
;      16: res='uint8'
;      13: res='uint32'
;      14: res='uint64'
;      else:message, 'conversion not allowed'
;    endcase
;  endif else begin
;    case destDataType of
;      1: res=byte(fix(sourceValue))
;      5: res=double(sourceValue)
;      4: res=float(sourceValue)
;      2: res=fix(sourceValue)
;      3: res=long(sourceValue)
;      7: res=string(sourceValue)
;      12: res=fix(sourceValue);res=uint(sourceValue)
;      16: res=byte(fix(sourceValue))
;      13: res=ulong(sourceValue)
;      14: res=ulong64(sourceValue)
;      else:message, 'conversion not allowed'
;    endcase
;  endelse
;  ;if keyword_set(UNSIGNED) and destDataType eq 1 then res=not(res)-1
;  return, res
;
;end

;NC_BYTE 8-bit signed integer
;NC_UBYTE 8-bit unsigned integer
;NC_CHAR 8-bit character byte
;NC_SHORT 16-bit signed integer
;NC_USHORT 16-bit unsigned integer *
;NC_INT (or NC_LONG) 32-bit signed integer
;NC_UINT 32-bit unsigned integer *
;NC_INT64 64-bit signed integer *
;NC_UINT64 64-bit unsigned integer *
;NC_FLOAT 32-bit floating point
;NC_DOUBLE 64-bit floating point
;NC_STRING variable length character string *

function convertDataType, sourceValue, destDataType, GET_NAME=GET_NAME, FORMAT=FORMAT, IS_UNSIGNED=IS_UNSIGNED

  unsignedList=[12,13,14,16]
  useLess=where(destDataType eq unsignedList, isUnsigned)
  if keyword_set(IS_UNSIGNED) then return, isUnsigned
  if keyword_set(GET_NAME) then begin
    if FORMAT eq 'HDF' then begin
      case destDataType of
        1: res=['DFNT_INT8 ', '8-bit signed integer']
        5: res=['DFNT_FLOAT64', '64-bit floating point']
        4: res=['DFNT_FLOAT32', '32-bit floating point']
        2: res=['DFNT_INT16', '16-bit signed integer']
        3: res=['DFNT_INT32', '32-bit signed integer']
        7: res=['STRING', 'variable length character string']
        12: res=['DFNT_UINT16', '16-bit unsigned integer']
        16: res=['DFNT_UINT8', '8-bit unsigned integer']
        13: res=['DFNT_UINT32', '32-bit unsigned integer']
        14: res=['DFNT_UINT64', '64-bit unsigned integer']
        else:message, 'conversion not allowed'
      endcase
    endif
    if FORMAT eq 'NC' then begin
      case destDataType of
        1: res=['NC_BYTE', '8-bit signed integer']
        5: res=['NC_DOUBLE', '64-bit floating point']
        4: res=['NC_FLOAT', '32-bit floating point']
        2: res=['NC_SHORT', '16-bit signed integer']
        3: res=['NC_INT', '32-bit signed integer']
        7: res=['NC_STRING', 'variable length character string']
        12: res=['NC_USHORT', '16-bit unsigned integer']
        16: res=['NC_UBYTE', '8-bit unsigned integer']
        13: res=['NC_UINT', '32-bit unsigned integer']
        14: res=['NC_UINT64', '64-bit unsigned integer']
        else:message, 'conversion not allowed'
      endcase
    endif
  endif else begin
    case destDataType of
      1: res=byte(fix(sourceValue))
      5: res=double(sourceValue)
      4: res=float(sourceValue)
      2: res=fix(sourceValue)
      3: res=long(sourceValue)
      7: res=string(sourceValue)
      12: res=fix(sourceValue);res=fix(sourceValue);
      16: res=byte(fix(sourceValue))
      13: res=ulong(sourceValue)
      14: res=ulong64(sourceValue)
      else:message, 'conversion not allowed'
    endcase
  endelse
  ;if keyword_set(UNSIGNED) and destDataType eq 1 then res=not(res)-1
  return, res

end