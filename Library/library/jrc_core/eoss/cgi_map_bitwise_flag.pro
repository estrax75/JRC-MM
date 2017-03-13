;FUNCTION cgi_map_bitwise_flag, statusmap, bitposition
;
;  return, BYTE((statusmap AND (2UL^bitposition))/(2UL^bitposition))
;
;END

FUNCTION cgi_map_bitwise_flag, statusmap, bitposition, bln32bit=bln32bit

  if KEYWORD_SET(bln32bit) then begin
    ; 32 bit
    strResult=strtrim(string(ulong(statusmap),format='(B)'),1)
    return,UINT(strmid(strResult,bitposition,1))
  endif else begin
    ; 16 bit
    return, BYTE((statusmap AND (2UL^bitposition))/(2UL^bitposition))
  endelse

END
