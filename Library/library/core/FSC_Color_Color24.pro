FUNCTION FSC_Color_Color24, color

  ; This FUNCTION accepts a [red, green, blue] triple that
  ; describes a particular color and returns a 24-bit long
  ; integer that is equivalent to (can be decomposed into)
  ; that color. The triple can be either a row or column
  ; vector of 3 elements or it can be an N-by-3 array of
  ; color triples.

  ON_ERROR, 2
  
  s = Size(color)
  
  IF s[0] EQ 1 THEN BEGIN
    IF s[1] NE 3 THEN Message, 'Input color parameter must be a 3-element vector.'
    RETURN, color[0] + (color[1] * 2L^8) + (color[2] * 2L^16)
  ENDIF ELSE BEGIN
    IF s[2] GT 3 THEN Message, 'Input color parameter must be an N-by-3 array.'
    RETURN, color[*,0] + (color[*,1] * 2L^8) + (color[*,2] * 2L^16)
  ENDELSE
  
END ;--------------------------------------------------------------------------------------------
