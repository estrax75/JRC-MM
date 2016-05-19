FUNCTION buildLego, centerX, yTop, xWIDTH, yBottom

  if n_elements(yBottom) eq 0 then yBottom=0
  lego=fltarr(2,5)
  lego[*,0]=[centerX-xWIDTH, yBottom]
  lego[*,1]=[centerX+xWIDTH, yBottom]
  lego[*,2]=[centerX+xWIDTH, yTop]
  lego[*,3]=[centerX-xWIDTH, yTop]
  lego[*,4]=[centerX-xWIDTH, yBottom]
  return, lego
  
END
