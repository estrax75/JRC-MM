PRO drawSideTitle, text, position, ALIGNMENT=ALIGNMENT, CHARSIZE=CHARSIZE, ORIENTATION=ORIENTATION, COLOR=COLOR

  ;data=findgen(10)
  ;cgPlot, data, POSITION=position, /NODATA, /NOERASE
  ;cgText, 0.5, 0.5, text, ALIGNMENT=ALIGNMENT, charSize=charSize, color=color, ORIENTATION=ORIENTATION, /NORM, POSITION=POSITION
  ;iperNormPos=[position[2]-position[0], position[3]-position[1]]
  location=[0.5,0.5]
  finalLocation=[0.0,0.5]
  cgText, finalLocation[0], finalLocation[1], text, ALIGNMENT=ALIGNMENT, charSize=charSize, color=color, $
    CharThick=2.0, ORIENTATION=ORIENTATION, /NORM;, POSITION=POSITION
  
END