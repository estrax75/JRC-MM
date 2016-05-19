PRO drawLabels, sourceTicks, charsize=charsize, charthick=charthick, XAXIS=XAXIS, YAXIS=YAXIS, xTickStep=xTickStep, xTickPos=xTickPos, $
 ORIENTATION=ORIENTATION, ALIGNEMENT=ALIGNEMENT, NOERASE=NOERASE, XDATASHIFT=XDATASHIFT, NoThickLine=NoThickLine

  if n_elements(ORIENTATION) eq 1 then orientationValue=ORIENTATION else orientationValue=90
  if n_elements(ALIGNEMENT) eq 1 then alignValue=ALIGNEMENT else alignValue=1.0
  ; a list return!!!
  checkContents=strsplit(sourceTicks, '-', /EXTRACT, /PRESERVE)
  
  shiftRight=0
  for i=0, n_elements(checkContents)-1 do begin
    thisEl=checkContents[i]
    if thisEl[0] eq '01' then begin
      shiftRight=i
      break
    endif
  endfor
  ;if count gt 0 then rightShift=idxs[0] else rightShift=0  
  
  totalTicksNo=float(n_elements(sourceTicks))
  maxDisplayTicks=12
  if n_elements(xTickStep) ne 0 then begin
    if maxDisplayTicks lt totalTicksNo then ticksToDraw=findgen(totalTicksNo/xTickStep+1)*xTickStep else ticksToDraw=findgen(totalTicksNo)
    ticksToDraw=ticksToDraw+shiftRight
    exitLabelsNo=n_elements(ticksToDraw)
    xTickLabels=strarr(exitLabelsNo)
  endif else begin
    if (n_elements(YAXIS) gt 40) then ticksToDraw=findgen(40)*n_elements(internalTNames)/40 else ticksToDraw=findgen(n_elements(y))
  endelse
  if keyword_set(XAXIS) then begin
    ;xDataShiftNorm=0
    ;if n_elements(XDATASHIFT) eq 1 then begin
    ;  xDataShiftNorm=convert_coord([XDATASHIFT,0,0], /DATA, /NORM)
    ;  xDataShiftNorm=xDataShiftNorm[0]
    ;endif
    if n_elements(xTickPos) eq n_elements(sourceTicks) then begin
      xys=fltarr(2,n_elements(xTickPos))
      xys[0,*]=xTickPos
      xys=convert_coord(xys, /DATA, /TO_NORMAL)
      xTicksPos=reform(xys[0,*])
      xTickLabels=sourceTicks
    endif else begin
      ;exitLabelsNo=n_elements(sourceTicks)
      xTickLabels=strarr(exitLabelsNo)
      for i=0, exitLabelsNo-1 do xTickLabels[i]=getXTickName(axis, round(ticksToDraw[i]), ticksToDraw[i])
      ;ticksToDraw=ticksToDraw+xDataShift
      xTickspos = !x.s[0] + !x.s[1]*ticksToDraw
    endelse
    ;xTickspos=xTickspos+xDataShiftNorm
    ccrange=fltarr(2,2)
    ccrange[*,0]=[!x.crange[0], 0]
    ccrange[*,1]=[!x.crange[1], 0]
    cc=convert_coord(ccrange, /DATA, /TO_NORM)
    
    ;validIdxs=where(xTickspos ge cc[0,0] and xTickspos le cc[0,1], count)
    validIdxs=where(xTickspos le cc[0,1], count)
    if count gt 0 then begin
      xTicksPos=xTicksPos[validIdxs]
      xTickLabels=xTickLabels[validIdxs]
    endif
    ; GET THE CHARACTER HEIGHT AND WIDTH IN NORMALIZED COORDINATES...
    textChSize= charsize
    y_ch_size = textChSize * float(!d.y_ch_size) / !d.y_vsize
    x_ch_size = textChSize * float(!d.x_ch_size) / !d.x_vsize
    ; PUT THE LABELS AT THE APPROPRIATE TICK POSITIONS...
    if orientationValue eq 0 then yPos=!y.window[0]-1.75*y_ch_size else yPos=!y.window[0]-0.75*y_ch_size
    
    ys=[!y.window[0], ypos]
    
    if ~keyword_set(NoThickLine) then for i=0, n_elements([xTicksPos])-1 do cgPlots, [xTicksPos[i], xTicksPos[i]], ys, thick=2.0, /NORM 
    
    cgText, xTicksPos+0.5*x_ch_size, yPos, /NORM, charsize=textChSize, charthick=charthick, $
      xTickLabels, ALIGN=alignValue, ORIENTATION=orientationValue, FONT=0
    
    ; Thick signs
    ;minusS=strarr(n_elements(xTickLabels))
    ;minusS[*]='-_+'
    ;cgText, xTicksPos+0.5*x_ch_size, yPos, /NORM, charsize=1.5, charthick=0.8, $
    ;  minusS, ALIGN=1.0, ORIENTATION=90, TT_FONT='Helvetica*12'
    ;xyouts, xtickpos+0.5*x_ch_size, !y.window[0]-0.5*y_ch_size, /NORM, $
    ;  xTickLabels, ALIGN=1.0, ORIENTATION=90.0, CHARSIZE=charsize
  endif
  
END