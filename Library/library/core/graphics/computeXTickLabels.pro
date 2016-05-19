FUNCTION computeXTickLabels, sourceTicks, xTickMarksPos, xTickPos=xTickPos, xTickStep=xTickStep

  totalTicksNo=float(n_elements(sourceTicks))
  maxDisplayTicks=12
  if n_elements(xTickStep) ne 0 then begin
    if maxDisplayTicks lt totalTicksNo then ticksToDraw=findgen(totalTicksNo/xTickStep+1)*xTickStep else ticksToDraw=findgen(totalTicksNo)
    exitLabelsNo=n_elements(ticksToDraw)
    xTickLabels=strarr(exitLabelsNo)
  endif else begin
    if (n_elements(YAXIS) gt 40) then ticksToDraw=findgen(40)*n_elements(internalTNames)/40 else ticksToDraw=findgen(n_elements(y))
  endelse
  if n_elements(xTickMarksPos) eq n_elements(sourceTicks) then begin
    xys=fltarr(2,n_elements(xTickPos))
    xys[0,*]=xTickMarksPos
    xys=convert_coord(xys, /DATA, /TO_NORMAL)
    xTicksPos=reform(xys[0,*])
    xTickLabels=sourceTicks
  endif else begin
    ;exitLabelsNo=n_elements(sourceTicks)
    xTickLabels=strarr(exitLabelsNo)
    for i=0, exitLabelsNo-1 do xTickLabels[i]=getXTickName(axis, round(ticksToDraw[i]), ticksToDraw[i])
    xTickspos = !x.s[0] + !x.s[1]*ticksToDraw
  endelse
 
  validIdxs=where(xTickspos ge !x.crange[0] and xTickspos le !x.crange[1], count)
  if count gt 0 then begin
    xTicksPos=xTicksPos[validIdxs]
    xTickLabels=xTickLabels[validIdxs]
  endif

  return,  xTickLabels
  
END