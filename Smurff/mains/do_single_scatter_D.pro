FUNCTION do_single_scatter_D, dataList, $
    month, year, roiCode, roiArchiveCode, inputDir, inputFilter, outputDir, $
    maintitle, xtitle, ytitle, $
    NOTFOUND=NOTFOUND, overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    LOGAXIS=LOGAXIS, day=day, ONEONEFITLINE=ONEONEFITLINE, SIMPLEFIT=SIMPLEFIT, $
    SCATTERGRAPHICFORMAT=SCATTERGRAPHICFORMAT, YRANGE=YRANGE
    
  COMMON smurffCB, mainApp
  
  roi=roiArchiveCode
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  if n_elements(maintitle) eq 1 then RAWIMAGEFILENAME=outputDir+path_sep()+strcompress(maintitle, /REMOVE_ALL)+'.'+strlowcase(SCATTERGRAPHICFORMAT)

  x=*dataList[0]
  y=*dataList[1]
  validIdx=where(finite (x) eq 1, countDS1)
  if countDS1 ne 0 then begin
    x=x[validIdx]
    y=y[validIdx]
  endif else begin
    doLog, 'Not enough valid data in the first data set (Nan?)', LEVEL=4
    return, 0
  endelse
  validIdx=where(finite (y) eq 1, countDS2)
  if countDS2 ne 0 then begin
    x=x[validIdx]
    y=y[validIdx]
  endif else begin
    doLog, 'Not enough valid data in the second data set (Nan?)', LEVEL=4
    return, 0
  endelse
  minX=min(x, max=maxX)
  minY=min(y, max=maxY)
  ;dataMin=max([minY, minX])
  ;dataMax=min([maxY, maxX])
  dataMax=max([maxY, maxX])
  dataMin=min([minY, minX])
  nElem=n_elements(x)
  if nElem gt 5 then begin
    pixelsLabel='-'+strcompress(nElem, /REMOVE)+' pixels '+'!C(main prod: '+strcompress(countDS1)+'px, 2nd prod: '+strcompress(countDS2)+'px)'
    xtitleLabel='(main prod)' ; xtitleLabel='(1st prod)'
    ytitleLabel='(2nd prod)'
    if n_elements(maintitle) eq 1 then mainTitle=mainTitle+pixelsLabel else mainTitle=pixelsLabel
    if n_elements(xtitle) eq 1 then xtitle=xtitle+xtitleLabel else xtitle=xtitleLabel
    if n_elements(ytitle) eq 1 then ytitle=ytitle+ytitleLabel else ytitle=ytitleLabel
    cgScatter2D, x, y, xrange=[dataMin, dataMax], yrange=[dataMin, dataMax], $
    xtitle=xtitle, ytitle=ytitle, title=maintitle, fthick=2., $
    ONEONEFITLINE=ONEONEFITLINE, XLOG=LOGAXIS, YLOG=LOGAXIS, SIMPLEFIT=SIMPLEFIT, /COEFFICIENT, /NOFIT, $
    output=SCATTERGRAPHICFORMAT, OutFilename=RAWIMAGEFILENAME
    return, 1
  endif
  doLog, 'Not enough data (lt 5 pixels)', LEVEL=4
  return, 0 
  ;cgScatter2D, x, y, xrange=[dataMin, dataMax], yrange=[dataMin, dataMax], /coefficient, XLOG=XLOG, YLOG=YLOG
  ;a=ScatterPlot(alog10(x) , y , xrange=[dataMin, dataMax], yrange=[dataMin, dataMax], /coefficient)
  
  
END