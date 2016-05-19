FUNCTION do_single_scatter_M, dataList, $
    month, year, roiCode, roiArchiveCode, inputDir, inputFilter, outputDir, $
    maintitle, xtitle, ytitle, $
    NOTFOUND=NOTFOUND, overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
    LOGAXIS=LOGAXIS, day=day, ONEONEFITLINE=ONEONEFITLINE, SIMPLEFIT=SIMPLEFIT, $
    SCATTERGRAPHICFORMAT=SCATTERGRAPHICFORMAT, outParCode=outParCode, YRANGE=YRANGE
    
  COMMON smurffCB, mainApp
  
  fs=mainApp->getFileSystem()
  ;new
  imgConv=mainApp->buildImageConverter('','')
  domainInfo=mainApp->getRoiInfoByCode(roiCode)
  ;end new
  
  roi=roiArchiveCode
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  if n_elements(maintitle) eq 1 then RAWIMAGEFILENAME=outputDir+path_sep()+strcompress(maintitle, /REMOVE_ALL)+'.'+strlowcase(SCATTERGRAPHICFORMAT)
  fName=fs->getFileNameInfo(RAWIMAGEFILENAME, filePath=filePath, extension=extension)
  fName=fs->removeFileExtension(fName)
  check1FName=filePath+fName+'_gt_2.'+extension
  check2FName=filePath+fName+'_le_2.'+extension
  
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
  ;temporary, for graph
  mainCheckIdxs=where(x ge 0.095 and x le 105. and y ge 0.095 and y le 105., countMainCheck)
  x=x[mainCheckIdxs]
  y=y[mainCheckIdxs]
  ;check1Idxs=where(x gt 2 or y gt 2, countCheck1, complement=check2Idxs, ncomplement=countCheck2)
  check1Idxs=where(x gt 2, countCheck1, complement=check2Idxs, ncomplement=countCheck2)
  if countCheck1 ne 0 then begin
    check1X=x[check1Idxs]
    check1Y=y[check1Idxs]
  endif else begin
    doLog, 'Not enough valid data in the second data set (Nan?)', LEVEL=4
    return, 0
  endelse
  
  if countCheck2 ne 0 then begin
    check2X=x[check2Idxs]
    check2Y=y[check2Idxs]
  endif else begin
    doLog, 'Not enough valid data in the second data set (Nan?)', LEVEL=4
    return, 0
  endelse
  ;end temporary
  
  minX=min(x, max=maxX)
  minY=min(y, max=maxY)
  ;dataMin=max([minY, minX])
  ;dataMax=min([maxY, maxX])
  dataMax=max([maxY, maxX])
  dataMin=min([minY, minX])
  nElem=n_elements(x)
  xRange=[0.1,100.]
  ;YRange=[0.1,100.]
  if n_elements(YRange) ne 2 then YRange=[0.1,100.]
  if nElem gt 5 then begin
    ;pixelsLabel='-'+strcompress(nElem, /REMOVE)+' pixels '+'!C(main prod: '+strcompress(countDS1)+'px, 2nd prod: '+strcompress(countDS2)+'px)'
    pixelsLabel='-'+strcompress(nElem, /REMOVE)+' pixels '+'!C(Chla -reg-: '+strcompress(countDS1)+'pxs, Chla -glob-: '+strcompress(countDS2)+'pxs)'
    ;xtitleLabel='(main prod)' ; xtitleLabel='(1st prod)'
    ;ytitleLabel='(2nd prod)'
    ;if n_elements(maintitle) eq 1 then mainTitle=mainTitle+pixelsLabel else mainTitle=pixelsLabel
    ;if n_elements(xtitle) eq 1 then xtitle=xtitle+xtitleLabel else xtitle=xtitleLabel
    ;if n_elements(ytitle) eq 1 then ytitle=ytitle+ytitleLabel else ytitle=ytitleLabel
    mapXColors=imgConv->mapDataColor(x, outParCode, domainInfo, colorTable=colorTable)
    mapYColors=imgConv->mapDataColor(y, outParCode, domainInfo, colorTable=colorTable)
    
    cgScatter2D, x, y, XRange=XRange, YRange=YRange, $;xrange=[dataMin, dataMax], yrange=[dataMin, dataMax], $
      xtitle=xtitle, ytitle=ytitle, title=maintitle, fthick=0.8, $
      ONEONEFITLINE=ONEONEFITLINE, XLOG=LOGAXIS, YLOG=LOGAXIS, SIMPLEFIT=SIMPLEFIT, /COEFFICIENT, /NOFIT, /nodisplay, $
      output=SCATTERGRAPHICFORMAT, OutFilename=RAWIMAGEFILENAME, /RMSD, /BIAS, /SCAT, /REVERTAXISSTAT, mapXColors=mapXColors, colorTable=colorTable, $
      POSITION=[0.15,0.18,0.92,0.8], charsize=1.3, charthick=0.8
      ;output=SCATTERGRAPHICFORMAT, OutFilename=RAWIMAGEFILENAME, /RMSD, /BIAS, /SCAT, /REVERTAXISSTAT, mapYColors=mapYColors, colorTable=colorTable
      
    if countCheck1 gt 0 then begin
    
      minX=min(check1X, max=maxX)
      minY=min(check1Y, max=maxY)
      ;dataMin=max([minY, minX])
      ;dataMax=min([maxY, maxX])
      dataMax=max([maxY, maxX])
      dataMin=min([minY, minX])
      ; test le 2 values
      ;mapXColors=imgConv->mapDataColor(check1X, outParCode, domainInfo, colorTable=colorTable)
      ;mapYColors=imgConv->mapDataColor(check1Y, outParCode, domainInfo, colorTable=colorTable)

;      cgScatter2D, check1X, check1Y, XRange=XRange, YRange=YRange, $;xrange=[dataMin, dataMax], yrange=[dataMin, dataMax], $
;        xtitle=xtitle, ytitle=ytitle, title=maintitle + '!C ('+strcompress(countCheck1, /REMOVE)+' pxs -where Chla gt 2-)', fthick=2., $
;        ONEONEFITLINE=ONEONEFITLINE, XLOG=LOGAXIS, YLOG=LOGAXIS, SIMPLEFIT=SIMPLEFIT, /COEFFICIENT, /NOFIT, /nodisplay, $
;        output=SCATTERGRAPHICFORMAT, OutFilename=check1FName, /BIAS, /RMSD, /SCAT, /REVERTAXISSTAT, mapXColors=mapXColors, colorTable=colorTable
;        ;output=SCATTERGRAPHICFORMAT, OutFilename=check1FName, /BIAS, /RMSD, /SCAT, /REVERTAXISSTAT, mapYColors=mapYColors, colorTable=colorTable
        
    endif
    
    if countCheck2 gt 0 then begin

      minX=min(check2X, max=maxX)
      minY=min(check2Y, max=maxY)
      ;dataMin=max([minY, minX])
      ;dataMax=min([maxY, maxX])
      dataMax=max([maxY, maxX])
      dataMin=min([minY, minX])
      ; test gt 2 values
;      mapXColors=imgConv->mapDataColor(check2X, outParCode, domainInfo, colorTable=colorTable)
;      mapYColors=imgConv->mapDataColor(check2X, outParCode, domainInfo, colorTable=colorTable)
;
;      cgScatter2D, check2X, check2Y, XRange=XRange, YRange=YRange, $;xrange=[dataMin, dataMax], yrange=[dataMin, dataMax], $
;        xtitle=xtitle, ytitle=ytitle, title=maintitle + '!C ('+strcompress(countCheck2, /REMOVE)+' pxs -where Chla le 2-)', fthick=2., $
;        ONEONEFITLINE=ONEONEFITLINE, XLOG=LOGAXIS, YLOG=LOGAXIS, SIMPLEFIT=SIMPLEFIT, /COEFFICIENT, /nodisplay, /NOFIT, $
;        output=SCATTERGRAPHICFORMAT, OutFilename=check2FName, /BIAS, /RMSD, /SCAT, /REVERTAXISSTAT, mapXColors=mapXColors, colorTable=colorTable
;        ;output=SCATTERGRAPHICFORMAT, OutFilename=check2FName, /BIAS, /RMSD, /SCAT, /REVERTAXISSTAT, mapYColors=mapYColors, colorTable=colorTable
        
    endif
    
    return, 1
  endif
  doLog, 'Not enough data (lt 5 pixels)', LEVEL=4
  return, 0
;cgScatter2D, x, y, xrange=[dataMin, dataMax], yrange=[dataMin, dataMax], /coefficient, XLOG=XLOG, YLOG=YLOG
;a=ScatterPlot(alog10(x) , y , xrange=[dataMin, dataMax], yrange=[dataMin, dataMax], /coefficient)
  
  
END