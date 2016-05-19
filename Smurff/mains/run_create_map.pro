;We have software packages for comprehensive inter-comparison analysis,
;but for now we could just start with simple things, like some scatter plots, and basic statistics, like mean bias, ratio, RMSD (for chlorophyll and log10-chlor).
PRO run_create_map, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  format=strupcase(mainApp->getKeyValue('OUT_GRAPHIC_FORMAT'))
  if format eq 'PS' or 'EPS' then doPs=1
  if format eq 'BMP' or format eq 'GIF' or format eq 'JPEG' or format eq 'JPG' or format eq 'PNG' or format eq 'TIFF' or format eq 'TIF' then doRaster=1
  format=strlowcase(format)
  
  someColors=['Blue Violet', 'Blue', 'Aquamarine', 'Green Yellow', 'Yellow', 'Orange', 'Orange Red', 'Red']
  someLineStyles=intarr(2, /NOZERO)
  someLineStyles[0]=0
  someLineStyles[1]=2
  
  utility=mainApp->getUtility()
  ignoreValue=float(mainApp->getKeyValue('NAN_VALUE'))
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  compareValidityThreshold=float(mainApp->getKeyValue('COMPARE_VALIDITY_THRESHOLD'))
  dataSetValidityThreshold=float(mainApp->getKeyValue('DATASET_VALIDITY_THRESHOLD'))
  
  if mainApp->isTrue(mainApp->getKeyValue('WAVELENGHT_XTICKS_CUSTOM_MULTIBAND_GRAPH')) then begin
    xWaveTick=mainApp->getKeyValue('WAVELENGHT_XTICKS_MULTIBAND_GRAPH')
    if xWaveTick ne '' then xWaveTicks=utility->stringListToArray(xWaveTick, separator=';', /STRING)
  endif
  
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  ;order by display names
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByDisplayName(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getRoiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
  outputDir=request->getOutputDir()
  roiNo=n_elements(roiCodeList)
  roiNameLoc=fltarr(2,roiNo)
  fileName='pool_spot'
  
  
  if keyword_set(doPS) then cgPS_Open, request->getOutputDir()+path_sep()+fileName+'.'+format 
  
  cgMap_Set, /Mercator, Limit=[-90, -180, 90.0, 180.0]
  cgMap_Continents, Background='snow', /Continents, /Fill, Color='Light Gray'
  cgMap_Continents, Color='navy', /Continents
  cgMap_Grid, MAP=map, /BOX_AXES, /cgGRID, FORMAT='(F0.2)'
  
  ; Add a label for each site.
  for i=0, n_elements(roiCodeList)-1 do begin
    roiGraphInfoList=mainApp->getROIColorDefinitionByCodes(roiCodeList[i])
    graphicInfo=mainApp->getROIColorDefinitionByCodes(roiCodeList[i])
    geoInfo=mainApp->getROIGeoInfoByCodes(roiCodeList[i])
    
    graphicInfo=strsplit(roiGraphInfoList, ';',/EXTRACT, /PRESERVE)
    roiColor=graphicInfo[0]
    roiLineStyle=graphicInfo[1]
    
    ;cgPlotS, geoInfo.centerLon, geoInfo.centerLat, PSYM='circle', SYMSIZE=3.0, MAP=map, COLOR=roiColor
    cgPlotS, geoInfo.centerLon, geoInfo.centerLat, PSYM='FILLEDCIRCLE', SYMSIZE=1.5, MAP=map, COLOR='black'
    ;xy = map -> Forward(geoInfo.centerLon, geoInfo.centerLat)
    loc = Convert_Coord(geoInfo.centerLon, geoInfo.centerLat, /Data, /To_Normal)
    shiftX=0.0
    shiftY=0.005
    Align=1.
;    if roiCodeList[i] eq 'MSea' then begin
;      shiftX=0.0
;      shiftY=-0.035
;      Align=0.5
;    endif
    if roiCodeList[i] eq 'MSea' then begin
      shiftX=0.0
      shiftY=-0.035
      Align=0.5
;      shiftX=0.0075
;      shiftY=0.0
;      Align=0.
    endif
    if roiCodeList[i] eq 'NPO' then begin
      shiftX=0.0
      shiftY=0.012
      Align=0.5
    endif
    if roiCodeList[i] eq 'SPG' then begin
      shiftX=0.0
      shiftY=-0.035
      Align=0.5
    endif
    if roiCodeList[i] eq 'EIO' then begin
      shiftX=0.0
      shiftY=-0.035
      Align=0.5
    endif
    if roiCodeList[i] eq 'NAO' then begin
      shiftX=0.0
      shiftY=0.012
      Align=0.5
    endif
    if roiCodeList[i] eq 'ASea' then begin
      shiftX=0.0
      shiftY=-0.035
      Align=0.5
    endif
    if roiCodeList[i] eq 'BSea' then begin
      shiftX=-0.005
      shiftY=0.0
      Align=1.0
    endif
    if roiCodeList[i] eq 'LSea' then begin
      shiftX=0.0
      shiftY=0.012
      Align=0.5
    endif
    if roiCodeList[i] eq 'CSea' then begin
      shiftX=0.0075
      shiftY=0.0
      Align=0.
    endif
    if roiCodeList[i] eq 'SoS' then begin
      shiftX=0.0
      shiftY=-0.035
      Align=0.5
    endif
    if keyword_set(doPS) then charSize=1.0 else charSize=2.0
    cgText, loc[0]+shiftX, loc[1]+shiftY, roiCodeList[i], /Normal, $
      Alignment=Align, charsize=charSize, Color='black', Font=0;Color=roiColor, Font=0
      ;Alignment=0.5, charsize=2.0, Color='black', Font=0;Color=roiColor, Font=0
      
  endfor
  if keyword_set(doPS) then cgPS_Close
  if keyword_set(direct_Print) then directPrint
  if keyword_set(doRaster) then begin 
    img=tvrd(TRUE=1)
    if format eq 'GIF' then img=tvrd() 
    a=dialog_Write_image(img, filename=fileName+'.'+format, path=request->getOutputDir(), type=format)
  endif
  
END