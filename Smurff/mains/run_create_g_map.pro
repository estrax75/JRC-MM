;We have software packages for comprehensive inter-comparison analysis,
;but for now we could just start with simple things, like some scatter plots, and basic statistics, like mean bias, ratio, RMSD (for chlorophyll and log10-chlor).
PRO run_create_g_map, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
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
  Aligns=fltarr(roiNo)
  
  roiNameLoc[0,*]=0.0
  roiNameLoc[1,*]=0.02
  Aligns[*]=0.5  

  idx1=where(roiCodeList eq 'BSea', count)
  idx2=where(roiCodeList eq 'LSea', count)
  idx3=where(roiCodeList eq 'MSea', count)
  idx4=where(roiCodeList eq 'Sos', count)
  
  ;Bsea label left shifted
  roiNameLoc[0,idx1]=0.0
  roiNameLoc[1,idx1]=-0.02
  Aligns[idx1]=1.

  ;Lsea label up shifted
  roiNameLoc[0,idx2]=0.02
  roiNameLoc[1,idx2]=0.5
  Aligns[idx2]=1.
  
  ;MSea label right shifted
  roiNameLoc[0,idx3]=0.0
  roiNameLoc[1,idx3]=-0.01
  Aligns[idx3]=1.

  ;Sos label right shifted
  roiNameLoc[0,idx4]=0.0
  roiNameLoc[1,idx4]=-0.01
  Aligns[idx4]=1.
  
  ;;
  zoom = 1
  scale = cgGoogle_MetersPerPixel(zoom)
  xsize = 600 < 640 ; Max size of Google image with this Google API
  ysize = 600 < 640 ; Max size of Google image with this Google API
  ;xsize = 1000 ; Max size of Google image with this Google API
  ;ysize = 1000 ; Max size of Google image with this Google API
  resolution = StrTrim(xsize,2) + 'x' + StrTrim(ysize,2)
  centerLat = 0.0D
  centerLon = 0.0D
  
  ; Gather the Google Map using the Google Static Map API.
  googleStr = "http://maps.googleapis.com/maps/api/staticmap?" + $
    "center=" + StrTrim(centerLat,2) + ',' + StrTrim(centerLon,2) + $
    "&zoom=" + StrTrim(zoom,2) + "&size=" + resolution + $
    "&maptype=terrain&sensor=false&format=png32"
  netObject = Obj_New('IDLnetURL')
  void = netObject -> Get(URL=googleStr, FILENAME="googleimg.png")
  Obj_Destroy, netObject
  googleImage = Read_Image('googleimg.png')
  
  ; Set up the map projection information to be able to draw on top
  ; of the Google map.
  map = Obj_New('cgMap', 'Mercator', ELLIPSOID='WGS 84')
  uv = map -> Forward(centerLon, centerLat)
  uv_xcenter = uv[0,0]
  uv_ycenter = uv[1,0]
  xrange = [uv_xcenter - (xsize/2.0D*scale), uv_xcenter + (xsize/2.0D*scale)]
  yrange = [uv_ycenter - (ysize/2.0D*scale), uv_ycenter + (ysize/2.0D*scale)]
  map -> SetProperty, XRANGE=xrange, YRANGE=yrange
  
  ; Open a window and display the Google Image with a map grid and
  ; location of Coyote's favorite prairie dog restaurant.
  cgDisplay, 950, 768, Aspect=googleImage, Title='Google Image with Coyote Graphics'
  cgImage, googleImage[0:2,*,*], Position=[50, 50, 650, 650]/ 700.0, $
    /Keep_Aspect, OPOS=outputPosition
  ;cgDisplay, 1600, 900, Title='Google Image with Coyote Graphics'
  ;cgImage, googleImage[0:2,*,*], Position=[50, 50, 650, 650]/ 700.0, $
  ;  OPOS=outputPosition
  map -> SetProperty, POSITION=outputPostion
  cgMap_Grid, MAP=map, /BOX_AXES, /cgGRID, FORMAT='(F0.2)'
  ;cgPlotS, -105.1, 40.6, PSYM='filledstar', SYMSIZE=3.0, MAP=map, COLOR='red'
  
  ; Add a label for each site.
  for i=0, n_elements(roiCodeList)-1 do begin
    roiGraphInfoList=mainApp->getROIColorDefinitionByCodes(roiCodeList[i])
    graphicInfo=mainApp->getROIColorDefinitionByCodes(roiCodeList[i])
    geoInfo=mainApp->getROIGeoInfoByCodes(roiCodeList[i])
    
    graphicInfo=strsplit(roiGraphInfoList, ';',/EXTRACT, /PRESERVE)
    roiColor=graphicInfo[0]
    roiLineStyle=graphicInfo[1]
    
    ;cgPlotS, geoInfo.centerLon, geoInfo.centerLat, PSYM='circle', SYMSIZE=3.0, MAP=map, COLOR=roiColor
    cgPlotS, geoInfo.centerLon, geoInfo.centerLat, PSYM='FILLEDCIRCLE', SYMSIZE=1.5, MAP=map, COLOR='dark gray'
    xy = map -> Forward(geoInfo.centerLon, geoInfo.centerLat)
    loc = Convert_Coord(xy[0], xy[1], /Data, /To_Normal)
    shiftX=0.0
    shiftY=0.005
    Align=1.
    if roiCodeList[i] eq 'MSea' then begin
      ;shiftX=0.005
      ;shiftY=0.0
      ;Align=0.
    endif
    if roiCodeList[i] eq 'BSea' then begin
      shiftX=-0.005
      shiftY=0.0
      Align=1.0
    endif
    if roiCodeList[i] eq 'LSea' then begin
      shiftX=0.005
      shiftY=0.0
      Align=0.
    endif
    if roiCodeList[i] eq 'Sos' then begin
      shiftX=0.005
      shiftY=0.0
      Align=0.
    endif
    cgText, loc[0]+shiftX, loc[1]+shiftY, roiCodeList[i], /Normal, $
      Alignment=Align, charsize=2.0, Color='black', Font=0;Color=roiColor, Font=0
      ;Alignment=0.5, charsize=2.0, Color='black', Font=0;Color=roiColor, Font=0
      
  endfor
  
END