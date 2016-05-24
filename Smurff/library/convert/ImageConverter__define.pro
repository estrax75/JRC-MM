; .run ~/OC/IDL/setScale
; .run ~/OC/IDL/DefineSDomain
; .run ~/OC/IDL/DefineADomain
; .run ~/OC/IDL/setScale
@addGraphicColorTable.pro
FUNCTION ImageConverter::convertFactorCodeToFactorSample, factorCode, NOTFOUND=NOTFOUND

  check=where(strupcase(factorCode) eq strupcase(self.availableFactorCodes), count)
  if count eq 1 then return, self.availableFactorSamples[check]
  NOTFOUND=1
  return, -1
  
END

FUNCTION ImageConverter::convertFactorSampleToFactorSample, factorSample, NOTFOUND=NOTFOUND

  check=where(strupcase(factorSample) eq strupcase(self.availableFactorSamples), count)
  if count eq 1 then return, self.availableFactorCodes[check]
  NOTFOUND=1
  return, -1
  
END

FUNCTION ImageConverter::isLutEmbedded

  return, (self.exportLutMode eq 3) or (self.exportLutMode eq 1)
  
END

FUNCTION ImageConverter::isLutExternal

  return, (self.exportLutMode eq 3) or (self.exportLutMode eq 2)
  
END

FUNCTION ImageConverter::getExportLutFileName

  return, self.exportLutFileName
  
END

FUNCTION ImageConverter::getLookUpTableCode

  return, self.lookUpTableCode
  
END

FUNCTION ImageConverter::mapDataColor, dataValues, parCode, domainInfo, colorTable=colorTable

  ilog=2 & ilin=1 & iUserStep=3
  
  colorScaleInfo=self->getScaleInfo(parCode, domainInfo.rangeTypeCode, lookUpTableCode=self.lookUpTableCode)
  ; check without mean word...
  IF ( colorScaleInfo.vScale EQ ilog ) THEN BEGIN
    b = alog10(colorScaleInfo.vLow)
    a = (alog10(colorScaleInfo.vHigh)-b)/250.
  ENDIF
  
  img2=FLOAT(dataValues)
  
  ; Bad values
  ixb = WHERE ( dataValues LE colorScaleInfo.badV, nnb)
  IF ( nnb GT 0 ) THEN img2[ixb] = 1.
  
  ; Good values
  ixg = WHERE ( dataValues GT colorScaleInfo.badV, nng)
  IF ( nng GT 0 AND parCode EQ 'PProd' ) THEN img2[ixg] = img2[ixg]/1000.
  
  ; Values exceeding the range for display
  ixl = WHERE ( img2 LE colorScaleInfo.vLow AND img2 GT colorScaleInfo.badV, nnl )
  ixh = WHERE ( img2 GE colorScaleInfo.vHigh, nnh )
  
  IF ( nnl GT 0 ) THEN img2[ixl] = colorScaleInfo.vLow
  IF ( nnh GT 0 ) THEN img2[ixh] = colorScaleInfo.vHigh
  
  ; Create byte map for display
  IF ( colorScaleInfo.vScale EQ ilog ) THEN bimg = byte( (alog10(img2)-b)/a )
  IF ( colorScaleInfo.vScale EQ ilin ) THEN bimg = byte( 250.*(img2-colorScaleInfo.vLow)/(colorScaleInfo.vHigh-colorScaleInfo.vLow) )
  
  IF ( nnb GT 0 ) THEN bimg[ixb] = self.badImgIndex
  
  dim=size(bimg, /N_DIM)
  ; do it only if data are a 2d grid
  if dim eq 4 then begin
    bimg = reverse(bimg,2)
  endif
  
  ; ########################################################
  ; COLORS
  
  ; read the color palette
  
  IF ( colorScaleInfo.cTableIdx LT 100)  THEN BEGIN
    loadct,colorScaleInfo.cTableIdx
    tvlct,r,g,b,/GET
  ENDIF ELSE BEGIN
    pal_file=self.colorTableDir+path_sep()+colorScaleInfo.cTableFilename
    openr,fileunit,pal_file,/GET_LUN
    r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
    readu,fileunit,r,g,b
    free_lun, fileunit & close, fileunit
  ENDELSE
  colorTable={r:r, g:g, b:b}
  return, bimg
  
END

FUNCTION ImageConverter::isFormatAvailable, format

  idxs=where(self.avFormats eq format, count)
  return, count ne 0
  
END

PRO ImageConverter::processFiles, filelist, varName, varDisplayName, varMeasureUnit, FORMAT=FORMAT, cropLimits=cropLimits 

  NbFiles= n_elements(filelist)
  domainlist=self.roiInfo->getArchiveCodes()
  domainCodeList=self.roiInfo->getAllCodes()
  domainCheckCodeList=strupcase(domainCodeList)
  
  ;NbDomains = n_elements(DomainStr)
  NbDomains = self.roiInfo->getNumberOfElements()
  fs=obj_new('FileSystem', /STAND_ALONE)
  
  for id=0,NbDomains-1 do begin
    fileToConvertList=['']
    for il = 0,NbFiles-1 do begin
    
      filename = filelist[il]
      
      ;dindex = intarr(NbDomains) & dindex[*]=-1
      
      ; Find the domain using the file name
      ;extract only file name
      fname=fs->getFileNameInfo(filename, filePath=filePath, extension=extension)
      ;strupcase of file only for check
      fName=strupcase(fname)
      
      ; try with archivecode
      ii = strpos(fname,domainlist[id])
      
      if ( ii ge 0 ) then begin
        fileToConvertList=[fileToConvertList, filename]
      endif else begin
        ; try with code
        ii = strpos(fname,domainCheckCodeList[id])
        if ( ii ge 0 ) then begin
          fileToConvertList=[fileToConvertList, filename]
        endif
      endelse
      
    endfor
    if n_elements(fileToConvertList) ne 1 then self->createImages, fileToConvertList[1:*], varName, varDisplayName, varMeasureUnit, self.roiInfo->getInfoByCode(domainCodeList[id]), FORMAT=FORMAT, cropLimits=cropLimits
    
  endfor
  obj_destroy, fs
  
END

;PRO ImageConverter::createImages, filelist, varName, varDisplayName, varMeasureUnit, domainInfo, FORMAT=FORMAT
;
;  if keyword_set(FORMAT) then begin
;    if ~self->isFormatAvailable(FORMAT) then message, 'format:'+FORMAT+' not available'
;  endif else begin
;    format=self.avFormats[0]
;  endelse
;
;  ;ibad = 255
;
;  varNamem = varName+'_'+'Mean'
;
;  NbFiles= n_elements(filelist)
;
;  currentdomain = 'none'
;  ;DefineDomain,DomainStr
;  ;domainlist = DomainStr(*).Name
;  domainlist=self.roiInfo->getArchiveCodes()
;
;  ;NbDomains = n_elements(DomainStr)
;  NbDomains = self.roiInfo->getNumberOfElements()
;
;  ; -------------------------------------------------------------------------
;
;  ilin = 1
;  ilog = 2
;  iuserstep = 3
;
;  ; color codes
;  cland = 80
;  ccoast = 250
;  cborder = 10
;  criver = 251
;
;  ;mapdir = '/home/data/OC/masks/'
;
;  ; ###################################################################################
;  ; Loop over the files
;  ;roiInfo=self.roiInfo->getInfoByArchiveCode(basin)
;
;  FOR il = 0,NbFiles-1 DO BEGIN
;
;    filename = filelist[il]
;
;    ;basin = domainlist[domainIndex]
;    basin = domainInfo.archiveCode
;    if (domainInfo.FACTORLIST eq 'N/A') or (domainInfo.FACTORLIST eq '') then factorList=1 else factorList=fix(strsplit(domainInfo.FACTORLIST, ';', /EXTRACT))
;
;    for i=0, n_elements(factorList)-1 do begin
;      check=where(factorList eq self.imageSizes, count)
;      if count eq 1 then
;    endfor
;
;    ; -------------------------------------------------------------------------
;    ; Definition for new domain
;
;    ; large, resampling factor = 1
;    lmap = self.mapDir+path_sep()+basin+'.l.png'
;    if ~((file_info(lmap)).exists and (file_info(lmap)).read) then continue
;    read_png,lmap,lmask
;
;    lland = WHERE ( lmask EQ cland )
;    lcoast = WHERE ( lmask EQ ccoast )
;    lborder = WHERE ( lmask EQ cborder )
;    lriver = WHERE ( lmask EQ criver )
;
;    ; medium, resampling factor = 2
;    mmap = self.mapDir+path_sep()+basin+'.m.png'
;    if ~((file_info(mmap)).exists and (file_info(mmap)).read) then continue
;    read_png,mmap,mmask
;
;    mland = WHERE ( mmask EQ cland )
;    mcoast = WHERE ( mmask EQ ccoast )
;    mborder = WHERE ( mmask EQ cborder )
;    mriver = WHERE ( mmask EQ criver )
;
;    ; small, resampling factor = 4
;    smap = self.mapDir+path_sep()+basin+'.s.png'
;    if ~((file_info(smap)).exists and (file_info(smap)).read) then continue
;    read_png,smap,smask
;
;    sland = WHERE ( smask EQ cland )
;    scoast = WHERE ( smask EQ ccoast )
;    sborder = WHERE ( smask EQ cborder )
;
;    ; medium, resampling factor = 2
;    imap = self.mapDir+path_sep()+basin+'.i.png'
;    if ~((file_info(imap)).exists and (file_info(imap)).read) then continue
;    read_png,imap,imask
;
;    iland = WHERE ( imask EQ cland )
;    icoast = WHERE ( imask EQ ccoast )
;    iborder = WHERE ( imask EQ cborder )
;
;    dim=size(lmask)
;    nxmask=dim[1]
;    nymask=dim[2]
;    vnam = varName
;    ;if self.statType ne '' then dataSetName=varName+'-'+self.statType else dataSetName=varName
;    if self.statType ne '' and self.statType ne 'N/A' then dataSetName=varName+'_'+self.statType else dataSetName=varName
;    IF ( STRPOS(varName,'PProd') GE 0 ) THEN BEGIN
;      vnam = 'PProd'
;      varNamem = varName
;      dataSetName= vnam
;    ENDIF
;    IF ( STRPOS(varName,'l3m_data') GE 0 ) THEN BEGIN
;      vnam = 'par'
;      varNamem = varName
;      dataSetName= vnam
;    ENDIF
;
;    ; Define min/max/scale values for that basin and variable
;    colorScaleInfo=self->getScaleInfo(vnam, domainInfo.rangeTypeCode, lookUpTableCode=self.lookUpTableCode)
;    ;setScale,basin,vnam,vlow,vhig,vunit,vscale,badv,ctable
;    ;colorScaleInfo.VHIGH=20
;    ;colorScaleInfo.VLOW=0.05
;
;    dataInfo=self->readData(filename, dataSetName, STATUS=STATUS)
;    IF ( status EQ 1 and obj_valid(colorScaleInfo)) THEN BEGIN
;
;      img=dataInfo.img
;      IF ( colorScaleInfo.vScale EQ iUserStep ) THEN BEGIN
;        scaleValues=float(strsplit(colorScaleInfo.valueList, ';', /EXTRACT))
;      ;        b = alog10(colorScaleInfo.vLow)
;      ;        a = (alog10(colorScaleInfo.vHigh)-b)/250.
;      ENDIF
;
;      IF ( colorScaleInfo.vScale EQ ilog ) THEN BEGIN
;        b = alog10(colorScaleInfo.vLow)
;        a = (alog10(colorScaleInfo.vHigh)-b)/250.
;      ENDIF
;
;      img2=FLOAT(img)
;
;      ; Bad values
;      ixb = WHERE ( img LE colorScaleInfo.badV, nnb)
;      IF ( nnb GT 0 ) THEN img2[ixb] = 1.
;
;      ; Good values
;      ixg = WHERE ( img GT colorScaleInfo.badV, nng)
;      IF ( nng GT 0 AND vnam EQ 'PProd' ) THEN img2[ixg] = img2[ixg]/1000.
;
;      ; Values exceeding the range for display
;      ixl = WHERE ( img2 LE colorScaleInfo.vLow AND img2 GT colorScaleInfo.badV, nnl )
;      ixh = WHERE ( img2 GE colorScaleInfo.vHigh, nnh )
;
;      IF ( nnl GT 0 ) THEN img2[ixl] = colorScaleInfo.vLow
;      IF ( nnh GT 0 ) THEN img2[ixh] = colorScaleInfo.vHigh
;
;      ; Create byte map for display
;      IF ( colorScaleInfo.vScale EQ iUserStep ) THEN begin
;        bimg = byte(img2)
;        colorIdx=0b
;        idxs=where(img2 lt scaleValues[0], count)
;        if count ne 0 then bimg[idxs]=colorIdx
;        for colorIdx=1b, n_elements(scaleValues)-1 do begin
;          idxs=where(img2 ge scaleValues[colorIdx-1] and img2 lt scaleValues[colorIdx], count)
;          if count ne 0 then bimg[idxs]=colorIdx
;        endfor
;        idxs=where(img2 ge scaleValues[colorIdx-1], count)
;        if count ne 0 then bimg[idxs]=colorIdx
;      ENDIF
;      IF ( colorScaleInfo.vScale EQ ilog ) THEN bimg = byte( (alog10(img2)-b)/a )
;      IF ( colorScaleInfo.vScale EQ ilin ) THEN bimg = byte( 250.*(img2-colorScaleInfo.vLow)/(colorScaleInfo.vHigh-colorScaleInfo.vLow) )
;
;      IF ( nnb GT 0 ) THEN bimg[ixb] = self.badImgIndex
;
;      bimg = reverse(bimg,2)
;      dim=size(bimg)
;      nx=dim[1]
;      ny=dim[2]
;
;      IF ( nx NE nxmask OR ny NE nymask ) THEN BEGIN
;        doLog, 'Inconsistent dimensions!!', level=1
;        doLog, 'data called: ', dataSetName, ' in ', filename, level=1
;        doLog, 'skip', level=1
;        continue
;      ENDIF
;
;      ; Geographical domain
;      ;latmin=limit[0]
;      ;latmax=limit[2]
;      ;lonmin=limit[1]
;      ;lonmax=limit[3]
;
;      latdel=5.
;      londel=5.
;
;
;      ; ########################################################
;      ; COLORS
;
;      ; read the color palette
;
;      IF ( colorScaleInfo.cTableIdx LT 100)  THEN BEGIN
;        loadct,colorScaleInfo.cTableIdx
;        tvlct,r,g,b,/GET
;      ENDIF ELSE BEGIN
;        pal_file=self.colorTableDir+path_sep()+colorScaleInfo.cTableFilename
;        openr,fileunit,pal_file,/GET_LUN
;        r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
;        readu,fileunit,r,g,b
;        free_lun, fileunit & close, fileunit
;      ENDELSE
;
;
;      ; color definition for bad value : black
;      cbad=0
;      r[255]=cbad
;      g[255]=cbad
;      b[255]=cbad
;      cbad=255
;
;      ; color definition for land : grey
;      cland2=120
;      r[254]=cland2
;      g[254]=cland2
;      b[254]=cland2
;      cland2=254
;
;      ; color definition for coasts : grey
;      ccoast2=80
;      r[253]=ccoast2
;      g[253]=ccoast2
;      b[253]=ccoast2
;      ccoast2=253
;
;      ; color definition for borders : grey
;      cborder2=160
;      r[252]=cborder2
;      g[252]=cborder2
;      b[252]=cborder2
;      cborder2=252
;
;      ; color definition for rivers : black
;      r[251]=0
;      g[251]=0
;      b[251]=255
;      criver2=251
;
;      ; ########################################################
;
;      if self.outputDir ne '' then begin
;        lastPos=strpos(fileName, path_sep(), /REVERSE_SEARCH)
;        ofile=self.outputDir+strmid(fileName, lastPos)
;      endif else begin
;        ofile=filename
;      endelse
;
;      ;IF ( varName EQ 'chlor_a' OR vnam EQ 'PProd' ) THEN BEGIN
;      if colorScaleInfo.vScale EQ ilog then stepType='log' else stepType='lin'
;      IF ( varName EQ 'chlor_a' ) THEN BEGIN
;        ofilel  =ofile+'.l.png'
;        ofilem  =ofile+'.m.png'
;        ofiles  =ofile+'.s.png'
;        ofilei  =ofile+'.i.png'
;      ;ofilect =ofile+'_'+stepType+'_min_'+strcompress(colorScaleInfo.vLow, /REMOVE_ALL)+'max_'+strcompress(colorScaleInfo.vHigh, /REMOVE_ALL)+'.ct.png'
;      ENDIF else begin
;        ofilel  =ofile+'.'+varName+'.l.png'
;        ofilem  =ofile+'.'+varName+'.m.png'
;        ofiles  =ofile+'.'+varName+'.s.png'
;        ofilei  =ofile+'.'+varName+'.i.png'
;        ;ofilect =ofile+'.'+varName+'_'+stepType+'_min_'+strcompress(colorScaleInfo.vLow, /REMOVE_ALL)+'_max_'+strcompress(colorScaleInfo.vHigh, /REMOVE_ALL)+'.ct.png'
;        ofilect =ofile+'_'+varName+'_ct.png'
;      endelse
;      if self.exportLutFileName ne '' then ofilect=self.outputDir+self.exportLutFileName+'.png' else ofilect =ofile+'_'+varName+'_ct.png'
;      ; Sub-dimensions
;      nxx1=nx/factorList[0]
;      nyy1=ny/factorList[0]
;      dnx1 = nx mod factorList[0]
;      dny1 = ny mod factorList[0]
;
;      nxx2=nx/factorList[1]
;      nyy2=ny/factorList[1]
;      dnx2 = nx mod factorList[1]
;      dny2 = ny mod factorList[1]
;
;      nxx3=nx/factorList[2]
;      nyy3=ny/factorList[2]
;      dnx3 = nx mod factorList[2]
;      dny3 = ny mod factorList[2]
;
;      if ( factorList[0] GT 0 ) then tab1=bytarr(nxx1,nyy1)
;      if ( factorList[1] GT 0 ) then tab2=bytarr(nxx2,nyy2)
;      if ( factorList[2] GT 0 ) then tab3=bytarr(nxx3,nyy3)
;
;
;      ; lower size image
;      if ( factorList[0] GT 0 ) then begin
;        ; 1
;
;        for i=0,nxx1-1 do begin
;          for j=0,nyy1-1 do begin
;            tab1[i,j]=bimg[factorList[0]*i,factorList[0]*j]
;          endfor
;        endfor
;      endif
;
;      if ( factorList[1] GT 0 ) then begin
;        ; 2
;        for i=0,nxx2-1 do begin
;          for j=0,nyy2-1 do begin
;            tab2[i,j]=bimg[factorList[1]*i,factorList[1]*j]
;          endfor
;        endfor
;      endif
;
;      if ( factorList[2] GT 0 ) then begin
;        ; 3
;        for i=0,nxx3-1 do begin
;          for j=0,nyy3-1 do begin
;            tab3[i,j]=bimg(factorList[2]*i,factorList[2]*j)
;          endfor
;        endfor
;      endif
;
;      ; ---------------------------------------------------------------------
;      ; Apply masking colors
;
;
;      ixl = WHERE ( bimg EQ self.badImgIndex AND lmask EQ cland, nnl)
;      IF ( nnl GT 0 ) THEN bimg[ixl] = cland2
;
;
;      IF ( lcoast[0] NE -1 ) THEN bimg[lcoast] = ccoast2
;      IF ( lborder[0] NE -1 ) THEN bimg[lborder] = cborder2
;      IF ( lriver[0] NE -1 ) THEN bimg[lriver] = criver2
;
;      ixm = WHERE ( tab1 EQ self.badImgIndex AND mmask EQ cland, nnm)
;      IF ( nnm GT 0 ) THEN tab1[ixm] = cland2
;
;      IF ( mcoast[0] NE -1 ) THEN tab1[mcoast] = ccoast2
;      IF ( mborder[0] NE -1 ) THEN tab1[mborder] = cland2 ; cborder2
;      IF ( mriver[0] NE -1 ) THEN tab1[mriver] = criver2
;
;      ixs = WHERE ( tab2 EQ self.badImgIndex AND smask EQ cland, nns)
;      IF ( nns GT 0 ) THEN tab2[ixs] = cland2
;
;      IF ( scoast[0] NE -1 ) THEN tab2[scoast] = ccoast2
;      IF ( sborder[0] NE -1 ) THEN tab2[sborder] = cborder2
;
;      ixi = WHERE ( tab3 EQ self.badImgIndex AND imask EQ cland, nni)
;      IF ( nni GT 0 ) THEN tab3[ixi] = cland2
;
;      IF ( icoast[0] NE -1 ) THEN tab3[icoast] = ccoast2
;
;
;      ; ---------------------------------------------------------------------
;      ; ---------------------------------------------------------------------
;      ; Create the images
;      ; ---------------------------------------------------------------------
;
;      ; Map Projection
;      i_stereographic = 1 ; Stereographic
;      i_orthographic = 2 ; Orthographic
;      i_lambertconic = 3 ;  Lambert's Conic
;      i_lambert = 4 ; Lambert
;      i_gnomic = 5 ; Gnomic
;      i_azimuthal = 6 ; Azimuthal
;      i_satellite = 7 ; Satellite
;      i_cylindrical = 8 ; Cylindrical
;      i_mercator = 9 ; Mercator
;      i_mollweide = 10 ; Mollweide
;      i_sinusoidal = 11 ; Sinusoidal
;      i_aitoff = 12 ; Aitoff
;      i_hammer_aitoff = 13 ; Hammer-Aitoff
;      i_albertconic = 14 ; Alber's Conic
;      i_transversemercator = 15 ; Transverse Mercator
;      i_millercylindrical = 16 ; Miller Cylindrical
;      i_robinson = 17 ; Robinson
;
;      ; ---------------------------------------------------------------------
;
;      ;IF ( iprog EQ  i_cylindrical ) THEN $
;      ;MAP_SET,  /cyl,/noerase,/noborder, xmargin=0, ymargin=0,limit=[latmin,lonmin,latmax,lonmax]
;
;
;      ; color table
;      ; 4
;
;
;      write_png,ofilem,finalImage,r,g,b
;      doLog, 'test integrated lut:', ofilem, LEVEL=4
;
;      ; lower size image
;      ; 1
;      if self.imageSizes[0] then begin
;        if self.exportLutMode gt 0 then lutEmbeddedImage=addGraphicColorTable(tab2, 250, colorScaleInfo.vLow, colorScaleInfo.vHigh, 252, 255, LOGSCALE=colorScaleInfo.vScale eq ilog, colorStripe=colorStripe, title=[varDisplayName, varMeasureUnit])
;        if self->isLutEmbedded() then ofilem=lutEmbeddedImage
;        write_png,ofilem,tab1,r,g,b
;      endif
;
;      ; 2
;      if self.imageSizes[1] then begin
;        if self.exportLutMode gt 0 then lutEmbeddedImage=addGraphicColorTable(tab2, 250, colorScaleInfo.vLow, colorScaleInfo.vHigh, 252, 255, LOGSCALE=colorScaleInfo.vScale eq ilog, colorStripe=colorStripe, title=[varDisplayName, varMeasureUnit])
;        if self->isLutEmbedded() then ofiles=lutEmbeddedImage
;        write_png,ofiles,tab2,r,g,b
;      endif
;
;      ; 3
;      if self.imageSizes[2] then begin
;        if self.exportLutMode gt 0 then lutEmbeddedImage=addGraphicColorTable(tab3, 250, colorScaleInfo.vLow, colorScaleInfo.vHigh, 252, 255, LOGSCALE=colorScaleInfo.vScale eq ilog, colorStripe=colorStripe, title=[varDisplayName, varMeasureUnit])
;        if self->isLutEmbedded() then ofilei=lutEmbeddedImage
;        write_png,ofilei,tab3,r,g,b
;      endif
;
;      ; 4
;      lutEmbeddedImage=addGraphicColorTable(bimg, 250, colorScaleInfo.vLow, colorScaleInfo.vHigh, 252, 255, LOGSCALE=colorScaleInfo.vScale eq ilog, colorStripe=colorStripe, title=[varDisplayName, varMeasureUnit])
;      if self.imageSizes[3] then begin
;        if self->isLutEmbedded() then bimg=lutEmbeddedImage
;        write_png,ofilel,bimg,r,g,b
;      endif
;      if self->isLutExternal() then writeColorTable, ofilect, colorStripe, r,g,b, 252, 255, labelColorRGB=[10,10,10], backGroundColorRGB=[235,235,235]
;
;    ENDIF
;
;  ENDFOR
;; ###################################################################################
;
;END

PRO ImageConverter::createImages, filelist, varName, varDisplayName, varMeasureUnit, domainInfo, FORMAT=FORMAT, cropLimits=cropLimits

  if keyword_set(FORMAT) then begin
    if ~self->isFormatAvailable(FORMAT) then message, 'format:'+FORMAT+' not available'
  endif else begin
    format=self.avFormats[0]
  endelse
  
  ;ibad = 255
  
  varNamem = varName+'_'+'Mean'
  
  NbFiles= n_elements(filelist)
  
  currentdomain = 'none'
  ;DefineDomain,DomainStr
  ;domainlist = DomainStr(*).Name
  domainlist=self.roiInfo->getArchiveCodes()
  
  ;NbDomains = n_elements(DomainStr)
  NbDomains = self.roiInfo->getNumberOfElements()
  
  ; -------------------------------------------------------------------------
  
  ilin = 1
  ilog = 2
  iuserstep = 3
  
  ; color codes
  cland = 80
  ccoast = 250
  cborder = 10
  criver = 251
  
  ;mapdir = '/home/data/OC/masks/'
  
  ; ###################################################################################
  ; Loop over the files
  ;roiInfo=self.roiInfo->getInfoByArchiveCode(basin)
  
  FOR il = 0,NbFiles-1 DO BEGIN
  
    filename = filelist[il]
    
    ;basin = domainlist[domainIndex]
    basin = domainInfo.archiveCode
    if (domainInfo.FACTORLIST eq 'N/A') or (domainInfo.FACTORLIST eq '') then factorList=1 else factorList=fix(strsplit(domainInfo.FACTORLIST, ';', /EXTRACT))
    
    vnam = varName
    ;if self.statType ne '' then dataSetName=varName+'-'+self.statType else dataSetName=varName
    if self.statType ne '' and self.statType ne 'N/A' then dataSetName=varName+'_'+self.statType else dataSetName=varName
    IF ( STRPOS(varName,'PProd') GE 0 ) THEN BEGIN
      vnam = 'PProd'
      varNamem = varName
      dataSetName= vnam
    ENDIF
    IF ( STRPOS(varName,'l3m_data') GE 0 ) THEN BEGIN
      vnam = 'par'
      varNamem = varName
      dataSetName= vnam
    ENDIF
    
    ; Define min/max/scale values for that basin and variable
    colorScaleInfo=self->getScaleInfo(vnam, domainInfo.rangeTypeCode, lookUpTableCode=self.lookUpTableCode)
    if ~obj_valid(colorScaleInfo) then doLog, 'No entry in colorMapscale for:', vnam, ' ', domainInfo.archiveCode, ' ', domainInfo.rangeTypeCode, LEVEL=4   
    
    dataInfo=self->readData(filename, dataSetName, STATUS=STATUS)
    IF ( status EQ 1 and obj_valid(colorScaleInfo)) THEN BEGIN
    
      img=dataInfo.img
      IF ( colorScaleInfo.vScale EQ iUserStep ) THEN BEGIN
        scaleValues=float(strsplit(colorScaleInfo.valueList, ';', /EXTRACT))
      ;        b = alog10(colorScaleInfo.vLow)
      ;        a = (alog10(colorScaleInfo.vHigh)-b)/250.
      ENDIF
      
      IF ( colorScaleInfo.vScale EQ ilog ) THEN BEGIN
        b = alog10(colorScaleInfo.vLow)
        a = (alog10(colorScaleInfo.vHigh)-b)/250.
      ENDIF
      
      img2=FLOAT(img)
      
      ; Bad values
      ixb = WHERE ( img LE colorScaleInfo.badV, nnb)
      IF ( nnb GT 0 ) THEN img2[ixb] = 1.
      
      ; Good values
      ixg = WHERE ( img GT colorScaleInfo.badV, nng)
      IF ( nng GT 0 AND vnam EQ 'PProd' ) THEN img2[ixg] = img2[ixg]/1000.
      
      ; Values exceeding the range for display
      ixl = WHERE ( img2 LE colorScaleInfo.vLow AND img2 GT colorScaleInfo.badV, nnl )
      ixh = WHERE ( img2 GE colorScaleInfo.vHigh, nnh )
      
      IF ( nnl GT 0 ) THEN img2[ixl] = colorScaleInfo.vLow
      IF ( nnh GT 0 ) THEN img2[ixh] = colorScaleInfo.vHigh
      
      ; Create byte map for display
      IF ( colorScaleInfo.vScale EQ iUserStep ) THEN begin
        bimg = byte(img2)
        colorIdx=0b
        idxs=where(img2 lt scaleValues[0], count)
        if count ne 0 then bimg[idxs]=colorIdx
        for colorIdx=1b, n_elements(scaleValues)-1 do begin
          idxs=where(img2 ge scaleValues[colorIdx-1] and img2 lt scaleValues[colorIdx], count)
          if count ne 0 then bimg[idxs]=colorIdx
        endfor
        idxs=where(img2 ge scaleValues[colorIdx-1], count)
        if count ne 0 then bimg[idxs]=colorIdx
      ENDIF
      IF ( colorScaleInfo.vScale EQ ilog ) THEN bimg = byte( (alog10(img2)-b)/a )
      IF ( colorScaleInfo.vScale EQ ilin ) THEN bimg = byte( 250.*(img2-colorScaleInfo.vLow)/(colorScaleInfo.vHigh-colorScaleInfo.vLow) )
      
      IF ( nnb GT 0 ) THEN bimg[ixb] = self.badImgIndex
      
      bimg = reverse(bimg,2)
      dim=size(bimg)
      nx=dim[1]
      ny=dim[2]
      
      ; Geographical domain
      ;latmin=limit[0]
      ;latmax=limit[2]
      ;lonmin=limit[1]
      ;lonmax=limit[3]
      
      latdel=5.
      londel=5.
      
      
      ; ########################################################
      ; COLORS
      
      ; read the color palette
      
      IF ( colorScaleInfo.cTableIdx LT 100)  THEN BEGIN
        loadct,colorScaleInfo.cTableIdx
        tvlct,r,g,b,/GET
      ENDIF ELSE BEGIN
        pal_file=self.colorTableDir+path_sep()+colorScaleInfo.cTableFilename
        openr,fileunit,pal_file,/GET_LUN
        r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
        readu,fileunit,r,g,b
        free_lun, fileunit & close, fileunit
      ENDELSE
      
      ; ########################################################
      
      if self.outputDir ne '' then begin
        lastPos=strpos(fileName, path_sep(), /REVERSE_SEARCH)
        ofile=self.outputDir+strmid(fileName, lastPos)
      endif else begin
        ofile=filename
      endelse
      
      ;IF ( varName EQ 'chlor_a' OR vnam EQ 'PProd' ) THEN BEGIN
      if colorScaleInfo.vScale EQ ilog then stepType='log' else stepType='lin'
      IF ( varName ne 'chlor_a' ) THEN ofile=ofile+'.'+varName
      ;        ofilect =ofile+'_'+varName+'_ct.png'
      ;      endelse
      if self.exportLutFileName ne '' then ofilect=self.outputDir+self.exportLutFileName+'.png' else ofilect =ofile+'_'+varName+'_ct.png'
      
      ; build a specific sampling domain
      if self.exportSize eq '' then samples=self.availableFactorCodes[3] else samples=strsplit(self.exportSize, ';', /EXTRACT)
      check=where(samples eq self.availableFactorCodes[3], count)
      if count eq 0 and self.exportLutMode eq 3 then samples=[samples, self.availableFactorCodes[3]]
      
      for k=0, n_elements(samples)-1 do begin
      
        extension='.'+samples[k]+'.png'
        thisofile =ofile+extension
        if self.exportLutFileName ne '' then thisofilect=self.outputDir+path_sep()+self.exportLutFileName+'.png' else thisofilect =ofile+'_ct.png'
        thisMap=self.mapDir+path_sep()+basin+extension
        
        
        sampleFactor=self->convertFactorCodeToFactorSample(samples[k])
        
        if ~((file_info(thisMap)).exists and (file_info(thisMap)).read) then continue else doLog, 'sample: '+samples[k]+' not available. No image exported'
        
        read_png,thisMap,thisMask
        
        dim=size(thisMask)
        nxmask=dim[1]
        nymask=dim[2]
        ;check consistency!!!
        
        IF ( nx NE nxmask OR ny NE nymask ) THEN BEGIN
          doLog, 'Inconsistent dimensions!!', level=1
          doLog, 'data called: ', dataSetName, ' in ', filename, level=1
          doLog, 'skip', level=1
          continue
        ENDIF
        
        nxx=(nx/sampleFactor)[0]
        nyy=(ny/sampleFactor)[0]
        dnx = nx mod sampleFactor
        dny = ny mod sampleFactor
        sampledImg=bytarr(nxx,nyy, /NO)
        
        for i=0,nxx-1 do begin
          for j=0,nyy-1 do begin
            sampledImg[i,j]=bimg[sampleFactor*i,sampleFactor*j]
          endfor
        endfor
        thisofile=ofile+extension
        
        sampledImg=self->assignColorsBySamplingType(sampledImg, thisMask, sampleFactor, $
          rgbcbad, Indexcbad, rgbcland2, Indexcland2, rgbccoast2, Indexccoast2, rgbcborder2, Indexcborder2, rgbcriver2, Indexcriver2)
          
        if n_elements(Indexcbad) ne 0 then begin
          r[Indexcbad]=rgbcbad[0] & g[Indexcbad]=rgbcbad[1] & b[Indexcbad]=rgbcbad[2]
        endif
        if n_elements(Indexcland2) ne 0 then begin
          r[Indexcland2]=rgbcland2[0] & g[Indexcland2]=rgbcland2[1] & b[Indexcland2]=rgbcland2[2]
        endif
        if n_elements(Indexccoast2) ne 0 then begin
          r[Indexccoast2]=rgbccoast2[0] & g[Indexccoast2]=rgbccoast2[1] & b[Indexccoast2]=rgbccoast2[2]
        endif
        if n_elements(Indexcborder2) ne 0 then begin
          r[Indexcborder2]=rgbcborder2[0] & g[Indexcborder2]=rgbcborder2[1] & b[Indexcborder2]=rgbcborder2[2]
        endif
        if n_elements(Indexcriver2) ne 0 then begin
          r[Indexcriver2]=rgbcriver2[0] & g[Indexcriver2]=rgbcriver2[1] & b[Indexcriver2]=rgbcriver2[2]
        endif
        
        
        imageToWrite=sampledImg
        if self.exportLutMode gt 0 and sampleFactor lt 4 then begin
          lutEmbeddedImage=addGraphicColorTable(sampledImg, 250, colorScaleInfo.vLow, colorScaleInfo.vHigh, 252, 255, LOGSCALE=colorScaleInfo.vScale eq ilog, colorStripe=colorStripe, title=[varDisplayName, varMeasureUnit])
          if self->isLutEmbedded() then imageToWrite=lutEmbeddedImage
        endif
        if n_elements(cropLimits) eq 4 then a=execute('imageToWrite=imageToWrite['+cropLimits[0]+':'+cropLimits[1]+','+cropLimits[2]+':'+cropLimits[3]+']' )
        write_png, thisofile, imageToWrite, r, g, b
        if self->isLutExternal() and sampleFactor eq 1 then writeColorTable, thisofilect, colorStripe, r,g,b, 252, 255, labelColorRGB=[10,10,10], backGroundColorRGB=[235,235,235]
        
      end
      
      
    ENDIF
    
  ENDFOR
; ###################################################################################
  
END

FUNCTION ImageConverter::assignColorsBySamplingType, sampledImg, mask, sampleFactor, $
    rgbcbad, Indexcbad, rgbcland2, Indexcland2, rgbccoast2, Indexccoast2, rgbcborder2, Indexcborder2, rgbcriver2, Indexcriver2
    
  ;    read_png,lmap,lmask
  ;
  ;    lland = WHERE ( lmask EQ cland )
  ;    lcoast = WHERE ( lmask EQ ccoast )
  ;    lborder = WHERE ( lmask EQ cborder )
  ;    lriver = WHERE ( lmask EQ criver )
  ;
  ;    ; medium, resampling factor = 2
  ;    mmap = self.mapDir+path_sep()+basin+'.m.png'
  ;    if ~((file_info(mmap)).exists and (file_info(mmap)).read) then continue
  ;    read_png,mmap,mmask
  ;
  ;    mland = WHERE ( mmask EQ cland )
  ;    mcoast = WHERE ( mmask EQ ccoast )
  ;    mborder = WHERE ( mmask EQ cborder )
  ;    mriver = WHERE ( mmask EQ criver )
  ;
  ;    ; small, resampling factor = 4
  ;    smap = self.mapDir+path_sep()+basin+'.s.png'
  ;    if ~((file_info(smap)).exists and (file_info(smap)).read) then continue
  ;    read_png,smap,smask
  ;
  ;    sland = WHERE ( smask EQ cland )
  ;    scoast = WHERE ( smask EQ ccoast )
  ;    sborder = WHERE ( smask EQ cborder )
  ;
  ;    ; medium, resampling factor = 2
  ;    imap = self.mapDir+path_sep()+basin+'.i.png'
  ;    if ~((file_info(imap)).exists and (file_info(imap)).read) then continue
  ;    read_png,imap,imask
  ;
  ;    iland = WHERE ( imask EQ cland )
  ;    icoast = WHERE ( imask EQ ccoast )
  ;    iborder = WHERE ( imask EQ cborder )
    
  ;      ixl = WHERE ( bimg EQ self.badImgIndex AND lmask EQ cland, nnl)
  ;      IF ( nnl GT 0 ) THEN bimg[ixl] = cland2
  ;
  ;
  ;      IF ( lcoast[0] NE -1 ) THEN bimg[lcoast] = ccoast2
  ;      IF ( lborder[0] NE -1 ) THEN bimg[lborder] = cborder2
  ;      IF ( lriver[0] NE -1 ) THEN bimg[lriver] = criver2
  ;
  ;      ixm = WHERE ( tab1 EQ self.badImgIndex AND mmask EQ cland, nnm)
  ;      IF ( nnm GT 0 ) THEN tab1[ixm] = cland2
  ;
  ;      IF ( mcoast[0] NE -1 ) THEN tab1[mcoast] = ccoast2
  ;      IF ( mborder[0] NE -1 ) THEN tab1[mborder] = cland2 ; cborder2
  ;      IF ( mriver[0] NE -1 ) THEN tab1[mriver] = criver2
  ;
  ;      ixs = WHERE ( tab2 EQ self.badImgIndex AND smask EQ cland, nns)
  ;      IF ( nns GT 0 ) THEN tab2[ixs] = cland2
  ;
  ;      IF ( scoast[0] NE -1 ) THEN tab2[scoast] = ccoast2
  ;      IF ( sborder[0] NE -1 ) THEN tab2[sborder] = cborder2
  ;
  ;      ixi = WHERE ( tab3 EQ self.badImgIndex AND imask EQ cland, nni)
  ;      IF ( nni GT 0 ) THEN tab3[ixi] = cland2
  ;
  ;      IF ( icoast[0] NE -1 ) THEN tab3[icoast] = ccoast2
  ;;*-*
  ;l
    
  ; input mask color codes
  cland = 80
  ccoast = 250
  cborder = 10
  criver = 251
  
  ; color definition for bad value : black
  cbad=0
  ;r[255]=cbad
  ;g[255]=cbad
  ;b[255]=cbad
  rgbcbad=[cbad,cbad,cbad]
  Indexcbad=255
  
  ; color definition for land : grey
  cland2=120
  ;r[254]=cland2
  ;g[254]=cland2
  ;b[254]=cland2
  rgbcland2=[cland2,cland2,cland2]
  Indexcland2=254
  
  ; color definition for coasts : grey
  ccoast2=80
  ;r[253]=ccoast2
  ;g[253]=ccoast2
  ;b[253]=ccoast2
  rgbccoast2=[ccoast2,ccoast2,ccoast2]
  Indexccoast2=253
  
  ; color definition for borders : grey
  cborder2=160
  ;r[252]=cborder2
  ;g[252]=cborder2
  ;b[252]=cborder2
  rgbcborder2=[cborder2,cborder2,cborder2]
  indexcborder2=252
  
  ; color definition for rivers : blue
  ;r[251]=0
  ;g[251]=0
  ;b[251]=255
  rgbcriver2=[0,0,255]
  Indexcriver2=251
  
  if samplefactor eq 1 then begin
  
    land = WHERE ( mask EQ cland )
    coast = WHERE ( mask EQ ccoast )
    border = WHERE ( mask EQ cborder )
    river = WHERE ( mask EQ criver )
    ixl = WHERE ( sampledImg EQ self.badImgIndex AND mask EQ cland, nnl)
    IF ( nnl GT 0 ) THEN sampledImg[ixl] = indexcland2
    IF ( coast[0] NE -1 ) THEN sampledImg[coast] = indexccoast2
    IF ( border[0] NE -1 ) THEN sampledImg[border] = indexcborder2
    IF ( river[0] NE -1 ) THEN sampledImg[river] = indexcriver2
    
  endif
  
  ;m
  if samplefactor eq 2 then begin
  
    land = WHERE ( mask EQ cland )
    coast = WHERE ( mask EQ ccoast )
    border = WHERE ( mask EQ cborder )
    river = WHERE ( mask EQ criver )
    ixm = WHERE ( sampledImg EQ self.badImgIndex AND mask EQ cland, nnm)
    IF ( nnm GT 0 ) THEN sampledImg[ixm] = indexcland2
    
    IF ( coast[0] NE -1 ) THEN sampledImg[coast] = indexccoast2
    IF ( border[0] NE -1 ) THEN sampledImg[border] = indexcland2 ; cborder2
    IF ( river[0] NE -1 ) THEN sampledImg[river] = indexcriver2
    
  endif
  
  ;s
  if samplefactor eq 4 then begin
  
    land = WHERE ( mask EQ cland )
    coast = WHERE ( mask EQ ccoast )
    border = WHERE ( mask EQ cborder )
    ixs = WHERE ( sampledImg EQ self.badImgIndex AND mask EQ cland, nns)
    IF ( nns GT 0 ) THEN sampledImg[ixs] = indexcland2
    
    IF ( coast[0] NE -1 ) THEN sampledImg[coast] = indexccoast2
    IF ( border[0] NE -1 ) THEN sampledImg[border] = indexcborder2
    
  endif
  
  ;i
  if samplefactor eq 8 then begin
  
    land = WHERE ( mask EQ cland )
    coast = WHERE ( mask EQ ccoast )
    border = WHERE ( mask EQ cborder )
    
    ixi = WHERE ( sampledImg EQ self.badImgIndex AND mask EQ cland, nni)
    IF ( nni GT 0 ) THEN sampledImg[ixi] = indexcland2
    
    IF ( coast[0] NE -1 ) THEN sampledImg[coast] = indexccoast2
  endif
  
  return, sampledImg
  
END

FUNCTION ImageConverter::readData, filename, dataSetName, STATUS=STATUS, FULL=FULL

  fileI=fileInfo(filename)
  STATUS=0
  if fileI.hdf then return, self->readHdfData(filename, dataSetName, STATUS=STATUS, FULL=FULL)
  if fileI.netcdf then return, self->readNcdfData(filename, dataSetName, STATUS=STATUS, FULL=FULL)
  doLog, 'no valid reader found for: ', filename, level=2
  return, 0
  
  
END

FUNCTION ImageConverter::readHdfData, filename, dataSetName, STATUS=STATUS, FULL=FULL

  ; ########################################################
  ; Read the data
  status = -1
  
  sd_id = HDF_SD_START (filename,/READ)
  
  index = HDF_SD_NAMETOINDEX(sd_id,dataSetName)
  
  if ( index GE 0 ) THEN BEGIN
    ; Open access to variable.
    sds_id = HDF_SD_SELECT(sd_id,index)
    limit=0 & scaling=0 & slope=0 & intercept=0 & iproj=0 & getExtra=0
    extraValsNo=n_elements(self.extraVar)
    extraValues=fltarr(extraValsNo)
    IF ( keyword_set(FULL) ) THEN BEGIN
    
      for i=0, extraValsNo-1 do begin
        index = HDF_SD_ATTRFIND(sds_id,self.extraVar[0])
        HDF_SD_ATTRINFO,sds_id,index,NAME=name,TYPE=t,COUNT=c,DATA=d
        extraValues[i]=d[0]
      endfor
    endif
    ; Get variable.
    HDF_SD_GETDATA,sds_id,img
    ; End access to variable.
    HDF_SD_ENDACCESS,sds_id
    
    status =1
    
  endif
  
  ; End access to HDF file.
  HDF_SD_END,sd_id
  ; ########################################################
  
  if ( status EQ -1 ) then begin
    doLog, 'no data called: ', dataSetName, ' in ', filename, level=2
    doLog, 'skip', level=2
    return, 0
  endif
  return, {limit:extraValues[0], scaling:extraValues[1], slope:extraValues[2], intercept:extraValues[3], iproj:extraValues[4], img:img, full:keyword_set(FULL)}
  
END

FUNCTION ImageConverter::readNcdfData, filename, dataSetName, STATUS=STATUS, FULL=FULL

  ; ########################################################
  ; Read the data
  status = -1
  res=self.reader->getNcdfDataValues(fileName, dataSetName, FOUND=FOUND, /REVERSE)
  if res.idx ge 0 then begin
    status = 1
    img=res.data
    
    limit=0 & scaling=0 & slope=0 & intercept=0 & iproj=0 & getExtra=0
    extraValsNo=n_elements(self.extraVar)
    extraValues=fltarr(extraValsNo)
    if keyword_set(FULL) then begin
      for i=0, extraValsNo-1 do begin
        ;self.extraVar[i]=self.reader->readNcdfAttr(fileName, dataSetName, self.extraVar[i], FOUND=FOUND, GLOBAL=GLOBAL)
        value=self.reader->readNcdfAttr(fileName, self.extraVar[i], FOUND=FOUND, GLOBAL=GLOBAL)
        if found ne 1 then begin
          status=-1
          break
        endif
        extraValues[i]=value
      endfor
    endif
  endif
  
  if ( status EQ -1 ) then begin
    doLog, 'no data/attrib called: ', dataSetName, ' in ', filename, level=2
    doLog, 'skip', level=2
    return, 0
  endif
  return, {limit:extraValues[0], scaling:extraValues[1], slope:extraValues[2], intercept:extraValues[3], iproj:extraValues[4], img:img, full:keyword_set(FULL)}
  
END

FUNCTION ImageConverter::getScaleInfo, parameterCode, basinTypeCode, lookUpTableCode=lookUpTableCode

  if n_elements(lookUpTableCode) eq 1 then if lookUpTableCode ne '' and strupcase(lookUpTableCode) ne 'N/A' then return, self.colorMapInfo->getEntryByCode(lookUpTableCode)
  ;if ~obj_valid(colorTable) then begin
  ;  parameterCode=self->removeMeanWord(parameterCode)
  ;  colorTable=self.colorMapInfo->getEntryByKeys(parameterCode, basinTypeCode)
  ;endif
  return, self.colorMapInfo->getEntryByKeys(self->removeMeanWord(parameterCode), basinTypeCode)
  
END

FUNCTION ImageConverter::removeMeanWord, parCode

  wordToFind='-MEAN'
  startPos=strpos(strupcase(parCode), wordToFind)
  if startPos ne -1 then parName=strmid(parCode, 0, startPos) else parName=parCode
  return, parName
  
END

PRO ImageConverter::writeDemoColor

  pal_file='rwb_bar8.dat'
  openw, fileunit,self.colorTableDir+path_sep()+pal_file,/GET_LUN
  
  r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
  r[7]=250 & g[7]=0 & b[7]=0
  r[6]=200 & g[6]=100 & b[6]=70
  r[5]=255 & g[5]=255 & b[5]=0
  ;r[4]=255 & g[4]=255 & b[4]=128
  r[4]=200 & g[4]=200 & b[4]=200
  r[3]=200 & g[3]=200 & b[3]=200
  ;r[3]=128 & g[3]=255 & b[3]=255
  r[2]=0 & g[2]=255 & b[2]=255
  r[1]=70 & g[1]=100 & b[1]=200
  r[0]=0 & g[0]=0 & b[0]=250
  
  writeu,fileunit,r,g,b
  free_lun, fileunit & close, fileunit
  
END

FUNCTION ImageConverter::init, colorMapFileName, roiFileName, colorMapInfo=colorMapInfo, roiInfo=roiInfo, $
    mapDir=mapDir, colorTableDir=colorTableDir, statType=statType, outputDir=outputDir, $
    lookUpTableCode=lookUpTableCode, exportLutMode=exportLutMode, exportLutFileName=exportLutFileName, $
    exportSize=exportSize
    
  if not(self -> Object::init()) then return, 0
  if ~obj_valid(colorMapInfo) then begin
    colorMapInfo=obj_new('ColorMap')
    colorMapInfo->buildMapEntryFromXMLFile, colorMapFileName
    self.colorMapInfo=colorMapInfo
  endif
  if obj_valid(colorMapInfo) then self.colorMapInfo=colorMapInfo
  
  if ~obj_valid(roiInfo) then begin
    roiInfo=obj_new('ROI')
    if ~roiInfo->fillDataFromXMLFile(roiFileName) then return, 0
    self.roiInfo=roiInfo
  endif
  if obj_valid(roiInfo) then self.roiInfo=roiInfo
  if n_elements(mapDir) ne 0 then begin
    self.mapDir=mapDir
  endif else begin
    cd, curr=curr
    self.mapDir=curr + path_sep()+ 'OC' + path_sep() + 'masks'
  endelse
  if n_elements(colorTableDir) ne 0 then self.colorTableDir=colorTableDir
  if n_elements(statType) ne 0 then self.statType=statType
  if n_elements(outputDir) ne 0 then self.outputDir=outputDir
  if n_elements(lookUpTableCode) eq 1 then self.lookUpTableCode=lookUpTableCode
  if n_elements(exportLutMode) eq 1 then begin
    availableLutMode=['EMBEDDED','EXTERNAL', 'BOTH']
    byteMode=where(strupcase(exportLutMode) eq availableLutMode, count)
    if count eq 1 then self.exportLutMode=byte(byteMode)+1 else doLog, 'No lut requested!', LEVELE=2
  endif
  if n_elements(exportLutFileName) eq 1 then self.exportLutFileName=exportLutFileName
  if self.exportLutFileName eq 'N/A' then self.exportLutFileName=''
  if n_elements(exportSize) eq 1 then self.exportSize=strlowcase(exportSize)
  self.avFormats=['PNG']
  ;ibad = 255
  self.badImgIndex=255b
  self.reader=obj_new('GenericOperator')
  self.extraVar=['Limit','Scaling','Slope','Intercept','Projection ID']
  self->writeDemoColor
  
  self.availableFactorCodes=['i','s','m','l']
  self.availableFactorSamples=[8,4,2,1]
  return, 1
  
END

PRO ImageConverter::cleanUp

  self -> Object::cleanUp
  
END

PRO ImageConverter__Define

  Struct = { ImageConverter , $
    colorMapInfo: obj_new(), $
    roiInfo: obj_new(), $
    mapdir: '', $
    colorTableDir: '', $
    avFormats: strarr(1), $
    badImgIndex: 0, $
    statType: '', $
    outputDir: '', $
    reader: obj_new(), $
    extraVar: strarr(6), $
    lookUpTableCode: '', $
    exportLutMode: 0b, $
    exportLutFileName: '', $
    exportSize: '', $
    availableFactorCodes: strarr(4), $
    availableFactorSamples: intarr(4), $
    Inherits Object $
    }
    
END