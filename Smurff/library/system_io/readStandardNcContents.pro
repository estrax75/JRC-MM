;function readModelPSAContents, infilenames, inputVarList, outputVarList, conv_functions, tempDir, roiName, ignoreValue, NOTFOUND=NOTFOUND
function readStandardNcContents, infilenames, inputVarList, outputVarList, conv_functions, tempDir, roiName, ignoreValue, outFuncList, NOTFOUND=NOTFOUND

  COMMON smurffCB, mainApp
  NOTFOUND=0
  
  varNo=n_elements(inputVarList)
  
  if n_elements(conv_functions) eq 0 then begin
    convFuncs=strarr(varNo)
    convFuncs[*]='N/A'
  endif else begin
    convFuncs=conv_functions
  endelse
  
  if n_elements(outFuncList) eq 0 then begin
    outConvFuncs=strarr(varNo)
    outConvFuncs[*]='N/A'
  endif else begin
    outConvFuncs=outFuncList
  endelse
  
  fileNo=n_elements(infilenames)
  var_file = intarr(varNo);
  varFileRes=intarr(varNo)
  
  utils=mainApp->getUtility()
  fS=mainApp->getFileSystem()
  pathSep=path_sep()
  
  tempfName=utils->getSysTime(/FILECOMPATIBILITY)
  enviFileName = tempDir+pathSep+tempfName+'_'+strcompress(roiName)+'_temp_psa.envi'
  finalEnviFileName = tempDir+pathSep+tempfName+'_'+strcompress(roiName)+'_out_psa.envi'
  
  openw, lun, enviFileName, /GET_LUN
  for i=0, varNo-1 do begin
  
    band=readStandardNcSingleBand(inputVarList[i], infilenames, mask, convFuncs[i], nanValue=ignoreValue, /REVERSE, NOTFOUND=NOTFOUND)
    if NOTFOUND then begin
      close, lun
      free_lun, lun
      NOTFOUND=1
      return, -1
    endif
    
    band.data=convertValues(band.data, outConvFuncs[i], ignoreValue=ignoreValue)
    ;maskIdxs=where(band.data eq ignoreValue, count)
    ;if count gt 0 then band.data[maskIdxs]=!VALUES.F_NAN
    writeu, lun, band.data
    ;tv, band.data
    mask=band.mask
    
  endfor
  close, lun
  free_lun, lun
  ;mapInfo=readPSAGeoInfo(infilenames, /REVERSE, GRIDASDATASET=GRIDASDATASET, lats=lats, lons=lons)
  mapInfo=readNcdfGeoInfo(infilenames, /REVERSE, GRIDASDATASET=GRIDASDATASET, lats=lats, lons=lons)
  
  finalMask=mask
  mask=0b
  
  ; apply mask to each band
  applyMaskToEnviFile, enviFileName, finalEnviFileName, varNo, band.data, finalMask, ignoreValue
  
  ; setup the header in the ENVI file
  
  if keyword_set(GRIDASDATASET) then begin
    delIDLVar,  mapinfo
    openw, lun, finalEnviFileName, /GET_LUN, /APPEND
    writeu, lun, lats
    writeu, lun, lons
    close, lun
    free_lun, lun
    varNo=varNo+2
    outputVarList=[outputVarList, 'lat2d', 'lon2d']
  endif
  
  envi_setup_head_oxy,  $
    FNAME=finalEnviFileName,$
    NS=band.ns,$
    NL=band.nl,$
    DATA_IGNORE_VALUE=ignoreValue, $
    NB=varNo, $
    DATA_TYPE=band.dt, $
    FILE_TYPE=0, $
    INTERLEAVE=0, $
    R_FID=stackFid, $
    MAP_INFO=mapinfo, $
    /WRITE,$
    /OPEN, $
    BNAMES=outputVarList
    
  ;fileName=fs->getFileNameInfo(infilenames[0], filePath=filePath, extension=extension)
    
  ;year = strmid(fileName, 1, 4);
  ;jday = strmid(fileName, 5, 3);
    
  ;caldat, julday(1,1,year) + jday, resMonth, resDay, resYear
    
  ;month = resMonth;
  ;  doLog, '*******'
  ;  doLog, finalEnviFileName
  ;  doLog, mapinfo
  ;  doLog, '*******'
    
  return, {enviDataFile:finalEnviFileName, year:0, month:0, jday:0}
  
end