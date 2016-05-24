;@/library/system_io/readSingleBand
@readSingleBand
function readJRCContents, infilenames, inputVarList, outputVarList, conv_functions, $
  tempDir, roiName, ignoreValue, NOTFOUND=NOTFOUND, MEMORY=MEMORY, targetCropInfo=targetCropInfo
  
  COMMON smurffCB, mainApp
  
  fileWriteFlag=(~keyword_set(MEMORY)) or (n_elements(targetCropInfo) eq 1)
  NOTFOUND=0
  varNo=n_elements(inputVarList)
  fileNo=n_elements(infilenames)
  var_file = intarr(varNo);
  varFileRes=intarr(varNo)
  
  utils=mainApp->getUtility()
  fS=mainApp->getFileSystem()
  pathSep=path_sep()
  
  tempfName=utils->getSysTime(/FILECOMPATIBILITY)
  enviFileName = tempDir+pathSep+tempfName+'_'+strcompress(roiName)+'_temp_jrc.envi'
  intermediateEnviFileName=tempDir+pathSep+tempfName+'_'+strcompress(roiName)+'_int_out_jrc.envi'
  finalEnviFileName = tempDir+pathSep+tempfName+'_'+strcompress(roiName)+'_out_jrc.envi'
  if keyword_set(MEMORY) then memoryData=ptrarr(n_elements(inputVarList)) else memoryData=-1
  
  if fileWriteFlag then openw, lun, enviFileName, /GET_LUN
  ;fakeBand=readSingleBand('mld', infilenames, mask, NOTFOUND=NOTFOUND)
  ;print All
  for i=0, varNo-1 do begin
  
    band=readSingleBand(inputVarList[i], infilenames, mask, NOTFOUND=NOTFOUND)
    if ~keyword_set(NOTFOUND) then begin
      dataSize=size(band.data, /STRUCT)
      if fileWriteFlag then writeu, lun, band.data else memoryData[i]=ptr_new(band.data, /NO_COPY)
      validIdxs=where(band.data ne -9999 and band.data gt 0 and finite(band.data), cnt)
      if cnt ne 0 then doLog, '**'+inputVarList[i]+strcompress(max(band.data))+strcompress(min(band.data))+strcompress(mean(band.data))+'**' else doLog, '**'+'No data'+'**'
      mask=band.mask
    endif else begin
      if fileWriteFlag then begin
        close, lun
        free_lun, lun
      endif
      return, -1
    endelse
    
  endfor
  if fileWriteFlag then begin
    close, lun
    free_lun, lun
  endif
  
  finalMask=mask
  mask=0b
  
  ; apply mask to each band
  if fileWriteFlag then begin
    applyMaskToEnviFile, enviFileName, intermediateEnviFileName, varNo, band.data, finalMask, ignoreValue
    ; setup the header in the ENVI file
    ;intermediateEnviFileName1=fs->removeFileExtension(intermediateEnviFileName)
    
    ;help, band, /struct
    envi_setup_head_oxy,  $
      FNAME=fs->removeFileExtension(intermediateEnviFileName),$
      NS=band.ns,$
      NL=band.nl, $
      DATA_IGNORE_VALUE=ignoreValue, $
      NB=varNo, $
      DATA_TYPE=band.dt, $
      FILE_TYPE=0, $
      INTERLEAVE=0, $
      R_FID=stackFid, $
      MAP_INFO=band.mapinfo, $
      /WRITE,$
      /OPEN, $
      BNAMES=outputVarList
  endif
  ;doLog, band.ns, band.nl
  ;doLog, band.mapinfo.ps, band.mapinfo.mc
  
  if n_elements(targetCropInfo) eq 1 then begin
    ;    ;test
    ;    ENVI_OPEN_DATA_FILE, intermediateEnviFileName, r_fid=fid;, /HDF_SD, HDFSD_DATASET=sourceDataSetIndex
    ;    ENVI_FILE_QUERY, fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
    ;    thisData = ENVI_GET_DATA(fid=fid, dims=dims, pos=0)
    ;    ;end test
    ; force pixel size to avoid rounding issues
    PIXEL_SIZE=[abs(targetCropInfo.mapWindowBoundary[1]-targetCropInfo.mapWindowBoundary[0])/targetCropInfo.mapPixelExtension[0], abs(targetCropInfo.mapWindowBoundary[3]-targetCropInfo.mapWindowBoundary[2])/targetCropInfo.mapPixelExtension[1]]
    mosaicFile=doMosaic(intermediateEnviFileName, finalEnviFileName, ignoreValue, targetCropInfo, PIXEL_SIZE=PIXEL_SIZE);/PRESERVE_RESOLUTION)
    fs->correctEnviHeaderFileName, mosaicFile
    ENVI_OPEN_DATA_FILE, mosaicFile, r_fid=fid;, /HDF_SD, HDFSD_DATASET=sourceDataSetIndex
    ENVI_FILE_QUERY, fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
    doLog, '-readJRCContents-', 'dims,', 'ns,', 'nl:', dims, ns, nl, LEVEL=4
    ;doLog, dataSize, LEVEL=4
    if keyword_set(MEMORY) then begin
      for i=0, varNo-1 do begin
        thisData = ENVI_GET_DATA(fid=fid, dims=dims, pos=i)
        memoryData[i]=ptr_new(thisData, /NO_COPY)
      endfor
    endif
    doLog, '...crop from ' + infilenames[0] + ' found.' , LEVEL=3
    envi_file_mng, id=fid, /REMOVE
    ;remove envi intermediate file!!!
    removeEnviFiles, intermediateEnviFileName;, /ALL
    ;file_delete, intermediateEnviFileName, /ALLOW
  endif else begin
    finalEnviFileName=intermediateEnviFileName
  endelse
  if (keyword_set(MEMORY)) then begin
    ;here extract data
  endif
  
  fileName=fs->getFileNameInfo(infilenames[0], filePath=filePath, extension=extension)
  band=0b
  
  year = strmid(fileName, 1, 4);
  jday = strmid(fileName, 5, 3);
  
  caldat, julday(1,1,year) + jday, resMonth, resDay, resYear
  
  month = resMonth;
  
  return, {enviDataFile:finalEnviFileName, year:year, month:month, jday:jday, memoryData:memoryData}
  
END