function buildFakeMosaicFile, sampleFileName, ignoreValue, targetMapInfo, PRESERVE_RESOLUTION=PRESERVE_RESOLUTION, PIXEL_SIZE=PIXEL_SIZE

  compile_opt  strictarr
  
  outFileName = ENVI_GET_TMP()
  
  ENVI_OPEN_FILE, sampleFileName, r_fid=fid
  if (fid[0] eq -1) then begin
    ;ENVI_BATCH_EXIT
    RETURN, ''
  endif
  ENVI_FILE_QUERY, fid, bnames=bnames
  mapInfo=ENVI_GET_MAP_INFO(fid=fid)
  envi_file_mng, id=fid, /remove, /no_warning
  if (targetMapInfo.mapPixelResolution)[0] eq 0 then pixSize=mapInfo.ps else pixSize=targetMapInfo.mapPixelResolution 
  
  if n_elements(PIXEL_SIZE) eq 1 then pixSize=[PIXEL_SIZE,PIXEL_SIZE]
  if n_elements(PIXEL_SIZE) eq 2 then pixSize=PIXEL_SIZE
  
  if keyword_set(PRESERVE_RESOLUTION) and n_elements(pixSize) eq 2 then begin
    ns=fix((targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/pixSize[0])
    nl=fix((targetMapInfo.mapWindowBoundary[2]-targetMapInfo.mapWindowBoundary[3])/pixSize[1])
  endif else begin
    ns=targetMapInfo.mapPixelExtension[0]
    nl=targetMapInfo.mapPixelExtension[1]
  endelse
  mapInfo.mc=[0.5, 0.5, targetMapInfo.mapWindowBoundary[0], targetMapInfo.mapWindowBoundary[2]]
  ;mapInfo.ps=[xPixRes, yPixRes]
  mapInfo.ps=pixSize
  data=fltarr(ns, nl, /NOZERO)
  if n_elements(ignoreValue) eq 1 then data[*,*]=ignoreValue
  openw, lun, outFileName, /GET_LUN
  for i=0, n_elements(bnames)-1 do writeu, lun, data
  close, lun
  free_lun, lun
  
  envi_setup_head_oxy,  $
    FNAME=outFileName,$
    NS=ns,$
    NL=nl, $
    DATA_IGNORE_VALUE=ignoreValue, $
    NB=n_elements(bnames), $
    DATA_TYPE=4, $
    FILE_TYPE=0, $
    INTERLEAVE=0, $
    MAP_INFO=mapInfo, $
    /WRITE,$
    /OPEN, $
    BNAMES=bnames
    
  return, outFileName
  
end