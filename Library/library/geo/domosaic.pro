function doMosaic, envifileList, mosaicFileName, ignoreValue, targetMapInfo, PRESERVE_RESOLUTION=PRESERVE_RESOLUTION, PIXEL_SIZE=PIXEL_SIZE

  compile_opt  strictarr
  COMMON smurffCB, mainApp
    
  ;resFile=buildFakeMosaicFile(envifileList[0], ignoreValue, targetMapInfo, PRESERVE_RESOLUTION=PRESERVE_RESOLUTION, PIXEL_SIZE=PIXEL_SIZE)
  ;doLog, '-doMosaic-, targetMapInfo:', targetMapInfo, LEVEL=2
  ; work around to manage the value "0" INSIDE THE BANDS before/after mosaic
  zeroMosaicWorkAround, envifileList, 0., float(mainApp->getKeyValue('EPS')), /SET 
  resFile=buildFakeMosaicFile(envifileList[0], ignoreValue, targetMapInfo, PRESERVE_RESOLUTION=PRESERVE_RESOLUTION, PIXEL_SIZE=PIXEL_SIZE)
  envifileList=[envifileList, resFile]
  ENVI_CHECK_SAVE, /MOSAIC
  ;
  ;Open the input files
  ;
  ; get files names to process
  fnames=envifileList
  N_files=n_elements(fnames)
  
  ; loop on files
  fids = LONARR(N_files)
  ;o_files = !Null
  o_files = objarr(N_files)
  
  for j=0, N_ELEMENTS(fnames)-1 do begin
  
    ENVI_OPEN_FILE, fnames[j], r_fid=c_fid
    c_fid=c_fid[0]
    if (c_fid eq -1) then begin
      ;ENVI_BATCH_EXIT
      RETURN, ''
    endif
    ; create fid array
    fids[j] = c_fid
    
    ; Build the necessary keywords by querying the input images
    ENVI_FILE_QUERY, c_fid, ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bnames, DATA_IGNORE_VALUE =DATA_IGNORE_VALUE
    ;doLog, 'ns', 'nl', 'c_dims', ns, nl, c_dims, LEVEL=4
    c_pos = LINDGEN(nb)
    o_files[j] = OBJ_NEW('enviobj_file_reference', fid=c_fid, pos=c_pos, dims=c_dims)
  ;envi_file_mng, id=fidEnviMainFile, /remove, /no_warning
    
  end
  
  base = WIDGET_BASE()
  xds = (yds = 350L)
  georef = 1
  p_info = PTR_NEW(/allocate)
  p_order = PTR_NEW(/allocate)
  p_proj = PTR_NEW(/allocate)
  p_data = PTR_NEW({cm:0L, base:base, $
    pm:0L, p_info:p_info, p_order:p_order, xds:xds, yds:yds, rfact:0d, $
    but:0, grab:LONARR(3), lptr:-1L, mosaic_size:LONARR(2), georef:georef, $
    lock:0, image_frames:1, thumbnails:1, ps:[1d,1d], p_proj:p_proj})
    
  mosaicImport, p_data, o_files, ignoreValue
  
  ;ignore_value=-9999.
  
  limits=targetMapInfo.mapWindowBoundary
  ;if ~keyword_set(PRESERVE_RESOLUTION) and n_elements(PIXEL_SIZE) eq 0 then PIXEL_SIZE=[abs(targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/targetMapInfo.mapPixelExtension[0], abs(targetMapInfo.mapWindowBoundary[3]-targetMapInfo.mapWindowBoundary[2])/targetMapInfo.mapPixelExtension[1]]
  if ~keyword_set(PRESERVE_RESOLUTION) then PIXEL_SIZE=[abs(targetMapInfo.mapWindowBoundary[1]-targetMapInfo.mapWindowBoundary[0])/targetMapInfo.mapPixelExtension[0], abs(targetMapInfo.mapWindowBoundary[3]-targetMapInfo.mapWindowBoundary[2])/targetMapInfo.mapPixelExtension[1]]
  ;PIXEL_SIZE=targetMapInfo.mapPixelResolution
  
  if n_elements(limits) gt 0 then doLog,limits
  if n_elements(PIXEL_SIZE) gt 0 then doLog,PIXEL_SIZE
  mosaicApply, p_data, $
    OUT_NAME=mosaicFileName, $
    IGNORE_VALUE=ignoreValue, $
    ;IGNORE_VALUE=!VALUES.F_NAN, $
    ;BACKGROUND=!VALUES.F_NAN, $
    BACKGROUND=0, $
    ;BACKGROUND=ignoreValue, $
    LIMITS=limits, $
    PIXEL_SIZE=PIXEL_SIZE, $
    bnames=bnames
    
  ;test
  ENVI_OPEN_DATA_FILE, mosaicFileName, r_fid=mfid;, /HDF_SD, HDFSD_DATASET=sourceDataSetIndex
  ENVI_FILE_QUERY, mfid, DATA_TYPE=dt, DIMS=mdims, NS=mns, NL=mnl, NB=mnb
  doLog, mdims, mns, mnl, LEVEL=4
  ;thisData = ENVI_GET_DATA(fid=fid, dims=[-1l, 0,ns-1,0,nl-1], pos=0)
  ;help, thisdata
  ;end test
  pos=strpos(resFile, '.tmp', /REVERSE_SEARCH)
  folder=strmid(resFile, 0, pos)
  files=file_search(folder+'*.*', count=count)
  if count ne 0 then file_delete, files
  ;Here change name ERROR on UNIX Systems!!!
  ; work around to manage 0 INSIDE THE BANDS after mosaic
  zeroMosaicWorkAround, mosaicFileName, 0., ignoreValue, /SET
  zeroMosaicWorkAround, mosaicFileName, 0., float(mainApp->getKeyValue('EPS')), /COMEBACK
  return, mosaicFileName
  
end