pro zeroMosaicWorkAround, envifiles, zeroValue, replacingValue, SET=SET, COMEBACK=COMEBACK

  compile_opt  strictarr
  COMMON smurffCB, mainApp
  
  ; read envi file and replace all the 0 value with another one (EPS if not specified)
  ; /SET create a work around envi file
  ; /COMEBACK do the comeback job
  no=n_elements(envifiles)
  if n_elements(zeroValue) ne 1 then zero=0. else zero=zeroValue
  if n_elements(replacingValue) ne 1 then newValue=float(mainApp->getKeyValue('EPS')) else newValue=replacingValue 
  
  if keyword_set(COMEBACK) then begin
    t=zero
    zero=newValue
    newValue=t
  endif
  fs=mainApp->getFileSystem()
  
  for i=0, no-1 do begin
    ENVI_OPEN_FILE, envifiles[i], r_fid=fid
    if (fid[0] eq -1) then begin
      ;ENVI_BATCH_EXIT
      RETURN
    endif
    ENVI_FILE_QUERY, fid, bnames=bnames
    mapInfo=ENVI_GET_MAP_INFO(fid=fid)
    ENVI_FILE_QUERY, fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb, bnames=bnames
    allData=ptrarr(nb)
    ;if nb gt 1 then message, 'replacing more than one band is not allowed!'
    ;envi_file_mng, id=fid, /remove, /no_warning
    dataFile=fs->getEnviDataFileName(envifiles[i])
    for j=0, nb-1 do begin
      thisData = ENVI_GET_DATA(fid=fid, dims=dims, pos=j)
      replaceIndexes=where(thisData eq zero, count)
      if count gt 0 then begin
        thisData[replaceIndexes]=newValue
        flag=1
      endif
      allData[j]=ptr_new(thisData, /NO_COPY)
    endfor
    envi_file_mng, id=fid, /remove, /no_warning
    
    if n_elements(flag) then begin
      openw, lun, dataFile, /GET_LUN
      for j=0, nb-1 do begin
        writeu, lun, *allData[j]
      ;ptr_free, allData[j]
      endfor
      ;ptr_free, allData
      close, lun
      free_lun, lun
    endif
    for j=0, nb-1 do ptr_free, allData[j]
    
  ;    envi_setup_head_oxy,  $
  ;      FNAME=outFileName,$
  ;      NS=ns,$
  ;      NL=nl, $
  ;      DATA_IGNORE_VALUE=ignoreValue, $
  ;      NB=n_elements(bnames), $
  ;      DATA_TYPE=4, $
  ;      FILE_TYPE=0, $
  ;      INTERLEAVE=0, $
  ;      MAP_INFO=mapInfo, $
  ;      /WRITE,$
  ;      /OPEN, $
  ;      BNAMES=bnames
    
  endfor
  
  
end