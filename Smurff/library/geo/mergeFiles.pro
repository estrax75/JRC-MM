function mergeFiles, fileList, outName, ignoreValue, MAX_DIFFERENCE_PERCENT=MAX_DIFFERENCE_PERCENT

  ; merge bands in only one file with same geo ref info
  ; check pixel resolution & map subwindow compatibility!

  compile_opt  strictarr
  
  fileNum=n_elements(fileList)
  mergeIds=lonarr(fileNum)
  i=0
  envi_open_file, fileList[i], r_fid=r_fid
  mergeIds[i]=r_fid
  envi_file_query, mergeIds[i], ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bnames
  mapInfo = envi_get_map_info(FID=mergeIds[i])
  checkNs=ns & checkNl=nl
  ;checkNb=nb & checkDt=dt
  allMapInfo=replicate(mapInfo, fileNum)
  allMapInfo[0]=mapInfo
  allBNames=bnames
  
  for i=1, n_elements(fileList)-1 do begin
    envi_open_file, fileList[i], r_fid=r_fid
    mergeIds[i]=r_fid
    ns=0 & nl=0
    envi_file_query, mergeIds[i], ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bnames
    allBNames=[allBNames, bnames]
    mapInfo = envi_get_map_info(FID=mergeIds[i])
    if checkNs eq ns and checkNl eq nl then begin
      allMapInfo[i]=mapInfo
      doLog, fileList[i], ': OK', level=0
      continue
    endif
    doLog, fileList[i], ': Not compatible', level=2
  endfor
  
  check=verifyMapInfoCompatibility(allMapInfo, MAX_DIFFERENCE_PERCENT=MAX_DIFFERENCE_PERCENT, averagePs=averagePs, averageMc=averageMc)
  wholeEnviFileName=outName+'.envi'
  
  if check then begin
    openw, lun, wholeEnviFileName, /GET_LUN
    for i=0, n_elements(mergeIds)-1 do begin
      envi_file_query, mergeIds[i], bnames=bnames, dims=c_dims, pos=j
      for j=0, n_elements(bnames)-1 do begin
        data=envi_get_data(fid=mergeIds[i], dims=c_dims, pos=j)
        writeu, lun, data
      endfor
      envi_file_mng, id=mergeIds[i], /remove, /no_warning
    endfor
  endif
  close, lun
  free_lun, lun
  
  ps = [averagePs[0], averagePs[1]]
  mc = [0.5D, 0.5D, averageMc[0], averageMc[1]]
  mapInfo = envi_map_info_create(/GEOGRAPHIC, mc=mc, ps=ps)
  
  envi_setup_head_oxy,  $
    FNAME=wholeEnviFileName,$
    NS=ns,$
    NL=nl, $
    DATA_IGNORE_VALUE=ignoreValue, $
    NB=n_elements(allBNames), $
    DATA_TYPE=dt, $
    FILE_TYPE=0, $
    INTERLEAVE=0, $
    R_FID=wholeId, $
    MAP_INFO=mapinfo, $
    /WRITE,$
    /OPEN, $
    BNAMES=allBNames
    
  removeEnviFiles, fileList
  return, wholeEnviFileName
  
end