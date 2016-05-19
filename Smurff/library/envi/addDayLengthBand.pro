function addDayLengthBand, enviMainFile, sjDay, ejday, year, tempDir, ignoreValue

  COMMON smurffCB, mainApp
  
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  
  doLog, '*start**addDayLengthBand***', level=0
  doLog, 'enviMainFile: ', enviMainFile, level=0
  envi_open_file, enviMainFile, r_fid=fidEnviMainFile
  envi_file_mng, id=fidEnviMainFile, /remove, /no_warning
  envi_open_file, enviMainFile, r_fid=fidEnviMainFile
  envi_file_query, fidEnviMainFile, ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bandnames
  mapInfo = envi_get_map_info(FID=fidEnviMainFile)
  parInfo=mainApp->getParameterByCode('dl')
  bandnames=[bandnames, parInfo.outputBandName]
  parInfo=mainApp->getParameterByCode('sza')
  bandnames=[bandnames, parInfo.outputBandName]
  
  bNumber=n_elements(bandnames)
  lats=mapInfo.mc[3]-findgen(nl)*mapInfo.ps[1]
  dayRes=get_dayLengthSza(lats, sjday+indgen(ejday-sjday+1), year)
  if sjday ne ejday then begin
    dl = avg(dayRes.dl,1);
    sza = avg(dayRes.theta_rd,1);
  endif else begin
    dl = dayRes.dl
    sza = dayRes.theta_rd
  endelse
  
  dl = float(repmat(dl, 1, ns));
  dl=transpose(dl)
  sza = float(repmat(sza, 1, ns));
  sza=transpose(sza)
  
  ;tempfName=utility->getSysTime(/FILECOMPATIBILITY)
  ;tempDir=fS->getTempDir(/WITH)
  ;newEnviFileName = tempDir+path_sep()+tempfName+'euro_sst_with_dayl.envi'
  ;newEnviFileName = tempDir+path_sep()+tempfName+'_'+year+'_'+strtrim(sjDay, 1)+'_'+strtrim(ejday, 1)+'.envi'
  ;file_copy,  enviMainFile, newEnviFileName
  openw, lun, enviMainFile, /GET_LUN, /APPEND
  ;for i=0, nb-1 do begin
  ;  thisData = ENVI_GET_DATA(fid=fidEnviMainFile, dims=dims, pos=i)
  ;  writeu, lun, band.data
  ;endfor
  writeu, lun, dl
  writeu, lun, sza
  close, lun
  free_lun, lun
  
  envi_setup_head,  $
    FNAME=enviMainFile,$
    NS=ns,$
    NL=nl, $
    DATA_IGNORE_VALUE=ignoreValue, $
    NB=bNumber, $
    DATA_TYPE=dt, $
    FILE_TYPE=0, $
    INTERLEAVE=0, $
    MAP_INFO=mapinfo, $
    /WRITE,$
    /OPEN, $
    BNAMES=bandNames
  ;doLog, 'removing envifiles..'
  ;file_delete, enviMainFile
  ;file_copy, newEnviFileName, enviMainFile
  envi_file_mng, id=fidEnviMainFile, /remove, /no_warning
  return, enviMainFile
  
end