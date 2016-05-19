pro HDF_TO_ENVI_STACK

  ; start the ENVI batch session
  compile_opt  strictarr
  ENVI, /restore_base_save_files
  ENVI_BATCH_INIT;, /NO_STATUS_WINDOW
  
  nan_value = -9999.
  ; get files names to process
  
  ;fnames = FILE_SEARCH('C:\Program Files\ITT\IDL\IDL81\STE_81\IDL_JRC_Mirko\Data\*.*',COUNT=n_files)
  fnames = FILE_SEARCH('E:\data\mariomi\application\oxyrisk\temp\stefano\*.*',COUNT=n_files)
  
  ; check existing files
  if (fnames[0] eq '') then begin
    q = DIALOG_MESSAGE('No file was found!', /ERROR)
    RETURN
  endif
  
  ; initialize progressbar
  str = ["Input File : " , 'Output File : ']
  ENVI_REPORT_INIT, str, TITLE='Computation Running...', BASE=base
  ENVI_REPORT_INC, base, 100
  num_of_steps = n_files
  step_num =  num_of_steps / 100.
  
  ; loop on files
  for j=0, N_ELEMENTS(fnames)-1 do begin
  
    ; get hdf attributes names and info
    sd_id = HDF_SD_START(fnames[j])
    HDF_SD_FILEINFO, sd_id, num_datasets, num_attributes
    sd_names = STRARR(num_datasets)
    for i=0, num_datasets-1 do begin
      sds_id = HDF_SD_SELECT(sd_id, i)
      HDF_SD_GETINFO, sds_id, dims=dims, ndims=ndims, label=label, type=type, name=sd_name
      sd_names[i] = sd_name
    endfor
    
    ; open hdf file in batch
    ENVI_OPEN_DATA_FILE, fnames[j], r_fid=fid, /HDF_SD, HDFSD_DATASET=INDGEN(num_datasets) ;da 0 a num_datasets
    
    ; check if file is open
    if (fid[0] eq -1) then begin
      q = DIALOG_MESSAGE('errore in lettura', /error)
      ENVI_BATCH_EXIT
      RETURN
    end
    
    ENVI_FILE_QUERY, Fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
    
    final_mask = BYTARR(ns,nl)+1b
    for i=0, N_ELEMENTS(fid)-1 do begin
      res = HDF_GET_ATTR(sd_id, 'Slope', sdsidx=i, slope)
      res = HDF_GET_ATTR(sd_id, 'Intercept', sdsidx=i, intercept)
      
      Image_data = ENVI_GET_DATA(fid=fid[i], dims=dims, pos=0)
      mask = ~(Image_data gt (0. - intercept) / slope)
      final_mask = final_mask*mask
    endfor
    
    ; open output file and write the stack
    outFname = FILE_DIRNAME(fnames[j],/MARK_DIRECTORY)+FILE_BASENAME(fnames[j])+'_hdf_stack.envi'
    OPENW, lun, outFname, /GET_LUN
    for i=0, N_ELEMENTS(fid)-1 do begin
      
      Image_data = ENVI_GET_DATA(fid=fid[i], dims=dims, pos=0)
      idx_nan = WHERE(final_mask eq 1, count)
      if (count gt 0) then Image_data[idx_nan] = nan_value
      WRITEU, lun, Image_data
    endfor
    FREE_LUN, lun
    
    
    ; get the geo limits from the hdf
    res = HDF_GET_ATTR(sd_id, 'Limit', sdsidx=0, map_limits)
    stepLon = (ABS(map_limits[3]-map_limits[1]))/(ns-1)
    stepLat = (ABS(map_limits[2]-map_limits[0]))/(nl-1)
    
    ; set the map info to the regular grid in ENVI file
    ps = [stepLon, stepLat]
    mc = [0.5D, 0.5D, map_limits[1], map_limits[2]]
    map_info = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)
    
    ; setup the header in the ENVI file
    envi_setup_head_oxy,  $
      FNAME=outFname,$
      NS=ns,$
      NL=nl, $
      DATA_IGNORE_VALUE=nan_value, $
      NB=N_ELEMENTS(fid), $
      DATA_TYPE=dt, $
      FILE_TYPE=0, $
      INTERLEAVE=0, $
      R_FID=stack_fid, $
      MAP_INFO=map_info, $
      /WRITE,$
      /OPEN, $
      BNAMES=sd_names
      
    ; update the progressbar
    ENVI_REPORT_STAT, Base, (j+1)/step_num, 100
    
  endfor
  
  ENVI_REPORT_INIT, BASE=base, /FINISH
  
  ; close the batch session
  ENVI_BATCH_EXIT
  
end