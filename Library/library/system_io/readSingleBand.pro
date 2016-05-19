;@/library/system_io/readBandFromFile
function readSingleBand, bandName, files, mask, NOTFOUND=NOTFOUND

  NOTFOUND=1
  filesNo=n_elements(files)
  doLog,'looking for--->', bandName, level=0
  ;only for test!!!
  ;bandName='fake'
  for i=0, filesNo-1 do begin
    ;close, /all
    sd_id = HDF_SD_START(files[i])
    HDF_SD_FILEINFO, sd_id, num_datasets, num_attributes
    sd_names = STRARR(num_datasets)
    doLog,'thisFile->', files[i], level=1
    for j=0, num_datasets-1 do begin
      sds_id = HDF_SD_SELECT(sd_id, j)
      HDF_SD_GETINFO, sds_id, dims=dims, ndims=ndims, label=label, type=type, name=sd_name
      ;doLog,'thisBand->', sd_name, level=2
      HDF_SD_ENDACCESS, sds_id
      if sd_name eq bandName then begin
        doLog, callingRoutine=callingRoutine, /STACK
        doLog, callingRoutine, files[i]
        info=readBandFromFile(files[i], sd_id, j, mask)
        mask=info.mask
        NOTFOUND=0
        ;HDF_SD_End, sd_id
        return, info
      endif
    endfor
    HDF_SD_Endaccess, sds_id
    HDF_SD_End, sd_id
  endfor
  
  NOTFOUND=1
  doLog, 'dataset: '+bandName+' not found in file like '+files[0], level=3
  return, 0
  
  
end