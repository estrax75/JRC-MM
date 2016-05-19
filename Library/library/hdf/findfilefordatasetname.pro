FUNCTION findFileForDataSetName, fileList, dataSetName

  returnStruct={fileName:'', dataSetIdx:-1, found:0,  nl:0, ns:0, dataType: 0}
  for i=0, n_elements(fileList)-1 do begin
    print, fileList[i], 'try to open...'
    sd_id = HDF_SD_START(fileList[i])
    HDF_SD_FILEINFO, sd_id, num_datasets, num_attributes
    ;if num_datasets gt
    sd_names = STRARR(num_datasets)
    for j=0, num_datasets-1 do begin
      ;ENVI_OPEN_DATA_FILE, parfilenames[0], r_fid=fidDaac, /HDF_SD, HDFSD_DATASET=j
      sds_id = HDF_SD_SELECT(sd_id, j)
      HDF_SD_GETINFO, sds_id, dims=dims, ndims=ndims, label=label, type=type, name=sd_name
      if dataSetName eq sd_name then begin
        typeNames=['BYTE', 'INT', 'LONG', 'FLOAT', 'DOUBLE', 'UINT', 'STRING', 'UNKNOWN']
        typeCodes=[1, 2, 3, 4, 5, 12, 0, 0]
        dataType=typeCodes[(where(type eq typeNames))]
        HDF_SD_END, sd_id
        return, {fileName:fileList[i], dataSetIdx:j, found:1, ns:dims[0], nl:dims[1], dataType: dataType}
      endif
    endfor
    HDF_SD_END, sd_id
    print, fileList[i], '...done'
  endfor
  return, returnStruct
  
END