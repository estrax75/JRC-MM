function appendCsvStatInfo, dataColumns, separator=separator

  dims=size(dataColumns, /DIMENSION)
  
  if n_elements(separator) ne 1 then sep=';' else sep=separator
  meanLine='MEAN_BY_COLUMN'+sep
  stdDevLine='STDDEV_BY_COLUMN'+sep
  coeffVLine='COEFF_OF_VAR_BY_COLUMN'+sep
  for i=0, dims[0]-1 do begin
    statRes=moment(float(dataColumns[i, *]), /NAN, SDEV=SDEV, MEAN=MEAN)
    cv=SDEV/MEAN
    meanLine=meanLine+strcompress(MEAN, /REMOVE)+sep
    stdDevLine=stdDevLine+strcompress(SDEV, /REMOVE)+sep
    coeffVLine=coeffVLine+strcompress(cv, /REMOVE)+sep
  endfor
  return, [ strmid(meanLine, 0, strlen(meanLine)-2), strmid(stdDevLine, 0, strlen(stdDevLine)-2), strmid(coeffVLine, 0, strlen(coeffVLine)-2)]
  
END