pro addBandToEnviFile, sourceFile, sourceDataSetIndex, destFile, sourceIgnoreValue, destIgnoreValue, slope=slope, intercept=intercept

  if n_elements(slope) eq 0 then slope=1.
  if n_elements(intercept) eq 0 then intercept=0.
  
  ENVI_OPEN_DATA_FILE, sourceFile, r_fid=fid, /HDF_SD, HDFSD_DATASET=sourceDataSetIndex
  ENVI_FILE_QUERY, fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
  thisData = ENVI_GET_DATA(fid=fid, dims=dims, pos=0)
  if n_elements(sourceIgnoreValue) eq 1 then qualityCheck = where(thisData eq sourceIgnoreValue, count, complement=goodIdxs, ncomplement=goodIdxsno); 
  resData=thisData
  if n_elements(destIgnoreValue) eq 1 then resData[qualityCheck]=destIgnoreValue
  
  if goodIdxsno ne 0 then resData[goodIdxs]=1.*thisData[goodIdxs] * slope + intercept
  if size(destFile, /TYPE) eq 7  then openw, lun, destFile, /GET_LUN, /APPEND else lun=destFile
  writeu, lun, resData

end