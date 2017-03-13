pro ncdfread, filename,variable_name, data_variable, slope, intercept, dims, fillValue, ERROR=ERROR, count=count, offset=offset
  ; This procedure will read netCDF data and place it in an IDL variable
  ; INPUT: filename - a string variable that includes the filepath
  ;        variable_name - a string that must match exactly that produced by
  ;                        ncdfshow.pro
  ; OUTPUT: data_variable - a user supplied variable for the data
  ;         dims - a vector of the dimensions

  ; get fileID, variable ID
  q=!QUIET
  !QUIET=1
  validDataType=['BYTE', 'CHAR', 'INT', 'LONG', 'FLOAT', 'DOUBLE']

  fileID = ncdf_open(filename)
  ; get the data and dimensions
  try=[variable_name, strupcase(variable_name), strlowcase(variable_name)]
  for i=0, n_elements(try)-1 do begin
    varID = ncdf_varid(fileID,try[i])
    if varID ne -1 then break
  endfor

  ncdf_varget,fileID, varID, data_variable, count=count, offset=offset
  ;ncdf_varget,fileID, varID, data_variable, count=[3,3,1], offset=[4066,1231,0]
  dims = size(data_variable,/dimensions)
  ; get the attribute

  try=['slope', 'scale_factor']

  for i=0, n_elements(try)-1 do begin
    attrInfo = NCDF_ATTINQ(fileID, varID, try[i])
    type=where(strupcase(attrInfo.datatype) eq validDataType, check)
    if check eq 1 then NCDF_ATTGET, fileID, Varid, try[i], slope
  endfor
  
  try=['intercept', 'add_offset']

  for i=0, n_elements(try)-1 do begin
    attrInfo = NCDF_ATTINQ(fileID, varID, try[i])
    type=where(strupcase(attrInfo.datatype) eq validDataType, check)
    if check eq 1 then NCDF_ATTGET, fileID, Varid, try[i], intercept
  endfor

  try=['_FillValue', '_fillvalue']

  for i=0, n_elements(try)-1 do begin
    attrInfo = NCDF_ATTINQ(fileID, varID, try[i])
    type=where(strupcase(attrInfo.datatype) eq validDataType, check)
    if check eq 1 then NCDF_ATTGET, fileID, Varid, try[i], fillValue
  endfor
  
  ncdf_close, fileID
  !QUIET=q 

end

;
