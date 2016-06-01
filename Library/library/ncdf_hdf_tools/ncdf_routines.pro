function ncdf_file_extension

  return,'ncdf'
  
end


function ncdf_file_create, filename, _EXTRA=extra

;  ERROR=0
;  catch, error_status
;  
;  if error_status NE 0 THEN BEGIN
;    ERROR=1
;    catch, /CANCEL
;    close, /all
;    errMsg=dialog_message('problem with file: <'+fileName+'> check contents, existence or read permission.', /ERROR)
;  endif
  
  file_id = NCDF_CREATE( filename, _EXTRA=extra)
  
  NCDF_CONTROL, file_id, /ENDEF
  
  return, file_id
  
end

function ncdf_file_open, filename, _EXTRA = extra

  if keyword_set(extra) then begin
  
    file_id = NCDF_OPEN(filename, _EXTRA = extra )
    
  endif else begin
  
    file_id = NCDF_OPEN(filename)
    
  endelse
  
  
  return, file_id
  
end


function ncdf_file_open4output, filename

  return, ncdf_file_open(filename,/WRITE)
  
end


function ncdf_get_dtype, var
  ; function to convert IDL to ncdf data type
  ; returns structure for use with NCDF_ATTPUT & NCDF_VARDEF

  case (SIZE(var,/TYPE)) of
    1 : dtype = {BYTE:1}
    2 : dtype = {SHORT:1}
    3 : dtype = {LONG:1}
    4 : dtype = {FLOAT:1}
    5 : dtype = {DOUBLE:1}
    7 : dtype = {CHAR:1}
    else: message, "Can't convert type of attribute"
    
  endcase
  
  return,dtype
end


pro ncdf_write_global_attribute, file_id, attname, attvalue

  NCDF_CONTROL, file_id, /REDEF
  
  if attvalue eq '' then attavalue = ' '
  NCDF_ATTPUT, file_id, /GLOBAL, attname, attvalue, _EXTRA = ncdf_get_dtype(attvalue)
  
  NCDF_CONTROL, file_id, /ENDEF
  
end


pro ncdf_write_var_attribute, file_id, var_id, attname, attvalue

  if attvalue eq '' then attvalue = ' '
  
  NCDF_ATTPUT, file_id, var_id, attname, attvalue, _EXTRA = ncdf_get_dtype(attvalue)
  
  
end


function ncdf_read_global_attribute, file_id, attname


  NCDF_ATTGET, file_id, /GLOBAL, attname, attvalue
  
  if n_elements(attvalue) eq 1 then attvalue = attvalue[0]
  
  return, attvalue
end


function ncdf_read_var_attribute, file_id, var_id, attname, attvalue

  NCDF_ATTGET, file_id, var_id, attname, attvalue
  
  if n_elements(attvalue) eq 1 then attvalue = attvalue[0]
  
  return, attvalue
  
end

function ncdf_ds_id, file_id, dsname

  ; Returns id of dataset named dsname otherwise, -1

  return, NCDF_VARID(file_id,dsname)
  
end


function ncdf_create_dimension, file_id, dimname, dimsize, _EXTRA = extra

  name = [dimname]
  
  size = [dimsize]
  
  dim_nb = n_elements(name)
  
  dim_id = lonarr(dim_nb)
  
  NCDF_CONTROL, file_id, /REDEF
  
  if keyword_set(extra) then begin
    for idim = 0, dim_nb - 1 do dim_id[idim] = NCDF_DIMDEF(file_id, name[idim], _EXTRA = extra)
  endif else begin
    for idim = 0, dim_nb - 1 do dim_id[idim] = NCDF_DIMDEF(file_id, name[idim], size[idim])
  endelse
  
  NCDF_CONTROL, file_id, /ENDEF
  
  return,dim_id
  
end


function ncdf_read_dimension, file_id, dimname, dimsize

  name = [dimname]
  
  dim_nb = n_elements(name)
  
  dim_id = lonarr(dim_nb)
  dimsize = lonarr(dim_nb)
  
  for idim = 0, dim_nb - 1 do begin
    dim_id[idim] = NCDF_DIMID( file_id, dimname[idim] )
    NCDF_DIMINQ, file_id, dim_id[idim], name, size
    dimsize[idim] = size
  endfor
  
  return,dim_id
  
end


function ncdf_create_ds, file_id, var, dim_id, varinfo


  NCDF_CONTROL, file_id, /NOFILL
  NCDF_CONTROL, file_id, /REDEF
  
  var_id = NCDF_VARDEF(file_id, varinfo.name, dim_id, _extra = ncdf_get_dtype(var))
  
  if var_id ge 0 then for iatt = 0, n_elements(varinfo.att) - 1 do ncdf_write_var_attribute, file_id, var_id, varinfo.att[iatt].name, *varinfo.att[iatt].ptr_value
  
  NCDF_CONTROL, file_id, /ENDEF
  
  ncdf_update_ds, file_id, var_id, var
  
  
  return, var_id
  
end


pro ncdf_update_ds, file_id, var_id, var, _EXTRA=constraint


  if keyword_set(constraint) then begin
  
    NCDF_VARPUT, file_id, var_id, var, _EXTRA=constraint
    
  endif else begin
  
    NCDF_VARPUT, file_id, var_id, var
    
  endelse
  
end

function ncdf_read_data, file_id, var_id, _EXTRA=constraint

  if keyword_set(constraint) then begin
  
    NCDF_VARGET, file_id, var_id, data, _EXTRA=constraint
    
  endif else begin
  
    NCDF_VARGET, file_id, var_id, data
    
  endelse
  
  return,data
  
end


function ncdf_read_data_byname, file_id, varname, var_id, _EXTRA=constraint


  var_id = ncdf_ds_id(file_id, varname)
  
  if var_id ne -1 then begin
  
    data = ncdf_read_data(file_id, var_id, _EXTRA=constraint)
    
  endif else begin
  
    data = var_id
    
  endelse
  
  return,data
  
end

pro ncdf_endaccess, var_id

; Procedure to end access to a data set - NOT necessary for NCDF


end


pro ncdf_file_close,file_id

  NCDF_CLOSE, file_id
  
end
