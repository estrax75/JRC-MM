;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function read_SWF_Data, folder, fileName, varname, FOUND=FOUND, APPLY_SLOPE=APPLY_SLOPE, REVERSE=REVERSE

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  ;  catch, error_status
  ;
  ;  if error_status NE 0 THEN BEGIN
  ;    ERROR=1
  ;    FOUND=0
  ;    catch, /CANCEL
  ;    msg='Problem with file '+folder+fileName+' check version, contents, existence or read permission.'
  ;    ;errMsg=dialog_message(msg, /ERROR)
  ;    ;message, msg
  ;    return, 0
  ;  endif

  if n_elements(varName) eq 0 then varName='Mean:SeaVI'
  interceptName='intercept'
  slopeName='slope'
  fillValueName='_FillValue'

  newFolder=ST_fileSystem->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  fName=ST_fileSystem->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  data=0
  slope_data=1
  offset_data=0
  valid=0
  ST_operator->readHdfFullInfoData, fullfilename[0], varName, array, slope, intercept, fillvalue, info=INFO, ERROR=ERROR
  ;dataSet=operatorObj->readNcdfVar(fullFileName, varName, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, $
  ;  interceptName=interceptName, slopeName=slopeName, fillValueName=fillValueName)
  if ~keyword_set(ERROR) then begin
    if n_elements(fillvalue) ne 1 then begin
      fillvalue_exist=0
      fillvalue=-1
    endif else begin
      fillvalue_exist=1
      fillvalue=fillvalue[0]
    endelse
    if fillvalue_exist then nanIdx=where(array eq fillvalue[0], nanCount) else nanCount=0
    data=1.*array
    valid=1
    FOUND=1
    if keyword_set(APPLY_SLOPE) then begin
      data=data*slope[0]+intercept[0]
      slope_data=1
      offset_data=0
    endif else begin
      slope_data=slope
      offset_data=offset
    endelse
    if nanCount gt 0 then data[nanIdx]=!VALUES.F_NAN
  endif

  if keyword_set(FOUND) then begin
    if keyword_set(REVERSE) then data=reverse(temporary(data), 2)
    return, {data:data, slope_data:slope_data, offset_data:offset_data, valid:valid, fillvalue:fillvalue, fillvalue_exist:fillvalue_exist}
  endif
  return, 0

end