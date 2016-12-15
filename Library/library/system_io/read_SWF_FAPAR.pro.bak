;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function read_SWF_FAPAR, folder, fileName, FOUND=FOUND, APPLY_SLOPE=APPLY_SLOPE, REVERSE=REVERSE


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

  varName='Mean:SeaVI'
  interceptName='intercept'
  slopeName='slope'
  fillValueName='_FillValue'

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  newFolder=fsObj->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  fName=fsObj->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  fapar=0
  slope_fapar=1
  offset_fapar=0
  valid=0
  operatorObj->readHdfFullInfoData, fullfilename, varName, array, slope, intercept, fillvalue, info=INFO, ERROR=ERROR
  ;dataSet=operatorObj->readNcdfVar(fullFileName, varName, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, $
  ;  interceptName=interceptName, slopeName=slopeName, fillValueName=fillValueName)
  if ~keyword_set(ERROR) then begin
    fapar=1.*array
    valid=1
    FOUND=1
    if keyword_set(APPLY_SLOPE) then begin
      fapar=fapar*slope[0]+intercept[0]
      slope_fapar=1
      offset_fapar=0
    endif else begin
      slope_fapar=slope
      offset_fapar=offset
    endelse
  endif

  if keyword_set(FOUND) then begin
    if keyword_set(REVERSE) then fapar=reverse(temporary(fapar), 2)
    return, {fapar:fapar, slope_fapar:slope_fapar, offset_fapar:offset_fapar, valid:valid}
  endif
  return, 0

end