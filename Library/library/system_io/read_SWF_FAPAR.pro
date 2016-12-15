;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function read_SWF_FAPAR, folder, fileName, FOUND=FOUND, APPLY_SLOPE=APPLY_SLOPE, REVERSE=REVERSE

  res=read_SWF_Data(folder, fileName, FOUND=FOUND, APPLY_SLOPE=APPLY_SLOPE, REVERSE=REVERSE)
  if size(res, /TYPE) eq 8 then begin
    newres={fapar:res.data, slope_fapar:res.slope_data, offset_fapar:res.offset_data, valid:res.valid, fillvalue:res.fillvalue, fillvalue_exist:res.fillvalue_exist}
    res=0
    res=newres
  endif
  return, res

end