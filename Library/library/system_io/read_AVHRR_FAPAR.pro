;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function read_AVHRR_FAPAR, folder, fileName, FOUND=FOUND, $
  FULL=FULL, APPLY_CONVERSION=APPLY_CONVERSION, varName=varName, offset=offset, count=count, fid=fid

 res=read_AVHRR_data(folder, fileName, FOUND=FOUND, $
    FULL=FULL, APPLY_CONVERSION=APPLY_CONVERSION, varName=varName, offset=offset, count=count, fid=fid)
 if size(res, /TYPE) eq 8 and n_elements(varname) eq 1 then begin
   newres={fapar:res.fapar, slope_fapar:res.slope_fapar, offset_fapar:res.offset_fapar, valid:res.valid, fillvalue:-1, fillvalue_exist:0}
   res=0
   res=newres
 endif
 return, res

end