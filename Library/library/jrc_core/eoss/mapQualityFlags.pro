FUNCTION mapQualityFlags, data, checkFlagMatrix, destValue

  if checkFlagMatrix[0] ge 0 then data[checkFlagMatrix]=destValue
  return, data
;  if idx_3(0) ge 0 then begin
;    output.fpar(idx_3)=253
;    output.sigma(idx_3)=253
;    output.red(idx_3)=253
;    output.nir(idx_3)=253
;    output.sigma_red(idx_3)=253
;    output.sigma_nir(idx_3)=253
;  endif
;  ;
;  if idx_2(0) ge 0 then begin
;    output.fpar(idx_2)=254
;    output.sigma(idx_3)=254
;    output.red(idx_3)=254
;    output.nir(idx_3)=254
;    output.sigma_red(idx_3)=254
;    output.sigma_nir(idx_3)=254
;  endif
;  ;
;  if idx_1(0) ge 0 then begin
;    output.fpar(idx_1)=255
;    output.sigma(idx_3)=255
;    output.red(idx_3)=255
;    output.nir(idx_3)=255
;    output.sigma_red(idx_3)=255
;    output.sigma_nir(idx_3)=255
;  endif
;  return, output
  
END