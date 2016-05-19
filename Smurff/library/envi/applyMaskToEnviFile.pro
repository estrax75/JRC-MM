pro applyMaskToEnviFile, inEnviFileName, outEnviFileName, bandNumber, exampleBand, mask, nanValue


  openr, lunIn, inEnviFileName, /GET_LUN
  openw, lunOut, outEnviFileName, /GET_LUN
  
  for i=0, bandNumber-1 do begin
  
    readu, lunIn, exampleBand
    idx_nan = WHERE(mask eq 1, count)
    if (count gt 0) then exampleBand[idx_nan] = nanValue
    writeu, lunOut, exampleBand
    
  endfor
  free_lun, lunIn
  free_lun, lunOut
  file_delete, inEnviFileName
  
end
