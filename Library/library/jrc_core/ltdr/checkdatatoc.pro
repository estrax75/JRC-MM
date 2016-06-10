;soilCoeffs[0] As,   soilCoeffs[1] Bs
function CheckDataTOC, red, nir, soilCoeffs
  ;
  ; to be updated
  ;
  ; for AVHRR
  ;
  mask = (((red le 0.) + (nir le 0.)) gt 0.) * 1
  mask = mask + (((red ge 0.5) + (nir ge 0.6)) gt 0) * (mask eq 0) * 2
  ; old version 
  ;mask = mask + (nir lt 0.15) * (mask eq 0) * 3
  ;mask = mask + (red gt 0.3) * (mask eq 0) * 4
  ;mask = mask + (nir lt (1.33*red)) * (mask eq 0) * 4
  ; new (check table) 
  print, 'Bare soil As', 'Bare soil Bs', soilCoeffs[0], soilCoeffs[1]
  mask = mask + (nir lt (soilCoeffs[0]*red+soilCoeffs[1])) * (mask eq 0) * 4
  return, byte(mask)
end
