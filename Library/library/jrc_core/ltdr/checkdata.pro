function CheckData, blue, red, nir
  ;
  ; to be updated
  ;
  ; for AVHRR
  ;
  mask = (((blue le 0.) + (red le 0.) + (nir le 0.)) gt 0.) * 1
  mask = mask + (((blue ge 0.3) + (red ge 0.5) + (nir ge 0.7)) gt 0) * (mask eq 0) * 2
  mask = mask + (blue gt nir) * (mask eq 0) * 3
  mask = mask + (nir lt (1.3*red)) * (mask eq 0) * 4
  ;
  ;
  ; for Landsat
  ;
  ;    mask = (((blue le 0) + (red le 0) + (nir le 0)) gt 0) * 1
  ;    mask = mask + (((blue ge 0.257752) + (red ge 0.484070) + (nir ge 0.683928)) gt 0) * (mask eq 0) * 2
  ;    mask = mask + (blue gt nir) * (mask eq 0) * 3
  ;    mask = mask + (nir le (1.26836*red)) * (mask eq 0) * 4
  ;
  ;
  ;
  return, byte(mask)
end
