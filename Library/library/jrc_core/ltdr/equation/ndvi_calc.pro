function NDVI_calc, b1, b2

  if ( (b2+b1) ne 0 ) then return, ((b2-b1)/(b2+b1)) else return, 0.0

end
