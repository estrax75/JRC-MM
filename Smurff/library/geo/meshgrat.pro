function meshgrat, latlim, lonlim, dim

  if n_elements(dim) eq 1 then dims=[dim,dim] else dims=dim
 
  stepLon=(abs(lonlim[1]-lonlim[0]))/(dim[0]-1)
  stepLat=(abs(latlim[1]-latlim[0]))/(dim[1]-1)

  lon = lonlim[0]+findgen(dim[0])*float(stepLon)
  lat = latlim[0]+findgen(dim[1])*float(stepLat)
  
  meshLon = lon #(intarr(dim[1])+1)
  meshLat = lat ##(intarr(dim[0])+1)
  
  return, {meshLat:meshLat, meshLon:meshLon}
  
end