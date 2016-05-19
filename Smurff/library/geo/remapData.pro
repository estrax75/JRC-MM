function RemapData, thisData, euroData, map_limits, euroLimit

  latlim = [map_limits[2],map_limits[0]]
  lonlim = [map_limits[1],map_limits[3]]
  
  dim = SIZE(thisData, /DIMENSIONS)
  stepLon = (ABS(lonlim[1]-lonlim[0]))/(dim[0]-1)
  stepLat = (ABS(latlim[1]-latlim[0]))/(dim[1]-1)
  
  euro_dim = SIZE(euroData, /DIMENSIONS)
  euro_stepLon = (ABS(euroLimit[3]-euroLimit[2]))/(euro_dim[0]-1)
  euro_stepLat = (ABS(euroLimit[1]-euroLimit[0]))/(euro_dim[1]-1)
  
  lon_scale_factor = steplon/euro_stepLon
  lat_scale_factor = steplat/euro_stepLat
  
  euro_lon = euroLimit[2]+FINDGEN(euro_dim[0])*FLOAT(euro_stepLon)
  euro_lat = euroLimit[0]+FINDGEN(euro_dim[1])*FLOAT(euro_stepLat)
  
  lon_loc = VALUE_LOCATE(euro_lon,lonlim[0])
  lat_loc = VALUE_LOCATE(euro_lon,latlim[0])
  ;controllare i limiti!
  
  final_x_dims = ROUND(dim[0]/lon_scale_factor)
  final_y_dims = ROUND(dim[1]/lat_scale_factor)
  
  ;conrollare che ci stia.....
  euroData[lon_loc,lat_loc] = CONGRID(thisData,final_x_dims,final_y_dims)
  RETURN, euroData
end