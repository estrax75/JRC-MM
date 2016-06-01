pro write_eos, fileName, bandName, bandList, boundaryInfo

  ;boundaryInfo=[-180, -90, 180, 90]

  upleft  = dblarr(2)
  lowrgt  = dblarr(2)
  exampleBand=*bandList[0]
  sizeInfo=size(exampleBand, /STRUCT)
  idx=where(sizeInfo.Dimensions ne 0)
  trueDims=sizeInfo.Dimensions[idx]

  upleft[0]=boundaryInfo[0]*1000000 ;-32000000.0d       ; Longitude (packed format)
  upleft[1]=boundaryInfo[1]*1000000 ;  72000000.0d       ; Latitude

  lowrgt[0]=boundaryInfo[2]*1000000 ;70000000.0d
  lowrgt[1]=boundaryInfo[3]*1000000 ;  25000000.0d

  xdim = dims[0];6258L
  ydim = dims[1];4352L

  file_id  = EOS_GD_OPEN(fileName,/create)
  grid_id = EOS_GD_CREATE(file_id,"map",xdim,ydim,upleft,lowrgt)
  status = EOS_GD_DEFCOMP(grid_id, 0)
  status  = EOS_GD_DEFPROJ(grid_id,0,0,0,0)
  status  = EOS_GD_DEFDIM(grid_id,'classes',9L)
  status  = EOS_GD_DEFDIM(grid_id,'lat',long(ydim));4352L)
  status  = EOS_GD_DEFDIM(grid_id,'lon',long(xdim));6258L)

  status  = EOS_GD_DEFFIELD(grid_id,'country',  'lon,lat',21)

  ;status  = EOS_GD_WRITEFIELD(grid_id,'country',a)
  status  = EOS_GD_WRITEFIELD(grid_id,'country',exampleBand)

  south_lat = boundaryInfo[3];25.
  st = EOS_GD_WRITEATTR(grid_id, 'Southernmost_Latitude',south_lat)
  north_lat = boundaryInfo[1];72.
  st = EOS_GD_WRITEATTR(grid_id, 'Northernmost_Latitude',north_lat)
  east_long = boundaryInfo[2];70.
  st = EOS_GD_WRITEATTR(grid_id, 'Easternmost_Longitude',east_long)
  west_long =boundaryInfo[0];-32.
  st = EOS_GD_WRITEATTR(grid_id, 'Westernmost_Longitude',west_long)
  num_colum =xdim;6258
  st = EOS_GD_WRITEATTR(grid_id, 'Number_of_Columns'    ,num_colum)
  num_lines =ydim;4352
  st = EOS_GD_WRITEATTR(grid_id, 'Number_of_Rows'       ,num_lines)

  vg_id   = EOS_GD_DETACH(grid_id)
  st = EOS_GD_CLOSE(file_id)

end