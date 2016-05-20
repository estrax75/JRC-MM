pro write_ncdf, fileName, bandNames, bandList, boundaryInfo
  ; procedure to read a geoTiff and export an a new one

  if strlen(fileName) gt 120 then message, 'choose a shorter full filename, less than 120 characters', fileName

  blnGSL=0
  ;  if (blnGSL eq 1)then begin
  ;    strFile='/home/robusmo/FAPAR/idl/readGeoTIFF/input/march_2016/GSL_TIFFs/EU_ GSL_1975.tif'
  ;    outputFile='/home/robusmo/FAPAR/idl/readGeoTIFF/input/march_2016/GSL_TIFFs/'
  ;    strFName=outputfile+'GSL.nc'
  ;  endif else begin
  ;    strFile='/home/robusmo/FAPAR/idl/readGeoTIFF/input/march_2016/SPI3_TIFFs/EU_SPI3_1975.tif'
  ;    outputFile='/home/robusmo/FAPAR/idl/readGeoTIFF/input/march_2016/SPI3_TIFFs/'
  ;    strFName=outputfile+'SPI3.nc'
  ;  endelse

  exampleBand=*bandList[0]

  ;inputArray=READ_TIFF(strFile, GEOTIFF=GeoKeys)

  ;help,exampleBand
  sizeInfo=size(exampleBand, /STRUCT)
  idx=where(sizeInfo.Dimensions ne 0)
  trueDims=sizeInfo.Dimensions[idx]

  xdim=sizeInfo.Dimensions[0]
  ydim=sizeInfo.Dimensions[1]
  ;print,GeoKeys

  id_new  = NCDF_CREATE(fileName,/CLOBBER)
  NCDF_CONTROL, id_new, /FILL
  ;zdim=40
  yid = NCDF_DIMDEF(id_new,'lat',ydim)
  xid = NCDF_DIMDEF(id_new,'lon',xdim)
  ;zid = NCDF_DIMDEF(id_new,'time',zdim)

  ;if (blnGSL eq 1)then begin
  ;ds_id    = NCDF_VARDEF(id_new,'GSL',[xid,yid,zid],/LONG)
  ;for i=0, n_elements(bandNames) -1 do begin
  ds_id = NCDF_VARDEF(id_new,bandNames[i],trueDims,$
    BYTE=sizeInfo.TYPE eq 1, DOUBLE=sizeInfo.TYPE eq 5, FLOAT=sizeInfo.TYPE eq 4, INT=sizeInfo.TYPE eq 2, LONG=sizeInfo.TYPE eq 3, STRING=sizeInfo.TYPE eq 7)
  ;endfor
  ;endif else begin
  ;ds_id    = NCDF_VARDEF(id_new,'SPI3',[xid,yid,zid],/DOUBLE)
  ;ds_id    = NCDF_VARDEF(id_new,'SPI3',[xid,yid],/DOUBLE)
  ;endelse
  ; definition fields
  lat_id      = NCDF_VARDEF(id_new,'lat',yid,/FLOAT)
  lon_id      = NCDF_VARDEF(id_new,'lon',xid,/FLOAT)
  ;time_id      = NCDF_VARDEF(id_new,'time',zid,/LONG)

  lon_ini=boundaryInfo[0];1600000
  lonMax=boundaryInfo[1];7625000
  lat_ini=boundaryInfo[2];766700
  latMax=boundaryInfo[3];5966700
  res_lon=(lonMax-lon_ini)/xdim
  res_lat=(latMax-lat_ini)/ydim
  lon=FLTARR(xdim)
  for i=0, xdim-1 do begin
    anchorPointShift=0.5
    lon[i]=FLOAT(lon_ini)+FLOAT(res_lon*(i+anchorPointShift))
  endfor
  lat=FLTARR(ydim)
  for i=0, ydim-1 do begin
    anchorPointShift=0.5
    lat[i]=FLOAT(lat_ini)+FLOAT(res_lat*(i+anchorPointShift))
  endfor
  ;time=UINTARR(zdim)
  ;for i=0, zdim-1 do begin
  ;	time[i]=1975+i
  ;endfor
  ;print ,time

  if (blnGSL eq 1)then begin
    ;arrayValues=LONARR(xdim,ydim,zdim)
    arrayValues=LONARR(xdim,ydim)
  endif else begin
    ;arrayValues=DBLARR(xdim,ydim,zdim)
    arrayValues=DBLARR(xdim,ydim)
  endelse

  ;for i=1975, 2014 do begin


  ;    if (blnGSL eq 1)then begin
  ;      strFile='/home/robusmo/FAPAR/idl/readGeoTIFF/input/march_2016/GSL_TIFFs/EU_ GSL_'+STRTRIM(string(i),1)+'.tif'
  ;    endif else begin
  ;      strFile='/home/robusmo/FAPAR/idl/readGeoTIFF/input/march_2016/SPI3_TIFFs/EU_SPI3_'+STRTRIM(string(i),1)+'.tif'
  ;    endelse
  ;    print,strFile
  ;    inputArray=READ_TIFF(strFile, GEOTIFF=GeoKeys)
  ;
  ;
  ;    help,inputArray
  ;    a=size(inputArray)
  ; Need?
  exampleBand=reverse(exampleBand,2)
  if (blnGSL eq 1)then begin
    inputArray=LONG(exampleBand)
  endif else begin
    inputArray=DOUBLE(exampleBand)
  endelse
  ;print,inputArray[100,100]
  ;stop

  ;arrayValues[*,*,i-1975]=inputArray[*,*]
  arrayValues[*,*]=inputArray[*,*]
  ;  endfor
  ;  stop


  if (blnGSL eq 1)then begin
    ;--------------------------------
    ;GSL
    ;--------------------------------
    ; global attributes
    NCDF_ATTPUT, id_new, "Conventions", "CF-1.6", /GLOBAL
    NCDF_ATTPUT, id_new, /GLOBAL,"title", "Length of thermal growing season", /CHAR
    strTemp='2'
    ;NCDF_ATTPUT, id_new, /GLOBAL, "tier" , strTemp, /CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"software_version",'1.0',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"platform",'all',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"algorithm", "Mitchell, T.D. and Hulme, M. 2002. Length of the growing season. Weather, 57, 196-198.", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"sensor",'AVHRR',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"contact_email", "nadine.gobron@jrc.ec.europa.eu", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_resolution",'1 day',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_duration",'1 day',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_start",'01-01-1975 T00:00:00',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_end",'31-12-2014 T23:59:59',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_start",date,/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_end",date,/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"crs",'EPSG:3035',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_resolution",'25000',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_resolution_unit",'meters',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_resolution",'25000',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_resolution_unit",'meters',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_min",'766700',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_max",'5966700',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_min",'1600000',/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_max",'7625000',/CHAR
    ;lon_ini=boundaryInfo[0];1600000
    ;lonMax=boundaryInfo[1];7625000
    ;lat_ini=boundaryInfo[2];766700
    ;latMax=boundaryInfo[3];5966700

    NCDF_ATTPUT, id_new, /GLOBAL,"crs",'EPSG:3035',/CHAR; change with the right one
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_resolution",string(res_lon, format='f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_resolution_unit",'deg',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_resolution",string(res_lat, format='f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_resolution_unit",'deg',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_min",string(boundaryInfo[2], format='(f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_max",string(boundaryInfo[3], format='(f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_min",string(boundaryInfo[0], format='(f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_max",string(boundaryInfo[1], format='(f(8.2)'),/CHAR
    ;a=systime()
    NCDF_ATTPUT, id_new, /GLOBAL,"data_created",'05-05-2016 T10:00:00',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"creator_name",'Nadine Gobron',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"creator_url",'https://ec.europa.eu/jrc/en/institutes/ies',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"creator_email",'nadine.gobron@jrc.ec.europa.eu',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"project", "LDTRR project", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"frequency", "--", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"frequency tag", "--", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"history", "--", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"comment", "--", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"standard_name_vocabulary", "--", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"keywords", "brdf, nir, red", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"summary", "", /CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"time_number_steps",STRTRIM('days',1),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"cdm_datatype","ETRS_1989_LAEA",/CHAR		; grid check
    NCDF_ATTPUT, id_new, /GLOBAL,"license","Copyright: European Communities, 2016",/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"source","JRC LDTR datasets", /CHAR

    ; latitude attributes
    NCDF_ATTPUT, id_new, lat_id, 'long_name', 'Latitude'
    NCDF_ATTPUT, id_new, lat_id, 'standard_name', 'grid_latitude'
    NCDF_ATTPUT, id_new, lat_id, 'units', 'deg'
    NCDF_ATTPUT, id_new, lat_id, 'axis' , 'Y'

    ; longitude attributes
    NCDF_ATTPUT, id_new, lon_id, 'long_name', 'Longitude'
    NCDF_ATTPUT, id_new, lon_id, 'standard_name', 'grid_longitude'
    NCDF_ATTPUT, id_new, lon_id, 'units', 'deg'
    NCDF_ATTPUT, id_new, lon_id, 'axis' , 'X'

    ; time attributes
    ;NCDF_ATTPUT, id_new, time_id, 'long_name', 'Time'
    ;NCDF_ATTPUT, id_new, time_id, 'standard_name', 'time'
    ;NCDF_ATTPUT, id_new, time_id, 'units', 'year'
    ;NCDF_ATTPUT, id_new, time_id, 'axis' , 'Z'


    ; in_var attributes
    ; GSL
    NCDF_ATTPUT, id_new, ds_id, 'long_name', 'BRDFs'
    NCDF_ATTPUT, id_new, ds_id, 'standard_name', 'BRDFs'
    NCDF_ATTPUT, id_new, ds_id, 'grid_mapping', 'LAEA'
    NCDF_ATTPUT, id_new, ds_id, 'units', 'day'
    NCDF_ATTPUT, id_new, ds_id, 'valid_min', 1,/LONG
    NCDF_ATTPUT, id_new, ds_id, 'valid_max', 1,/LONG
    NCDF_ATTPUT, id_new, ds_id, '_fillValue',32767,/LONG
    NCDF_ATTPUT, id_new, ds_id, '_NoDataValue',-999,/LONG

  endif else begin
    ;-------------------------------------
    ; SPI3
    ;-------------------------------------
    ; global attributes
    NCDF_ATTPUT, id_new, "Conventions", "CF-1.6", /GLOBAL
    NCDF_ATTPUT, id_new, /GLOBAL,"title", "BRFs", /CHAR
    ;strTemp='2'
    ;NCDF_ATTPUT, id_new, /GLOBAL, "tier" , strTemp, /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"software_version",'1.0',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"platform",'all',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"algorithm", "alg name author", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"sensor",'AVHRR',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"contact_email", "nadine.gobron@jrc.ec.europa.eu", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_resolution",'',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_duration",'1 day',/CHAR
    ;use date as parameter
    NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_start",'',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"time_coverage_end",'',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"crs",'EPSG:3035',/CHAR ;change with the right one
    ;lon_ini=boundaryInfo[0];1600000
    ;lonMax=boundaryInfo[1];7625000
    ;lat_ini=boundaryInfo[2];766700
    ;latMax=boundaryInfo[3];5966700
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_resolution",string(res_lon, format='f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_resolution_unit",'deg',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_resolution",string(res_lon, format='f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_resolution_unit",'deg',/CHAR
    ;;
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_min",string(boundaryInfo[2], format='(f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_max",string(boundaryInfo[3], format='(f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_min",string(boundaryInfo[0], format='(f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_max",string(boundaryInfo[1], format='(f(8.2)'),/CHAR
    ;;
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_min",string(boundaryInfo[2], format='(f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lat_max",string(boundaryInfo[3], format='(f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_min",string(boundaryInfo[0], format='(f(8.2)'),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"geospatial_lon_max",string(boundaryInfo[1], format='(f(8.2)'),/CHAR
    nowTime=systime()
    NCDF_ATTPUT, id_new, /GLOBAL,"data_created",nowTime,/CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"data_created",'09-03-2016 T10:00:00',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"creator_name",'Nadine Gobron',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"creator_url",'https://ec.europa.eu/jrc/en/institutes/ies',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"creator_email",'nadine.gobron@jrc.ec.europa.eu',/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"project", "LDTR project", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"frequency", "--", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"frequency tag", "--", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"history", "--", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"comment", "--", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"standard_name_vocabulary", "--", /CHAR
    ; some additional missing information
    NCDF_ATTPUT, id_new, /GLOBAL,"keywords", "put keywords here", /CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"summary", "Put summary here", /CHAR
    ;NCDF_ATTPUT, id_new, /GLOBAL,"time_number_steps",STRTRIM('days',1),/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"cdm_datatype","ETRS_1989_LAEA",/CHAR		; grid change with right one
    NCDF_ATTPUT, id_new, /GLOBAL,"license","Copyright: European Communities, 2016",/CHAR
    NCDF_ATTPUT, id_new, /GLOBAL,"source","JRC LDTR datasets", /CHAR

    ; latitude attributes
    NCDF_ATTPUT, id_new, lat_id, 'long_name', 'Latitude'
    NCDF_ATTPUT, id_new, lat_id, 'standard_name', 'grid_latitude'
    NCDF_ATTPUT, id_new, lat_id, 'units', 'deg'
    NCDF_ATTPUT, id_new, lat_id, 'axis' , 'Y'

    ; longitude attributes
    NCDF_ATTPUT, id_new, lon_id, 'long_name', 'Longitude'
    NCDF_ATTPUT, id_new, lon_id, 'standard_name', 'grid_longitude'
    NCDF_ATTPUT, id_new, lon_id, 'units', 'deg'
    NCDF_ATTPUT, id_new, lon_id, 'axis' , 'X'

    ; time attributes
    ;NCDF_ATTPUT, id_new, time_id, 'long_name', 'time'
    ;NCDF_ATTPUT, id_new, time_id, 'standard_name', 'time'
    ;NCDF_ATTPUT, id_new, time_id, 'units', 'year'
    ;NCDF_ATTPUT, id_new, time_id, 'axis' , 'Z'

    ; in_var attributes
    ; SPI3
    NCDF_ATTPUT, id_new, ds_id, 'long_name', 'BRFs (Corrected)'
    NCDF_ATTPUT, id_new, ds_id, 'standard_name', 'BRFs'
    NCDF_ATTPUT, id_new, ds_id, 'grid_mapping', 'Put here grid mapping'
    NCDF_ATTPUT, id_new, ds_id, 'units', 'put here measure unit'
    NCDF_ATTPUT, id_new, ds_id, 'valid_min', '-1.0',/DOUBLE
    NCDF_ATTPUT, id_new, ds_id, 'valid_max','1.0',/DOUBLE
    NCDF_ATTPUT, id_new, ds_id, '_fillValue',-1.79769313486e+308,/DOUBLE
    NCDF_ATTPUT, id_new, ds_id, '_NoDataValue',-9999,/DOUBLE

  endelse

  NCDF_CONTROL, id_new, /ENDEF

  NCDF_VARPUT, id_new, lat_id, lat
  NCDF_VARPUT, id_new, lon_id, lon
  ;NCDF_VARPUT, id_new, time_id, time
  NCDF_VARPUT, id_new, ds_id, arrayValues[0:xdim-1,0:ydim-1]
  NCDF_CLOSE, id_new

end