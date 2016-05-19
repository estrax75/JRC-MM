;;@library/hdf/hdf_get_attr
;function readBandFromFile, fileName, hdfId, index, mask, ns, nl
;
;  ; get the mask info from the hdf
;  res = HDF_GET_ATTR(hdfId, 'Slope', sdsidx=0, slope)
;  res2 = HDF_GET_ATTR(hdfId, 'Intercept', sdsidx=0, intercept)
;  ; get the geo limits from the hdf
;  res3 = HDF_GET_ATTR(hdfId, 'Limit', sdsidx=0, mapLimits)
;  ;HDF_SD_ATTRINFO,hdfId, 1, NAME=atn,COUNT=atc,TYPE=att,DATA=d
;  HDF_SD_End, hdfId
;  
;  
;  ENVI_OPEN_DATA_FILE, fileName, r_fid=fid, /HDF_SD, HDFSD_DATASET=index
;  a=ENVI_GET_FILE_IDS()
;  ;if fid eq -1 then close, /all
;  ENVI_FILE_QUERY, Fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
;  
;  stepLon = (ABS(mapLimits[3]-mapLimits[1]))/(ns-1)
;  stepLat = (ABS(mapLimits[2]-mapLimits[0]))/(nl-1)
;  
;  ; set the map info to the regular grid in ENVI file
;  ps = [stepLon, stepLat]
;  mc = [0.5D, 0.5D, mapLimits[1], mapLimits[2]]
;  mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)
;  
;  data = ENVI_GET_DATA(fid=fid, dims=dims, pos=0)
;  validIdxs=where(data ne -9999 and data gt 0 and finite(data), count)
;  if count ne 0 then begin
;    mdata=data[validIdxs]
;    doLog, '**'+strcompress(max(mdata))+strcompress(min(mdata))+strcompress(mean(mdata))+'**'
;  endif else begin
;    doLog, '**', 'No data', '**'
;  endelse
;  thisMask = ~(data gt (0. - intercept) / slope)
;  if n_elements(mask) eq 0 then mask = thisMask else mask = mask*thisMask
;  envi_file_mng, id=fid, /REMOVE
;  
;  return, {data:data, mask:mask, mapInfo:mapInfo, nl:nl, ns:ns, dt:dt}
;;endfor
;  
;end

function readBandFromFile, fileName, hdfId, index, mask, ns, nl

  ; get the mask info from the hdf
  res = HDF_GET_ATTR(hdfId, 'Slope', sdsidx=0, slope)
  res2 = HDF_GET_ATTR(hdfId, 'Intercept', sdsidx=0, intercept)
  ; get the geo limits from the hdf
  res3 = HDF_GET_ATTR(hdfId, 'Limit', sdsidx=0, mapLimits)
  ;HDF_SD_ATTRINFO,hdfId, 1, NAME=atn,COUNT=atc,TYPE=att,DATA=d
  SDdataset_id=HDF_SD_SELECT(hdfId, index)
  HDF_SD_GETDATA, SDdataset_id, data

  HDF_SD_ENDACCESS, SDdataset_id
  HDF_SD_End, hdfId


  ;ENVI_OPEN_DATA_FILE, fileName, r_fid=fid, /HDF_SD, HDFSD_DATASET=index
  dims=size(data)
  ns=dims[1] & nl=dims[2] & dt=dims[3]
  
  ;ENVI_FILE_QUERY, Fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb

  stepLon = (ABS(mapLimits[3]-mapLimits[1]))/(ns-1)
  stepLat = (ABS(mapLimits[2]-mapLimits[0]))/(nl-1)

  ; set the map info to the regular grid in ENVI file
  ps = [stepLon, stepLat]
  mc = [0.5D, 0.5D, mapLimits[1], mapLimits[2]]
  mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)

  ;data = ENVI_GET_DATA(fid=fid, dims=dims, pos=0)
  ;if ~ARRAY_EQUAL(datahdf,data) then stop

  validIdxs=where(data ne -9999 and data gt 0 and finite(data), count)
  if count ne 0 then begin
    mdata=data[validIdxs]
    doLog, '**'+strcompress(max(mdata))+strcompress(min(mdata))+strcompress(mean(mdata))+'**'
  endif else begin
    doLog, '**', 'No data', '**'
  endelse
  thisMask = ~(data gt (0. - intercept) / slope)
  if n_elements(mask) eq 0 then mask = thisMask else mask = mask*thisMask
  ;envi_file_mng, id=fid, /REMOVE

  return, {data:data, mask:mask, mapInfo:mapInfo, nl:nl, ns:ns, dt:dt}
  ;endfor

end
