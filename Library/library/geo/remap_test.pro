pro REMAP_TEST, OUT_IMAGE=newEuroData

  file1='C:\Program Files\ITT\IDL\IDL81\STE_81\IDL_JRC_Mirko\test_baltico\A20030322003059.L3m_MO.BALT'
  
  fileID = HDF_SD_START(file1,/READ)
  dataIndex=0
  iD = HDF_SD_SELECT(fileID, dataIndex)
  HDF_SD_GETINFO, iD, name = name, natts = natts, $
    dims=dims
  ;[dsname, dsndims, dsdims, dstype, dsatts, stat] = hdfsd('getinfo',sds_id);
  ;map_limits = hdfsd('readattr', sds_id, hdfsd('findattr',sds_id,'Limit'));
  res=HDF_GET_ATTR(fileID, 'Limit', sdsidx=0, map_limits)
  ;  % Swap the latitudes round because the map is 'upside down'
  
  swap=map_limits[0]
  map_limits[0] = map_limits[2];
  map_limits[2]=swap
  
  ;hdf_close,  sds_id
  
  dataset_index = HDF_SD_NAMETOINDEX(fileID, 'taua_443-Mean')
  ;dataset_index=0
  ;for i=0, 6 do begin
  ;datasetID = HDF_SD_Select(fileID, dataset_index)
  datasetID = HDF_SD_SELECT(fileID, dataset_index)
  HDF_SD_GETDATA, datasetID, thisData;, $
  ;a=where(thisData ne 0., count)
  ;doLog, i, count
  ;endfor
  ;hdf_close, fileID
  
  dims=SIZE(thisData, /DIMENSIONS)
  ;;forse questa e' piu' veloce non ci sono cicli!
  grid=meshgrat([map_limits[2],map_limits[0]], [map_limits[1],map_limits[3]], dims)
  
  ;grid=meshgrat([map_limits[0],map_limits[2]], [map_limits[1],map_limits[3]], dims)
  
  euroLimit=[30,66.0,-13,42.0]
  euroData=FLTARR(4000,3000)
  
  ;testGrid=meshgrat([-90,90], [-180,180], [3,5])
  ;  dims_euro = size(euroData, /DIMENSIONS)
  ;  euroGrid=meshgrat(euroLimit[0:2], euroLimit[1:3], dims_euro)
  
  ;newEuroData should replace ONLY pixels coming from thisData without change others pixels
  newEuroData=LOCATEONGRID(thisData, euroData, map_limits, euroLimit)
  IIMAGE, newEuroData
end
