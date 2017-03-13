PRO get_s_hdf_data1, filename, name, array, fillvalue, add_offset, scale_factor, ERROR=ERROR

  ERROR=0
  Catch, theError
  
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    ERROR=1
    print, '*****'
    print, name
    print, 'GetData failed, check file contents and/or download it again'
    print, '*****'
    RETURN
  ENDIF

  newFileID = HDF_SD_START(filename, /READ)
  ;HDF_SD_FILEINFO, newFileID, datasets, attributes

  index = HDF_SD_NAMETOINDEX(newFileID, strupcase(name))

  sds_id = HDF_SD_SELECT(newFileID,index)
  HDF_SD_GETDATA, SDS_ID, array

  prev=!QUIET
  !QUIET=1
  dindex = HDF_SD_ATTRFIND(sds_id, '_FillValue')
  IF dindex GE 0 THEN $
    HDF_SD_ATTRINFO, sds_id, dindex, DATA=fillvalue

  dindex = HDF_SD_ATTRFIND(sds_id, 'add_offset')
  IF dindex GE 0 THEN $
    HDF_SD_ATTRINFO, sds_id, dindex, DATA=add_offset

  dindex = HDF_SD_ATTRFIND(sds_id, 'scale_factor')
  IF dindex GE 0 THEN $
    HDF_SD_ATTRINFO, sds_id, dindex, DATA=scale_factor
  !QUIET=prev
  
  HDF_SD_END, newFileID
  
  ;HDF_CLOSE, newFileID

  ;------------------------------------
  RETURN
END