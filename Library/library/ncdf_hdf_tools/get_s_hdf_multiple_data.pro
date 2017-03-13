PRO get_s_hdf_multiple_data, filename, name, arrays, fillvalues, add_offsets, scale_factors, ERROR=ERROR, start=start, count=count

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

  toFind=n_elements(name)
  if toFind gt 1 then begin
    arrays=ptrarr(toFind)
    fillvalues=ptrarr(toFind)
    add_offsets=ptrarr(toFind)
    scale_factors=ptrarr(toFind)
  endif

  for i=0, toFind-1 do begin
    index = HDF_SD_NAMETOINDEX(newFileID, strupcase(name[i]))

    sds_id = HDF_SD_SELECT(newFileID,index)
    HDF_SD_GETDATA, SDS_ID, array, start=long(START), count=[3,3]
    arrays[i]=ptr_new(array, /NO_COPY)

    prev=!QUIET
    
    !QUIET=1
    dindex = HDF_SD_ATTRFIND(sds_id, '_FillValue')
    IF dindex GE 0 THEN begin
      HDF_SD_ATTRINFO, sds_id, dindex, DATA=fillvalue
      fillvalues[i]=ptr_new(fillvalue[0], /NO_COPY)
    endif

    dindex = HDF_SD_ATTRFIND(sds_id, 'add_offset')
    IF dindex GE 0 THEN begin
      HDF_SD_ATTRINFO, sds_id, dindex, DATA=add_offset
      add_offsets[i]=ptr_new(add_offset[0], /NO_COPY)
    endif

    dindex = HDF_SD_ATTRFIND(sds_id, 'scale_factor')
    IF dindex GE 0 THEN begin
      HDF_SD_ATTRINFO, sds_id, dindex, DATA=scale_factor
      scale_factors[i]=ptr_new(scale_factor[0], /NO_COPY)
    endif

  endfor
  !QUIET=prev

  HDF_SD_END, newFileID

  ;HDF_CLOSE, newFileID

  ;------------------------------------
  RETURN
END