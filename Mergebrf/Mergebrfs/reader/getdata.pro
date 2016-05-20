PRO GETDATA, sd_id, SDSNo, name, array, dims, chanid, fillvalue

  chanid=0

  Catch, theError
  
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    print, 'GetData failed, check file contents and/or download it again'
    RETURN
  ENDIF

  sds_id = HDF_SD_SELECT(SD_ID,SDSNo)
  HDF_SD_GETDATA, SDS_ID, array

  HDF_SD_GETINFO, sds_id, LABEL=l, UNIT=u, FORMAT=f, $
    COORDSYS=c,NDIMS=ndims, DIMS=dims, TYPE=ty,NAME=name
  PRINT,name
  ;find the id of the band_names
  !QUIET=1
  dindex = HDF_SD_ATTRFIND(sds_id, 'band_names')
  !QUIET=0
  IF dindex GT 0 THEN BEGIN        ;if Earth View SD is selected
    HDF_SD_ATTRINFO, sds_id, dindex, DATA=chanid
    chanid=STR_SEP(chanid, ",")
    ;chanid=FIX(chanid[0])      ;find the first channel in the band_names
  ENDIF ELSE BEGIN
    ;if cannot find band names,
    ;then simply use 1,2,...
    IF ndims GT 2 THEN $
      chanid=STRING(INDGEN(dims[ndims-1])+1)
    chanid=STRCOMPRESS(chanid,/REMOVE_ALL)
  ENDELSE
  ;---find the id of the fill value---
  !QUIET=1
  dindex = HDF_SD_ATTRFIND(sds_id, '_FillValue')
  IF dindex GT 0 THEN $
    HDF_SD_ATTRINFO, sds_id, dindex, DATA=fillvalue
  !QUIET=0
  ;------------------------------------
  RETURN
END