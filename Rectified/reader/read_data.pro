pro read_data, dir, filename, name, array, slope, offset, fillvalue, info=INFO
  ;
  ;
  ; Inputs:
  ; dir=directory of the files
  ; filename = name file
  ; name = name of the field
  ;   i.e. 'FAPAR ????'
  ;
  ; to know ALL name just call the routine with /INFO
  ;
  ;
  ;print, 'Read ', name
  ;dir='/home/cecchgu/MODIS_FAPAR/FAPAR_MODIS/'
  ;filename=dir+'MODIS_250_127_1_955_2003.hdf'
  ;
  ;dir='/net/froz/exports/vol07/cecchgu/MODIS2010/FAPAR/FAPAR_MODIS/'

  ;file=dir+filename ;'MODIS_250_guido_122_2010.hdf'
  ;print, file
  ;stop
  ;
  ; open the hdf file
  ;
  print, dir+filename
  sd_id=HDF_SD_START(dir+Filename,/READ)
  ;
  ;
  ; when /info is on - read all names attributes and print them ....
  ;
  ;
  ;
  IF KEYWORD_SET(INFO) THEN BEGIN
    ;Take the number of data sets, SDS, and assign
    ;  to the variable datasets:
    HDF_SD_FILEINFO, sd_id, datasets, attributes
    FOR I=0,datasets-1 DO begin
      sds_id=HDF_SD_SELECT(sd_id,I)
      HDF_SD_GETINFO,sds_id,DIMS=dims,NDIMS=ndims, NAME=name,NATTS=natts,TYPE=ty
      ;
      ;
      PRINT,FORMAT='(I0,".",3A0,'+STRING(ndims)+'(I0,:,"x"),$)', I, "Short name:  ", name , ", size: " , dims
      PRINT,", type: ",ty
      FOR J=0,natts-1 DO BEGIN
        HDF_SD_ATTRINFO,sds_id,J,NAME=name,DATA=attr_dat
        PRINT,FORMAT='(A0,":  ",5(A0))',name,attr_dat
        attr_dat=""
        name=""
      ENDFOR
      ;endif
      ;
    ENDFOR

    ;stop
  ENDIF       ;;;; INFO

  ;stop
  print,'Field is:' + name
  ;stop
  SDSNo = HDF_SD_NAMETOINDEX(sd_id, name) ;find the SDS offset number
  print, SDSNo
  IF SDSNo NE -1 THEN GETDATA, sd_id, SDSNo, name, array, dims, chanid, fillvalue
  sds_id = HDF_SD_SELECT(SD_ID, SDSNo)
  aindex = HDF_SD_ATTRFIND(sds_id, 'scale_factor')

  ;stop
  IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=Slope else begin
    aindex = HDF_SD_ATTRFIND(sds_id, 'slope')
    IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=Slope else begin
      aindex = HDF_SD_ATTRFIND(sds_id, 'reflectance_scales')
      IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=Slope
    endelse
  endelse

  aindex = HDF_SD_ATTRFIND(sds_id, 'add_offset')
  IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=offset else begin
    aindex = HDF_SD_ATTRFIND(sds_id, 'intercept')
    IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=offset else begin
      aindex = HDF_SD_ATTRFIND(sds_id, 'reflectance_offsets')
      IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=offset
    endelse
  endelse

  aindex = HDF_SD_ATTRFIND(sds_id, '_FillValue')
  IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=fillvalue

  ;print, slope,offset
  ;image=FLOAT(array)
  ;image[*,*]=slope*image[*,*]+offset
  ;stop
  ;window, /free, xsize = dims(0)/10, ysize = dims(1)/10
  ;tvscl, reverse(congrid(array/slope(0)+offset(0), dims(0)/10, dims(1)/10),2)
  ;stop
  HDF_SD_END, sd_id
  ;stop
END