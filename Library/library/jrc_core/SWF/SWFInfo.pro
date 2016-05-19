;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       SWFInfo
;
; PURPOSE:
;
;       Get general information in SeaWiFS images + georeference
;
; CATEGORY:
;
;       I/O
;
; CALLING SEQUENCE:
;
;       SWFInfo,FileName,SWFStr
;
; INPUTS:
;			FileName:		input file name
;
; OUTPUTS:
;			SWFStr:		output structure
;
; KEYWORD PARAMETERS:
;					none
;
; COMMENTS:
;
; REFERENCES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Written by: F. MELIN, 02/2016, JRC/IES.
;
;------------------------------------------------------------------------------

PRO SWFInfo,FileName,SWFStr, HOUR_SHIFT=HOUR_SHIFT


  catch, error_status

  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    msg='Problem with file '+fileName+' check version, contents, existence or read permission.'
    ;errMsg=dialog_message(msg, /ERROR)
    ;message, msg
    return
  endif

  fid = H5F_OPEN(filename)

  attr_id = H5A_OPEN_NAME(fid,'title')
  title = H5A_READ(attr_id)
  H5A_CLOSE,attr_id

  level="unknown"
  IF ( STRPOS(title,"Level-1A") GE 0 ) THEN level = "L1A"
  IF ( STRPOS(title,"Level-1B") GE 0 ) THEN level = "L1B"
  IF ( STRPOS(title,"Level-2") GE 0 ) THEN level = "L2"


  attr_id = H5A_OPEN_NAME(fid,'orbit_number')
  OrbitNumber = H5A_READ(attr_id)
  H5A_CLOSE,attr_id
  OrbitNumber = OrbitNumber[0]

  attr_id = H5A_OPEN_NAME(fid,'processing_version')
  ProcessingVersion = H5A_READ(attr_id)
  H5A_CLOSE,attr_id

  ; start time
  attr_id = H5A_OPEN_NAME(fid,'time_coverage_start')
  cstime = H5A_READ(attr_id)
  H5A_CLOSE,attr_id
  ; format: 2003-07-04T12:05:05.519Z

  syear = FIX(STRMID(cstime,0,4))
  smonth = FIX(STRMID(cstime,5,2))
  sday = FIX(STRMID(cstime,8,2))
  shour = FIX(STRMID(cstime,11,2))

  sjday = JULDAY(smonth,sday,syear) - JULDAY(12,31,syear-1)

  tt = STRSPLIT(cstime,'T',/EXTRACT)
  hh = FIX(STRMID(tt[1],0,2))
  mm = FIX(STRMID(tt[1],3,2))
  ttt = STRSPLIT(tt[1],':',/EXTRACT)
  ss = FIX(STRMID(ttt[2],0,STRLEN(ttt[2]-1))) ; assumes 'Z' ends the string
  stime = FLOAT(hh)+FLOAT(mm)/60.+FLOAT(ss)/3600.

  cjd = STRING(sjday)
  IF ( sjday LT 100 ) THEN cjd = '0'+cjd
  IF ( sjday LT 10 ) THEN cjd = '0'+cjd

  chh = STRING(hh)
  IF ( hh LT 10 ) THEN chh = '0'+chh
  cmm = STRING(mm)
  IF ( mm LT 10 ) THEN cmm = '0'+cmm
  css = STRING(ss)
  IF ( ss LT 10 ) THEN cs = '0'+css

  name = 'S'+STRING(syear)+cjd+chh+cmm+css
  name = STRCOMPRESS(name,/REMOVE_ALL)

  ; end time
  attr_id = H5A_OPEN_NAME(fid,'time_coverage_end')
  cetime = H5A_READ(attr_id)
  H5A_CLOSE,attr_id

  eyear = FIX(STRMID(cetime,0,4))
  emonth = FIX(STRMID(cetime,5,2))
  eday = FIX(STRMID(cetime,8,2))

  ejday = JULDAY(emonth,eday,eyear) - JULDAY(12,31,eyear-1)

  tt = STRSPLIT(cetime,'T',/EXTRACT)
  hh = FIX(STRMID(tt[1],0,2))
  mm = FIX(STRMID(tt[1],3,2))
  ttt = STRSPLIT(tt[1],':',/EXTRACT)
  ss = FIX(STRMID(ttt[2],0,STRLEN(ttt[2]-1))) ; assumes 'Z' ends the string
  etime = FLOAT(hh)+FLOAT(mm)/60.+FLOAT(ss)/3600.

  cjd = STRING(ejday)
  IF ( ejday LT 100 ) THEN cjd = '0'+cjd
  IF ( ejday LT 10 ) THEN cjd = '0'+cjd

  chh = STRING(hh)
  IF ( hh LT 10 ) THEN chh = '0'+chh
  cmm = STRING(mm)
  IF ( mm LT 10 ) THEN cmm = '0'+cmm
  css = STRING(ss)
  IF ( ss LT 10 ) THEN cs = '0'+css

  ; software version
  gid = H5G_OPEN(fid,'/processing_control')

  attr_id = H5A_OPEN_NAME(gid,'software_name')
  sname = H5A_READ(attr_id)
  H5A_CLOSE,attr_id

  attr_id = H5A_OPEN_NAME(gid,'software_version')
  sver = H5A_READ(attr_id)
  H5A_CLOSE,attr_id

  sver = sname + '_' + sver
  H5G_CLOSE,gid


  ; Reads lat/lon

  FOR iv=0,1 DO BEGIN

    IF ( iv EQ 0 ) THEN vname = '/navigation_data/latitude'
    IF ( iv EQ 1 ) THEN vname = '/navigation_data/longitude'

    scale_factor = 1.
    add_offset = 0.
    hbad = -32767.

    iflag = -1


    attrlist = [' ']


    data_id = H5D_OPEN(fid,vname)

    header = { name:vname, longname:' ',units: ' ', slope:1., intercept:0., badvalue:-999999. }

    nattrs = H5A_GET_NUM_ATTRS(data_id) ; number of attributes

    FOR idx=0,nattrs-1 DO BEGIN

      attr_id = H5A_OPEN_IDX(data_id,idx)
      res = H5A_GET_NAME(attr_id)
      H5A_CLOSE,attr_id

      attrlist = [attrlist,res]
    ENDFOR

    ii = WHERE ( attrlist EQ 'long_name',cnt )
    IF ( cnt EQ 1 ) THEN BEGIN

      attr_id = H5A_OPEN_NAME(data_id,'long_name')

      res = H5A_READ(attr_id)
      H5A_CLOSE,attr_id
      header.longname = res[0]
    ENDIF

    ii = WHERE ( attrlist EQ 'scale_factor',cnt )
    IF ( cnt EQ 1 ) THEN BEGIN
      attr_id = H5A_OPEN_NAME(data_id,'scale_factor')

      res = H5A_READ(attr_id)
      H5A_CLOSE,attr_id
      header.slope = res[0]
    ENDIF

    ii = WHERE ( attrlist EQ 'add_offset',cnt )
    IF ( cnt EQ 1 ) THEN BEGIN
      attr_id = H5A_OPEN_NAME(data_id,'add_offset')

      res = H5A_READ(attr_id)
      H5A_CLOSE,attr_id
      header.intercept = res[0]
    ENDIF

    ii = WHERE ( attrlist EQ '_FillValue',cnt )
    IF ( cnt EQ 1 ) THEN BEGIN
      attr_id = H5A_OPEN_NAME(data_id,'_FillValue')

      res = H5A_READ(attr_id)
      H5A_CLOSE,attr_id
      header.badvalue = res[0]
    ENDIF

    ii = WHERE ( attrlist EQ 'units',cnt )
    IF ( cnt EQ 1 ) THEN BEGIN
      attr_id = H5A_OPEN_NAME(data_id,'units')

      res = H5A_READ(attr_id)
      H5A_CLOSE,attr_id
      header.units = res[0]
    ENDIF


    dataspace_id = H5D_GET_SPACE(data_id)

    dimensions = H5S_GET_SIMPLE_EXTENT_DIMS(dataspace_id)

    data = H5D_READ(data_id) ; read actual data

    H5S_CLOSE, dataspace_id


    H5D_CLOSE, data_id

    IF ( iv EQ 0 ) THEN BEGIN
      lat = data
      lat_header = header
    ENDIF

    IF ( iv EQ 1 ) THEN BEGIN
      lon = data
      lon_header = header
    ENDIF


  ENDFOR

  ; Reads msec
  vname = '/scan_line_attributes/msec'

  scale_factor = 1.
  add_offset = 0.
  hbad = -32767.

  iflag = -1

  attrlist = [' ']

  data_id = H5D_OPEN(fid,vname)

  header = { name:vname, longname:' ',units: ' ', slope:1., intercept:0., badvalue:-999999. }

  nattrs = H5A_GET_NUM_ATTRS(data_id) ; number of attributes

  FOR idx=0,nattrs-1 DO BEGIN

    attr_id = H5A_OPEN_IDX(data_id,idx)
    res = H5A_GET_NAME(attr_id)
    H5A_CLOSE,attr_id

    attrlist = [attrlist,res]
  ENDFOR

  ii = WHERE ( attrlist EQ 'long_name',cnt )
  IF ( cnt EQ 1 ) THEN BEGIN

    attr_id = H5A_OPEN_NAME(data_id,'long_name')

    res = H5A_READ(attr_id)
    H5A_CLOSE,attr_id
    header.longname = res[0]
  ENDIF

  ii = WHERE ( attrlist EQ 'scale_factor',cnt )
  IF ( cnt EQ 1 ) THEN BEGIN
    attr_id = H5A_OPEN_NAME(data_id,'scale_factor')

    res = H5A_READ(attr_id)
    H5A_CLOSE,attr_id
    header.slope = res[0]
  ENDIF

  ii = WHERE ( attrlist EQ 'add_offset',cnt )
  IF ( cnt EQ 1 ) THEN BEGIN
    attr_id = H5A_OPEN_NAME(data_id,'add_offset')

    res = H5A_READ(attr_id)
    H5A_CLOSE,attr_id
    header.intercept = res[0]
  ENDIF

  ii = WHERE ( attrlist EQ '_FillValue',cnt )
  IF ( cnt EQ 1 ) THEN BEGIN
    attr_id = H5A_OPEN_NAME(data_id,'_FillValue')

    res = H5A_READ(attr_id)
    H5A_CLOSE,attr_id
    header.badvalue = res[0]
  ENDIF

  ii = WHERE ( attrlist EQ 'units',cnt )
  IF ( cnt EQ 1 ) THEN BEGIN
    attr_id = H5A_OPEN_NAME(data_id,'units')

    res = H5A_READ(attr_id)
    H5A_CLOSE,attr_id
    header.units = res[0]
  ENDIF

  dataspace_id = H5D_GET_SPACE(data_id)

  dimensions = H5S_GET_SIMPLE_EXTENT_DIMS(dataspace_id)

  msec = H5D_READ(data_id) ; read actual data
  msec_header = header

  H5S_CLOSE, dataspace_id

  H5D_CLOSE, data_id


  ; Close file
  H5F_CLOSE, fid

  dd =SIZE(lat,/DIMENSIONS)
  nline = dd[1]
  nelem = dd[0]

  if n_elements(HOUR_SHIFT) eq 1 then begin
    Delta_t = HOUR_SHIFT

    ;time = time-delta_t
    shour = shour+delta_t
    IF ( shour LT 0. ) THEN BEGIN
      shour = shour + 24.
      sday = sday-1
      ; 0-365/366
      IF ( sday LE 0 ) THEN BEGIN
        smonth--
        nextYear=syear
        if smonth le 0 then begin
          smonth=12
          nextYear=syear+1
        endif
        jDayFirst = julday(smonth, 1, syear, 0)
        jDayLast = julday((smonth mod 12)+1, 1, nextYear, 0)
        sday=jDayLast-jDayFirst
      endif
    ENDIF
    ;date[0]=syear & date[1]=smonth & date[2]=sday & date[3]=2
    ;dayOfYear=calcDayOfYear(date)+1
    ;if dayOfYear le 0 then stop
    ;doLog, dayOfYear, LEVEL=4
    ;CALDAT,jj,month,day
    ;sjday=dayOfYear
    doLog, 'dayOfYear:', sjday, LEVEL=4

  ENDIF
  
  date=lonarr(4)
  date[0]=syear & date[1]=smonth & date[2]=sday & date[3]=2
  sjday=calcDayOfYear(date)+1

  SWFStr = {  Source: ' ', $
    Name: name, $
    Level: level, $
    Type: ' ', $
    Software: sver, $
    Version:ProcessingVersion, $
    OrbitNumber: OrbitNumber, $
    NumberLine:nline, $
    NumberPixel:nelem, $
    ExtractPixelOffset:-1, $
    StartYear:syear, $
    StartMonth:smonth, $
    StartDay:sday, $
    StartJDay:sjday, $
    StartTime:stime, $
    StartMillisec: -1L, $
    EndYear:eyear, $
    EndMonth:emonth, $
    EndDay:eday, $
    EndJDay:ejday, $
    EndTime:etime, $
    EndMillisec: -1L, $
    Latitude:lat, LatitudeBadV:lat_header.badvalue,  $
    Longitude:lon, LongitudeBadV:lon_header.badvalue, $
    msec:msec, msecBadV: msec_header.badvalue $
  }



END
