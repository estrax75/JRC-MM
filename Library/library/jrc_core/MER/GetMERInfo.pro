
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       GetMERInfo
;
; PURPOSE:
;
;       read MERIS information for an image (HDF file).
;
; CATEGORY:
;
;       I/O
;
; CALLING SEQUENCE:
;
;       GetMERInfo,file,DataStruct
;
; INPUTS:
;			file:	input file name (string)
;
; OUTPUTS:
;			DataStruct:		output structure
;
; KEYWORD PARAMETERS:
;					none
;
; COMMENTS:
;		Needs routines :
;			read_hdf_data
;			GetSWFName
;
;
;
; REFERENCES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Written by: F. MELIN, 09/2010, JRC/IES/GEM.
;
;
;------------------------------------------------------------------------------


PRO GetMERInfo,file,DataStruct ,i_latlon, HOUR_SHIFT=HOUR_SHIFT

  DataStruct = -1

  ; Bad value standard.

  hbad=-9999.
  lbad = -1L

  ; Read attributes

  ; Opens HDF file
  catch, error_status

  ;if error_status NE 0 THEN BEGIN
    ;ERROR=1
    ;catch, /CANCEL
    ;msg='Problem with file '+fileName+' check version, contents, existence or read permission.'
    ;;errMsg=dialog_message(msg, /ERROR)
    ;;message, msg
;    return
  ;endif

  sd_id = HDF_SD_START (file,/READ)

  ; --------------------------------------------------------------
  ; Data Level.
  index = HDF_SD_ATTRFIND(sd_id,"Title")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  level="unknown"
  st = d[0]
  if ( strpos(st,"Level-1A") GE 0 ) then level = "L1A"
  if ( strpos(st,"Level-1B") GE 0 ) then level = "L1B"
  if ( strpos(st,"Level-2") GE 0 ) then level = "L2"

  ; Default level for MERIS data
  level = "L2"

  ; --------------------------------------------------------------
  ; L1 name
  index = HDF_SD_ATTRFIND(sd_id,"Input Parameters")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE
  str = STR_SEP(d,' ')
  ii=WHERE(STRPOS(str,'MER_RR') GE 0,cnt)
  IF ( cnt EQ 1 ) THEN l1name = str[ii[0]] ELSE l1name = 'unknown'

  ; --------------------------------------------------------------
  ; Software
  index = HDF_SD_ATTRFIND(sd_id,"Software Version")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  sver= d[0]

  ; --------------------------------------------------------------
  ; Orbit Number
  index = HDF_SD_ATTRFIND(sd_id,"Orbit Number")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  orbit=long(d[0])

  ; --------------------------------------------------------------
  ; Number of scan lines.
  index = HDF_SD_ATTRFIND(sd_id,"Number of Scan Lines")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  nline = fix(d[0])

  ; Number of pixels
  index = HDF_SD_ATTRFIND(sd_id,"Pixels per Scan Line")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  nelem = fix(d[0])

  ; --------------------------------------------------------------

  index = HDF_SD_ATTRFIND(sd_id,"Start Year")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  syear = fix(d[0])

  index = HDF_SD_ATTRFIND(sd_id,"End Year")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  eyear = fix(d[0])

  ; --------------------------------------------------------------

  index = HDF_SD_ATTRFIND(sd_id,"Start Day")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  sday = fix(d[0])

  index = HDF_SD_ATTRFIND(sd_id,"End Day")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  eday = fix(d[0])

  ; --------------------------------------------------------------

  index = HDF_SD_ATTRFIND(sd_id,"Start Millisec")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  smsec = long(d[0])

  index = HDF_SD_ATTRFIND(sd_id,"End Millisec")

  IF ( index GE 0 ) THEN HDF_SD_ATTRINFO,sd_id,index,name=n,type=t,data=d,hdf_type=h $
  ELSE BEGIN
    print,"Attribute not found"
    stop
  ENDELSE

  emsec = long(d[0])


  ; Close HDF session.

  HDF_SD_END,sd_id

  ; #############################################################
  IF ( i_latlon EQ 1 ) THEN BEGIN
    ; Read scan line data for geolocation.

    name="latitude"
    mer_read_hdf_dataset,file,name,latitude,iflag
    if ( iflag LT 0 ) then begin
      print,"Not in file:",name
      stop
    endif

    name="longitude"
    mer_read_hdf_dataset,file,name,longitude,iflag
    if ( iflag LT 0 ) then begin
      print,"Not in file:",name
      stop
    endif

    res = SIZE(latitude)

    IF ( nline NE res[2] OR nelem NE res[1] ) THEN BEGIN
      print,"Inconsistent dimensions",nline,res[2],nelem,res[1]
      stop
    ENDIF

  ENDIF
  ; #############################################################

  ; Output value.

  stime = float ( smsec ) / 1000. / 3600.
  etime = float ( emsec ) / 1000. / 3600.
  shour=stime & ehour=etime

  if n_elements(HOUR_SHIFT) eq 1 then begin
    Delta_t = HOUR_SHIFT

    ;time = time-delta_t
    shour = shour+delta_t
    IF ( shour LT 0. ) THEN BEGIN
      shour = shour + 24.
      sday = sday-1
      IF ( sday LE 0 ) THEN BEGIN
        jDayFirst = julday(smonth, 1, syear, 0)
        jDayLast = julday((smonth mod 12)+1, 1, nextYear, 0)
        sday=jDayLast-jDayFirst
      endif
    ENDIF
    ;doLog, dayOfYear, LEVEL=4
    ;CALDAT,jj,month,day
    eday=sday
    doLog, 'dayOfYear:', sday, LEVEL=4

  ENDIF

  DataStruct    = { Name:" ", $
    Source:l1name, $
    Level:level, $
    Type: "LAC", $
    Software:sver, $
    OrbitNumber:orbit, $
    StartYear:syear, $
    EndYear:eyear, $
    StartDay:sday, $
    EndDay:eday, $
    StartMillisec:smsec, $
    EndMillisec:emsec, $
    StartTime:stime, $
    EndTime:etime, $
    NumberLine:nline, $
    NumberPixel:nelem }

  IF ( i_latlon EQ 1 ) THEN BEGIN

    DataStruct = CREATE_STRUCT(DataStruct,"latitude",latitude)

    DataStruct = CREATE_STRUCT(DataStruct,"longitude",longitude)

  ENDIF

  GetMERName,DataStruct,name

  name=STRMID(name,0,14)

  DataStruct.Name = name

END
