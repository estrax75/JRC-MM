
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       GetVRSInfo
;
; PURPOSE:
;
;       read VIIRS information for an image (L2 HDF file).
;
; CATEGORY:
;
;       I/O
;
; CALLING SEQUENCE:
;
;       GetVRSInfo,file,SatStruct,latitude,longitude
;
; INPUTS:
;			file:	input file name (string)
;
; OUTPUTS:
;			SatStruct:		output structure
;			latitude:		latitude for each pixel
;			longitude:		longitude for each pixel
;
; KEYWORD PARAMETERS:
;					none
;
; COMMENTS:
;		Needs routines :
;			read_hdf_dataset
;
; Output structure definition:
;
;			    SatStruct = { Name:name, $
;                  Source:' ', $
;                  Level:level, $
;                  Type:'LAC', $
;                  Software:sver, $
;                  Processing: pver, $
;                  OrbitNumber:orbit, $
;                  StartYear:syear, $
;                  EndYear:eyear, $
;                  StartDay:sday, $
;                  EndDay:eday, $
;                  StartMillisec:smsec, $
;                  EndMillisec:emsec, $
;                  StartTime:stime, $
;                  EndTime:etime, $
;                  NumberLine:nline, $
;                  NumberPixel:nelem $
;                 }
;
; REFERENCES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Written by: F. MELIN, 11/2012, JRC/IES/WRU.
;
;
;------------------------------------------------------------------------------


PRO GetSWFInfo,file,SatStruct,fstr,latitude,longitude, HOUR_SHIFT=HOUR_SHIFT


  ; Bad value standard.

  hbad=-9999.
  lbad = -1L

  ; Read attributes
  fstr = 0.
  ; Opens HDF file
  catch, error_status

  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    msg='Problem with file '+fileName+' check version, contents, existence or read permission.'
    ;errMsg=dialog_message(msg, /ERROR)
    ;message, msg
    return
  endif

  fstr = H5_PARSE(file)

  orbit = 0L

  ; --------------------------------------------------------------
  ; Data Level

  st = fstr.title._data

  level="unknown"

  IF ( STRPOS(st,"Level-1A") GE 0 ) THEN level = "L1A"
  IF ( STRPOS(st,"Level-1B") GE 0 ) THEN level = "L1B"
  IF ( STRPOS(st,"Level-2") GE 0 ) THEN level = "L2"

  ; --------------------------------------------------------------
  ; Product

  name = fstr.product_name._data

  name=STRMID(name,0,14)

  ; --------------------------------------------------------------
  ; Software version

  sver= fstr.processing_control.software_version._data

  ; --------------------------------------------------------------
  ; Processing

  pver= fstr.processing_version._data

  ;IF ( pver NE '2014.0' ) THEN print,pver ; STOP


  ; --------------------------------------------------------------

  dd = fstr.geophysical_data.l2_flags._dimensions

  ; Number of scan lines.
  nline = FIX(dd[1])
  ; Number of pixels
  nelem = FIX(dd[0])

  ; --------------------------------------------------------------
  ; Start Time

  tt = fstr.time_coverage_start._data

  sdat = STRMID(tt,0,10)
  syear = FIX(STRMID(sdat,0,4))
  smonth = FIX(STRMID(sdat,5,2))
  sd = FIX(STRMID(sdat,8,2))

  sday = JULDAY(smonth,sd,syear)-JULDAY(12,31,syear-1)

  xt = STRMID(tt,STRPOS(tt,'T')+1,STRPOS(tt,'Z')-STRPOS(tt,'T')-1)
  at = STRSPLIT(xt,':',/EXTRACT)
  stime = FLOAT(at[0])+FLOAT(at[1])/60.+FLOAT(at[2])/3600.
  smsec = (LONG(at[0])*3600+LONG(at[1])*60)*1000+LONG(at[2]*1000.)

  tt = fstr.time_coverage_end._data
  sdat = STRMID(tt,0,10)
  eyear = FIX(STRMID(sdat,0,4))
  emonth = FIX(STRMID(sdat,5,2))
  ed = FIX(STRMID(sdat,8,2))

  eday = JULDAY(emonth,ed,eyear)-JULDAY(12,31,eyear-1)

  xt = STRMID(tt,STRPOS(tt,'T')+1,STRPOS(tt,'Z')-STRPOS(tt,'T')-1)
  at = STRSPLIT(xt,':',/EXTRACT)
  etime = FLOAT(at[0])+FLOAT(at[1])/60.+FLOAT(at[2])/3600.
  emsec = (LONG(at[0])*3600+LONG(at[1])*60)*1000+LONG(at[2]*1000.)

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
    date[0]=syear & date[1]=smonth & date[2]=sday & date[3]=2
    dayOfYear=calcDayOfYear(date)+1
    if dayOfYear le 0 then stop
    ;doLog, dayOfYear, LEVEL=4
    ;CALDAT,jj,month,day
    sjday=dayOfYear
    doLog, 'dayOfYear:', dayOfYear, LEVEL=4

  ENDIF

  ; --------------------------------------------------------------


  SatStruct = { Name:name, $
    Source:' ', $
    Level:level, $
    Type:'LAC', $
    Software:sver, $
    Processing: pver, $
    OrbitNumber:orbit, $
    StartYear:syear, $
    EndYear:eyear, $
    StartDay:sday, $
    EndDay:eday, $
    StartMillisec:smsec, $
    EndMillisec:emsec, $
    StartTime:stime, $
    EndTime:etime, $
    StartHour:shour, $
    NumberLine:nline, $
    NumberPixel:nelem $
  }

  iflag = 1
  vname="latitude"
  read_nc4_data,file,fstr,vname,header,latitude,iflag
  IF ( iflag LT 0 ) THEN STOP,"Not in file: ",vname

  vname="longitude"
  read_nc4_data,file,fstr,vname,header,longitude,iflag
  IF ( iflag LT 0 ) THEN STOP,"Not in file: ",vname

END