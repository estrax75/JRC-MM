; docformat:'rst'

;+
; :Author:
;   
;   Frédéric Mélin 
;
; :version: 1
;   
; :History:
; 
;   Created by Frédéric Mélin, 03/2014
;   
; :Categories:
; 
;   format, ncdf
;   
; :Description:
; 
;  Read netcdf data from a name list
; 
; :Examples:
;
;   Example syntax::
;     read_nc_data,file,namelist,str(,count=count,offset=offset)
;   
; :Pre:
;			
;   
; :Requires:
; 
;   IDL 7.1
;   
; :Returns:
;  
;   Arrays containing following fields::
;   
;     strdata: structure with required data as tags
;     
; :Params:
; 
;   file: in, required, input file name (netcdf)
;			
;   namelist: in, required, type="strarr(nn)"   
; 			list of names
;			
;	strdata: out, required, required list of data, type: depends on file content
;			
;	count: in, option, type="fltarr(number of dimensions)"
;			number of elements read
;			
;	offset: in, option, type="fltarr(number of dimensions)"
;			position to start reading
;	iflag:		flag value: 1: good status, -1 otherwise
;         
;-

PRO vrs_read_nc4_data,file,fstr,vname,header,data,iflag

iflag = 1
hbad = -9999.

;fstr = 0.
;fstr = H5_PARSE(file)

taglist = TAG_NAMES(fstr.geophysical_data)
name = '/geophysical_data/'+vname

IF ( vname EQ 'longitude' OR vname EQ 'latitude'  ) THEN BEGIN
     taglist = TAG_NAMES(fstr.navigation_data)
	 name = '/navigation_data/'+vname
ENDIF

ii = WHERE ( taglist EQ STRUPCASE(vname), cnt)

IF ( cnt EQ 1 ) THEN BEGIN ; variable in the list

  it = ii[0] ; index for variable

fid = H5F_OPEN(file) ; open file

IF ( vname NE 'l2_flags' AND vname NE 'longitude' AND vname NE 'latitude'  ) THEN BEGIN ; normal geophysical variables
	
  scale_factor = 1.
  add_offset = 0.
  IF ( fstr.geophysical_data.(it)._datatype EQ 'H5T_INTEGER' ) THEN BEGIN
    scale_factor = fstr.geophysical_data.(it).scale_factor._data[0]
    add_offset = fstr.geophysical_data.(it).add_offset._data[0]
  ENDIF
  fillvalue = fstr.geophysical_data.(it)._fillvalue._data[0]

  header = { slope: scale_factor, intercept: add_offset, badvalue: fillvalue}

  data_id = H5D_OPEN(fid,name)
  data = H5D_READ(data_id)
  H5D_CLOSE,data_id

ENDIF ELSE BEGIN ; case of the flags, latitude, longitude

  data_id = H5D_OPEN(fid,name)
  data = H5D_READ(data_id)
  H5D_CLOSE,data_id

  header = { slope: 1., intercept: 0., badvalue: hbad}

ENDELSE

H5F_CLOSE, fid

ENDIF ELSE iflag = -1 ; variable not in list

;fstr = 0.
END