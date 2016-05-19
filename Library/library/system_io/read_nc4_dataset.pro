
; docformat:'rst'

;+
; Function to read netcdf-4 data.  
;
; :version: 1
;
; :Author:
;			
;   Frederic Melin (European Commission/JRC/IES)
;
; :Categories:
; 
;   reading
;   
; :Description:
; 
;   reads Level-3 data in netcdf-4 format (typically NASA L3 SMI data), and returns a structure.
;	by defining 'start' and 'count', only a slab of the data is read.
;   
;   Contains::
;   
;     vname: name of the variable
;	  data: data (2D) 
;	  scale: scaling factor (if available, default = 1)
;	  offset: intercept (if available, default = 0)
;	  badv: bad value (if available, default = -32767.)
;     
; :Requires:
; 
;   IDL
;   
; :Uses:
; 
;   strdata = read_nc4_dataset(ifile,vname,start=[selem,sline],count=[nx,ny])
;			or:
;   strdata = read_nc4_dataset(ifile,vname)
;   
; :Params:
; 
;   ifile: in, type=string
;			
;      input file
;			
;   vname: in, type=string
;   
;     name of the variable to be read (for example: "/chlor_a")
;			
;   start: in, type integer, optional (default: [0,0])
;			
;	    vector with start element and line;
;
;	count: in, type integer, optional (ignored if 'start' absent)
;			
;	    vector with number of elements and lines to be read;
;     
;              
; :Keywords:
;   
;-


FUNCTION read_nc4_dataset,file,vname,start=start,count=count

hyperslab = 0
IF KEYWORD_SET(start) THEN hyperslab=1

scale_factor = 1.
add_offset = 0.
hbad = -32767.

fid = H5F_OPEN(file)

data_id = H5D_OPEN(fid,vname)

attr_id = H5A_OPEN_NAME(data_id,'scale_factor')
IF ( attr_id GT 0 ) THEN BEGIN
  res = H5A_READ(attr_id)
  H5A_CLOSE,attr_id
  scale_factor = res[0]
ENDIF

attr_id = H5A_OPEN_NAME(data_id,'add_offset')
IF ( attr_id GT 0 ) THEN BEGIN
  res = H5A_READ(attr_id)
  H5A_CLOSE,attr_id
  add_offset = res[0]
ENDIF

attr_id = H5A_OPEN_NAME(data_id,'_FillValue')
IF ( attr_id GT 0 ) THEN BEGIN
  res = H5A_READ(attr_id)
  H5A_CLOSE,attr_id
  hbad = res[0]
ENDIF

; print,scale_factor,add_offset, hbad

dataspace_id = H5D_GET_SPACE(data_id)

dimensions = H5S_GET_SIMPLE_EXTENT_DIMS(dataspace_id)

IF ( hyperslab EQ 1 ) THEN BEGIN

; hyperslab portion
H5S_SELECT_HYPERSLAB, dataspace_id, start, count, STRIDE=[1,1], /RESET
memory_space_id = H5S_CREATE_SIMPLE(count)

data = H5D_READ(data_id, FILE_SPACE=dataspace_id, MEMORY_SPACE=memory_space_id)
ENDIF ELSE $
data = H5D_READ(data_id) ; read actual data

IF ( hyperslab EQ 1 ) THEN H5S_CLOSE, memory_space_id
H5S_CLOSE, dataspace_id
H5D_CLOSE, data_id

H5F_CLOSE, fid

strdata = {name:vname, data:data, scale:scale_factor, offset: add_offset, badv: hbad}

RETURN,strdata

END
