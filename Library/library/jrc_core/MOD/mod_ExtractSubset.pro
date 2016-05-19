
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       ExtractSubset
;
; PURPOSE:
;
;       read a variable from an HDF file returning the information
;		of the header, extract a square of data at a given position
;		and arrange it as a vector.
;
; CATEGORY:
; 
;       I/O
;
; CALLING SEQUENCE:
;
;       ExtractSubset,filename,varname,nb_square,line,elem,subset,header,iflag
;
; INPUTS:
;			filename:	input file name (string)
;			varname:	name of the variable to be read (string)
;			nb_square:	mid-size of the square to be extracted (distance from the center)
;			line:		line number for square center
;			elem:		element (column) number for aquare center
;
; OUTPUTS:
;			subset:		subset of SDS output data set, as vector of size (1 + 2*nb_square)^2
;			header:		header of SDS data set
;			iflag:		flag value: 1: good status, -1 otherwise
;			
; KEYWORD PARAMETERS:
;					none
;
; COMMENTS:
;		header: structure { name:varname, longname:' ',units: ' ', slope:hbad, intercept:hbad }
;		Needs routine:
;			 read_hdf_data
;			
; REFERENCES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Written by: F. MELIN, 02/2001,JRC/IES/GEM.
;			
;
;------------------------------------------------------------------------------

PRO mod_ExtractSubset,filename,varname,varlist,nb_square,line,elem,subset,header,iflag

nn = 1 + 2*nb_square

vname = '/geophysical_data/'+varname

iflag = -1

ii = WHERE ( varlist EQ vname, cnt )

IF ( cnt EQ 1 ) THEN BEGIN

!QUIET=1

; Opens HDF file
;catch, error_status

;if error_status NE 0 THEN BEGIN
  ;ERROR=1
  ;catch, /CANCEL
  ;msg='Problem with file '+fileName+' check version, contents, existence or read permission.'
  ;errMsg=dialog_message(msg, /ERROR)
  ;message, msg
  ;return
;endif

mod_read_nc4_data,filename,vname,header,img,iflag

IF ( iflag EQ 1 ) THEN BEGIN

   subset1 = img[elem-nb_square:elem+nb_square,line-nb_square:line+nb_square]

   nsize = nn * nn

; Change from bi-dimension to mono-dimension.   
   subset = REFORM(subset1,nsize)

ENDIF ; else begin
   
;   print,"No such variable in file: ",name
;   stop

; endelse

!QUIET=0

ENDIF

END
