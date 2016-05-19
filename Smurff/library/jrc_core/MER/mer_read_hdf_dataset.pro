
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       read_hdf_dataset
;
; PURPOSE:
;
;       read a variable from an HDF file.
;
;
; CATEGORY:
; 
;       I/O
;
; CALLING SEQUENCE:
;
;       read_hdf_dataset,filename,varname,data,iflag
;
; INPUTS:
;			filename:	input file name (string)
;			varname:	name of the variable to be read (string)
;
; OUTPUTS:
;			data:		SDS output data set
;			iflag:		flag value: 1: good status, -1 otherwise
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
;       Written by: F. MELIN, 02/2001, JRC/IES/GEM.
;			
;
;------------------------------------------------------------------------------

PRO mer_read_hdf_dataset,filename,varname,data,iflag

iflag = -1

; Opens HDF file
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

sd_id = HDF_SD_START (filename,/READ)

index = HDF_SD_NAMETOINDEX(sd_id,varname)

IF ( index GE 0 ) THEN BEGIN
; Open access to variable.
   sds_id = HDF_SD_SELECT(sd_id,index)
; Get variable.
   HDF_SD_GETDATA,sds_id,data
; End access to variable.
   HDF_SD_ENDACCESS,sds_id

   iflag = 1 ; Variable read.

ENDIF

HDF_SD_END,sd_id

END
