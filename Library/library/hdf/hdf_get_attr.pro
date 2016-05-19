;+NAME/ONE LINE DESCRIPTION OF ROUTINE:
;    HDF_GET_ATTR returns the data in a named attribute of an hdf file.
;
;  function to retrieve the global attribute or an SDS's attribute
;  a -1 value will be returned, if failed
;
;  file_id - input file name (if it is string) or file id (if it is a integer)
;            returned by HDF_SD_START
;  attr_name - name of the attribute to be retrieved
;  sds - sds name of which the attribute is to be retrieved
;  attr_val - attribute value to be returned
;
FUNCTION HDF_GET_ATTR, file_id, attr_name, sdsidx=sds_index, sds=sds_name, attr_val

fclose = 0
err = -1
pname = 'HDF_GET_ATTR: '

isize = SIZE(file_id)
if (isize(1) EQ 2 or isize(1) EQ 3) then BEGIN    ; integer or long integer
  fid = file_id
  fclose = 0            ; does not need to close the file
END ELSE BEGIN
  tmp_ifile = FINDFILE(file_id)  ; IDL HDF routine doesn't take environment variable
  tmp_ifile = tmp_ifile(0)       ; for file name
  if (HDF_ISHDF(tmp_ifile) EQ 0) then BEGIN
    errmsg = pname + 'The input file is not an HDF file.'
    GOTO, EndExit
  END
  fid = HDF_SD_START(tmp_ifile, /READ)
  if (fid LT 0) then BEGIN
    errmsg = pname  + 'Cannot open file ' + ifile
    GOTO, EndExit
  END
  fclose = 1            ; need to close the file
END

if (KEYWORD_SET(sds_name)) or (n_elements(sds_index) eq 1) then BEGIN

 ; start for retrieving SDS's attribute

   if n_elements(sds_index) eq 1 then index=sds_index else index = HDF_SD_NAMETOINDEX(fid, sds_name) 
   sds_id = HDF_SD_SELECT(fid, index)
   gindex = HDF_SD_ATTRFIND(sds_id, attr_name)
   if (gindex EQ -1) then BEGIN
      errmsg = "SDS attribute - " + attr_name + " not found"
      GOTO, EndExit
    END

   HDF_SD_ATTRINFO, sds_id, gindex, data=attr_val, type=t, count=c
   if (c EQ 1) then attr_val = attr_val(0)
;
;  fix an IDL problem, that there may have an garbage letter at the end of the string 
;
   if (t EQ 'STRING') then attr_val = STRING(BYTE(attr_val))   


ENDIF ELSE BEGIN
;  retrieve global attribute
;
   gindex = HDF_SD_ATTRFIND(fid, attr_name)
   if (gindex EQ -1) then BEGIN
     errmsg = pname + 'Global attribute - ' + attr_name + ' not found.'
     GOTO, EndExit
   END

   HDF_SD_ATTRINFO, fid, gindex, data=attr_val, count=c, type=t
   if (c EQ 1) then attr_val = attr_val(0)
;
;  fix an IDL problem, that there may have an garbage letter at the end of the string 
;
   if (t EQ 'STRING') then attr_val = STRING(BYTE(attr_val))   
ENDELSE
err = 0
;

EndExit:

if (fclose EQ 1) then HDF_SD_END, fid
IF (err NE 0) THEN PRINT,errmsg

RETURN, err
END
