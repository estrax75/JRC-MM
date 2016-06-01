;EXAMPLE IDL PROGRAM

; $Id$
; Author: Glenn Hamell, Ed Santiago
; NAME: get_HDF_data.pro
; Copyright (c) 2003, California Institute of Technology. All rights
; reserved. Unauthorized reproduction prohibited.
;
;
; CALLING SEQUENCE:  get_HDF_data
;
; PURPOSE:
; Quickly & easily access HDF data
;
; INPUT:
; The HDF file(s) named in the "filepath" variable below.
;
; OUTPUT:
; A structure of the desired data fields. Optional plots or whatever
; code modifications you would like to add.
;
; RETURN:
;
;
; MODIFICATION HISTORY:
; =====================
; 2003Feb21-Glenn Hamell, Created. Utilizes routines by Ed Santiago.
;
;
;################################################
;

PRO get_HDF_data

  ;filepath  = '/home/ull9/glennh/apps/asc/mag_data_1hr_1997.hdf'
  ;vdataname = 'MAG_data_1hr'
  ;lstofflds = 'year,day,hr,fp_doy,Bgse_x,Bgse_y,Bgse_z' ; NOTE: no spaces allowed
  filepath  = 'E:\data\mariomi\application\oxyrisk\temp\A20071522007181.L3b_MO_NSST.main'
  vdataname = 'sst'
  lstofflds = 'sst_sum' ; NOTE: no spaces allowed
  
  s = read_asc_hdf( filepath, vdataname, lstofflds ); GET ONLY FIELDS NAMED
  ;s = read_asc_hdf( filepath, vdataname )    ; GET ALL FIELDS IN vdataname
  
  HELP, s, /st
  ;PRINT, s
  
  PLOT, s.fp_doy, s.Bgse_x, MIN_VALUE=-999
  
END
;------------------------------------------------;



;ASSOCIATED IDL

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;+
; NAME:
;     READ_ASC_HDF
;
; IDENT:
;     $Id: read_asc_hdf.pro,v 1.1 2003/02/14 20:19:48 glennh Exp $
;
; PURPOSE:
;     read in a range of ASC (hdfgen'ed) HDF files
;
; AUTHOR:
;      Ed Santiago
;
; CALLING SEQUENCE:
;     x = READ_ASC_HDF(filelist, basename, fields)
;
; INPUTS:
;     filelist     - array of input files
;     basename     - "hdfgen" name of vdata/sdata, eg, "swepam_dswi"
;     fields       - list of fields to read; if empty all fields are read
;
; OUTPUTS:
;     extracted data is in an array of structures
;
; SIDE EFFECTS:
;
; EXAMPLE:
;
;-
;FUNCTION read_asc_hdf, files, basename, fields, Quiet=quiet, Debug=debug
;
;  IF N_Elements(quiet) EQ 0 THEN quiet = 0
;  IF N_Elements(debug) EQ 0 THEN debug = 0
;  
;  ; Error conditions returned when things don't work out right with one HDF
;  ERR_NO_SUCH_VD = 'IDL_M_HDF_VS_SET_FIELDS'
;  ERR_NO_SUCH_SD = 'IDL_M_HDF_SD_SELECT'
;  
;  nfiles = N_Elements(files)
;  
;  FOR i=0,nfiles-1 DO BEGIN
;    IF NOT quiet THEN BEGIN
;      status = string(format='("-> ",A)', files[i])
;      IF nfiles GT 1 THEN $
;        status = status + string(format='(" (",I0," of ",I0,")")',$
;        i+1, nfiles)
;      print, status
;    ENDIF
;    
;    ; Initialize the HDF thingies
;    fid   = HDF_Open(files[i])
;    IF fid LE 0 THEN BEGIN
;      MESSAGE, 'HDF_Open(' + files[i] + ') failed!', /Continue
;      RETURN, 0
;    ENDIF
;    
;    checkAllfid, all.vdatanames
;    v     = HDF_VD_Find(fid,basename)
;    IF v LE 0 THEN BEGIN
;      MESSAGE, 'HDF_Find('+basename+') in '+files[i]+' failed!', /Continue
;      HDF_Close, fid
;      RETURN, 0
;    ENDIF
;    
;    vdata = HDF_VD_Attach(fid, v)
;    IF vdata LE 0 THEN BEGIN
;      HDF_Close, fid
;      MESSAGE, 'HDF_VD_Attach('+files[i]+') failed!'
;    ENDIF
;    
;    ; First time around, figure out what the data types are
;    vvdataInfo= HDF_VD_VDATAINFO(fid, basename)
;    IF N_Elements(fieldinfo) EQ 0 THEN BEGIN
;      fieldinfo   = get_fieldinfo(files[i], basename, vdata, fields)
;      fieldstruct = get_fieldstruct(fieldinfo)
;    ENDIF
;    
;    ; read the file contents, and extract fields
;    HDF_VD_Get, vdata, count=nr
;    IF nr LE 0 THEN GOTO, skipThisFile
;    
;    today = REPLICATE(fieldstruct, nr)
;    
;    ; Loop over each field, reading it in using the appropriate HDF interface
;    FOR j=0,N_Elements(fieldinfo)-1 DO BEGIN
;      ; If field is a multimensional array, get it from the SD interface
;      IF fieldinfo[j].ndims GT 1 THEN BEGIN
;        sd = -1
;        Catch, err_sts
;        IF err_sts EQ 0 THEN BEGIN
;          sd  = HDF_SD_Start(files[i])
;          sdi = HDF_SD_NameToIndex(sd, basename+'_'+fieldinfo[j].name)
;          sds = HDF_SD_Select(sd, sdi)
;          IF debug THEN print,'Reading SD: '+fieldinfo[j].name+'..'
;          HDF_SD_GetData, sds, data
;          IF debug THEN print,'Copying...'
;          today.(j+1) = temporary(data)
;          IF debug THEN print,'done!'
;          HDF_SD_EndAccess, sds
;          HDF_SD_End, sd
;          sd = -1
;        ENDIF ELSE IF !Error_State.Name EQ ERR_NO_SUCH_SD THEN BEGIN
;          print,'['+files[i]+': no such SD: ' + fieldinfo[j].name + ']'
;        ENDIF ELSE BEGIN
;          help,!error_state,/st
;          Catch, /Cancel
;          print, 'hdf_sd_getdata(): err=',err_sts
;          HDF_Close, fid
;          Message, !ERR_STRING
;        ENDELSE
;        Catch, /Cancel
;        IF sd GE 0 THEN HDF_SD_End, sd
;      ENDIF ELSE BEGIN
;        ; Field is scalar or vector; read it using the VDATA interface
;        Catch, err_sts
;        IF err_sts EQ 0 THEN BEGIN
;          HDF_VD_Seek, vdata, 0
;          status = HDF_VD_Read(vdata, data, fields=fieldinfo[j].name)
;          IF status NE nr THEN MESSAGE, 'mismatch'
;          today.(j+1) = temporary(data)
;        ENDIF ELSE IF !Error_State.Name EQ ERR_NO_SUCH_VD THEN BEGIN
;          print,'['+files[i]+': no such VD: ' + fieldinfo[j].name + ']'
;        ENDIF ELSE BEGIN
;          Catch, /Cancel
;          print, 'hdf_vd_read(): err=',err_sts
;          Message, !ERR_STRING
;        ENDELSE
;        Catch, /Cancel
;      ENDELSE
;    ENDFOR
;    
;    ; Done reading all fields.  Make a list of all structure elements.
;    IF N_Elements(matchlist) EQ 0 THEN BEGIN
;      matchlist = today
;    ENDIF ELSE BEGIN
;      matchlist = [ matchlist, today ]
;    ENDELSE
;    
;    ; done.
;    skipThisFile:
;    HDF_VD_Detach, vdata
;    HDF_Close, fid
;  ENDFOR
;  
;  IF N_Elements(matchlist) EQ 0 THEN BEGIN
;    IF NOT quiet THEN MESSAGE, 'Could not find any data in this range',/Cont
;    return, 0
;  ENDIF
;  
;  RETURN, matchlist
;END

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;+
; NAME:
;     GET_FIELDINFO
;
; IDENT:
;     $Id: get_fieldinfo_hdf.pro,v 1.1 2003/02/14 20:17:45 glennh Exp $
;
; PURPOSE:
;     determine the dimensions and types of HDF file field thingies
;
; AUTHOR:
;     Ed Santiago
;
; CALLING SEQUENCE:
;     x = GET_FIELDINFO( filename, basename, vdata, fields )
;
; INPUTS:
;     filename     - name of the HDF input file
;     basename     - name of the ASC thingy (eg, "swepam_i")
;     vdata        - HDF vdata thingy
;     fields       - comma-separated string of fields to be extracted
;
; OUTPUTS:
;     an array of structures
;
; SIDE EFFECTS:
;
; EXAMPLE:
;      fid   = HDF_Open(files[i])
;      v     = HDF_VD_Find(fid,'swepam_i')
;      vdata = HDF_VD_Attach(fid, v)
;      fields = 'foo,bar'
;
;      fieldinfo   = get_fieldinfo(files[i], 'swepam_i', vdata, fields)
;
;-
;FUNCTION get_fieldinfo, filename, basename, vdata, fields
;  ; define the "fieldinfo" structure, containing field name/type/dimensions
;  fieldinfo = { fieldinfo, name:'', type:'', ndims:0L, dims:LonArr(8) }
;  fieldlist = [ fieldinfo ]
;  
;  ; Yuk.  IDL is case-insensitive, but HDFs are not.  The fields are
;  ; stored in case-sensitive manner, so if we are called with "bmag"
;  ; and the field is actually called "Bmag", we will fail.
;  ;
;  ; To avoid this, we always read in all the VD and SD field names,
;  ; then do a case-insensitive comparison of the fields passed in.
;  ; This allows us to find out the *real* HDF name of the field.
;  
;  ; obtain the VDATA fields
;  HDF_VD_Get, vdata, fields=fields_all
;  
;  ; obtain the SDATA fields
;  sd = HDF_SD_Start(filename)
;  HDF_SD_FileInfo, sd, datasets, attributes
;  FOR i=0,datasets-1 DO BEGIN
;    sds = HDF_SD_Select(sd,i)
;    HDF_SD_GetInfo, sds, name=nnn
;    IF STRMID(nnn,0,STRLEN(basename)+1) EQ basename + '_' THEN BEGIN
;      fields_all = fields_all + ',' + STRMID(nnn, STRLEN(basename)+1, 999)
;    ENDIF
;    HDF_SD_EndAccess, sds
;  ENDFOR
;  HDF_SD_End, sd
;  
;  
;  ; If no fields were given, assume user wants them all
;  IF N_Elements(fields) EQ 0 THEN fields = fields_all
;  
;  ;
;  ; Split the list of fields into an array.  For each element, do some
;  ; HDF info-get stuff to figure out what it is.
;  ;
;  fieldarr     = split(',', fields)
;  fieldarr_all = split(',', fields_all)
;  FOR i = 0, N_Elements(fieldarr)-1 DO BEGIN
;    ; Find out the real (HDF case-sensitive) name of the field
;    tmp = where(StrUpCase(fieldarr_all) EQ StrUpCase(fieldarr[i]), c)
;    IF c NE 1 THEN BEGIN
;      MESSAGE, 'no such VD or SD: ' + fieldarr[i], /Continue
;      GOTO, skipThisField
;    ENDIF
;    fieldinfo.name  = fieldarr_all[tmp]
;    
;    ; HDF routines choke on errors, but we want to proceed.
;    Catch, err_sts
;    
;    IF err_sts EQ 0 THEN BEGIN
;      ;
;      ; First, see if it's a VDATA.  If it is, life is simple.  If
;      ; it isn't, the following line will choke, and control will
;      ; pass on to the ELSE below.
;      ;
;      HDF_VD_GetInfo, vdata, fieldinfo.name, Type=t, Order=o
;      IF o EQ 1 THEN fieldinfo.ndims = 0 ELSE fieldinfo.ndims = 1
;      fieldinfo.dims[0] = o
;    ENDIF ELSE IF !Error_State.Name EQ 'IDL_M_HDF_VS_FEXIST' THEN BEGIN
;      ;
;      ; Sigh, the HDF_VD_GetInfo call failed.  That means that the
;      ; field in question is not a VDATA.  Now try to see if it's
;      ; an SDATA.  Since this HDF routine chokes also, we need a
;      ; new CATCH.
;      ;
;      Catch, err_sts
;      IF err_sts EQ 0 THEN BEGIN
;        ;
;        ; Initialize the SDATA interface, and try to get info.  If
;        ; it works, we set the dimensions and type.
;        ;
;        sd  = HDF_SD_Start(filename)
;        sdi = HDF_SD_NameToIndex(sd, basename + '_' + fieldinfo.name)
;        sds = HDF_SD_Select(sd, sdi)
;        
;        HDF_SD_GetInfo, sds, Type=t, Dims=o
;        fieldinfo.ndims = N_Elements(o) - 1
;        fieldinfo.dims  = o
;        HDF_SD_EndAccess, sds
;        HDF_SD_End, sd
;      ENDIF ELSE IF !Error_State.Name EQ 'IDL_M_HDF_SD_SELECT' THEN BEGIN
;        ; Nope, could not find a VDATA or SDATA by that name.
;        print, '['+filename+': no such VD or SD: ' + fieldarr[i] + ']'
;      ENDIF ELSE BEGIN
;        ; Weird unexpected error
;        print, 'hdf_sd failed, err=',err_sts
;        stop
;      ENDELSE
;    ENDIF ELSE BEGIN
;      ; Weird unexpected error
;      Catch, /Cancel
;      print, 'hdf_vd_info failed with status ', err_sts, fieldarr[i]
;      Message, !ERR_STRING, /NoName
;    ENDELSE
;    Catch, /Cancel
;    
;    IF err_sts EQ 0 THEN BEGIN
;      fieldinfo.type  = t
;      fieldlist = [ fieldlist, fieldinfo ]
;    ENDIF
;    skipThisField:
;  ENDFOR  ; i=0,nfields
;  
;  RETURN, fieldlist[1:*]
;END

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;+
; NAME:
;     GET_FIELDSTRUCT
;
; IDENT:
;     $Id: get_fieldstruct_hdf.pro,v 1.1 2003/02/14 20:18:57 glennh Exp $
;
; PURPOSE:
;     Given a fieldinfo struct (see GET_FIELDINFO.PRO), build a data
;     structure of the right type.
;
; AUTHOR:
;     Ed Santiago
;
; CALLING SEQUENCE:
;     data = GET_FIELDSTRUCT( fieldinfo )
;
; INPUTS:
;     fieldinfo    - data structure returned by GET_FIELDINFO()
;
; OUTPUTS:
;     a structure template
;
; SIDE EFFECTS:
;
; EXAMPLE:
;
;-
;FUNCTION get_fieldstruct, fieldinfo
;  mystruct = create_struct( 'dnum', 0D )
;  
;  FOR i=0, N_Elements(fieldinfo)-1 DO BEGIN
;    type  = fieldinfo[i].type   ; String describing the type
;    ndims = fieldinfo[i].ndims    ; Number of array dims (eg, 1, 2, 3)
;    dims  = fieldinfo[i].dims   ; The dimensions themselves (eg, [9x3])
;    
;    ; Sigh.  Rather than give us something *USEFUL*, such as its internal
;    ; data type number (float=4,int=2,etc), IDL gives us a string.  It's
;    ; up to us to convert that to a real data type.  We do so in a gross
;    ; CASE statement, converting each string descriptor to a variable
;    ; of the correct IDL type.
;    ;
;    ; To make things worse, with the introduction of unsigned types
;    ; in 5.2, IDL has added the 'UINT' and 'ULONG' strings.  Piece
;    ; o' cake to add those, along with their respective definitions
;    ; as "0U" and "0UL", right?  Well, no.  IDL <5.2 gives a compile-
;    ; time error with those, so we can't do that if we want to remain
;    ; compatible with 5.1.  The solution is to define the type as a
;    ; string, then EXECUTE() it.  This way we don't get the compilation
;    ; errors, nor will EXECUTE() fail, since UINT/ULONG don't exist in <5.2.
;    CASE type OF
;      'BYTE':   t = '0B'
;      'INT':    t = '0'
;      'UINT': t = '0U'
;      'LONG':   t = '0L'
;      'ULONG':  t = '0UL'
;      'FLOAT':  t = '0.0'
;      'DOUBLE': t = '0.0D'
;    ENDCASE
;    
;    r = execute('xxx = ' + t)
;    
;    ; If field is an array, make it so.
;    IF ndims NE 0 THEN $
;      xxx = Make_Array(size=[ndims, dims[0:ndims-1], (size(xxx))[1], 0])
;      
;    ; Append this new field to the Master Data Structure
;    mystruct = create_struct( mystruct, '_'+fieldinfo[i].name, xxx)
;  ENDFOR
;  
;  RETURN, mystruct
;END

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;+
; NAME:
;     SPLIT
;
; IDENT:
;     $Id: split_hdf.pro,v 1.1 2003/02/14 20:20:07 glennh Exp $
;
; PURPOSE:
;     Just like perl.  Splits a string into an array of strings.
;
; AUTHOR:
;     Ed Santiago
;
; CALLING SEQUENCE:
;     stringarr = split(delimiter, string)
;
; INPUTS:
;     delimiter  -- character to split on
;     string     -- string to split
;
; OUTPUTS:
;     an array of strings
;
; SIDE EFFECTS:
;
; EXAMPLE:
;     IDL> x=split(',', 'this,is,a,test')
;     IDL> print, N_Elements(x), x
;           4 this is a test
;
;-


;FUNCTION HDF_VD_VDATALIST, HDFID, NULLCLASS=NULLCLASS
;
;  ;+
;  ; NAME:
;  ;    HDF_VD_VDATALIST
;  ;
;  ; PURPOSE:
;  ;    Return the number, names, and class names of all Vdatas in a HDF file.
;  ;
;  ; CATEGORY:
;  ;    HDF utilities.
;  ;
;  ; CALLING SEQUENCE:
;  ;    RESULT = HDF_VD_VDATALIST(HDFID)
;  ;
;  ; INPUTS:
;  ;    HDFID       Identifier of HDF file opened by caller with HDF_OPEN.
;  ;
;  ; OPTIONAL INPUTS:
;  ;    None.
;  ;
;  ; KEYWORD PARAMETERS:
;  ;    NULLCLASS    If set, only returns information about Vdatas which have
;  ;                 null class names. (i.e. class name is the null string '').
;  ;
;  ; OUTPUTS:
;  ;    An anonymous structure containing the number of Vdatas,
;  ;    the name of each Vdata, and the class name of each Vdata in the file.
;  ;    The fields in the structure are as follows:
;  ;    NVDATAS       Number of Vdatas in the file (0 if none were found).
;  ;    VDATANAMES    Array of Vdata names ('' if none were found).
;  ;    CLASSNAMES    Array of class names ('' if none were found).
;  ;
;  ; OPTIONAL OUTPUTS:
;  ;    None
;  ;
;  ; COMMON BLOCKS:
;  ;    None
;  ;
;  ; SIDE EFFECTS:
;  ;    None.
;  ;
;  ; RESTRICTIONS:
;  ;    Requires IDL 5.0 or higher (square bracket array syntax).
;  ;
;  ; EXAMPLE:
;  ;
;  ;file = 'sdsvdata.hdf'
;  ;hdfid = hdf_open(file)
;  ;result = hdf_vd_vdatalist(hdfid)
;  ;hdf_close, hdfid
;  ;help, result, /structure
;  ;
;  ; MODIFICATION HISTORY:
;  ; Liam.Gumley@ssec.wisc.edu
;  ; http://cimss.ssec.wisc.edu/~gumley
;  ; $Id: hdf_vd_vdatalist.pro,v 1.1 2000/01/04 22:22:11 gumley Exp $
;  ;
;  ; Copyright (C) 1999, 2000 Liam E. Gumley
;  ;
;  ; This program is free software; you can redistribute it and/or
;  ; modify it under the terms of the GNU General Public License
;  ; as published by the Free Software Foundation; either version 2
;  ; of the License, or (at your option) any later version.
;  ;
;  ; This program is distributed in the hope that it will be useful,
;  ; but WITHOUT ANY WARRANTY; without even the implied warranty of
;  ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  ; GNU General Public License for more details.
;  ;
;  ; You should have received a copy of the GNU General Public License
;  ; along with this program; if not, write to the Free Software
;  ; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;  ;-
;
;  rcs_id = '$Id: hdf_vd_vdatalist.pro,v 1.1 2000/01/04 22:22:11 gumley Exp $'
;  
;  ;- Check arguments
;  if (n_params() ne 1) then message, 'Usage: RESULT = HDF_SD_VDATALIST(HDFID)'
;  if (n_elements(hdfid) eq 0) then message, 'Argument HDFID is undefined'
;  
;  ;- Set default return values
;  nvdatas = 0L
;  vdatanames = ''
;  classnames = ''
;  
;  ;- Create name storage array
;  maxnames = 100000L
;  namelist = strarr(maxnames)
;  classlist = strarr(maxnames)
;  
;  ;- Loop over all vdatas
;  lastid = -1
;  thisid = 0
;  while (thisid ge 0) do begin
;  
;    ;- Select next vdata
;    thisid = hdf_vd_getid(hdfid, lastid)
;    lastid = thisid
;    
;    ;- If vdata is valid, get vdata name
;    if (thisid ge 0) then begin
;    
;      ;- Get the vdata name and class
;      vdata = hdf_vd_attach(hdfid, thisid)
;      hdf_vd_get, vdata, name=name, class=class
;      hdf_vd_detach, vdata
;      
;      ;- Store the vdata name and class
;      if (keyword_set(nullclass) eq 0) or $
;        ((keyword_set(nullclass) eq 1) and (class eq '')) then begin
;        namelist[nvdatas] = name
;        classlist[nvdatas] = class
;        nvdatas = nvdatas + 1L
;      endif
;      
;      ;- Check maximum number of vdata names
;      if (nvdatas gt maxnames) then message, 'MAXNAMES exceeded'
;      
;    endif
;    
;  endwhile
;  
;  ;- Extract result arrays
;  if (nvdatas gt 0) then begin
;    vdatanames = namelist[0 : nvdatas - 1]
;    classnames = classlist[0 : nvdatas - 1]
;  endif
;  
;  ;- Return the result
;  return, {nvdatas:nvdatas, vdatanames:vdatanames, classnames:classnames}
;  
;END

;FUNCTION HDF_VD_VDATAINFO, HDFID, VDATANAME
;
;  ;+
;  ; NAME:
;  ;    HDF_VD_VDATAINFO
;  ;
;  ; PURPOSE:
;  ;    Return information about a Vdata in a HDF file.
;  ;
;  ; CATEGORY:
;  ;    HDF utilities.
;  ;
;  ; CALLING SEQUENCE:
;  ;    RESULT = HDF_VD_VDATAINFO(HDFID, VDATANAME)
;  ;
;  ; INPUTS:
;  ;    HDFID        Identifier of HDF file opened by caller with HDF_OPEN.
;  ;    VDATANAME    Name of Vdata.
;  ;
;  ; OPTIONAL INPUTS:
;  ;    None.
;  ;
;  ; KEYWORD PARAMETERS:
;  ;    None.
;  ;
;  ; OUTPUTS:
;  ;    An anonymous structure containing information about the Vdata.
;  ;    The fields in the structure are as follows:
;  ;    NAME          Name of the Vdata ('' if Vdata not found).
;  ;    NFIELDS       Number of fields (-1 if Vdata not found).
;  ;    FIELDNAMES    Array of field names ('' if Vdata not found).
;  ;    NRECORDS      Number of records (-1 if Vdata not found).
;  ;
;  ; OPTIONAL OUTPUTS:
;  ;    None
;  ;
;  ; COMMON BLOCKS:
;  ;    None
;  ;
;  ; SIDE EFFECTS:
;  ;    None.
;  ;
;  ; RESTRICTIONS:
;  ;    Requires IDL 5.0 or higher (square bracket array syntax).
;  ;
;  ; EXAMPLE:
;  ;
;  ;file = 'sdsvdata.hdf'
;  ;hdfid = hdf_open(file)
;  ;result = hdf_vd_vdatainfo(hdfid, 'Vdata with mixed types')
;  ;hdf_close, hdfid
;  ;help, result, /structure
;  ;
;  ; MODIFICATION HISTORY:
;  ; Liam.Gumley@ssec.wisc.edu
;  ; http://cimss.ssec.wisc.edu/~gumley
;  ; $Id: hdf_vd_vdatainfo.pro,v 1.2 2000/01/05 15:53:21 gumley Exp $
;  ;
;  ; Copyright (C) 1999, 2000 Liam E. Gumley
;  ;
;  ; This program is free software; you can redistribute it and/or
;  ; modify it under the terms of the GNU General Public License
;  ; as published by the Free Software Foundation; either version 2
;  ; of the License, or (at your option) any later version.
;  ;
;  ; This program is distributed in the hope that it will be useful,
;  ; but WITHOUT ANY WARRANTY; without even the implied warranty of
;  ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  ; GNU General Public License for more details.
;  ;
;  ; You should have received a copy of the GNU General Public License
;  ; along with this program; if not, write to the Free Software
;  ; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;  ;-
;
;  rcs_id = '$Id: hdf_vd_vdatainfo.pro,v 1.2 2000/01/05 15:53:21 gumley Exp $'
;  
;  ;- Check arguments
;  if (n_params() ne 2) then message, 'Usage: RESULT = HDF_SD_VDATAINFO(HDFID, VDATANAME)'
;  if (n_elements(hdfid) eq 0) then message, 'Argument HDFID is undefined'
;  if (n_elements(vdataname) eq 0) then message, 'Argument VDATANAME is undefined'
;  
;  ;- Set default return values
;  name = ''
;  nfields = -1
;  fieldnames = ''
;  nrecords = -1
;  
;  ;- Get the index for the vdata
;  index = hdf_vd_find(hdfid, vdataname)
;  
;  ;- If the vdata is valid, get vdata information
;  if (index gt 0) then begin
;  
;    ;- Attach to the vdata
;    vdataid = hdf_vd_attach(hdfid, index)
;    
;    ;- Get information
;    hdf_vd_get, vdataid, name=name, nfields=nfields, fields=fields, count=nrecords
;    
;    ;- Detach from the vdata
;    hdf_vd_detach, vdataid
;    
;    ;- Convert field name string to array
;    fieldnames = str_sep(fields, ',', /trim)
;    
;  endif
;  
;  ;- Return result to caller
;  return, {name:name, nfields:nfields, fieldnames:fieldnames, nrecords:nrecords}
;  
;END

;PRO checkAll
;
;  filepath  = 'E:\data\mariomi\application\oxyrisk\temp\A20071522007181.L3b_MO_NSST.main'
;  filepath  = 'E:\data\mariomi\application\oxyrisk\temp\A20071522007181.L3b_MO_SST_4.main'
;  fid   = HDF_Open(filepath)
;  vdatainfolist = HDF_VD_VDATALIST(fid)
;  vdnames=vdatainfolist.vdatanames
;  for i=0, n_elements(vdnames)-1 do begin
;    v     = HDF_VD_Find(fid,vdnames[i])
;    vdata = HDF_VD_Attach(fid, v)
;    vvdataInfo= HDF_VD_VDATAINFO(fid, vdnames[i])
;    HDF_VD_Get, vdata, count=nr
;    subFieldNames=vvdataInfo.fieldnames
;    for j=0, n_elements(subFieldNames)-1 do begin
;      ;IF N_Elements(fieldinfo) EQ 0 THEN BEGIN
;        fieldinfo   = get_fieldinfo(filepath, vdnames[i], vdata, subFieldNames[j])
;        fieldstruct = get_fieldstruct(fieldinfo)
;      ;ENDIF
;      print, strcompress(vdnames[i],/REMOVE)+'**'+strcompress(subFieldNames[j],/REMOVE)+'**'+strcompress(fieldinfo.ndims, /REMOVE)
;    endfor
;  endfor
;  
;END
;+NAME/ONE LINE DESCRIPTION OF ROUTINE:
;    HDF_INFO obtains file description information from a given hdf file.
;
;pro hdf_info,filename,sds=sds,rig=rig,all=all,global=global
;
;    F1='(" ",A,I4)'
;    F2='("-----",A,A,"-----")'
;    F3='("-----",A,"-----")'
;    FI='(A24," ",A8," ",I6," ",I)'
;    FS='(A24," ",A8," ",I6," ",A)'
;    FF='(A24," ",A8," ",I6," ",F)'
;    FD='(A24," ",A8," ",I6," ",D)'
;    F4='(A24," ",A8," ",I6)'
;    F5='(A24," ",A8," ",A6)'
;
;    ;
;    ; Check that it is HDF
;    ;
;    if hdf_ishdf(filename) ne 1 then begin
;        message,"File "+filename+" is not an HDF file."
;    endif
;
;    ;
;    ; Open the HDF file readonly
;    ;
;    fileid=hdf_open(filename,/read)
;    print
;    print,"Contents of ",filename
;    print
;
;    ; Get and report the number of free palettes.
;    numfpals=hdf_dfp_npals(filename)>0
;    print,'====================================='
;    print," # of free palettes = ",numfpals,FORMAT=F1
;
;    ; Get the number of file ids and read them.
;    numid=hdf_number(fileid,tag=100)>0
;    print,'====================================='
;    print," # of file file ids = ",numid,FORMAT=F1
;    if numid ne 0 then begin
;        if numid eq 1 then begin
;            hdf_dfan_getfid,filename,fid,/first
;            print,"File Id",FORMAT=F3 & print,strcompress(string(fid))
;        endif else begin
;            print,"File Id",string(1),FORMAT=F2
;            hdf_dfan_getfid,filename,fid,/first & print,strcompress(string(fid))
;            for i=2,numid do  begin
;                print,"File Id",strtrim(string(i)),FORMAT=F2
;                hdf_dfan_getfid,filename,fid & print,strcompress(string(fid))
;            endfor
;        endelse
;    endif
;
;    ; Get the number of file descriptions and read them.
;    numdesc=hdf_number(fileid,tag=101)>0
;    print,'====================================='
;    print," # of file descriptions = ",numdesc,FORMAT=F1
;    if numdesc ne 0 then begin
;  if numdesc eq 1 then begin
;      hdf_dfan_getfds,filename,desc,/first
;      print,"File Description",FORMAT=F3 & print,string(desc)
;  endif else begin
;      print,"File Description",string(1),FORMAT=F2
;      hdf_dfan_getfds,filename,desc,/first & print,string(desc)
;      for i=2,numdesc do  begin
;          print,"File Description",string(i),FORMAT=F2
;          hdf_dfan_getfds,filename,desc & print,string(desc)
;      endfor
;  endelse
;    endif
;
;
;    if (keyword_set(rig) or keyword_set(all)) then begin
;
;        ; Get the number of RIG groups in the file
;        numrig=hdf_number(fileid,tag=306)>0
;  print,'====================================='
;  print," # of RIG groups = ",numrig,FORMAT=F1
;  if numrig gt 0 then begin
;      result=hdf_dfan_lablist(filename,306,reflist,RIGLABELS)
;      print,reflist,string(RIGLABELS)
;  endif
;
;    endif 
;
;
;    if (keyword_set(sds) or keyword_set(all)) then begin
;
;        ; Get the number of SDSs in the file
;        numndg=hdf_number(fileid,tag=721)>0
;        numsdd=hdf_number(fileid,tag=701)>0
;        numsd=hdf_number(fileid,tag=702)>0
;  hdf_dfsd_getinfo,filename, NSDS=numsds
;  print,'====================================='
;  print," # of SDS = ",numsds,FORMAT=F1
;  print," 
;
;        ;Get the number of MFSDs in the file
;        sd_id=HDF_SD_START(filename,/read)
;        HDF_SD_FILEINFO,sd_id,nmfsds,nglobatts
;  print,'====================================='
;  print," # of MFSD = ",nmfsds,FORMAT=F1
;  print," 
;
;  if nmfsds gt 0 then begin
;      print," "
;      print,"     Name        Rank      Type     DIMS      Nattrs   Attribute Value"
;      print,"---------------------------------------------------------------"
;            FSD='(A14," ",I4,"      ",A8,"     ",I4)'
;
;      for i=0,nmfsds-1 do begin
;          sds_id=HDF_SD_SELECT(sd_id,i)
;          HDF_SD_GETINFO,sds_id,name=n,ndims=r,type=t,natts=nats,dims=dims
;          print,n,r,t,nats,FORMAT=FSD
;          print,'                                       DIMS = ',dims
;
;            for j=0,nats-1 do begin
;              d=''
;              HDF_SD_ATTRINFO,sds_id,j,name=n,type=t,count=c,data=d
;              print,'                                         ',n,'= ',d
;          endfor
;          HDF_SD_ENDACCESS,sds_id
;      endfor
;  endif
;
;        HDF_SD_END,sd_id
;
;    endif
;
;    if (keyword_set(all) or keyword_set(global)) then begin
;
;        print,'====================================='
;        print,"    # of Global Attributes ",nglobatts
;  if nglobatts gt 0 then begin
;            print
;      print,"------------------------------------------------"
;      print,'Name','Type','Number',FORMAT=F5
;      print
;      for i=0,nglobatts-1 do begin
;    HDF_SD_ATTRINFO,sd_id,i,name=n,type=t,count=c,data=d
;          if (t eq 'STRING' ) then print,strtrim(n),t,c,d(0,0),FORMAT=FS else $
;          if (t eq 'FLOAT' )  then print,strtrim(n),t,c,d(0,0),FORMAT=FF else $
;          if (t eq 'DOUBLE' ) then print,strtrim(n),t,c,d(0,0),FORMAT=FD else $
;                             print,strtrim(n),t,c,d(0,0),FORMAT=FI
;        endfor
;  endif
;
;    endif
;
;
;    HDF_CLOSE,fileid
;
;end

;EXAMPLE IDL PROGRAM

; $Id$
; Author: Glenn Hamell, Ed Santiago
; NAME: get_HDF_data.pro
; Copyright (c) 2003, California Institute of Technology. All rights
; reserved. Unauthorized reproduction prohibited.
;
;
; CALLING SEQUENCE:  get_HDF_data
;
; PURPOSE:
; Quickly & easily access HDF data
;
; INPUT:
; The HDF file(s) named in the "filepath" variable below.
;
; OUTPUT:
; A structure of the desired data fields. Optional plots or whatever
; code modifications you would like to add.
;
; RETURN:
;
;
; MODIFICATION HISTORY:
; =====================
; 2003Feb21-Glenn Hamell, Created. Utilizes routines by Ed Santiago.
;
;
;################################################
;
PRO _HDF_data

  ;filepath  = '/home/ull9/glennh/apps/asc/mag_data_1hr_1997.hdf'
  ;vdataname = 'MAG_data_1hr'
  ;lstofflds = 'year,day,hr,fp_doy,Bgse_x,Bgse_y,Bgse_z' ; NOTE: no spaces allowed
  filepath  = 'E:\data\mariomi\application\oxyrisk\temp\A20071522007181.L3b_MO_NSST.main'
  vdataname = 'sst'
  lstofflds = 'sst_sum' ; NOTE: no spaces allowed
  
  s = read_asc_hdf( filepath, vdataname, lstofflds ); GET ONLY FIELDS NAMED
  ;s = read_asc_hdf( filepath, vdataname )    ; GET ALL FIELDS IN vdataname
  
  HELP, s, /st
  ;PRINT, s
  
  PLOT, s.fp_doy, s.Bgse_x, MIN_VALUE=-999
  
END
;------------------------------------------------;



;ASSOCIATED IDL

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;+
; NAME:
;     READ_ASC_HDF
;
; IDENT:
;     $Id: read_asc_hdf.pro,v 1.1 2003/02/14 20:19:48 glennh Exp $
;
; PURPOSE:
;     read in a range of ASC (hdfgen'ed) HDF files
;
; AUTHOR:
;      Ed Santiago
;
; CALLING SEQUENCE:
;     x = READ_ASC_HDF(filelist, basename, fields)
;
; INPUTS:
;     filelist     - array of input files
;     basename     - "hdfgen" name of vdata/sdata, eg, "swepam_dswi"
;     fields       - list of fields to read; if empty all fields are read
;
; OUTPUTS:
;     extracted data is in an array of structures
;
; SIDE EFFECTS:
;
; EXAMPLE:
;
;-
FUNCTION read_asc_hdf, files, basename, fields, Quiet=quiet, Debug=debug

  IF N_Elements(quiet) EQ 0 THEN quiet = 0
  IF N_Elements(debug) EQ 0 THEN debug = 0
  
  ; Error conditions returned when things don't work out right with one HDF
  ERR_NO_SUCH_VD = 'IDL_M_HDF_VS_SET_FIELDS'
  ERR_NO_SUCH_SD = 'IDL_M_HDF_SD_SELECT'
  
  nfiles = N_Elements(files)
  
  FOR i=0,nfiles-1 DO BEGIN
    IF NOT quiet THEN BEGIN
      status = string(format='("-> ",A)', files[i])
      IF nfiles GT 1 THEN $
        status = status + string(format='(" (",I0," of ",I0,")")',$
        i+1, nfiles)
      print, status
    ENDIF
    
    ; Initialize the HDF thingies
    fid   = HDF_Open(files[i])
    IF fid LE 0 THEN BEGIN
      MESSAGE, 'HDF_Open(' + files[i] + ') failed!', /Continue
      RETURN, 0
    ENDIF
    
    checkAllfid, all.vdatanames
    v     = HDF_VD_Find(fid,basename)
    IF v LE 0 THEN BEGIN
      MESSAGE, 'HDF_Find('+basename+') in '+files[i]+' failed!', /Continue
      HDF_Close, fid
      RETURN, 0
    ENDIF
    
    vdata = HDF_VD_Attach(fid, v)
    IF vdata LE 0 THEN BEGIN
      HDF_Close, fid
      MESSAGE, 'HDF_VD_Attach('+files[i]+') failed!'
    ENDIF
    
    ; First time around, figure out what the data types are
    vvdataInfo= HDF_VD_VDATAINFO(fid, basename)
    IF N_Elements(fieldinfo) EQ 0 THEN BEGIN
      fieldinfo   = get_fieldinfo(files[i], basename, vdata, fields)
      fieldstruct = get_fieldstruct(fieldinfo)
    ENDIF
    
    ; read the file contents, and extract fields
    HDF_VD_Get, vdata, count=nr
    IF nr LE 0 THEN GOTO, skipThisFile
    
    today = REPLICATE(fieldstruct, nr)
    
    ; Loop over each field, reading it in using the appropriate HDF interface
    FOR j=0,N_Elements(fieldinfo)-1 DO BEGIN
      ; If field is a multimensional array, get it from the SD interface
      IF fieldinfo[j].ndims GT 1 THEN BEGIN
        sd = -1
        Catch, err_sts
        IF err_sts EQ 0 THEN BEGIN
          sd  = HDF_SD_Start(files[i])
          sdi = HDF_SD_NameToIndex(sd, basename+'_'+fieldinfo[j].name)
          sds = HDF_SD_Select(sd, sdi)
          IF debug THEN print,'Reading SD: '+fieldinfo[j].name+'..'
          HDF_SD_GetData, sds, data
          IF debug THEN print,'Copying...'
          today.(j+1) = temporary(data)
          IF debug THEN print,'done!'
          HDF_SD_EndAccess, sds
          HDF_SD_End, sd
          sd = -1
        ENDIF ELSE IF !Error_State.Name EQ ERR_NO_SUCH_SD THEN BEGIN
          print,'['+files[i]+': no such SD: ' + fieldinfo[j].name + ']'
        ENDIF ELSE BEGIN
          help,!error_state,/st
          Catch, /Cancel
          print, 'hdf_sd_getdata(): err=',err_sts
          HDF_Close, fid
          Message, !ERR_STRING
        ENDELSE
        Catch, /Cancel
        IF sd GE 0 THEN HDF_SD_End, sd
      ENDIF ELSE BEGIN
        ; Field is scalar or vector; read it using the VDATA interface
        Catch, err_sts
        IF err_sts EQ 0 THEN BEGIN
          HDF_VD_Seek, vdata, 0
          status = HDF_VD_Read(vdata, data, fields=fieldinfo[j].name)
          IF status NE nr THEN MESSAGE, 'mismatch'
          today.(j+1) = temporary(data)
        ENDIF ELSE IF !Error_State.Name EQ ERR_NO_SUCH_VD THEN BEGIN
          print,'['+files[i]+': no such VD: ' + fieldinfo[j].name + ']'
        ENDIF ELSE BEGIN
          Catch, /Cancel
          print, 'hdf_vd_read(): err=',err_sts
          Message, !ERR_STRING
        ENDELSE
        Catch, /Cancel
      ENDELSE
    ENDFOR
    
    ; Done reading all fields.  Make a list of all structure elements.
    IF N_Elements(matchlist) EQ 0 THEN BEGIN
      matchlist = today
    ENDIF ELSE BEGIN
      matchlist = [ matchlist, today ]
    ENDELSE
    
    ; done.
    skipThisFile:
    HDF_VD_Detach, vdata
    HDF_Close, fid
  ENDFOR
  
  IF N_Elements(matchlist) EQ 0 THEN BEGIN
    IF NOT quiet THEN MESSAGE, 'Could not find any data in this range',/Cont
    return, 0
  ENDIF
  
  RETURN, matchlist
END

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;+
; NAME:
;     GET_FIELDINFO
;
; IDENT:
;     $Id: get_fieldinfo_hdf.pro,v 1.1 2003/02/14 20:17:45 glennh Exp $
;
; PURPOSE:
;     determine the dimensions and types of HDF file field thingies
;
; AUTHOR:
;     Ed Santiago
;
; CALLING SEQUENCE:
;     x = GET_FIELDINFO( filename, basename, vdata, fields )
;
; INPUTS:
;     filename     - name of the HDF input file
;     basename     - name of the ASC thingy (eg, "swepam_i")
;     vdata        - HDF vdata thingy
;     fields       - comma-separated string of fields to be extracted
;
; OUTPUTS:
;     an array of structures
;
; SIDE EFFECTS:
;
; EXAMPLE:
;      fid   = HDF_Open(files[i])
;      v     = HDF_VD_Find(fid,'swepam_i')
;      vdata = HDF_VD_Attach(fid, v)
;      fields = 'foo,bar'
;
;      fieldinfo   = get_fieldinfo(files[i], 'swepam_i', vdata, fields)
;
;-
FUNCTION get_fieldinfo, filename, basename, vdata, fields
  ; define the "fieldinfo" structure, containing field name/type/dimensions
  fieldinfo = { fieldinfo, name:'', type:'', ndims:0L, dims:LonArr(8) }
  fieldlist = [ fieldinfo ]
  
  ; Yuk.  IDL is case-insensitive, but HDFs are not.  The fields are
  ; stored in case-sensitive manner, so if we are called with "bmag"
  ; and the field is actually called "Bmag", we will fail.
  ;
  ; To avoid this, we always read in all the VD and SD field names,
  ; then do a case-insensitive comparison of the fields passed in.
  ; This allows us to find out the *real* HDF name of the field.
  
  ; obtain the VDATA fields
  HDF_VD_Get, vdata, fields=fields_all
  
  ; obtain the SDATA fields
  sd = HDF_SD_Start(filename)
  HDF_SD_FileInfo, sd, datasets, attributes
  FOR i=0,datasets-1 DO BEGIN
    sds = HDF_SD_Select(sd,i)
    HDF_SD_GetInfo, sds, name=nnn
    IF STRMID(nnn,0,STRLEN(basename)+1) EQ basename + '_' THEN BEGIN
      fields_all = fields_all + ',' + STRMID(nnn, STRLEN(basename)+1, 999)
    ENDIF
    HDF_SD_EndAccess, sds
  ENDFOR
  HDF_SD_End, sd
  
  
  ; If no fields were given, assume user wants them all
  IF N_Elements(fields) EQ 0 THEN fields = fields_all
  
  ;
  ; Split the list of fields into an array.  For each element, do some
  ; HDF info-get stuff to figure out what it is.
  ;
  fieldarr     = split(',', fields)
  fieldarr_all = split(',', fields_all)
  FOR i = 0, N_Elements(fieldarr)-1 DO BEGIN
    ; Find out the real (HDF case-sensitive) name of the field
    tmp = where(StrUpCase(fieldarr_all) EQ StrUpCase(fieldarr[i]), c)
    IF c NE 1 THEN BEGIN
      MESSAGE, 'no such VD or SD: ' + fieldarr[i], /Continue
      GOTO, skipThisField
    ENDIF
    fieldinfo.name  = fieldarr_all[tmp]
    
    ; HDF routines choke on errors, but we want to proceed.
    Catch, err_sts
    
    IF err_sts EQ 0 THEN BEGIN
      ;
      ; First, see if it's a VDATA.  If it is, life is simple.  If
      ; it isn't, the following line will choke, and control will
      ; pass on to the ELSE below.
      ;
      HDF_VD_GetInfo, vdata, fieldinfo.name, Type=t, Order=o
      IF o EQ 1 THEN fieldinfo.ndims = 0 ELSE fieldinfo.ndims = 1
      fieldinfo.dims[0] = o
    ENDIF ELSE IF !Error_State.Name EQ 'IDL_M_HDF_VS_FEXIST' THEN BEGIN
      ;
      ; Sigh, the HDF_VD_GetInfo call failed.  That means that the
      ; field in question is not a VDATA.  Now try to see if it's
      ; an SDATA.  Since this HDF routine chokes also, we need a
      ; new CATCH.
      ;
      Catch, err_sts
      IF err_sts EQ 0 THEN BEGIN
        ;
        ; Initialize the SDATA interface, and try to get info.  If
        ; it works, we set the dimensions and type.
        ;
        sd  = HDF_SD_Start(filename)
        sdi = HDF_SD_NameToIndex(sd, basename + '_' + fieldinfo.name)
        sds = HDF_SD_Select(sd, sdi)
        
        HDF_SD_GetInfo, sds, Type=t, Dims=o
        fieldinfo.ndims = N_Elements(o) - 1
        fieldinfo.dims  = o
        HDF_SD_EndAccess, sds
        HDF_SD_End, sd
      ENDIF ELSE IF !Error_State.Name EQ 'IDL_M_HDF_SD_SELECT' THEN BEGIN
        ; Nope, could not find a VDATA or SDATA by that name.
        print, '['+filename+': no such VD or SD: ' + fieldarr[i] + ']'
      ENDIF ELSE BEGIN
        ; Weird unexpected error
        print, 'hdf_sd failed, err=',err_sts
        stop
      ENDELSE
    ENDIF ELSE BEGIN
      ; Weird unexpected error
      Catch, /Cancel
      print, 'hdf_vd_info failed with status ', err_sts, fieldarr[i]
      Message, !ERR_STRING, /NoName
    ENDELSE
    Catch, /Cancel
    
    IF err_sts EQ 0 THEN BEGIN
      fieldinfo.type  = t
      fieldlist = [ fieldlist, fieldinfo ]
    ENDIF
    skipThisField:
  ENDFOR  ; i=0,nfields
  
  RETURN, fieldlist[1:*]
END

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;+
; NAME:
;     GET_FIELDSTRUCT
;
; IDENT:
;     $Id: get_fieldstruct_hdf.pro,v 1.1 2003/02/14 20:18:57 glennh Exp $
;
; PURPOSE:
;     Given a fieldinfo struct (see GET_FIELDINFO.PRO), build a data
;     structure of the right type.
;
; AUTHOR:
;     Ed Santiago
;
; CALLING SEQUENCE:
;     data = GET_FIELDSTRUCT( fieldinfo )
;
; INPUTS:
;     fieldinfo    - data structure returned by GET_FIELDINFO()
;
; OUTPUTS:
;     a structure template
;
; SIDE EFFECTS:
;
; EXAMPLE:
;
;-
FUNCTION get_fieldstruct, fieldinfo
  mystruct = create_struct( 'dnum', 0D )
  
  FOR i=0, N_Elements(fieldinfo)-1 DO BEGIN
    type  = fieldinfo[i].type   ; String describing the type
    ndims = fieldinfo[i].ndims    ; Number of array dims (eg, 1, 2, 3)
    dims  = fieldinfo[i].dims   ; The dimensions themselves (eg, [9x3])
    
    ; Sigh.  Rather than give us something *USEFUL*, such as its internal
    ; data type number (float=4,int=2,etc), IDL gives us a string.  It's
    ; up to us to convert that to a real data type.  We do so in a gross
    ; CASE statement, converting each string descriptor to a variable
    ; of the correct IDL type.
    ;
    ; To make things worse, with the introduction of unsigned types
    ; in 5.2, IDL has added the 'UINT' and 'ULONG' strings.  Piece
    ; o' cake to add those, along with their respective definitions
    ; as "0U" and "0UL", right?  Well, no.  IDL <5.2 gives a compile-
    ; time error with those, so we can't do that if we want to remain
    ; compatible with 5.1.  The solution is to define the type as a
    ; string, then EXECUTE() it.  This way we don't get the compilation
    ; errors, nor will EXECUTE() fail, since UINT/ULONG don't exist in <5.2.
    CASE type OF
      'BYTE':   t = '0B'
      'INT':    t = '0'
      'UINT': t = '0U'
      'LONG':   t = '0L'
      'ULONG':  t = '0UL'
      'FLOAT':  t = '0.0'
      'DOUBLE': t = '0.0D'
    ENDCASE
    
    r = execute('xxx = ' + t)
    
    ; If field is an array, make it so.
    IF ndims NE 0 THEN $
      xxx = Make_Array(size=[ndims, dims[0:ndims-1], (size(xxx))[1], 0])
      
    ; Append this new field to the Master Data Structure
    mystruct = create_struct( mystruct, '_'+fieldinfo[i].name, xxx)
  ENDFOR
  
  RETURN, mystruct
END

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;+
; NAME:
;     SPLIT
;
; IDENT:
;     $Id: split_hdf.pro,v 1.1 2003/02/14 20:20:07 glennh Exp $
;
; PURPOSE:
;     Just like perl.  Splits a string into an array of strings.
;
; AUTHOR:
;     Ed Santiago
;
; CALLING SEQUENCE:
;     stringarr = split(delimiter, string)
;
; INPUTS:
;     delimiter  -- character to split on
;     string     -- string to split
;
; OUTPUTS:
;     an array of strings
;
; SIDE EFFECTS:
;
; EXAMPLE:
;     IDL> x=split(',', 'this,is,a,test')
;     IDL> print, N_Elements(x), x
;           4 this is a test
;
;-


;FUNCTION split, delimiter, string ; Just like perl
;  arr     = [ 'x' ] ; sigh
;  
;  len     = strlen(string)
;  lastpos = 0
;  WHILE lastpos LT len DO BEGIN
;    pos     = STRPOS(string, delimiter, lastpos)
;    IF pos EQ -1 THEN pos = len
;    arr     = [ arr, STRMID(string, lastpos, pos-lastpos) ]
;    
;    ; Collapse multiple spaces into one
;    IF delimiter EQ ' ' THEN WHILE StrMid(string,pos+1,1) EQ ' ' DO pos=pos+1
;    
;    lastpos = pos+1
;  ENDWHILE
;  
;  ; Always guaranteed at least one hit, unless string is null
;  RETURN, arr[1:*]
;END

FUNCTION HDF_VD_VDATALIST, HDFID, NULLCLASS=NULLCLASS

  ;+
  ; NAME:
  ;    HDF_VD_VDATALIST
  ;
  ; PURPOSE:
  ;    Return the number, names, and class names of all Vdatas in a HDF file.
  ;
  ; CATEGORY:
  ;    HDF utilities.
  ;
  ; CALLING SEQUENCE:
  ;    RESULT = HDF_VD_VDATALIST(HDFID)
  ;
  ; INPUTS:
  ;    HDFID       Identifier of HDF file opened by caller with HDF_OPEN.
  ;
  ; OPTIONAL INPUTS:
  ;    None.
  ;
  ; KEYWORD PARAMETERS:
  ;    NULLCLASS    If set, only returns information about Vdatas which have
  ;                 null class names. (i.e. class name is the null string '').
  ;
  ; OUTPUTS:
  ;    An anonymous structure containing the number of Vdatas,
  ;    the name of each Vdata, and the class name of each Vdata in the file.
  ;    The fields in the structure are as follows:
  ;    NVDATAS       Number of Vdatas in the file (0 if none were found).
  ;    VDATANAMES    Array of Vdata names ('' if none were found).
  ;    CLASSNAMES    Array of class names ('' if none were found).
  ;
  ; OPTIONAL OUTPUTS:
  ;    None
  ;
  ; COMMON BLOCKS:
  ;    None
  ;
  ; SIDE EFFECTS:
  ;    None.
  ;
  ; RESTRICTIONS:
  ;    Requires IDL 5.0 or higher (square bracket array syntax).
  ;
  ; EXAMPLE:
  ;
  ;file = 'sdsvdata.hdf'
  ;hdfid = hdf_open(file)
  ;result = hdf_vd_vdatalist(hdfid)
  ;hdf_close, hdfid
  ;help, result, /structure
  ;
  ; MODIFICATION HISTORY:
  ; Liam.Gumley@ssec.wisc.edu
  ; http://cimss.ssec.wisc.edu/~gumley
  ; $Id: hdf_vd_vdatalist.pro,v 1.1 2000/01/04 22:22:11 gumley Exp $
  ;
  ; Copyright (C) 1999, 2000 Liam E. Gumley
  ;
  ; This program is free software; you can redistribute it and/or
  ; modify it under the terms of the GNU General Public License
  ; as published by the Free Software Foundation; either version 2
  ; of the License, or (at your option) any later version.
  ;
  ; This program is distributed in the hope that it will be useful,
  ; but WITHOUT ANY WARRANTY; without even the implied warranty of
  ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ; GNU General Public License for more details.
  ;
  ; You should have received a copy of the GNU General Public License
  ; along with this program; if not, write to the Free Software
  ; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
  ;-

  rcs_id = '$Id: hdf_vd_vdatalist.pro,v 1.1 2000/01/04 22:22:11 gumley Exp $'
  
  ;- Check arguments
  if (n_params() ne 1) then message, 'Usage: RESULT = HDF_SD_VDATALIST(HDFID)'
  if (n_elements(hdfid) eq 0) then message, 'Argument HDFID is undefined'
  
  ;- Set default return values
  nvdatas = 0L
  vdatanames = ''
  classnames = ''
  
  ;- Create name storage array
  maxnames = 100000L
  namelist = strarr(maxnames)
  classlist = strarr(maxnames)
  
  ;- Loop over all vdatas
  lastid = -1
  thisid = 0
  while (thisid ge 0) do begin
  
    ;- Select next vdata
    thisid = hdf_vd_getid(hdfid, lastid)
    lastid = thisid
    
    ;- If vdata is valid, get vdata name
    if (thisid ge 0) then begin
    
      ;- Get the vdata name and class
      vdata = hdf_vd_attach(hdfid, thisid)
      hdf_vd_get, vdata, name=name, class=class
      hdf_vd_detach, vdata
      
      ;- Store the vdata name and class
      if (keyword_set(nullclass) eq 0) or $
        ((keyword_set(nullclass) eq 1) and (class eq '')) then begin
        namelist[nvdatas] = name
        classlist[nvdatas] = class
        nvdatas = nvdatas + 1L
      endif
      
      ;- Check maximum number of vdata names
      if (nvdatas gt maxnames) then message, 'MAXNAMES exceeded'
      
    endif
    
  endwhile
  
  ;- Extract result arrays
  if (nvdatas gt 0) then begin
    vdatanames = namelist[0 : nvdatas - 1]
    classnames = classlist[0 : nvdatas - 1]
  endif
  
  ;- Return the result
  return, {nvdatas:nvdatas, vdatanames:vdatanames, classnames:classnames}
  
END

FUNCTION HDF_VD_VDATAINFO, HDFID, VDATANAME

  ;+
  ; NAME:
  ;    HDF_VD_VDATAINFO
  ;
  ; PURPOSE:
  ;    Return information about a Vdata in a HDF file.
  ;
  ; CATEGORY:
  ;    HDF utilities.
  ;
  ; CALLING SEQUENCE:
  ;    RESULT = HDF_VD_VDATAINFO(HDFID, VDATANAME)
  ;
  ; INPUTS:
  ;    HDFID        Identifier of HDF file opened by caller with HDF_OPEN.
  ;    VDATANAME    Name of Vdata.
  ;
  ; OPTIONAL INPUTS:
  ;    None.
  ;
  ; KEYWORD PARAMETERS:
  ;    None.
  ;
  ; OUTPUTS:
  ;    An anonymous structure containing information about the Vdata.
  ;    The fields in the structure are as follows:
  ;    NAME          Name of the Vdata ('' if Vdata not found).
  ;    NFIELDS       Number of fields (-1 if Vdata not found).
  ;    FIELDNAMES    Array of field names ('' if Vdata not found).
  ;    NRECORDS      Number of records (-1 if Vdata not found).
  ;
  ; OPTIONAL OUTPUTS:
  ;    None
  ;
  ; COMMON BLOCKS:
  ;    None
  ;
  ; SIDE EFFECTS:
  ;    None.
  ;
  ; RESTRICTIONS:
  ;    Requires IDL 5.0 or higher (square bracket array syntax).
  ;
  ; EXAMPLE:
  ;
  ;file = 'sdsvdata.hdf'
  ;hdfid = hdf_open(file)
  ;result = hdf_vd_vdatainfo(hdfid, 'Vdata with mixed types')
  ;hdf_close, hdfid
  ;help, result, /structure
  ;
  ; MODIFICATION HISTORY:
  ; Liam.Gumley@ssec.wisc.edu
  ; http://cimss.ssec.wisc.edu/~gumley
  ; $Id: hdf_vd_vdatainfo.pro,v 1.2 2000/01/05 15:53:21 gumley Exp $
  ;
  ; Copyright (C) 1999, 2000 Liam E. Gumley
  ;
  ; This program is free software; you can redistribute it and/or
  ; modify it under the terms of the GNU General Public License
  ; as published by the Free Software Foundation; either version 2
  ; of the License, or (at your option) any later version.
  ;
  ; This program is distributed in the hope that it will be useful,
  ; but WITHOUT ANY WARRANTY; without even the implied warranty of
  ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ; GNU General Public License for more details.
  ;
  ; You should have received a copy of the GNU General Public License
  ; along with this program; if not, write to the Free Software
  ; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
  ;-

  rcs_id = '$Id: hdf_vd_vdatainfo.pro,v 1.2 2000/01/05 15:53:21 gumley Exp $'
  
  ;- Check arguments
  if (n_params() ne 2) then message, 'Usage: RESULT = HDF_SD_VDATAINFO(HDFID, VDATANAME)'
  if (n_elements(hdfid) eq 0) then message, 'Argument HDFID is undefined'
  if (n_elements(vdataname) eq 0) then message, 'Argument VDATANAME is undefined'
  
  ;- Set default return values
  name = ''
  nfields = -1
  fieldnames = ''
  nrecords = -1
  
  ;- Get the index for the vdata
  index = hdf_vd_find(hdfid, vdataname)
  
  ;- If the vdata is valid, get vdata information
  if (index gt 0) then begin
  
    ;- Attach to the vdata
    vdataid = hdf_vd_attach(hdfid, index)
    
    ;- Get information
    hdf_vd_get, vdataid, name=name, nfields=nfields, fields=fields, count=nrecords
    
    ;- Detach from the vdata
    hdf_vd_detach, vdataid
    
    ;- Convert field name string to array
    fieldnames = str_sep(fields, ',', /trim)
    
  endif
  
  ;- Return result to caller
  return, {name:name, nfields:nfields, fieldnames:fieldnames, nrecords:nrecords}
  
END

PRO checkAll

  filepath  = 'E:\data\mariomi\application\oxyrisk\temp\A20071522007181.L3b_MO_NSST.main'
  filepath  = 'E:\data\mariomi\application\oxyrisk\temp\A20071522007181.L3b_MO_SST_4.main'
  fid   = HDF_Open(filepath)
  vdatainfolist = HDF_VD_VDATALIST(fid)
  vdnames=vdatainfolist.vdatanames
  for i=0, n_elements(vdnames)-1 do begin
    v     = HDF_VD_Find(fid,vdnames[i])
    vdata = HDF_VD_Attach(fid, v)
    vvdataInfo= HDF_VD_VDATAINFO(fid, vdnames[i])
    HDF_VD_Get, vdata, count=nr
    subFieldNames=vvdataInfo.fieldnames
    for j=0, n_elements(subFieldNames)-1 do begin
      ;IF N_Elements(fieldinfo) EQ 0 THEN BEGIN
      fieldinfo   = get_fieldinfo(filepath, vdnames[i], vdata, subFieldNames[j])
      fieldstruct = get_fieldstruct(fieldinfo)
      ;ENDIF
      print, strcompress(vdnames[i],/REMOVE)+'**'+strcompress(subFieldNames[j],/REMOVE)+'**'+strcompress(fieldinfo.ndims, /REMOVE)
    endfor
  endfor
  
END