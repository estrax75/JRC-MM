pro write_georef_ncdf, fileName, bandNames, bandStandardNames, bandLongNames, $
  bandunits, bandList, bandDataType, bandIntercepts, bandSlopes, $
  tempDir, boundaryInfo, $
  postcompression=postcompression, gzipLevel=gzipLevel, $
  trueMinMaxs=trueMinMaxs, scaledminmaxs=scaledminmaxs, nanlist=nanlist, $
  trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
  header=header, NOREVERSE=NOREVERSE, gridMapping=gridMapping, $
  time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End, $
  Id=Id, satellite=satellite, date_created=date_created, cdr_variable=cdr_variable, $
  OLDSTYLE=OLDSTYLE
  ; procedure to read a geoTiff and export an a new one

  nvar=n_elements(bandNames)
  if n_elements(trueMinMaxs) ne nvar*2 then minMaxs=fltarr(nvar,2) else minMaxs=trueMinMaxs

  fs=obj_new('FileSystem', /STAND)
  utils=obj_new('Utility')
  tempDir=fs->adjustDirSep(tempDir, /ADD)

  pointpos=strpos(filename, '.', /REVERSE_SEARCH)
  slashpos=strpos(filename, path_sep(), /REVERSE_SEARCH)

  folder=strmid(filename, 0, slashpos)

  onlyRealFileName=strmid(filename, slashpos+1, strlen(filename)-slashpos+1)
  onlyFileName=strmid(filename, slashpos+1, 140-strlen(tempDir))

  if strlen(fileName) gt 120 then begin
    ;message, 'choose a shorter full filename, less than 120 characters'+fileName
    print, 'workaround for too long filename...'
    trueFName=fileName
    tempFileName=utils->getSysTime(/FILECOMPATIBILITY)
    tempFileName=tempDir+tempFileName+strcompress(fix(randomu(seed)*1000), /REMOVE)
  endif else begin
    tempFileName=fileName
  endelse

  if n_elements(trueSlopes) ne n_elements(bandSlopes) then trueSlopes=bandSlopes
  if n_elements(trueIntercepts) ne n_elements(bandIntercepts) then trueIntercepts=bandIntercepts

  dims=size(*bandList[0],/DIM)
  onx = dims[0];360
  ony = dims[1];180
  odx = 360.d0 / double(onx)
  ody = 180.d0 / double(ony)
  olon = dindgen(onx) * odx - 180.d0 + 0.5d0 * odx
  olat = dindgen(ony) * ody - 90.d0 + 0.5d0 * ody
  
  ;otime=dblarr(1)
  ;otime[0]=1d

  ncid = NCDF_CREATE(tempFileName,/CLOBBER)

  ; add NC_GLOBAL#Conventions=CF-1.6
  ;NCDF_ATTPUT, ncid, /GLOBAL, 'Conventions', 'CF-1.6'

  londim = NCDF_DIMDEF(ncid, 'lon',onx)
  latdim = NCDF_DIMDEF(ncid, 'lat',ony)

  lonid = NCDF_VARDEF(ncid, 'lon', [londim], /DOUBLE, gzip=gzipLevel)
  NCDF_ATTPUT,ncid,lonid,'long_name','Longitude'
  NCDF_ATTPUT,ncid,lonid,'standard_name','Longitude'
  NCDF_ATTPUT,ncid,lonid,'units','degrees_east'
  NCDF_ATTPUT,ncid,lonid,'axis','X'

  latid = NCDF_VARDEF(ncid, 'lat', [latdim], /DOUBLE, gzip=gzipLevel)
  NCDF_ATTPUT,ncid,latid,'long_name','Latitude'
  NCDF_ATTPUT,ncid,latid,'standard_name','Latitude'
  NCDF_ATTPUT,ncid,latid,'units','degrees_north'
  NCDF_ATTPUT,ncid,latid,'axis','Y'

;  timedim = NCDF_VARDEF(ncid, 'time', [1], /DOUBLE, gzip=gzipLevel)
;  NCDF_ATTPUT,ncid,timedim,'long_name','time'
;  NCDF_ATTPUT,ncid,timedim,'standard_name','time'
;  NCDF_ATTPUT,ncid,timedim,'units','days from 1st january'
;  NCDF_ATTPUT,ncid,timedim,'bounds','time_bnds'
;  NCDF_ATTPUT,ncid,timedim,'axis','T'

  varnames=bandNames
  varstandardnames=bandNames;bandStandardNames
  varlongnames=bandLongNames
  varunits=bandunits

  ncvarid = lonarr(nvar)
  ;NAN=-9999.
  FOR v = 0,nvar-1 DO BEGIN
    ;  stop
    ; watch out for Unsigned... (byte, int...)
    
    ncvarid[v] = NCDF_VARDEF(ncid,varnames[v], [londim,latdim], $
      BYTE=bandDataType[v] eq 16 or bandDataType[v] eq 1, SHORT=bandDataType[v] eq 2 or bandDataType[v] eq 12, $
      LONG=bandDataType[v] eq 3, $
      ;USHORT=bandDataType[v] eq 12, $
      ULONG=bandDataType[v] eq 13, $
      FLOAT=bandDataType[v] eq 4, DOUBLE=bandDataType[v] eq 5, $
      STRING=bandDataType[v] eq 7, UINT64=bandDataType[v] eq 14, $
      gzip=gzipLevel)
    ;    ncvarid[v] = NCDF_VARDEF(ncid,varnames[v], [londim,latdim], $
    ;      BYTE=bandDataType[v] eq 16 or bandDataType[v] eq 1, SHORT=bandDataType[v] eq 2 or bandDataType[v] eq 12, $
    ;      LONG=bandDataType[v] eq 3, $
    ;      ;USHORT=bandDataType[v] eq 12, $ ;UBYTE=bandDataType[v] eq 16,
    ;      ULONG=bandDataType[v] eq 13, $
    ;      FLOAT=bandDataType[v] eq 4, DOUBLE=bandDataType[v] eq 5, $
    ;      STRING=bandDataType[v] eq 7, UINT64=bandDataType[v] eq 14, $
    ;      gzip=gzipLevel)

    ;; Attributes for data variables
    ; fillvalue type MUST match "varput" datatype!!!
    ;NCDF_ATTPUT, ncid, ncvarid[v], '_FillValue',float(NAN)
    ;convert to int _fillvalue (slope/intercept approach)
    ;NCDF_ATTPUT, ncid, ncvarid[v], '_FillValue',fix(NAN)
    my_NCDF_ATTPUT, ncid, ncvarid[v], 'standard_name',varstandardnames[v]
    my_NCDF_ATTPUT, ncid, ncvarid[v], 'long_name',varlongnames[v]

    NAN=nanlist[v]
    isFLag=strpos(varstandardnames[v], 'FLAG')
    ; no Nan to set...
    nullNum=0
    if keyword_set(NOREVERSE) then thisBand=1.*(*(bandList[v])) else thisBand=reverse(1.*(*(bandList[v])),2)

    ; _Unsinned is a "not-well-documented" feature for unsigned data type (INT and BYTE)
    IS_UNSIGNED=convertDataType(undef, bandDataType[v], /IS_UNSIGNED)
    if keyword_set(IS_UNSIGNED) and ~(keyword_set(OLDSTYLE)) then my_NCDF_ATTPUT, ncid, ncvarid[v], '_Unsigned', "true"

    ;flag Nan...
    fillNanIdxs=where(thisBand eq NAN, nullNum, complement=valid_Idxs)
    ;apply "reversed" slope/intercept AND map to output datatype
    thisBand=convertDataType(float(thisBand)/bandSlopes[v]+bandIntercepts[v], bandDataType[v])
    ;map to output datatype NaN also
    if nullNum ne 0 then thisBand[fillNanIdxs]=convertDataType(NAN, bandDataType[v])
    DelIdlVar, fillNanIdxs
    ;find the data type name (to avoid misinterpretation of data)
    ;    my_NCDF_ATTPUT, ncid, ncvarid[v], 'valid_range', valid_range, TYPE=bandDataType[v]

    ; If matrix has some valid range/Nan, fill the attribs
    ; set FillValue, valid_min & valid_max only where available
    if isFlag eq -1 then begin
      my_NCDF_ATTPUT, ncid, ncvarid[v], '_FillValue', convertDataType(nanList[v], bandDataType[v]), TYPE=bandDataType[v]
      ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'missing_value', convertDataType(nanList[v], bandDataType[v]), TYPE=bandDataType[v]
      valid_out_range=fltarr(2)
      ;valid_range=fltarr(2)
      thisMin=min(thisBand[valid_Idxs], /NAN, max=thisMax)
      valid_range=[thisMin, thisMax]
      ;find the min and the max (from matrix)
      valid_out_range=[thisMin*float(trueSlopes[v])+float(trueIntercepts[v]), thisMax*float(trueSlopes[v])+float(trueIntercepts[v])]
      ; min & max BEFORE apply slope+intercept
      if scaledminmaxs[v,0] ne scaledminmaxs[v,1] then begin
        valid_range=reform(convertDataType(scaledminmaxs[v,*], bandDataType[v]))
      endif
      if minMaxs[v,0] ne minMaxs[v,1] then begin
        valid_out_range=reform(minMaxs[v,*])
      endif
      my_NCDF_ATTPUT, ncid, ncvarid[v], 'valid_min', valid_range[0], TYPE=bandDataType[v];minMaxs[v,0]
      my_NCDF_ATTPUT, ncid, ncvarid[v], 'valid_max', valid_range[1], TYPE=bandDataType[v];minMaxs[v,1]
      ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'valid_min', valid_range[0]
      ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'valid_max', valid_range[1]
      ;find the min and the max (from external setting)
      ; min & max AFTER apply slope+intercept
      my_NCDF_ATTPUT, ncid, ncvarid[v], 'real_valid_min', valid_out_range[0], TYPE=4
      my_NCDF_ATTPUT, ncid, ncvarid[v], 'real_valid_max', valid_out_range[1], TYPE=4
      ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'valid_min_comment', 'min value before apply scale and offset', type=7 ;STRING
      ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'valid_max_comment', 'max value before apply scale and offset', type=7 ;STRING
    endif
    my_NCDF_ATTPUT, ncid, ncvarid[v], 'units', varunits[v]
    ; add additional info only if it is necessary
    if ~(trueSlopes[v] eq 1. and trueIntercepts[v] eq 0.) then begin
      if keyword_set(OLDSTYLE) then my_NCDF_ATTPUT, ncid, ncvarid[v], 'slope', trueSlopes[v] else my_NCDF_ATTPUT, ncid, ncvarid[v], 'scale_factor', trueSlopes[v]
      if keyword_set(OLDSTYLE) then my_NCDF_ATTPUT, ncid, ncvarid[v], 'intercept', trueIntercepts[v] else my_NCDF_ATTPUT, ncid, ncvarid[v], 'add_offset', trueIntercepts[v] 
    endif
    data_type=convertDataType(nanList[v], bandDataType[v], /GET_NAME, FORMAT='NC')
    my_NCDF_ATTPUT, ncid, ncvarid[v], 'data_type', (reform(data_type[0]))[0], type=7 ;STRING
    my_NCDF_ATTPUT, ncid, ncvarid[v], 'data_type_full_name', (reform(data_type[1]))[0], type=7 ;STRING
    my_NCDF_ATTPUT, ncid, ncvarid[v], 'data_type_url', 'http://www.unidata.ucar.edu/software/netcdf/docs/data_type.html', type=7 ;STRING
    
    ;if n_elements(gridMapping) eq 0 then my_NCDF_ATTPUT, ncid, ncvarid[v], 'grid_mapping', 'latitude_longitude' else my_NCDF_ATTPUT, ncid, ncvarid[v], 'grid_mapping', gridMapping
    ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'axis','YX'
    ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'version', header.versionNumber
    ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'prod_date', header.versionDate
    ;NCDF_ATTPUT, ncid, ncvarid[v], 'slope', bandSlopes[v]
    ;NCDF_ATTPUT, ncid, ncvarid[v], 'intercept', bandIntercepts[v]
    ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'valid_min', minMaxs[v,0]
    ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'valid_max', minMaxs[v,1]

    ;if n_elements(sdn_parameter_urn) eq nvar then value=sdn_parameter_urn[v] else value='n_a'
    ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'sdn_parameter_urn', value
    ;if n_elements(sdn_parameter_name) eq nvar then value=sdn_parameter_name[v] else value='n_a'
    ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'sdn_parameter_name', value
    ;if n_elements(sdn_uom_urn) eq nvar then value=sdn_uom_urn[v] else value='n_a'
    ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'sdn_uom_urn', value
    ;if n_elements(sdn_uom_name) eq nvar then value=sdn_uom_name[v] else value='n_a'
    ;my_NCDF_ATTPUT, ncid, ncvarid[v], 'sdn_uom_name', value
    ;NCDF_ATTPUT, ncid, ncvarid[v], 'minmax comment', 'If true min eq true max skip information (not available)'
    ;count = [onx,ony] & stride = [1,1] & offset=[0,0]
    ;NCDF_VARPUT, ncid, ncvarid[v], *(bandList[v]), count=count, offset=offset;, stride=stride
  ENDFOR

  ;; set global attributes from a structure (map tag to tag)
  fillHeaderInfo, ncid, header, $
    time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End, $
    Id=Id, satellite=satellite, date_created=date_created, TYPE='NC';, cdr_variable=cdr_variable
  ;fillHeaderInfo_full, ncid, header
  ;getJRCHeader

  ;; Put file in data mode.
  NCDF_CONTROL, ncid, /ENDEF

  NCDF_VARPUT, ncid, lonid, olon
  NCDF_VARPUT, ncid, latid, olat

  count = [onx,ony] & stride = [1,1] & offset=[0,0]
  FOR v = 0,nvar-1 DO BEGIN
    NAN=nanlist[v]
    if keyword_set(NOREVERSE) then thisBand=1.*(*(bandList[v])) else thisBand=reverse(1.*(*(bandList[v])),2)
    ;window, 0, xsize=720, ysize=360, title=varlongnames[v]+'preNan'
    ;tv, congrid(bytscl(thisBand), 720, 360)
    ;slope/intercept approach
    fillNanIdxs=where(thisBand eq NAN, nullNum)
    thisBand=convertDataType(float(thisBand)/bandSlopes[v]+bandIntercepts[v], bandDataType[v])
    if nullNum ne 0 then thisBand[fillNanIdxs]=convertDataType(NAN, bandDataType[v])
    ;window, 1, xsize=720, ysize=360, title=varlongnames[v]+'postNan'
    ;tv, congrid(bytscl(thisBand), 720, 360)
    NCDF_VARPUT, ncid, ncvarid[v], thisBand, count=count, offset=offset;, stride=stride
    DelIdlVar, thisBand
  ENDFOR

  NCDF_CLOSE, ncid
  if keyword_set(postcompression) then begin
    if strupcase(!VERSION.OS_FAMILY) eq 'WINDOWS' then begin
      zipCommand="C:\Program Files\7-Zip\7z.exe"
      type="zip"
      command="a"
      option="-t{"+type+"}";+" ";+"-slp"
      ; big file & delete original file
      option=' -slp -sdel'
      iFile=onlyRealFileName;fileName
      zipFile=onlyRealFileName+'.'+type;fileName+'.'+type
      spawn, '"'+zipCommand+'"'+" "+command+option+" "+zipFile+" "+iFile, /HIDE
    endif
    if strupcase(!VERSION.OS_FAMILY) eq 'UNIX' then begin
      zipCommand='zip'
      type='zip'
      command=''
      option=''
      iFile=onlyRealFileName;fileName
      zipFile=onlyRealFileName+'.'+type;fileName+'.'+type
      spawn, '"'+zipCommand+'"'+" "+command+option+" "+zipFile+" "+iFile, /HIDE
    endif
  endif

  obj_destroy, fs
  obj_destroy, utils
  file_move, tempFileName, fileName, /OVERWRITE, /ALLOW_SAME

end

pro my_NCDF_ATTPUT, ncid, ncvarid, attName, value, TYPE=typeCode

  if n_elements(typeCode) eq 1 then NCDF_ATTPUT, ncid, ncvarid, attName, value, BYTE=typeCode eq 1 or typeCode eq 16, SHORT=typeCode eq 2, $
    LONG=typeCode eq 3, $
    USHORT=typeCode eq 12, $ ;UBYTE=typeCode eq 16,
    ULONG=typeCode eq 13, $
    FLOAT=typeCode eq 4, DOUBLE=typeCode eq 5, $
    STRING=typeCode eq 7, UINT64=typeCode eq 14 else NCDF_ATTPUT, ncid, ncvarid, attName, value

end