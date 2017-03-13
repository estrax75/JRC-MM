pro write_hdf, fileName, bandNames, bandStandardNames, bandLongNames, $
  bandunits, bandList, bandDataType, bandIntercepts, bandSlopes, $
  tempDir, boundaryInfo, $
  postcompression=postcompression, gzipLevel=gzipLevel, NOREVERSE=NOREVERSE, $
  trueMinMaxs=trueMinMaxs, nanlist=nanlist, scaledminmaxs=scaledminmaxs, $
  trueSlopes=trueSlopes, trueIntercepts=trueIntercepts, $
  header=header, gridMapping=gridMapping, $
  time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End, $
  Id=Id, satellite=satellite, date_created=date_created

  nvar=n_elements(bandNames)
  if n_elements(trueMinMaxs) ne nvar*2 then minMaxs=fltarr(2,nvar) else minMaxs=trueMinMaxs

  fs=obj_new('FileSystem', /STAND)
  utils=obj_new('Utility')
  tempDir=fs->adjustDirSep(tempDir, /ADD)

  if n_elements(trueMinMaxs) ne nvar*2 then minMaxs=fltarr(2,nvar)
  pointpos=strpos(filename, '.', /REVERSE_SEARCH)
  slashpos=strpos(filename, path_sep(), /REVERSE_SEARCH)

  folder=strmid(filename, 0, slashpos)

  onlyRealFileName=strmid(filename, slashpos+1, strlen(filename)-slashpos+1)
  onlyFileName=strmid(filename, slashpos+1, 140-strlen(tempDir))

  if strlen(fileName) gt 120 then begin
    ;message, 'choose a shorter full filename, less than 120 characters'+fileName
    print, 'workaround for too long filename...'
    trueFName=fileName
    print, trueFName
    tempFileName=utils->getSysTime(/FILECOMPATIBILITY)
    tempFileName=tempDir+tempFileName+strcompress(fix(randomu(seed)*1000), /REMOVE)
  endif else begin
    tempFileName=tempDir+onlyRealFileName
    print, onlyRealFileName
  endelse

  sdid_file = HDF_SD_START(tempFileName, /CREATE)
  fillHeaderInfo, sdid_file, header, TYPE='HDF', $
    time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End, $
    Id=Id, satellite=satellite, date_created=date_created

  if n_elements(trueSlopes) ne n_elements(bandSlopes) then trueSlopes=bandSlopes
  if n_elements(trueIntercepts) ne n_elements(bandIntercepts) then trueIntercepts=bandIntercepts
  bandStandardNames=bandNames

  for i=0, nvar-1 do begin
    if keyword_set(NOREVERSE) then thisBand=1.*(*(bandList[i])) else thisBand=reverse(1.*(*(bandList[i])),2)
    ;iii=12
    ;if keyword_set(NOREVERSE) then thisBandF=1.*(*(bandList[iii])) else thisBandF=reverse(1.*(*(bandList[iii])),2)

    sizeInfo=size(thisBand, /STRUCT)
    idx=where(sizeInfo.Dimensions ne 0)
    trueDims=sizeInfo.Dimensions[idx]

    ;slope/intercept approach
    isFLag=strpos(bandStandardNames[i], 'FLAG')
    nullnum=0
    if isFlag eq -1 then if n_elements(nanList) eq nvar then fillNanIdxs=where(thisBand eq nanList[i], nullNum, complement=valid_idxs) else valid_idxs=indgen(n_elements(thisBand))

    ;aa=histogram(thisBand)
    ;    device, decompose=0
    ;    loadct, 12

    ;    if i eq 0 or i eq 12 then begin
    ;      window, i, xsize=720, ysize=360, title=bandLongNames[i]+' before conversion'
    ;      tv, reverse(congrid(bytscl(thisBand), 720, 360), 2)
    ;      if i eq 0 then fBand1=thisBand
    ;      if i eq 12 then begin
    ;        idx=where(thisBand eq 0 and fBand1 eq 0, cnt)
    ;        if cnt gt 0 then begin
    ;          checkBand=thisBand*0
    ;          checkBand[idx]=1
    ;          window, 98, xsize=720, ysize=360, title='difference before conversion'
    ;          tv, reverse(congrid(bytscl(checkBand), 720, 360), 2)
    ;        endif
    ;      endif
    ;    endif
    thisBand=convertDataType(thisBand/bandSlopes[i]+bandIntercepts[i], bandDataType[i])
    if nullNum ne 0 then thisBand[fillNanIdxs]=convertDataType(nanList[i], bandDataType[i])
    DelIdlVar, fillNanIdxs
    ;aa1=histogram(thisBand)

    ;    if i eq 0 or i eq 12 then begin
    ;      window, i+1, xsize=720, ysize=360, title=bandLongNames[i]+' after conversion'
    ;      tv, reverse(congrid(bytscl(thisBand), 720, 360), 2)
    ;      if i eq 0 then fBand2=thisBand
    ;      if i eq 12 then begin
    ;        idx=where(thisBand eq 0 and fBand2 eq 0, cnt)
    ;        if cnt gt 0 then begin
    ;          checkBand=thisBand*0
    ;          checkBand[idx]=1
    ;          window, 99, xsize=720, ysize=360, title='difference after conversion'
    ;          tv, reverse(congrid(bytscl(checkBand), 720, 360), 2)
    ;        endif
    ;      endif
    ;    endif
    bDataType=bandDataType[i] ;bDataType=sizeInfo.TYPE
    attrDataType=4
    bandStandardNames=bandNames

    sdid_thisband = HDF_SD_CREATE(sdid_file, bandNames[i], trueDims, $
      BYTE=bDataType eq 1 or bDataType eq 16, DOUBLE=bDataType eq 5, FLOAT=bDataType eq 4, INT=bDataType eq 2 or bDataType eq 12, LONG=bDataType eq 3, STRING=bDataType eq 7)

    ;tv, congrid(bytscl(thisBand), 720, 360)
    HDF_SD_ADDDATA, sdid_thisband, thisBand

    ;CAL={Cal:1.0D, Cal_Err:0.1D, Offset:2.5D, Offset_Err:0.1D, $
    ;Num_Type:6L}

    ;CAL={scale_factor:1d*bandSlopes[i], scale_factor_Err:0.0D, add_Offset:1d*bandIntercepts[i], add_Offset_Err:0.0D, $
    ;calibrated_nt:6L}

    HDF_SD_ATTRSET, sdid_thisband, 'standard_name', bandStandardNames[i], /STRING
    HDF_SD_ATTRSET, sdid_thisband, 'long_name', bandLongNames[i], /STRING

    IS_UNSIGNED=convertDataType(nanList[i], bDataType, /IS_UNSIGNED)
    if keyword_set(IS_UNSIGNED) then HDF_SD_ATTRSET, sdid_thisband, '_Unsigned', "true", /STRING

    if isFlag eq -1 then begin
      HDF_SD_ATTRSET, sdid_thisband, '_FillValue', convertDataType(nanList[i], bDataType), $
        BYTE=bDataType eq 1 or bDataType eq 16, DOUBLE=bDataType eq 5, FLOAT=bDataType eq 4, INT=bDataType eq 2 or bDataType eq 12, LONG=bDataType eq 3, STRING=bDataType eq 7
      valid_out_range=fltarr(2)
      ;valid_range=fltarr(2)
      thisMin=min(thisBand[valid_Idxs], /NAN, max=thisMax)
      valid_range=[thisMin, thisMax]
      ;find the min and the max (from matrix)
      valid_out_range=[thisMin*float(trueSlopes[i])+float(trueIntercepts[i]), thisMax*float(trueSlopes[i])+float(trueIntercepts[i])]
      ; min & max BEFORE apply slope+intercept
      if scaledminmaxs[i,0] ne scaledminmaxs[i,1] then begin
        valid_range=reform(convertDataType(scaledminmaxs[i,*], bandDataType[i]))
      endif
      if minMaxs[i,0] ne minMaxs[i,1] then begin
        valid_out_range=reform(minMaxs[i,*])
      endif
      HDF_SD_ATTRSET, sdid_thisband, 'valid_min', valid_range[0],$
        BYTE=bandDataType[i] eq 1 or bandDataType[i] eq 16, DOUBLE=bandDataType[i] eq 5, FLOAT=bandDataType[i] eq 4, INT=bandDataType[i] eq 2, LONG=bandDataType[i] eq 3, STRING=bandDataType[i] eq 7;, UBYTE=attrDataType eq 16
      HDF_SD_ATTRSET, sdid_thisband, 'valid_max', valid_range[1],$
        BYTE=bandDataType[i] eq 1 or bandDataType[i] eq 16, DOUBLE=bandDataType[i] eq 5, FLOAT=bandDataType[i] eq 4, INT=bandDataType[i] eq 2, LONG=bandDataType[i] eq 3, STRING=bandDataType[i] eq 7;, UBYTE=attrDataType eq 16
      if minMaxs[i,0] ne minMaxs[i,1] then begin
        HDF_SD_ATTRSET, sdid_thisband, 'real_valid_min', valid_out_range[0], /FLOAT
        HDF_SD_ATTRSET, sdid_thisband, 'real_valid_max', valid_out_range[1], /FLOAT
      endif
    endif

    ;HDF_SD_ATTRSET, sdid_thisband, 'missing_value', convertDataType(nanList[i], bandDataType[i]), $
    ;BYTE=bDataType eq 1 or bDataType eq 16, DOUBLE=bDataType eq 5, FLOAT=bDataType eq 4, INT=bDataType eq 2 or bDataType eq 12, LONG=bDataType eq 3, STRING=bDataType eq 7;, UBYTE=bDataType eq 16
    ;    HDF_SD_ATTRSET, sdid_thisband, 'valid_min', float(minMaxs[i,0]),$
    ;      BYTE=bandDataType[i] eq 1 or bandDataType[i] eq 16, DOUBLE=attrDataType eq 5, FLOAT=attrDataType eq 4, INT=attrDataType eq 2 or attrDataType eq 12, LONG=attrDataType eq 3, STRING=attrDataType eq 7;, UBYTE=attrDataType eq 16
    ;    HDF_SD_ATTRSET, sdid_thisband, 'valid_max', float(minMaxs[i,1]),$
    ;      BYTE=bandDataType[i] eq 1 or bandDataType[i] eq 16, DOUBLE=attrDataType eq 5, FLOAT=attrDataType eq 4, INT=attrDataType eq 2 or attrDataType eq 12, LONG=attrDataType eq 3, STRING=attrDataType eq 7;, UBYTE=attrDataType eq 16
    ;    HDF_SD_ATTRSET, sdid_thisband, 'valid_range', reform(float(minMaxs[i,*])*trueSlopes[i]+trueIntercepts[i]),$
    ;      BYTE=bandDataType[i] eq 1 or bandDataType[i] eq 16, DOUBLE=attrDataType eq 5, FLOAT=attrDataType eq 4, INT=attrDataType eq 2 or attrDataType eq 12, LONG=attrDataType eq 3, STRING=attrDataType eq 7;, UBYTE=attrDataType eq 16
    ;    print,  bandLongNames[i]
    ;    print,  'data_type:', data_type
    ;    print,  'valid_min:', valid_range[0]
    ;    print,  'valid_max:', valid_range[1]
    ;    print, 'true minmax:', minMaxs[i,*]
    ;print,  'valid_range:', valid_range
    ;print, '********'
    ;HDF_SD_ATTRSET, sdid_thisband, 'valid_range', valid_range,$
    ;  BYTE=bandDataType[i] eq 1 or bandDataType[i] eq 16, DOUBLE=bandDataType[i] eq 5, FLOAT=bandDataType[i] eq 4, INT=bandDataType[i] eq 2, LONG=bandDataType[i] eq 3, STRING=bandDataType[i] eq 7;, UBYTE=attrDataType eq 16
    HDF_SD_ATTRSET, sdid_thisband, 'units', bandunits[i], /STRING
    if ~(trueSlopes[i] eq 1. and trueIntercepts[i] eq 0.) then begin
      HDF_SD_ATTRSET, sdid_thisband, 'scale_factor', float(trueSlopes[i]), /FLOAT
      HDF_SD_ATTRSET, sdid_thisband, 'add_offset', float(trueIntercepts[i]), /FLOAT
    endif
    ;if n_elements(gridMapping) eq 0 then HDF_SD_ATTRSET, sdid_thisband, 'grid_mapping', 'latitude_longitude' else HDF_SD_ATTRSET, sdid_thisband, 'grid_mapping', gridMapping
    data_type=convertDataType(nanList[i], bandDataType[i], /GET_NAME, FORMAT='HDF')
    HDF_SD_ATTRSET, sdid_thisband, 'data_type', (reform(data_type[0]))[0], /STRING
    HDF_SD_ATTRSET, sdid_thisband, 'data_type_full_name', (reform(data_type[1]))[0], /STRING
    HDF_SD_ATTRSET, sdid_thisband, 'data_type_url', 'http://www.unidata.ucar.edu/software/netcdf/docs/data_type.html', /STRING
    ;HDF_SD_ATTRSET, sdid_thisband, 'valid_min_comment', 'min value before apply scale and offset', /STRING
    ;HDF_SD_ATTRSET, sdid_thisband, 'valid_max_comment', 'max value before apply scale and offset', /STRING


    ;if n_elements(sdn_parameter_urn) eq nvar then value=sdn_parameter_urn[v] else value='n_a'
    ;HDF_SD_ATTRSET, sdid_thisband, 'sdn_parameter_urn', value, /STRING
    ;if n_elements(sdn_parameter_name) eq nvar then value=sdn_parameter_name[v] else value='n_a'
    ;HDF_SD_ATTRSET, sdid_thisband, 'sdn_parameter_name', value, /STRING
    ;if n_elements(sdn_uom_urn) eq nvar then value=sdn_uom_urn[v] else value='n_a'
    ;HDF_SD_ATTRSET, sdid_thisband, 'sdn_uom_urn', value, /STRING
    ;if n_elements(sdn_uom_name) eq nvar then value=sdn_uom_name[v] else value='n_a'
    ;HDF_SD_ATTRSET, sdid_thisband, 'sdn_uom_name', value, /STRING
    ;HDF_SD_SETINFO, sdid_thisband, LABEL=bandNames[i], unit=bandunits[i], $
    ;  format='IDLCODE-'+strcompress(bDataType, /REMOVE), coordsys='N/A', FILL=nanList[i], $
    ;  RANGE=1d*bandSlopes[i]*minMaxs[i,*], cal=cal
    ;NCDF_ATTPUT, ncid, ncvarid[v], 'true min', minMaxs[v,0]
    ;NCDF_ATTPUT, ncid, ncvarid[v], 'true max', minMaxs[v,1]
  endfor
  obj_destroy, fs
  obj_destroy, utils
  HDF_SD_END, sdid_file
  file_move, tempFileName, fileName, /OVERWRITE, /ALLOW_SAME

  if keyword_set(postcompression) then begin
    if strupcase(!VERSION.OS_FAMILY) eq 'WINDOWS' then begin
      zipCommand="C:\Program Files\7-Zip\7z.exe"
      type="zip"
      command="a"
      option="-t{"+type+"}";+" ";+"-slp"
      ; big file & delete original file
      option=' -slp -sdel'
      iFile=fileName;fileName
      zipFile=iFile+'.'+type;fileName+'.'+type
      spawn, '"'+zipCommand+'"'+" "+command+option+" "+zipFile+" "+iFile, /HIDE
    endif
    if strupcase(!VERSION.OS_FAMILY) eq 'UNIX' then begin
      zipCommand='zip'
      type='zip'
      command=''
      option=' -j'
      ;iFile=onlyRealFileName;fileName
      iFile=fileName
      zipFile=iFile+'.'+type;fileName+'.'+type
      gzfile=iFile+'.'+'gz';fileName+'.'+type
      spawn, '"'+zipCommand+'"'+" "+command+option+" "+zipFile+" "+iFile;, /HIDE
      file_delete, iFile, /ALLOW_NONEXISTENT, /QUIET
      file_delete, gzfile, /ALLOW_NONEXISTENT, /QUIET
    endif
  endif

  ;  1  BYTE
  ;  2  INT
  ;  3  LONG  Longword integer
  ;  4   FLOAT Floating point
  ;  5  DOUBLE  Double-precision floating
  ;  6  COMPLEX  Complex floating
  ;  7  STRING
  ;  12  UINT  Unsigned Integer
  ;  13  ULONG  Unsigned Longword Integer
  ;  14  LONG64  64-bit Integer
  ;  15  ULONG64  Unsigned 64-bit Integer

end