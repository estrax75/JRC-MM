pro write_hdf, fileName, bandNames, bandMU, bandValues, bandDataType, bandIntercepts, bandSlopes, tempDir, boundary, $
  NOREVERSE=NOREVERSE, nan=nan, trueMinMaxs=trueMinMaxs, reservedvalues=reservedvalues, nanList=nanList

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
    tempFileName=utils->getSysTime(/FILECOMPATIBILITY)
    tempFileName=tempDir+tempFileName+strcompress(fix(randomu(seed)*1000), /REMOVE)
  endif else begin
    tempFileName=tempDir+fileName
  endelse

  sdid_file = HDF_SD_START(tempFileName, /CREATE)
  for i=0, nvar-1 do begin
    if keyword_set(NOREVERSE) then thisBand=1.*(*(bandValues[i])) else thisBand=reverse(1.*(*(bandValues[i])),2)
    
    sizeInfo=size(thisBand, /STRUCT)
    idx=where(sizeInfo.Dimensions ne 0)
    trueDims=sizeInfo.Dimensions[idx]

    ;slope/intercept approach
    nullnum=0
    if n_elements(NAN) ne 0 then fillNanIdxs=where(thisBand eq NAN, nullNum)
    ;aa=histogram(thisBand)
    thisBand=convertDataType(thisBand/bandSlopes[i]+bandIntercepts[i], bandDataType[i])
    if nullNum ne 0 then thisBand[fillNanIdxs]=convertDataType(NAN, bandDataType[i])
    ;aa1=histogram(thisBand)
    bDataType=bandDataType[i] ;bDataType=sizeInfo.TYPE
    sdid_thisband = HDF_SD_CREATE(sdid_file, bandNames[i], trueDims, $
      BYTE=bDataType eq 1, DOUBLE=bDataType eq 5, FLOAT=bDataType eq 4, INT=bDataType eq 2, LONG=bDataType eq 3, STRING=bDataType eq 7)
    HDF_SD_ADDDATA, sdid_thisband, thisBand
    HDF_SD_SETINFO, sdid_thisband, LABEL=bandNames[i], unit=bandMU[i], $
      format='IDLCODE-'+strcompress(bDataType, /REMOVE), coordsys='N/A', FILL=nanList[i], $
      RANGE=minMaxs[i,*]
    ;NCDF_ATTPUT, ncid, ncvarid[v], 'true min', minMaxs[v,0]
    ;NCDF_ATTPUT, ncid, ncvarid[v], 'true max', minMaxs[v,1]
  endfor
  obj_destroy, fs
  obj_destroy, utils
  HDF_SD_END, sdid_file
  file_move, tempFileName, fileName, /OVERWRITE, /ALLOW_SAME

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