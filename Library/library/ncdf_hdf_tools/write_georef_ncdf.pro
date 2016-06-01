pro write_georef_ncdf, fileName, bandNames, bandunits, bandList, bandDataType, bandIntercepts, bandSlopes, tempDir, boundaryInfo, $
  postcompression=postcompression, gzipLevel=gzipLevel, NOREVERSE=NOREVERSE, trueMinMaxs=trueMinMaxs, nanlist=nanlist, $
  trueSlopes=trueSlopes, trueIntercepts=trueIntercepts
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

  ncid = NCDF_CREATE(tempFileName,/CLOBBER)
  londim = NCDF_DIMDEF(ncid, 'lon',onx)
  latdim = NCDF_DIMDEF(ncid, 'lat',ony)

  lonid = NCDF_VARDEF(ncid, 'lon', [londim], /DOUBLE, gzip=gzipLevel)
  NCDF_ATTPUT,ncid,lonid,'long_name','Longitude'
  NCDF_ATTPUT,ncid,lonid,'units','degrees_east'

  latid = NCDF_VARDEF(ncid, 'lat', [latdim], /DOUBLE, gzip=gzipLevel)
  NCDF_ATTPUT,ncid,latid,'long_name','Latitude'
  NCDF_ATTPUT,ncid,latid,'units','degrees_north'

  varnames=bandNames
  varlongnames=bandNames
  varunits=bandunits
  version='1.1'
  creation_date='31/5/2016'
  ncvarid = lonarr(nvar)
  ;NAN=-9999.
  FOR v = 0,nvar-1 DO BEGIN

    ; watch out for Unsigned byte, code i UBYTE NOT BYTE! (but doesn't wotk on organa, hdf version too old?)
    ncvarid[v] = NCDF_VARDEF(ncid,varnames[v], [londim,latdim], $
      BYTE=bandDataType[v] eq 1, DOUBLE=bandDataType[v] eq 5, $
      FLOAT=bandDataType[v] eq 4, SHORT=bandDataType[v] eq 2, $
      LONG=bandDataType[v] eq 3, STRING=bandDataType[v] eq 7, gzip=gzipLevel)

    ;; Attributes for data variables
    ; fillvalue type MUST match "varput" datatype!!!
    ;NCDF_ATTPUT, ncid, ncvarid[v], '_FillValue',float(NAN)
    ;convert to int _fillvalue (slope/intercept approach)
    ;NCDF_ATTPUT, ncid, ncvarid[v], '_FillValue',fix(NAN)
    NCDF_ATTPUT, ncid, ncvarid[v], '_FillValue',convertDataType(nanList[v], bandDataType[v])
    NCDF_ATTPUT, ncid, ncvarid[v], 'axis','YX'
    NCDF_ATTPUT, ncid, ncvarid[v], 'long_name',varlongnames[v]
    NCDF_ATTPUT, ncid, ncvarid[v], 'units', varunits[v]
    NCDF_ATTPUT, ncid, ncvarid[v], 'version', version
    NCDF_ATTPUT, ncid, ncvarid[v], 'prod_date', creation_date
    ;NCDF_ATTPUT, ncid, ncvarid[v], 'slope', bandSlopes[v]
    ;NCDF_ATTPUT, ncid, ncvarid[v], 'intercept', bandIntercepts[v]
    NCDF_ATTPUT, ncid, ncvarid[v], 'slope', trueSlopes[v]
    NCDF_ATTPUT, ncid, ncvarid[v], 'intercept', trueIntercepts[v]
    NCDF_ATTPUT, ncid, ncvarid[v], 'true min', minMaxs[v,0]
    NCDF_ATTPUT, ncid, ncvarid[v], 'true max', minMaxs[v,1]
    NCDF_ATTPUT, ncid, ncvarid[v], 'minmax comment', 'If true min eq true max skip information (not available)'
    ;count = [onx,ony] & stride = [1,1] & offset=[0,0]
    ;NCDF_VARPUT, ncid, ncvarid[v], *(bandList[v]), count=count, offset=offset;, stride=stride
  ENDFOR

  title='BRF'
  technique=' ' & contact=' ' & telephone=' ' & facsimile=' ' & internet=' ' & post_processing=' ' & technique=' '
  ;; create global attributes
  NCDF_ATTPUT, ncid, /GLOBAL,'title',title
  NCDF_ATTPUT, ncid, /GLOBAL,'technique',technique
  NCDF_ATTPUT, ncid, /GLOBAL,'contact',contact
  NCDF_ATTPUT, ncid, /GLOBAL,'telephone',telephone
  NCDF_ATTPUT, ncid, /GLOBAL,'facsimile',facsimile
  NCDF_ATTPUT, ncid, /GLOBAL,'internet',internet
  NCDF_ATTPUT, ncid, /GLOBAL,'post-processing',post_processing

  ;; Put file in data mode.
  NCDF_CONTROL, ncid, /ENDEF

  ;; write longitude, latitude
  NCDF_VARPUT, ncid, lonid, olon
  NCDF_VARPUT, ncid, latid, olat

  FOR v = 0,nvar-1 DO BEGIN
    NAN=nanlist[v]
    count = [onx,ony] & stride = [1,1] & offset=[0,0]
    if keyword_set(NOREVERSE) then thisBand=1.*(*(bandList[v])) else thisBand=reverse(1.*(*(bandList[v])),2)
    ;slope/intercept approach
    fillNanIdxs=where(thisBand eq NAN, nullNum)
    thisBand=convertDataType(float(thisBand)/bandSlopes[v]+bandIntercepts[v], bandDataType[v])
    if nullNum ne 0 then thisBand[fillNanIdxs]=convertDataType(NAN, bandDataType[v])
    NCDF_VARPUT, ncid, ncvarid[v], thisBand, count=count, offset=offset;, stride=stride
  ENDFOR

  if keyword_set(postcompression) then begin
    if strupcase(!VERSION.OS_FAMILY eq 'WINDOWS') then begin
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
    if strupcase(!VERSION.OS_FAMILY eq 'UNIX') then begin
      zipCommand='zip'
      type='zip'
      command=''
      option=''
      iFile=onlyRealFileName;fileName
      zipFile=onlyRealFileName+'.'+type;fileName+'.'+type
      spawn, '"'+zipCommand+'"'+" "+command+option+" "+zipFile+" "+iFile, /HIDE
    endif
  endif

  NCDF_CLOSE, ncid
  obj_destroy, fs
  obj_destroy, utils
  file_move, tempFileName, fileName, /OVERWRITE, /ALLOW_SAME

end