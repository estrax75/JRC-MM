;function data = read_hdffile(infilename,var_list,varargin)
function readEnviFile, infilename, varList, nanValue, bandIndex=bandIndex

  COMMON smurffCB, mainApp
  ;  % Read multiple datasets from Envi file
  ;  %
  ;  % Input:
  ;  %  infilename - string - name of file to read
  ;  %
  ;  % Output:
  ;  %  data - array(elemnts,nvars)
  ;  %
  
  ;sd_id = hdfsd('start',infilename,'read');
  
  doLog,'*start**read_envifile***', level=0
  doLog,'enviMainFile: ', infilename, level=0
  if ~(FILE_INFO(infilename)).exists then infilename=infilename+'.envi'
  ENVI_OPEN_FILE, infilename, r_fid=fidEnviMainFile
  ENVI_FILE_QUERY, fidEnviMainFile, ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bnames
  varList=bnames
  i=0
  thisData = ENVI_GET_DATA(fid=fidEnviMainFile, dims=c_dims, pos=i)
  mapInfo = ENVI_GET_MAP_INFO(FID=fidEnviMainFile)
  matrixDims=size(thisData, /DIMENSIONS)
  ;nanValue=mainApp->getMosaicNaN()
  allIdxs=where(~finite(thisData) eq 0, countItems)
  if n_elements(nanValue) ne 0 then begin
    validIdxs=where(thisData ne nanValue, countValid)
  endif else begin
    ;validIdxs=where(thisData ne nanValue, countValid)
    countValid=countItems
    validIdxs=indgen(countValid)
  endelse
  ;data=make_array(dims[0], dims[1], nb, type=dt)
  data=make_array(countItems, nb, type=dt)
  data[*,i]=thisData[allIdxs]
  thisData=0
  
  for i=1, nb-1 do begin
    thisData = ENVI_GET_DATA(fid=fidEnviMainFile, dims=c_dims, pos=i)
    data[*,i]=thisData[allIdxs]
  ;validIdxs=where(thisData ne nanValue)
  ;data[*,i]=thisData
  ;thisData=0
  endfor
  ;return, {data:data, validIdxs:validIdxs, mapInfo:mapInfo, dims:c_dims}
  return, {data:data, validIdxs:validIdxs, dims:c_dims}
  
end