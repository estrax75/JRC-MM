function getGlobalInfo, dir, file, dataSetName, globDim=globDim, FORCEREAD=FORCEREAD, opObj=opObj, fsObj=fsObj, testFile=testFile

  COMMON storePGE, BRDF_params, GlobInfo
  
  ; check if already loaded in this session
  if n_elements(GlobInfo) eq 1 and ~keyword_set(FORCEREAD) then begin
    idx=where(GlobInfo.dataSetName eq dataSetName, count)
    if count eq 1 then return, GlobInfo[idx] 
  endif

  if obj_valid(opObj) then op=opObj else op=obj_new('GenericOperator')  
  if obj_valid(fsObj) then fs=fsObj else fs=obj_new('FileSystem_manager', /STAND)
  
  ; ignore input file OR get year/month/day/noaa info
  ; testFile='GLOBAL_L3_GEOG_0.05DEG_001-001_03.NOAA-16.hdf'
  
  sepSign=strpos(dir, path_sep(), /REVERSE_SEARCH)
  if sepSign ne strlen(dir)-1 then dir=dir+path_sep()
  ff1=dir[0]+testFile[0]

  ; Channel 1
  ; V
  op->readHdfFullInfoData, ff1, datasetname, dataValue, slope, offset, fillvalue, info=INFO
  ;globDim=n_elements(dataValue)
  ;refStruct={dataSetName:'', slope:fltarr(globDim),offset:fltarr(globDim), fillvalue:fltarr(globDim)}
  refStruct={dataSetName:'', slope:0.0d, offset:0.0d, fillvalue:0.0d}
  thisStruct=refStruct
  thisStruct.dataSetName=datasetname
  if n_elements(slope) ne 0 then thisStruct.slope=slope
  if n_elements(offset) ne 0 then thisStruct.offset=offset
  if n_elements(fillvalue) ne 0 then thisStruct.fillvalue=fillvalue
  
  newStruct=replicate(refStruct, n_elements(GlobInfo)+1)
  newStruct[n_elements(GlobInfo)]=thisStruct
  if n_elements(GlobInfo) ne 0 then newStruct[0:n_elements(GlobInfo)-1]=GlobInfo[0:n_elements(GlobInfo)-1]
  GlobInfo=newStruct
  newStruct=0

  if ~obj_valid(opObj) then obj_destroy, op  
  if ~obj_valid(fsObj) then obj_destroy, fs
  
  return, GlobInfo[n_elements(GlobInfo)-1]
  ;datasetname='regression_band01_parameter_R'
  ;read_data, confDir, confFile, datasetname, arrayR, slope, offset, fillvalue, info=INFO
end