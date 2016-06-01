function getPGEInfo, dir, file, globDim=globDim, FORCEREAD=FORCEREAD, opObj=opObj, fsObj=fsObj

  COMMON storePGE, BRDF_params, GlobInfo
  
  ; check if already loaded in this session
  ;if n_elements(BRDF_params) eq 1 and ~keyword_set(FORCEREAD) then begin
  ;  globDim=n_elements(BRDF_params.slope_v1)
  ;  return, BRDF_params
  ;endif

  if obj_valid(opObj) then op=opObj else op=obj_new('GenericOperator')  
  if obj_valid(fsObj) then fs=fsObj else fs=obj_new('FileSystem', /STAND)
  
  ;dir='E:\mariomi\Documents\projects\LDTR\data\AVHRR\data'
  file1='PGE11_band01_BRDF_slope_intercept_params.hdf'
  file2='PGE11_band02_BRDF_slope_intercept_params.hdf'
  
  ff1=dir+path_sep()+file1
  ff2=dir+path_sep()+file2

  ; Channel 1
  ; V
  datasetname='slope_band01_parameter_V1'
  ;read_data, dir, file1, datasetname, slope_v1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff1, datasetname, slope_v1, slope, offset, fillvalue, info=INFO
  globDim=n_elements(slope_v1)
  BRDFStruct={slope_v1:fltarr(globDim),intercept_v1:fltarr(globDim), $
    slope_r1:fltarr(globDim),intercept_r1:fltarr(globDim), $
    NDVImin:fltarr(globDim),NDVImax:fltarr(globDim)}

  BRDF_params1=replicate(temporary(BRDFStruct), 2)
  BRDF_params1[0].slope_v1=reform(slope_v1, globDim)
  slope_v1=0
  datasetname='intercept_band01_parameter_V0'
  ;read_data, dir, file1, datasetname, intercept_v1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff1, datasetname, intercept_v0, slope, offset, fillvalue, info=INFO
  BRDF_params1[0].intercept_v1=reform(intercept_v0, globDim)
  intercept_v0=0

  ; R
  datasetname='slope_band01_parameter_R1'
  ;read_data, dir, file1, datasetname, slope_r1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff1, datasetname, slope_r1, slope, offset, fillvalue, info=INFO
  BRDF_params1[0].slope_r1=reform(slope_r1, globDim)
  slope_r1=0
  datasetname='intercept_band01_parameter_R0'
  ;read_data, dir, file1, datasetname, intercept_r1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff1, datasetname, intercept_r1, slope, offset, fillvalue, info=INFO
  BRDF_params1[0].intercept_r1=reform(intercept_r1, globDim)
  intercept_r1=0

  datasetname='ndvi min'
  ;read_data, dir, file2, datasetname, slope_r1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff1, datasetname, NDVImin, slope, offset, fillvalue, info=INFO
  BRDF_params1[0].NDVImin=reform(NDVImin, globDim)
  NDVImin=0

  datasetname='ndvi max'
  ;read_data, dir, file2, datasetname, intercept_r1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff1, datasetname, NDVImax, slope, offset, fillvalue, info=INFO
  BRDF_params1[0].NDVImax=reform(NDVImax, globDim)
  NDVImax=0

  ; Channel 2
  datasetname='slope_band02_parameter_V1'
  ;read_data, dir, file2, datasetname, slope_v1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff2, datasetname, slope_v1, slope, offset, fillvalue, info=INFO
  BRDF_params1[1].slope_v1=reform(slope_v1, globDim)
  slope_v1=0
  datasetname='intercept_band02_parameter_V0'
  ;read_data, dir, file2, datasetname, intercept_v1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff2, datasetname, intercept_v1, slope, offset, fillvalue, info=INFO
  BRDF_params1[1].intercept_v1=reform(intercept_v1, globDim)
  intercept_v1=0

  datasetname='slope_band02_parameter_R1'
  ;read_data, dir, file2, datasetname, slope_r1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff2, datasetname, slope_r1, slope, offset, fillvalue, info=INFO
  BRDF_params1[1].slope_r1=reform(slope_r1, globDim)
  slope_r1=0
  datasetname='intercept_band02_parameter_R0'
  ;read_data, dir, file2, datasetname, intercept_r1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff2, datasetname, intercept_r1, slope, offset, fillvalue, info=INFO
  BRDF_params1[1].intercept_r1=reform(intercept_r1, globDim)
  intercept_r1=0

  datasetname='ndvi min'
  ;read_data, dir, file2, datasetname, slope_r1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff2, datasetname, NDVImin, slope, offset, fillvalue, info=INFO
  BRDF_params1[1].NDVImin=reform(NDVImin, globDim)
  NDVImin=0

  datasetname='ndvi max'
  ;read_data, dir, file2, datasetname, intercept_r1, slope, offset, fillvalue, info=INFO
  op->readHdfFullInfoData, ff2, datasetname, NDVImax, slope, offset, fillvalue, info=INFO
  BRDF_params1[1].NDVImax=reform(NDVImax, globDim)
  NDVImax=0

  if ~obj_valid(opObj) then obj_destroy, op  
  if ~obj_valid(fsObj) then obj_destroy, fs
  
  BRDF_params=BRDF_params1
  BRDF_params1=0
  return, BRDF_params
  ;datasetname='regression_band01_parameter_R'
  ;read_data, confDir, confFile, datasetname, arrayR, slope, offset, fillvalue, info=INFO
end