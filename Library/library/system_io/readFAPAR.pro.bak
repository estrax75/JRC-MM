;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function readFAPAR, folder, fileName, FOUND=FOUND

  ;  avBandNames=['FAPAR','Sigma FAPAR','RECTIFIED RED','Sigma RECTIFIED RED',$
  ;'RECTIFIED NIR','Sigma RECTIFIED NIR','FLAG','BR TOC RED','BRF TOC NIR',$
  ;'JRC QA']
  faparDSInfo=getStandardFaparDataSetInfo()
  avBandNames=faparDSInfo.bandNames
  
  ; 0, 1, 2,3,4,5,6,(7),(8),(9),10,11,12

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  newFolder=fsObj->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  fName=fsObj->getFileNameInfo(fullFileName, filePath=dir, extension=ext)

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    type=size(dataset.data, /TYPE)
    if type eq 1 then fapar=1.*dataSet.data/255. else fapar=dataSet.data
    fapar=1.*fapar  
    slope_fapar=dataSet.slope
    offset_fapar=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[1], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    type=size(dataset.data, /TYPE)
    if type eq 1 then sigma=1.*dataSet.data/255. else sigma=dataSet.data
    sigma=1.*sigma  
    slope_sigma=dataSet.slope
    offset_sigma=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[2], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    type=size(dataset.data, /TYPE)
    if type eq 1 then red=1.*dataSet.data/255. else red=dataSet.data
    red=1.*red
    slope_red=dataSet.slope
    offset_red=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[3], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    type=size(dataset.data, /TYPE)
    if type eq 1 then sigma_red=1.*dataSet.data/255. else sigma_red=dataSet.data
    sigma_red=1.*sigma_red
    slope_sigma_red=dataSet.slope
    offset_sigma_red=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[4], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    type=size(dataset.data, /TYPE)
    if type eq 1 then nir=1.*dataSet.data/255 else nir=dataSet.data
    nir=1.*nir
    slope_nir=dataSet.slope
    offset_nir=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[5], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    type=size(dataset.data, /TYPE)
    if type eq 1 then sigma_nir=1.*dataSet.data/255 else sigma_nir=dataSet.data
    sigma_nir=1.*sigma_nir
    slope_sigma_nir=dataSet.slope
    offset_sigma_nir=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[6], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    flag=dataSet.data
    slope_flag=dataSet.slope
    offset_flag=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[11], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    type=size(dataset.data, /TYPE)
    if type eq 1 then toc_red=1.*dataSet.data/255 else toc_red=dataSet.data
    toc_red=1.*toc_red
    slope_toc_red=dataSet.slope
    offset_toc_red=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[12], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    type=size(dataset.data, /TYPE)
    if type eq 1 then toc_nir=1.*dataSet.data/255 else toc_nir=dataSet.data
    toc_nir=1.*toc_nir
    slope_toc_nir=dataSet.slope
    offset_toc_nir=dataSet.intercept
  endif

  dataSet=operatorObj->readNcdfVar(fullFileName, avBandNames[7], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    qa=dataSet.data
    slope_qa=dataSet.slope
    offset_qa=dataSet.intercept
  endif


  if keyword_set(FOUND) then allData={fapar:fapar, sigma:sigma, $
    red:red, nir:nir,$
    sigma_nir:sigma_nir, sigma_red:sigma_red, $
    qa:qa, flag:flag, $
    toc_nir:toc_nir, toc_red:toc_red, $
    slope_fapar:slope_fapar,slope_sigma:slope_sigma,$
    slope_nir:slope_nir,slope_red:slope_red,$
    slope_qa:slope_qa, slope_flag:slope_flag, $
    slope_toc_nir:slope_toc_nir,slope_toc_red:slope_toc_red,$
    offset_fapar:offset_fapar,offset_sigma:offset_sigma,$
    offset_nir:offset_nir,offset_red:offset_red,$
    offset_toc_nir:offset_toc_nir, offset_toc_red:offset_toc_red,$
    offset_qa:offset_qa, offset_flag:offset_flag, $ 
    day:qa}
  return, allData

end