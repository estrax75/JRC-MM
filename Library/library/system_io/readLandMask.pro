function readLandMask, folder, fileName, bandName, water, FOUND=FOUND, FULL=FULL


  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  if n_elements(folder) eq 0 then folder='/space3/storage/products/LAN/AVH/L1/PLC/GLOBAL'
  if n_elements(filename) eq 0 then fileName='MCD12C1.A2012001.051.2013178154403.hdf'
  if n_elements(bandName) eq 0 then bandName='Majority_Land_Cover_Type_1'
  if n_elements(water) eq 0 then water=0

  newFolder=ST_fileSystem->adjustDirSep(folder, /ADD)
  fullFileName=newFolder+fileName

  ST_operator->readHdfFullInfoData, fullfilename, bandName, landtype, slope, intercept, fillvalue, ERROR=ERROR, /REVERSE
  if ~keyword_set(ERROR) then begin
    FOUND=1
    data=landtype
    if n_elements(slope) eq 0 then slope=1
    if n_elements(intercept) eq 0 then intercept=0
    waterIdx=where(landtype eq water, countW, COMPLEMENT=landIdx, NCOMPLEMENT=countL)
    if countW ne 0 then landtype[waterIdx]=0
    if countL ne 0 then landtype[landIdx]=1
    return, landtype
  endif

  FOUND=0
  return, -1

end