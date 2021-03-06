;launch_fapar, 2001, 2001, 1, 1, 1, 'NC'
;launch_fapar, 2003, 2003, 1, 1, 1, 'NCHDF', /OVERWRITE, TC_TYPE='DAILY'
;launch_fapar, 1987, 1987, 1, 1, 1, 'NCHDF', /OVERWRITE, TC_TYPE='DAILY'
pro launch_fapar, startYear, endYear, startMonth, endMonth, outputtype, outputformat, $
  TC_TYPE=TC_TYPE, OVERWRITE=OVERWRITE

  TC_TYPE='DAILY'
  confDir='/home/mariomi/config'
  ;rootDir1=''
  ;sourceDir2='/space3/storage/products/AVHRR_LDTR'
  ;sourceDir='/space3/storage/products/results/BRFs'
  ;sourceDir1='/space3/storage/products'
  ;sourceDir2='/space4/storage/products'
  ;sourceDir='/space3/storage/products/results/BRFs'
  ;sourceDir4=''
  ;sourceDir5=''
  TYPE1=1
  if n_elements(sourceformattype) eq 1 then begin
    if sourceformattype eq 2 then TYPE2=1
  endif
  TYPE2=1-keyword_set(TYPE1)
  if keyword_set(TYPE1) then typeFolder='type1' else typeFolder='type2'

  ;outputDir='/space3/storage/products/results/FAPAR/'+typeFolder+path_sep()+TC_TYPE
  outputDir1='/space3/storage/products'
  outputDir2='/space4/storage/products'

  if n_elements(outputformat) eq 0 then outputformat='NC' 
  HDF=strpos(strupcase(outputformat), 'HDF') ge 0 ? 1 : 0
  NC=strpos(strupcase(outputformat), 'NC') ge 0 ? 1 : 0
  if HDF+NC eq 0 then NC=1  
  ;if strupcase(format) eq  then HDF=1 else NC=1
  MISSIONOVERLAPINDEX=0
  tempDir='/home/mariomi/temp/'
  ;level='L2'

  runDailyFapar, confDir, tempDir, startYear, endYear, startMonth, endMonth, $
    TYPE2=TYPE2, TYPE1=TYPE1, NC=NC, HDF=HDF, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, $
    OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE;, SWITCH_TS_TV=SWITCH_TS_TV

end

pro convert_fapar_filename

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  ;confDir='/space3/storage/products/scripts/data'
  ;sourceBaseDir='/space4/storage/products/LAN/AVH/L2/LAEA/BRDF/DAILY/'
  ;  /space3/storage/products/results/FAPAR/type1/DAILY
  sourceBaseDir='/space3/storage/products/results/FAPAR/type1/DAILY'
  ;sourceBaseDir='/space3/storage/products/results/BRFs/DAILY/'
  destBaseDir='/space4/storage/products'
  ;sourceFileExample='AVHRR_NOA16_20031231000000_20031231000000_L2_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO.NC'
  ;destFileExample='AVH09C1_GEOG_0.05DEG_2005_09_11_N18_BRF.nc'

  outputBaseDir=ST_fileSystem->adjustDirSep(destBaseDir, /ADD)
  ;rootDir=''
  fileFromList=file_search(sourceBaseDir, '*.HDF', /FULLY_QUALIFY, count=count)
  for i=0, count-1 do begin
    thisFile=ST_fileSystem->getFileNameInfo(fileFromList[i], filePath=filePath, extension=extension)

    year=strmid(thisFile, 12, 4)
    month=strmid(thisFile,16, 2)
    day=strmid(thisFile,18, 2)
    noaaMission=strmid(thisFile,9, 2)
    print, 'year:', year
    print, 'month:', month
    print, 'day:', day
    print, 'noaaMission:', noaaMission
    noaList=getAVHRRNOAANumber(fix(year), undef)
    instrument='AVH'
    nMiss=fix(noaamission)
    idx=where(nMiss eq noaList, noacount)
    if noacount ne 1 then noaaMission=string(noaList[0], format='(I02)')
    version='N'+noaaMission;
    spatialResolution='0005D'
    indicator='LAN'
    level='L2'

    resFileNCInfo=build_JRC_FPA_AVH_Daily_Product_FileName(instrument, fix(year), fix(month), fix(day), timestamp, temporalResolution, $
      location, spatialResolution, product, version, 'HDF',$
      indicator=indicator, level, projection=projection)
    outputDir=outputBaseDir+resFileNCInfo.filePath
    outputDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)
    resFileNC=outputDir+resFileNCInfo.fileName
    ;print, fileFromList[i], '-to->', resFileNC
    file_move, fileFromList[i], resFileNC, /VERBOSE;, /OVERWRITE
    ;stop

  endfor

end
