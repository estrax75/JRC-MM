;launch_fapar_daily, 2001, 2001, 1, 1, 1, 'NC'
;launch_fapar_daily, 2003, 2003, 1, 1, 1, 'NCHDF', /OVERWRITE, TC_TYPE='DAILY'
;launch_fapar_daily, 1991, 1991, 9, 9, 1, 'NCHDF', OVERWRITE=0, TC_TYPE='DAILY'
;launch_fapar_daily, 1991, 1991, 10, 10, 1, 'NCHDF', OVERWRITE=0, TC_TYPE='DAILY'
;launch_fapar_daily, 1991, 1991, 11, 11, 1, 'NCHDF', OVERWRITE=0, TC_TYPE='DAILY'
;launch_fapar_daily, 1991, 1991, 12, 12, 1, 'NCHDF', OVERWRITE=0, TC_TYPE='DAILY'
;launch_fapar_daily, 1982, 1982, 11, 11, 1, 'NCHDF', OVERWRITE=0, TC_TYPE='DAILY'
;launch_fapar_daily, 1999, 1999, 6, 6, 3, 'NCHDF', OVERWRITE=0, TC_TYPE='DAILY'
;launch_fapar_daily, 1996, 1996, 8, 8, 3, 'NCHDF', OVERWRITE=0, TC_TYPE='DAILY'
;launch_fapar_daily, 1999, 1999, 6, 6, 3, 'NCHDF', OVERWRITE=1, TC_TYPE='DAILY', datadir='C:\data\input\BRDF\1999\06'
;launch_fapar_daily, 1999, 1999, 6, 6, 3, 'CSV', OVERWRITE=1, TC_TYPE='DAILY', datadir='C:\data\AVHRR\spp\tmp\AVHRR_JANINA_shrubland.20161216_1.avh\tmp', /PIXEL
pro launch_fapar_daily_SPP, startYear, endYear, startMonth, endMonth, outputtype, outputformat, $
  TC_TYPE=TC_TYPE, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, OVERWRITE=OVERWRITE, datadir=datadir, $
  PIXELS_PROCESS=PIXELS_PROCESS, PP_CSV=PP_CSV, PP_NC=PP_NC

  testSitesFolders=file_search('C:\data\AVHRR\spp\tmp\*.*', /TEST_DIRECTORY, /MARK_DIR)
  for i=0, n_elements(testSitesFolders)-1 do launch_fapar_daily, startYear, endYear, startMonth, endMonth, outputtype, outputformat, $
    TC_TYPE=TC_TYPE, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, OVERWRITE=OVERWRITE, datadir=testSitesFolders[i]+'tmp', $
    PIXELS_PROCESS=PIXELS_PROCESS, PP_CSV=PP_CSV, PP_NC=PP_NC
  

end

;pro convert_fapar_filename
;
;  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem
;
;  declareSingleTons
;
;  ;confDir='/space3/storage/products/scripts/data'
;  ;sourceBaseDir='/space4/storage/products/LAN/AVH/L2/PLC/BRDF/DAILY/'
;  ;  /space3/storage/products/results/FAPAR/type1/DAILY
;  sourceBaseDir='/space3/storage/products/results/FAPAR/type1/DAILY'
;  ;sourceBaseDir='/space3/storage/products/results/BRFs/DAILY/'
;  destBaseDir='/space4/storage/products'
;  ;sourceFileExample='AVHRR_NOA16_20031231000000_20031231000000_L2_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO.NC'
;  ;destFileExample='AVH09C1_GEOG_0.05DEG_2005_09_11_N18_BRF.nc'
;
;  outputBaseDir=ST_fileSystem->adjustDirSep(destBaseDir, /ADD)
;  ;rootDir=''
;  fileFromList=file_search(sourceBaseDir, '*.HDF', /FULLY_QUALIFY, count=count)
;  for i=0, count-1 do begin
;    thisFile=ST_fileSystem->getFileNameInfo(fileFromList[i], filePath=filePath, extension=extension)
;
;    year=strmid(thisFile, 12, 4)
;    month=strmid(thisFile,16, 2)
;    day=strmid(thisFile,18, 2)
;    noaaMission=strmid(thisFile,9, 2)
;    print, 'year:', year
;    print, 'month:', month
;    print, 'day:', day
;    print, 'noaaMission:', noaaMission
;    noaList=getAVHRRNOAANumber(fix(year), undef)
;    instrument='AVH'
;    nMiss=fix(noaamission)
;    idx=where(nMiss eq noaList, noacount)
;    if noacount ne 1 then noaaMission=string(noaList[0], format='(I02)')
;    version='N'+noaaMission;
;    spatialResolution='0005D'
;    indicator='LAN'
;    level='L2'
;
;    resFileNCInfo=build_JRC_FPA_AVH_Daily_Product_FileName(instrument, fix(year), fix(month), fix(day), timestamp, temporalResolution, $
;      location, spatialResolution, product, version, 'HDF',$
;      indicator=indicator, level, projection=projection)
;    outputDir=outputBaseDir+resFileNCInfo.filePath
;    outputDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)
;    resFileNC=outputDir+resFileNCInfo.fileName
;    ;print, fileFromList[i], '-to->', resFileNC
;    file_move, fileFromList[i], resFileNC, /VERBOSE;, /OVERWRITE
;    ;stop
;
;  endfor
;
;end
