;launch_brf, 1988, 1988, 11, 12, format, MISSIONOVERLAPINDEX=1, OVERWRITE=1
;launch_brf, 1988, 1988, 11, 12, format, MISSIONOVERLAPINDEX=0, OVERWRITE=0
;launch_brf, 1985, 1985, 4, 4, 'NC', MISSIONOVERLAPINDEX=0, OVERWRITE=0
pro launch_brf, startYear, endYear, startMonth, endMonth, format, missionIndex, $
  MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, OVERWRITE=OVERWRITE, singleday=singleday, OLDSTYLE=OLDSTYLE

 confDir='/home/mariomi/config/data'
 ;rootDir=''
 ;rootDir='/space3/storage/products/AVHRR_LDTR'
 ;rootDir='/space3/storage/products/results/AVHRR/DAILY'
 rootDir=strarr(2)

 ;rootDir[0]='/space4/storage/products/LAN/AVH/L2/PLC/AVHRR_LDTR'
 ;rootDir[1]='/space4/storage/products/LAN/AVH/L2/PLC/AVHRR_Land'
 ;rootDir[0]='/space3/storage/products/LAN/AVH/L2/PLC'
 ;rootDir[1]='/space3/storage/products/LAN/AVH/L2/PLC'
 ;rootDir[0]='/space4/storage/products/LAN/AVH/L2/PLC'
 ;rootDir[1]='/space4/storage/products/LAN/AVH/L2/PLC'

 ;checkDestDir='/space4/storage/products'
 ;writeDestDir='/space3/storage/products'

 ;rootDir[0]='/space4/storage/products/LAN/AVH/L2/PLC'
 ;rootDir[1]='/space4/storage/products/LAN/AVH/L2/PLC'
 
 tempDir=''
 ;outputDir='/space3/storage/products/results/BRFs'
 outputBaseDir1='/space3/storage/products/'
 outputBaseDir2='/space4/storage/products/'
 
 tempDir='/home/mariomi/temp'
 
 if n_elements(format) eq 0 then format='NC'
 HDF=strpos(strupcase(format), 'HDF') ge 0 ? 1 : 0
 NC=strpos(strupcase(format), 'NC') ge 0 ? 1 : 0
 if HDF+NC eq 0 then NC=1
 if n_elements(MISSIONOVERLAPINDEX) eq 0 then MISSIONOVERLAPINDEX=0

 runDailyBrf, confDir, rootDir, tempDir, outputBaseDir2, $
  startYear, endYear, startMonth, endMonth, missionIndex, $
  HDF=HDF, NC=NC, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, OVERWRITE=OVERWRITE, OLDSTYLE=OLDSTYLE, singleday=singleday

end

;pro convert_brf_filename
;
;  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem
;
;  declareSingleTons
;
;  ;confDir='/space3/storage/products/scripts/data'
;  ;sourceBaseDir='/space4/storage/products/LAN/AVH/L2/PLC/BRDF/DAILY/'
;;  /space3/storage/products/results/FAPAR/type1/DAILY
;  sourceBaseDir1='/space3/storage/products'
;  sourceBaseDir2='/space4/storage/products'
;  
;  checkDestDir='/space4/storage/products'
;  writeDestDir='/space4/storage/products'
;  
;  ;destBaseDir1='/space3/storage/products'
;  ;destBaseDir2='/space4/storage/products'
;  ;destFileExample='AVH_19870111001D_900S900N1800W1800E_BRDF_001.NC'; add missionnumber/missiontype
;
;  outputBaseDir=ST_fileSystem->adjustDirSep(destBaseDir, /ADD)
;  ;rootDir=''
;  fileFromList=file_search(sourceBaseDir, '*.nc', /FULLY_QUALIFY, count=count)
;  for i=0, count-1 do begin
;    thisFile=ST_fileSystem->getFileNameInfo(fileFromList[i], filePath=filePath, extension=extension)
;
;    year=strmid(thisFile, 21, 4)
;    month=strmid(thisFile,26, 2)
;    day=strmid(thisFile,29, 2)
;    noaaMission=strmid(thisFile,32, 3)
;    print, 'year:', year  
;    print, 'month:', month
;    print, 'day:', day
;    print, 'noaaMission:', noaaMission
;    instrument='AVH'
;    version=noaaMission;
;    spatialResolution='0005D'
;    indicator='LAN'
;    level='L2'
;
;    resFileNCInfo=build_JRC_BRDF_AVH_Daily_Product_FileName(instrument, fix(year), fix(month), fix(day), timestamp, temporalResolution, $
;      location, spatialResolution, product, version, 'NC',$
;      indicator=indicator, level, projection=projection)
;    outputDir=outputBaseDir+resFileNCInfo.filePath
;    outputDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)
;    resFileNC=outputDir+resFileNCInfo.fileName
;    ;print, fileFromList[i], '-to->', resFileNC
;    file_move, fileFromList[i], resFileNC, /VERBOSE, /OVERWRITE
;    ;stop
;
;  endfor
;
;end
