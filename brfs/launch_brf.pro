pro launch_brf, startYear, endYear, startMonth, endMonth, format, SWITCH_TS_TV=SWITCH_TS_TV

 confDir='/space3/storage/products/scripts/data'
 rootDir1=''
 rootDir2='/space3/storage/products/AVHRR_LDTR'
 rootDir3=''
 rootDir4=''
 tempDir=''
 outputDir='/space3/storage/products/results/BRFs'
 tempDir='/space3/storage/products/results/temp'
 MISSIONOVERLAPINDEX=0
 
 if n_elements(format) eq 0 then format='NC'
 HDF=strpos(strupcase(format), 'HDF') ge 0 ? 1 : 0
 NC=strpos(strupcase(format), 'NC') ge 0 ? 1 : 0
 if HDF+NC eq 0 then NC=1

 runDailyBrdf, confDir, rootDir2, tempDir, outputDir, startYear, endYear, startMonth, endMonth, $
  HDF=HDF, NC=NC, SWITCH_TS_TV=SWITCH_TS_TV, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, /OVERWRITE

end
