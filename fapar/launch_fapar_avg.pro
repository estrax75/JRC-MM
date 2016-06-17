pro launch_fapar_avg, startYear, endYear, startMonth, endMonth, type, format, SWITCH_TS_TV=SWITCH_TS_TV

  confDir='/space3/storage/products/scripts/data'
  ;rootDir1=''
  ;sourceDir2='/space3/storage/products/AVHRR_LDTR'
  sourceDir3='/space3/storage/products/results/BRFs'
  ;sourceDir4=''
  ;sourceDir5=''
  TYPE1=1
  if n_elements(type) eq 1 then begin
    if type eq 1 then TYPE1=1 else TYPE1=0 
  endif
  TYPE2=1-keyword_set(TYPE1)

  if strupcase(format) eq 'HDF' then HDF=1 else NC=1
  if keyword_set(TYPE1) then outputDir='/space3/storage/products/results/FAPAR/type1/'
  if keyword_set(TYPE2) then outputDir='/space3/storage/products/results/FAPAR/type2/'
  tempDir='/space3/storage/products/results/temp/'

  runDailyFapar, confDir, sourceDir3, tempDir, outputDir, startYear, endYear, startMonth, endMonth, TYPE2=TYPE2, TYPE1=TYPE1, NC=NC, HDF=HDF, SWITCH_TS_TV=SWITCH_TS_TV

end