;launch_fapar_ta_composite, 2003, 2003, 3, 3, 1, 'M'
pro launch_fapar_tc, startYear, endYear, startMonth, endMonth, sourceformattype, aggregationType, computationType

  confDir='/space3/storage/products/scripts/data'
  ;rootDir1=''
  ;sourceDir2='/space3/storage/products/AVHRR_LDTR'
  ;sourceDir3='/space3/storage/products/results/BRFs'
  ;sourceDir4=''
  ;sourceDir5=''
  TYPE1=1
  if n_elements(sourceformattype) eq 1 then begin
    if sourceformattype eq 2 then TYPE2=1
  endif
  TYPE2=1-keyword_set(TYPE1)

  if keyword_set(TYPE1) then begin
    outputDir='/space3/storage/products/results/FAPAR/TC/type1/'
    sourceDir='/space3/storage/products/results/FAPAR/type1/'
  endif
  if keyword_set(TYPE2) then begin
    outputDir='/space3/storage/products/results/FAPAR/TC/type2/'
    sourceDir='/space3/storage/products/results/FAPAR/type2/'
  endif

  aggType='M'
  if n_elements(aggregationType) eq 1 then begin
    if aggregationType eq '10' then aggType='10D' 
    if aggregationType eq '16' then aggType='16D'
  endif
  
  ELAB_TC=1
  if n_elements(computationType) eq 1 then begin
    if computationType eq 'M' then ELAB_TC=0
  endif
  tempDir='/space3/storage/products/results/temp/'

  ;runDailyFapar, confDir, sourceDir3, tempDir, outputDir, startYear, endYear, startMonth, endMonth, TYPE2=TYPE2, TYPE1=TYPE1
  runTCFapar, confDir, sourceDir, tempDir, outputDir, startYear, endYear, startMonth, endMonth, $
    MONTHLY=aggType eq 'M', TENDAYS=aggType eq '10D', SIXTEENDAYS=aggType eq '16D', $
    ELAB_TC=ELAB_TC, ELAB_MEAN=1-ELAB_TC

end