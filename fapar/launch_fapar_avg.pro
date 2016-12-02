pro launch_fapar_avg, startYear, endYear, startMonth, endMonth, type, format, TC_TYPE=TC_TYPE

  confDir='/home/mariomi/config'
  ;rootDir1=''
  sourceDir1='/space3/storage/products'
  sourceDir2='/space4/storage/products'

  if strupcase(format) eq 'HDF' then HDF=1 else NC=1
  tempDir='/home/mariomi/temp/'
  level='L1'

  runDailyFapar, confDir, sourceDir3, tempDir, outputDir, startYear, endYear, startMonth, endMonth, level, $
    NC=NC, HDF=HDF, TC_TYPE=TC_TYPE

end