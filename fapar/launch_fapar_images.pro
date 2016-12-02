pro launch_fapar_images, startYear, endYear, startMonth, endMonth, type, format

  confDir='/home/mariomi/config'
  ;rootDir1=''
  ;sourceDir2='/space3/storage/products/AVHRR_LDTR'
  sourceDir1='/space3/storage/products'
  sourceDir2='/space4/storage/products'
  ;sourceDir4=''
  ;sourceDir5=''
  if strupcase(format) eq 'HDF' then HDF=1 else NC=1
  tempDir='/home/mariomi/temp/'
  level='L2'

  runDailyFapar, confDir, sourceDir3, tempDir, outputDir, startYear, endYear, startMonth, endMonth, level, NC=NC, HDF=HDF

end