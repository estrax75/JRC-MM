pro launch_fapar

 confDir='/space3/storage/products/scripts/data'
 ;rootDir1=''
 ;sourceDir2='/space3/storage/products/AVHRR_LDTR'
 sourceDir3='/space3/storage/products/results/BRFs'
 ;sourceDir4=''
 ;sourceDir5=''
 TYPE1=1
 TYPE2=1-keyword_set(TYPE1)
 
 if keyword_set(TYPE1) then outputDir='/space3/storage/products/results/FAPAR/type1/'
 if keyword_set(TYPE2) then outputDir='/space3/storage/products/results/FAPAR/type2/'
 tempDir='/space3/storage/products/results/temp/'
 
 runDailyFapar, confDir, sourceDir3, tempDir, outputDir, TYPE2=TYPE2, TYPE1=TYPE1

end