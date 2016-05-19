;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
pro test_read

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)

  file4='/space3/storage/products/results/temp/AVHRR_GEOG_0.05DEG_2006_04_03_NOAA-N16_BRF.nc'
  testVar='BRF_BAND_1'
  ;AVHRR-Land_v004_AVH09C1_NOAA-16_20030101_c20140421105754.nc
  ;fName1=fsObj->getFileNameInfo(file1, filePath=dir1, extension=ext1)
  ;AVH09C1.A2003001.N16.004.2014111105754.hdf
  ;fName2=fsObj->getFileNameInfo(file2, filePath=dir2, extension=ext2)
  ;GLOBAL_L3_GEOG_0.05DEG_001-001_03.NOAA-16_BRF.hdf // GLOBAL_P17_GEOG_0.05DEG_203-203_99.NOAA-14
  ;fName3=fsObj->getFileNameInfo(file3, filePath=dir3, extension=ext3)
  ;fName4=fsObj->getFileNameInfo(file4, filePath=dir4, extension=ext4)
  fName4=fsObj->getFileNameInfo(file4, filePath=dir4, extension=ext4)

  ;infoVar1a=['SREFL_CH1', file1, fName1, dir1, ext1, 'input_data[0,*]/red_avhrr']
  ;infoVar1b=[testVar, testFile, fName2, dir2, ext2, 'input_data[0,*]/red_avhrr']
  ;infoVar1c=['RHO1', file3, fName3, dir3, ext3, 'input_data[0,*]/red_avhrr']
  infoVar1d=[testVar, file4, fName4, dir4, ext4, 'input_data[0,*]/red_avhrr']

  ;dataSet=operatorObj->readNcdfVar(infoVar1a[1], infoVar1a[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)

  dataSet=operatorObj->readNcdfVar(infoVar1d[1], infoVar1d[0], FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  if keyword_set(FOUND) then begin
    data=dataSet.data
    slope=dataSet.slope
    intercept=dataSet.intercept
  endif
  input_data=fltarr(2, globDim)
  input_data[0, *]=reform(temporary(dataset.data), globDim)
  ;operatorObj->readHdfFullInfoData, infoVar1b[1], infoVar1b[0], red_avhrr
  ;globInfo=getGlobalInfo(dir3, file3, infoVar1b[0], opObj=operatorObj, fsObj=fsObj, testFile=testFile)
  ;slope_red=globInfo.slope
  ;offset_red=globInfo.offset
  ;final_red_avhrr=red_avhrr*slope_red+offset_red
  dataSet=0

end