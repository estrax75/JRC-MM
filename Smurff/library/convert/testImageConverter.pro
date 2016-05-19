pro testImageConverter

  fileFilter='E:\data\mariomi\application\oxyrisk\data\convert\input\*.hdf'
  filelist = file_search(fileFilter, count=count);
  mapdir = 'E:\data\mariomi\application\oxyrisk\input\masks'
  
  ;imgConv=obj_new('ImageConverter', colorMapFileName, roiFileName, colorMapInfo=colorMapInfo, roiInfo=roiInfo, mapDir=mapDir)
  imgConv=obj_new('ImageConverter', 'E:\data\mariomi\application\oxyrisk\resource\colorMap.xml', 'E:\data\mariomi\application\oxyrisk\configuration\roi.xml', mapDir=mapDir)
  imgConv->processFiles, filelist, 'chl_oc3','chl_a', 'mg / m-3'
  
  
END
