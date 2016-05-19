FUNCTION unzipSingleFile, zipfilenames, outputDir

  COMMON smurffCB, mainApp
  
  unzipper=mainApp->getKeyValue('ZIP_APPLICATION')
  
  nElem=n_elements(zipfilenames)
  unzipfilenames=strarr(nElem)
  for i=0, nElem-1 do unzipfilenames[i] = unzipit(unzipper, zipfilenames[i], outputDir); 
  return, outputDir+path_sep()+unzipfilenames
  
END
