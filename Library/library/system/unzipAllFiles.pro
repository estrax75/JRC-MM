FUNCTION unzipAllFiles, zipfilenames, outputDir, fileList=fileList, NULL=NULL, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  unzipper=mainApp->getKeyValue('ZIP_APPLICATION_MULTIPLE')
  
  nElem=n_elements(zipfilenames)
  res=intarr(nElem)
  resultList=''
  prevList=file_search(outputDir, '*.*')
  
  nLoops=nElem
  doLog, /STACK, callingRoutine=callingRoutine
  title='execute (unzip): '+callingRoutine
  progCount=1
  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  
  for i=0, nElem-1 do begin
    res[i] = unzipall(unzipper, zipfilenames[i], outputDir);
    if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
    progCount++
  endfor
  afterList=file_search(outputDir, '*.*')
  util=mainApp->getUtility()
  fileList=util->getListDifference(afterList, prevList, NULL=NULL)
  if ~keyword_set(NODISPLAY) then closeProgressBar
  return, res
  
END
