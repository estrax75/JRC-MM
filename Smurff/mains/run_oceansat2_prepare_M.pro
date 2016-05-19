;@/library/system/unzipAllFiles
PRO run_oceansat2_prepare_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  fs=mainApp->getfileSystem()
  yearList=request->getYearList()
  monthList=request->getMonthList()
  
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByPriority(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getroiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
  inputDir=request->getInputDir()
  inputFileFilter=request->getInputFileFilter()
  outputDir=request->getOutputDir()
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  ;unzip all
  oceanSat2=mainApp->getPhysicalFromCode('OCEAN_SAT2')
  oceanSat2ArchiveRoot=oceanSat2->getArchiveRoot()
  inputParameterList=request->getInputParameterList(NOTFOUND=NOTFOUND)
  bandToExportList=request->getOutputParameterList(NOTFOUND=NOTFOUND)
  overWriteFlag=request->getOverwriteResultFlag()
  deleteInputFlag=request->getDeleteInputFlag()
  periodType=request->getPeriodType()
  destRoiCode=request->getOutputRoi()
  
  if keyword_set(NOTFOUND) then delIDLVar, bandToExportList
  
  zipFileNames=FILE_SEARCH(oceanSat2ArchiveRoot+path_sep()+'*.zip', count=NFiles)
  list=file_search(inputDir+path_sep()+inputFileFilter, count=nFiles2)
  emptyFolder=nFiles2 eq 0
  
  if NFiles gt 0 or ~emptyFolder then begin
    ;if NFiles gt 0 then res = unzipAllFiles(zipFileNames, inputDir)
    NULL=NFiles eq 0
    
    if NFiles gt 0 then begin
      res = unzipAllFiles(zipFileNames, tempDir, fileList=fileList, NULL=NULL, NODISPLAY=NODISPLAY)
      if ~keyword_set(NULL) then uncompressedList=fileList
    endif
    if ~keyword_set(NULL) then begin
      fs->moveFiles, uncompressedList, inputDir, NOOVERWRITE=1-keyword_set(overwriteFlag)
      file_delete, uncompressedList, /ALLOW_NONEXISTENT , /QUIET
    endif
    
    nLoops=n_elements(yearList)*n_elements(monthList)
    doLog, /STACK, callingRoutine=callingRoutine
    title='processing (computation): '+callingRoutine
    progCount=1
    if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
    
    for i=0, n_elements(yearList)-1 do begin
      for j=0, n_elements(monthList)-1 do begin
        ;thisList=mainApp->getRunnableFileList(yearList[i], monthList[j], NO_SELECTION=NO_SELECTION)
        ;doLog,thislist
        doLog,'year: ', yearList[i], 'month: ', monthList[j], level=0
        ;if ~keyword_set(NO_SELECTION) then begin
        ;  doLog,thisList ; cancel ALL uncompressed files
        res = do_single_oceansat2_prepare_M(periodType, monthList[j], yearList[i], $
          inputDir, inputFileFilter, outputDir, destRoiCode, $
          bandToExportList=bandToExportList, overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag)
        ;endif else doLog,'No files'
        doLog,'**************', level=0
        if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
        progCount++
        heap_gc
      endfor
    endfor
    if NFiles gt 0 then fs->movefiles, zipFileNames, oceanSat2ArchiveRoot+path_sep()+'processed'
  endif
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
END