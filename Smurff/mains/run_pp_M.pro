PRO run_pp_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  yearList=request->getYearList()
  monthList=request->getMonthList()
  
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  roiArchiveList=roiCodeList
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByPriority(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getroiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
  inputDir=request->getInputDir()
  outputDir=request->getoutputDir()
  inputFileFilter=request->getInputFileFilter()
  inputParameterList=request->getInputParameterList(NOTFOUND=NOTFOUND)
  bandToExportList=request->getOutputParameterList(NOTFOUND=NOTFOUND)
  overWriteFlag=request->getOverwriteResultFlag()
  deleteInputFlag=request->getDeleteInputFlag()
  periodType=request->getPeriodType()
  destRoiCode=request->getOutputRoi()
  
  if keyword_set(NOTFOUND) then delIDLVar, bandToExportList
  
  nLoops=n_elements(yearList)*n_elements(monthList)
  doLog, /STACK, callingRoutine=callingRoutine
  title='processing: '+callingRoutine
  progCount=1
  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  
  for i=0, n_elements(yearList)-1 do begin
    for j=0, n_elements(monthList)-1 do begin
      ;thisList=mainApp->getRunnableFileList(yearList[i], monthList[j], NO_SELECTION=NO_SELECTION)
      ;doLog,thislist
      doLog,'year: ', yearList[i], 'month: ', monthList[j], level=0
      ;if ~keyword_set(NO_SELECTION) then begin
      ;  doLog,thisList
      ;res = create_euro_file(monthList[j], yearList[i], inputDir, inputFileFilter, outputDir)
      res = do_single_pp_M(periodType, monthList[j], yearList[i], roiCodeList, roiArchiveList, $
        inputDir, inputFileFilter, outputDir, destRoiCode, $
        bandToExportList=bandToExportList, overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag)
      ;endif else doLog,'No files'
      doLog,'**************', level=0
      if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
      progCount++
      heap_gc
    endfor
  endfor
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
END