PRO run_e_regional_D_to_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  common refDate, myMonth, myYear, myDay
  
  yearList=request->getYearList()
  monthList=request->getMonthList()
  
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
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
  
  nLoops=n_elements(yearList)*(n_elements(monthList)*n_elements(roiArchiveList))
  doLog, /STACK, callingRoutine=callingRoutine
  title='processing: '+callingRoutine
  progCount=1
  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  
  for i=0, n_elements(yearList)-1 do begin
    for j=0, n_elements(monthList)-1 do begin
      for k=0, n_elements(roiArchiveList)-1 do begin
        ;thisList=mainApp->getRunnableFileList(yearList[i], monthList[j], NO_SELECTION=NO_SELECTION)
        ;doLog,thislist
        ;progressBar->updateCount, pbCount
        refRoiCode=mainApp->getRoiRefRoiCodesByCodes(roiCodeList[k])
        if refRoiCode eq '' or refRoiCode eq 'N/A' then delIdlVar, refRoiCode;refRoi=mainApp->getROIInfoByCode(refRoiCode)
        myMonth=monthList[j]
        myYear=yearList[i]
        doLog,'year: ', yearList[i], 'month: ', monthList[j], 'roi: ', roiArchiveList[k], level=0
        ;if ~keyword_set(NO_SELECTION) then begin
        ;  doLog,thisList
        ;res = create_euro_file(monthList[j], yearList[i], inputDir, inputFileFilter, outputDir)
        res = do_single_e_regional_D_to_M(periodType, monthList[j], yearList[i], roiCodeList[k], roiArchiveList[k], $
        inputDir, inputFileFilter, outputDir, destRoiCode, $
          bandToExportList=bandToExportList, NOTFOUND=NOTFOUND, overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, roiRef=refRoiCode)
        if keyword_set(NOTFOUND) then doLog,'skip file month: ', monthList[j], ' year: ', yearList[i], ' roi: ', roiList[k], level=2
        ;endif else doLog,'No files'
        
        doLog,'**************', level=0
        if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
        progCount++
      endfor
    ;heap_gc
    endfor
  endfor
  if ~keyword_set(NODISPLAY) then closeProgressBar
;obj_destroy, progressBar
  
END