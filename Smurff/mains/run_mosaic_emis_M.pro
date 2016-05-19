PRO run_mosaic_emis_M, request, mosaicRoiMapCode, NODISPLAY=NODISPLAY, $
    buildRoiFileNameFunction=buildRoiFileNameFunction, $
    readVarDataFunction=readVarDataFunction, $
    buildMosaicFileName=buildMosaicFileName, $
    refOperatorName=refOperatorName
    
  COMMON smurffCB, mainApp
  
  
  yearList=request->getYearList()
  monthList=request->getMonthList()
  
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByPriority(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getroiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  
  inputDir=request->getInputDir()
  outputDir=request->getOutputDir()
  inputFileFilter=request->getInputFileFilter()
  inputParameterList=request->getInputParameterList(NOTFOUND=NOTFOUND)
  outputParameterList=request->getOutputParameterList(NOTFOUND=NOTFOUND)
  overWriteFlag=request->getOverwriteResultFlag()
  deleteInputFlag=request->getDeleteInputFlag()
  periodType=request->getPeriodType()
  destRoiCode=request->getOutputRoi()
  
  if keyword_set(NOTFOUND) then delIDLVar, bandToExportList
  
  outputParN=n_elements(outputParameterList)
  bandToExportList=strarr(outputParN)
  outConversionFunctions=strarr(outputParN)
  outScallingTypeList=strarr(outputParN)
  
  for i=0, outputParN-1 do begin
    pars=strsplit(outputParameterList[i], '$', /EXTRACT)
    bandToExportList[i]=pars[0]
    outConversionFunctions[i]=pars[1]
    if n_elements(pars) eq 3 then outScallingTypeList[i]=pars[2] else outScallingTypeList[i]='linear'
  endfor
  
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
      res = do_single_mosaic_emis_M(periodType, monthList[j], yearList[i], roiCodeList, roiArchiveList, $
        inputDir, inputFileFilter, outputDir, destRoiCode, $
        bandToExportList=bandToExportList, NOTFOUND=NOTFOUND, overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag, $
        buildRoiFileNameFunction=buildRoiFileNameFunction, readVarDataFunction=readVarDataFunction, $
        buildMosaicFileName=buildMosaicFileName, refOperatorName=refOperatorName, mosaicRoiMapCode=mosaicRoiMapCode, $
        outVarNames=outVarNames, outConversionFunctions=outConversionFunctions, outScallingTypeList=outScallingTypeList)
      if keyword_set(NOTFOUND) then doLog,'skip file month: ', monthList[j], ' year: ', yearList[i], level=2
      ;endif else doLog,'No files'
      doLog,'**************', level=0
      if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
      progCount++
    endfor
    heap_gc
  endfor
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
END