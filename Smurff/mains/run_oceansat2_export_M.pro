PRO run_oceansat2_export_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  ;read for each month read the chlorophille data for specific roi(s)
  ;do the mean calculation on each roi
  ;write in a csv file
  ;oxyFS=mainApp->getOxyFileSystem()
  yearList=request->getYearList()
  monthList=request->getMonthList()
  
  roiCodeList=request->getRoiList(NOTFOUND=NOTFOUND)
  if ~keyword_set(NOTFOUND) then begin
    roiCodeList=mainApp->orderRoisByPriority(roiCodeList, sortArray=sortArray)
    roiArchiveList=request->getroiArchiveList()
    roiArchiveList=roiArchiveList[sortArray]
  endif
  yearList=mainApp->orderList(yearList, /NUMBER)
  inputDir=request->getInputDir()
  inputFileFilter=request->getInputFileFilter()
  outputDir=request->getOutputDir()
  inputParameterList=request->getInputParameterList(NOTFOUND=NOTFOUND)
  outputParameterList=request->getOutputParameterList(NOTFOUND=NOTFOUND)
  overWriteFlag=request->getOverwriteResultFlag()
  deleteInputFlag=request->getDeleteInputFlag()
  periodType=request->getPeriodType()
  destRoiCode=request->getOutputRoi()

  outputParN=n_elements(outputParameterList)
  parameterList=strarr(outputParN)
  statList=strarr(outputParN)
  
  for i=0, outputParN-1 do begin
    pars=strsplit(outputParameterList[i], '$', /EXTRACT)
    parameterList[i]=pars[0]
    statList[i]=pars[1]
  endfor

  if keyword_set(NOTFOUND) then delIDLVar, bandToExportList
  
  ;unzip all
  struct=getStatInfoStruct()
  structs=replicate(struct, n_elements(yearList)*n_elements(monthList)*n_elements(roiCodeList)*n_elements(parameterList))
  ;fileName=outputDir+'ocean_sat2_average_val.csv'
  fileName=mainApp->getKeyValue('CSV_OCSAT2_OUTPUT_FILE')
  exists=(file_info(fileName)).exists
  newFileName=outputDir+path_sep()+filename
  
  nLoops=n_elements(yearList)*n_elements(monthList)
  doLog, /STACK, callingRoutine=callingRoutine
  title='processing: '+callingRoutine
  progCount=1
  if ~keyword_set(NODISPLAY) then initProgressBar, nLoops, title=title, numberUpdates=numberUpdates
  
  if keyword_set(overWriteFlag) or exists eq 0 then begin
    openw, unit, newFileName, /GET_LUN
    printf, unit, 'year'+';'+'month'+';'+'roiName'+';'+'roiBoundary'+';'+'parameter name'+';'+'statName'+';'+'statValue'+';'+'# of valid pixels (not zero)'+';'+'# of pixels'+';'+'% of valid pixels'
    k=0
    for i=0, n_elements(yearList)-1 do begin
      for j=0, n_elements(monthList)-1 do begin
        ;thisList=mainApp->getRunnableFileList(yearList[i], monthList[j], NO_SELECTION=NO_SELECTION)
        ;doLog,thislist
        doLog,'year: ', yearList[i], 'month: ', monthList[j], level=0
        ;if ~keyword_set(NO_SELECTION) then begin
        ;  doLog,thisList
        ;res = do_single_oceansat2_mean_export_M(periodType, monthList[j], yearList[i], roiCodeList, roiArchiveList, inputDir, inputFileFilter, outputDir, $
        ;  bandToExportList=bandToExportList, overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag)
        res = do_single_oceansat2_export_M(periodType, monthList[j], yearList[i], roiCodeList, roiArchiveList, inputDir, inputFileFilter, outputDir, $
          parameterList, statList, destRoiCode, bandToExportList=bandToExportList, overwriteFlag=overwriteFlag, deleteInputFlag=deleteInputFlag)
        valid=size(res, /TYPE)
        if valid eq 2 then continue
        for l=0, n_elements(res)-1 do begin
          structs[k].month=monthList[j]
          structs[k].year=string(format='(I4)', yearList[i])
          structs[k].roiName=res[l].roiName
          structs[k].roiBoundary=res[l].roiBoundary
          structs[k].parName=res[l].parName
          structs[k].statName=res[l].statName
          structs[k].statValue=res[l].statValue
          structs[k].validValues=res[l].validValues
          structs[k].totValues=res[l].totValues
          structs[k].percValues=res[l].percValues
          printf, unit, structs[k].year+';'+structs[k].month+';'+structs[k].roiName+';'+structs[k].roiBoundary+';'+structs[k].parName+';'+structs[k].statName+';'+structs[k].statValue+';'+structs[k].validValues+';'+structs[k].totValues+';'+structs[k].percValues
          k++
        endfor
        ;endif else doLog,'No files'
        doLog,'**************', level=0
        if ~keyword_set(NODISPLAY) then updateProgressBar, progCount
        progCount++
        heap_gc
      endfor
    endfor
    close, unit
    free_lun, unit
  endif
  if ~keyword_set(NODISPLAY) then closeProgressBar
  
END