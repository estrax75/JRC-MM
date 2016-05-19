PRO oxyrisk_outputfile_selection, ev

 doLog,'oxyrisk_outputfile_selection', level=0

END

PRO oxyrisk_overwriteResultFlagButton, ev

  widget_control, ev.id, get_uvalue=type
  selection=ev.select
  widgetId=ev.id
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;doLog,'oxyrisk_overwriteResultFlagButton'
  view->userOverwriteResultFlag, selection;, widgetId

END

PRO oxyrisk_deleteInputFlagButton, ev

  widget_control, ev.id, get_uvalue=type
  selection=ev.select
  widgetId=ev.id
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;doLog,'oxyrisk_deleteInputFlagButton'
  view->userDeleteInputFlag, selection;, widgetId

END
;PRO oxyrisk_removeSourceFilesFlagButton, ev
;
;  widget_control, ev.id, get_uvalue=type
;  selection=ev.select
;  widgetId=ev.id
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_deleteFilesFlagButton'
;  view->userRemoveSourceFilesFlag, selection;, widgetId
;
;END

PRO oxyrisk_DoRun, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;doLog,'oxyrisk_DoRun'
  view->DoRun
  
END

PRO oxyrisk_OKRequest, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;doLog,'oxyrisk_OKRequest'
  view->OKRequest
  
END

PRO oxyrisk_execRequest, ev

  ;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;doLog,'oxyrisk_execRequest'
  view->execRequest
  
END

PRO oxyrisk_aboutOKBTT, ev

  ;;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
;doLog,'oxyrisk_aboutOKBTT'
;view->exitRequest
  
END

PRO oxyrisk_aboutSplash, ev

  ;;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;doLog,'oxyrisk_aboutSplash'
  view->showAboutSplash
  
END

PRO oxyrisk_disclaimer, ev

  ;;help, ev, /str
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;doLog,'oxyrisk_disclaimer'
  view->showDisclaimer
  
END

PRO oxyrisk_event, event

  widget_control, event.top, get_uvalue=view
  ;doLog,'oxyrisk_event'
  if tag_names(event, /STRUCTURE_NAME) EQ $
    'WIDGET_KILL_REQUEST' then view->exitRequest
    
END

PRO oxyrisk_destroyWindow, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;doLog,'oxyrisk_destroyWindow'
  view->exitRequest
  
END

PRO oxyrisk_modaldestroyWindow, ev

  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  ;doLog,'oxyrisk_modaldestroyWindow'
  obj_destroy, view
  
END

PRO oxyrisk_inputfile_selection, ev

  doLog,'oxyrisk_inputfile_selection', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=codes
  names=widget_info(ev.id, /UNAME)
  ALLTEST=names[0] eq 'INPUTFILE*ALL'
  if widget_info(ev.id, /TYPE) eq 6 then begin
    indexes=widget_info(ev.id, /LIST_SELECT )
    if indexes[0] eq -1 then view->userInputFileSelections, /NOSELECTION else begin
      codes=codes[indexes]
      view->userInputFileSelections, indexes, codes
    endelse
  endif else begin
    if n_elements(codes) gt 1 then begin
      view->userInputFileSelections, codes, ALLTEST=ALLTEST
      return
    endif
    if ev.select eq 1 then view->userInputFileAddSelection, codes else view->userInputFileRemoveSelection, codes
  endelse
  
END

PRO oxyrisk_runListSelection, ev

  ;index=widget_info(ev.id, /LIST_SELECT)
  index=ev.index
  ;help, ev, /str
  widget_control, ev.id, get_uvalue=uvalue
  code=uvalue[index]
  if size(ev, /TYPE) eq 8 then ev=ev.top
  widget_control, ev, get_uvalue=view
  doLog,'oxyrisk_runListSelection', level=0
  view->userRunSelection, code
  
END

PRO oxyrisk_roi_selection, ev

  doLog,'oxyrisk_roi_selection', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=codes
  names=widget_info(ev.id, /UNAME)
  ALLTEST=names[0] eq 'ROI*ALL'
  if widget_info(ev.id, /TYPE) eq 6 then begin
    indexes=widget_info(ev.id, /LIST_SELECT )
    if indexes[0] eq -1 then view->userROISelections, /NOSELECTION else view->userROISelections, codes[indexes]
  endif else begin
    if n_elements(codes) gt 1 then begin
      view->userROISelections, codes, ALLTEST=ALLTEST
      return
    endif
    if ev.select eq 1 then view->userROIAddSelection, codes else view->userROIRemoveSelection, codes
  endelse
  
END

PRO oxyrisk_resolution_selection, ev

  doLog,'oxyrisk_resolution_selection', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=codes
  names=widget_info(ev.id, /UNAME)
  ALLTEST=names[0] eq 'RESOLUTION*ALL'
  if widget_info(ev.id, /TYPE) eq 6 then begin
    indexes=widget_info(ev.id, /LIST_SELECT )
    if indexes[0] eq -1 then view->userResolutionSelections, /NOSELECTION else view->userResolutionSelections, codes[indexes]
  endif else begin
    if n_elements(codes) gt 1 then begin
      view->userResolutionSelections, codes, ALLTEST=ALLTEST
      return
    endif
    if ev.select eq 1 then view->userResolutionAddSelection, codes else view->userResolutionRemoveSelection, codes
  endelse
  
END

PRO oxyrisk_year_selection, ev

  doLog,'oxyrisk_year_selection', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=codes
  names=widget_info(ev.id, /UNAME)
  ALLTEST=names[0] eq 'YEAR*ALL'
  if widget_info(ev.id, /TYPE) eq 6 then begin
    indexes=widget_info(ev.id, /LIST_SELECT )
    if indexes[0] eq -1 then view->userYearSelections, /NOSELECTION else view->userYearSelections, codes[indexes]
  endif else begin
    if n_elements(codes) gt 1 then begin
      view->userYearSelections, codes, ALLTEST=ALLTEST
      return
    endif
    if ev.select eq 1 then view->userYearAddSelection, codes else view->userYearRemoveSelection, codes
  endelse
  
END

PRO oxyrisk_month_selection, ev

  doLog,'oxyrisk_month_selection', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=codes
  names=widget_info(ev.id, /UNAME)
  ALLTEST=names[0] eq 'MONTH*ALL'
  if widget_info(ev.id, /TYPE) eq 6 then begin
    indexes=widget_info(ev.id, /LIST_SELECT )
    if indexes[0] eq -1 then view->userMonthSelections, /NOSELECTION else view->userMonthSelections, codes[indexes]
  endif else begin
    if n_elements(codes) gt 1 then begin
      view->userMonthSelections, codes, ALLTEST=ALLTEST
      return
    endif
    if ev.select eq 1 then view->userMonthAddSelection, codes else view->userMonthRemoveSelection, codes
  endelse
  
END

PRO oxyrisk_saveRequest, ev

  doLog,'oxyrisk_saveRequest', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  
  view->saveRequest
  
END

PRO oxyrisk_convertToImage, ev

  doLog,'oxyrisk_convertToImage', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  
  view->convertToImage
  
END

PRO oxyrisk_restoreRequest, ev

  doLog,'oxyrisk_restoreRequest', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  
  view->restoreRequest
  
END

PRO oxyrisk_saveGui, ev

  doLog,'oxyrisk_saveGui', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  
  view->saveGui
  
END

PRO oxyrisk_restoreGui, ev

  doLog,'oxyrisk_restoreGui', level=0
  widget_control, ev.top, get_uvalue=view
  widget_control, ev.id, get_uvalue=index
  
  view->restoreGui
  
END

;PRO oxyrisk_benchmarkOKRequest, ev
;
;  widget_control, ev.top, get_uvalue=view
;  widget_control, ev.id, get_uvalue=index
;  
;  ;doLog,'oxyrisk_benchmarkOKRequest'
;  view->okRequest
;;view->setObsGroupStatIndex, index
;  
;END
;
;PRO oxyrisk_groupStatSelection, ev
;
;  widget_control, ev.top, get_uvalue=view
;  widget_control, ev.id, get_uvalue=index
;  
;  view->setObsGroupStatIndex, index
;  
;END
;
;PRO oxyrisk_groupNameOKRequest, ev
;
;  ;help, ev, /str
;  widget_control, ev.top, get_uvalue=view
;  widget_control, ev.id, get_uvalue=widgetText
;  widget_control, widgetText, get_value=groupName
;  widget_control, widgetText, get_uvalue=selectedCodes
;  
;  ;doLog,'oxyrisk_groupNameOKRequest'
;  view->groupNameOKRequest, ev.top, groupName, selectedCodes
;  
;END
;
;PRO oxyrisk_editBatch, ev
;
;  ;help, ev, /str
;  widget_control, ev.id, get_uvalue=type
;  widgetId=ev.id
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_graphicTypeRadioButton'
;  view->userEditBatch
;  
;END
;
;PRO oxyrisk_graphicTypeRadioButton, ev
;
;  ;help, ev, /str
;  widget_control, ev.id, get_uvalue=type
;  widgetId=ev.id
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_graphicTypeRadioButton'
;  view->userUpdateGraphicTypeCode, type, widgetId
;  
;END
;
;PRO oxyrisk_iterateOptionRadioButton, ev
;
;  ;help, ev, /str
;  widget_control, ev.id, get_uvalue=type
;  widgetId=ev.id
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_iterateOptionRadioButton'
;  view->userUpdateIterateOptionCode, type, widgetId
;  
;END
;
;PRO oxyrisk_splitStyleRadioButton, ev
;
;  ;help, ev, /str
;  widget_control, ev.id, get_uvalue=type
;  widgetId=ev.id
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_splitStyleRadioButton'
;  view->userUpdateSplitStyleCode, type, widgetId
;  
;END
;
;PRO oxyrisk_splitDrawMouse, ev
;
;  doLog,"clicking...", ev.press
;  if (ev.press eq 1) then begin
;    doLog,"left split click"
;    xCoord = ev.x
;    yCoord = ev.y
;    if size(ev, /TYPE) eq 8 then ev=ev.top
;    widget_control, ev, get_uvalue=view
;    view->splitMouseClick, xCoord, yCoord
;  endif
;;doLog,'oxyrisk_mapDrawMouse'
;  
;END
;
;PRO oxyrisk_benchmarkOperationButton, ev
;
;  ;help, ev, /str
;  select=ev.select
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  doLog,'oxyrisk_benchmarkOperationButton'
;;view->userAllStationsButton, select
;  
;END
;
;PRO oxyrisk_helpMenuSelection, ev
;
;END
;
;PRO oxyrisk_downloadMenuSelection, ev
;
;END
;
;PRO oxyrisk_flagAllObservationsButton, ev
;
;  ;help, ev, /str
;  select=ev.select
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_singleObsRadioButton'
;  view->userAllObservationsButton, select
;  
;END
;
;PRO oxyrisk_flagAllModelsButton, ev
;
;  ;help, ev, /str
;  select=ev.select
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_singleObsRadioButton'
;  view->userAllModelsButton, select
;  
;END
;
;PRO oxyrisk_flagAllScenariosButton, ev
;
;  ;help, ev, /str
;  select=ev.select
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_singleObsRadioButton'
;  view->userAllScenariosButton, select
;  
;END
;
;PRO oxyrisk_flagAllParametersButton, ev
;
;  ;help, ev, /str
;  select=ev.select
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_singleObsRadioButton'
;  view->userAllParametersButton, select
;  
;END
;
;PRO oxyrisk_recognizeSwitchBtt, ev
;
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  view->SwitchRecognize
;  
;END

;PRO oxyrisk_destroyObsTable, ev
;
;  ;;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_destroyObsTable'
;  view->unlockObsTable
;  
;END
;
;PRO oxyrisk_userViewObsDetails, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_userViewObsDetails'
;  view->showObsDetails
;  
;END
;
;PRO oxyrisk_userUpdateStartDate, ev
;
;  ;help, ev, /str
;  index=ev.index
;  widget_control, ev.id, get_uvalue=type
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_userUpdateStartDate'
;  view->changeStartDate, type, index
;  
;END
;
;PRO oxyrisk_userUpdateEndDate, ev
;
;  ;help, ev, /str
;  index=ev.index
;  widget_control, ev.id, get_uvalue=type
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_userUpdateEndDate'
;  view->changeEndDate, type, index
;  
;END
;
;PRO oxyrisk_benchmarkBtt, ev
;
;  benchMarkFileName=widget_info(ev.id, /UNAME)
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_obsModMainBtt'
;  view->startBenchMark, benchMarkFileName
;  
;END
;
;PRO oxyrisk_obsModMainBtt, ev
;
;  ;help, ev, /str
;  select=ev.select
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_obsModMainBtt'
;  view->userObsModSelection, select
;  
;END


;PRO oxyrisk_startHourSelection, ev
;
;  ;help, ev, /str
;  index=ev.index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_startHourSelection'
;  view->userStartHourSelection, index
;  
;END
;
;PRO oxyrisk_startDaySelection, ev
;
;  ;help, ev, /str
;  index=ev.index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_startDaySelection'
;  view->userStartDaySelection, index
;  
;END
;
;PRO oxyrisk_startMonthSelection, ev
;
;  ;help, ev, /str
;  index=ev.index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_startMonthSelection'
;  view->userStartMonthSelection, index
;  
;END
;
;PRO oxyrisk_startYearSelection, ev
;
;  ;help, ev, /str
;  index=ev.index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;;doLog,'oxyrisk_startYearSelection'
;  
;END
;
;PRO oxyrisk_endHourSelection, ev
;
;  ;help, ev, /str
;  index=ev.index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_endHourSelection'
;  view->userEndHourSelection, index
;  
;END
;
;PRO oxyrisk_endDaySelection, ev
;
;  ;help, ev, /str
;  index=ev.index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_endDaySelection'
;  view->userEndDaySelection, index
;  
;END
;
;PRO oxyrisk_endMonthSelection, ev
;
;  ;help, ev, /str
;  index=ev.index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_endMonthSelection'
;  view->userEndMonthSelection, index
;  
;END
;
;PRO oxyrisk_endYearSelection, ev
;
;  ;help, ev, /str
;  index=ev.index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;;doLog,'oxyrisk_endYearSelection'
;;view->userGroupByTimeSelection, index
;  
;END
;
;PRO oxyrisk_groupByTimeSelection, ev
;
;  ;help, ev, /str
;  widget_control, ev.id, get_uvalue=index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_groupByTimeSelection'
;  view->userGroupByTimeSelection, index
;  
;END
;
;PRO oxyrisk_groupByStatUserSelection, ev
;
;  ;help, ev, /str
;  widget_control, ev.id, get_uvalue=index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_GroupByStatUserSelection'
;  view->userGroupByStatSelection, index
;  
;END
;
;PRO oxyrisk_periodSeasonSelection, ev
;
;  ;help, ev, /str
;  widget_control, ev.id, get_uvalue=index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_PeriodSeasonSelection'
;  view->userSeasonSelection, index
;  
;END
;
;PRO oxyrisk_periodDaySelection, ev
;
;  ;help, ev, /str
;  widget_control, ev.id, get_uvalue=index
;  ;doLog,index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_PeriodDaySelection'
;  view->userDayPeriodSelection, index
;  
;END

;PRO oxyrisk_useObsModButton, ev
;
;  ;help, ev, /str
;  select=ev.select
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_singleObsRadioButton'
;  view->userUseObsModButton, select
;  
;END
;
;PRO oxyrisk_singleObsRadioButton, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_singleObsRadioButton'
;  view->singleObsRadioButton
;  
;END
;
;PRO oxyrisk_groupObsRadioButton, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_groupObsRadioButton'
;  view->groupObsRadioButton
;  
;END
;
;PRO oxyrisk_addObsButton, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_addObsButton'
;  view->addObsButton
;  
;END
;
;PRO oxyrisk_removeObsButton, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_removeObsButton'
;  view->removeObsButton
;  
;END
;
;PRO oxyrisk_obsStoredListSelection, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_obsStoredListSelection'
;  view->obsStoredListSelection
;  
;END
;
;PRO oxyrisk_loadObsButton, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_loadObsButton'
;  view->loadObsButton
;  
;END
;
;PRO oxyrisk_saveObsButton, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_saveObsButton'
;  view->saveObsButton
;  
;END
;
;PRO oxyrisk_obsQueryListSelection, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_obsQueryListSelection'
;  view->obsQueryListSelection
;  
;END
;
;PRO oxyrisk_categoryValuesExclusiveSelection, ev
;
;  ;help, ev, /str
;  wid=ev.id
;  widget_control, wid, get_uvalue=uvalue
;  index=uvalue.thisidx & catIndex=uvalue.catIdx
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_categoryValuesExclusiveSelection'
;  view->categoryValuesExclusiveSelection, wid, catIndex, index
;  
;END
;
;PRO oxyrisk_categoryValuesListSelection, ev
;
;  ;help, ev, /str
;  wid=ev.id
;  widget_control, wid, get_uvalue=uvalue
;  index=ev.index & catIndex=uvalue.catIdx
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_categoryValuesListSelection'
;  view->categoryValuesListSelection, wid, catIndex, index-1
;  
;END
;
;PRO oxyrisk_useCriteriaButton, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;;doLog,'oxyrisk_useCriteriaButton'
;;view->useCriteriaButton
;  
;END
;
;PRO oxyrisk_useThresholdsButton, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_useThresholdsButton'
;  view->useThresholdsButton
;  
;END
;
;PRO oxyrisk_parameterListSelection, ev
;
;  ;help, ev, /str
;  indexes=widget_info(ev.id, /LIST_SELECT)
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_parameterListSelection'
;  view->userParameterSelections, indexes
;  
;END
;
;PRO oxyrisk_parameterTypeListSelection, ev
;
;  ;help, ev, /str
;  indexes=widget_info(ev.id, /LIST_SELECT)
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_parameterTypeListSelection'
;  view->userParameterTypeSelections, indexes
;  
;END
;
;PRO oxyrisk_elabNameListSelection, ev
;
;  ;help, ev, /str
;  index=ev.index
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_elabNameListSelection'
;  view->userElabSelection, index
;  
;END
;
;PRO oxyrisk_elabDiagramListSelection, ev
;
;  ;help, ev, /str
;  ;wid=ev.id
;  index=ev.index
;  ;widget_control, wid, get_uvalue=uvalue
;  ;index=uvalue.thisidx & catIndex=uvalue.catIdx
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_elabDiagramListSelection'
;  view->userDiagramSelection, index
;  
;END
;
;;PRO oxyrisk_elabAxisListSelection, ev
;;
;; ;help, ev, /str
;; index=ev.index
;; if size(ev, /TYPE) eq 8 then ev=ev.top
;; widget_control, ev, get_uvalue=view
;; ;doLog,'oxyrisk_elabAxisListSelection'
;; view->userAxisSelection, index
;;
;;END
;
;PRO oxyrisk_scenarioListSelection, ev
;
;  indexes=widget_info(ev.id, /LIST_SELECT)
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_scenarioListSelection'
;  view->userScenarioSelections, indexes
;  
;END
;
;PRO oxyrisk_modelListSelection, ev
;
;  indexes=widget_info(ev.id, /LIST_SELECT)
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_modelListSelection'
;  view->userModelSelections, indexes
;  
;END
;
;PRO oxyrisk_saveImage, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_saveImage'
;  view->saveImage
;  
;END
;
;PRO oxyrisk_saveImageBlack, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_saveImageBlack'
;  view->saveImageBlack
;  
;END
;
;PRO oxyrisk_saveImageWhite, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_saveImageWhite'
;  view->saveImageWhite
;  
;END
;
;PRO oxyrisk_saveBatch, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_saveBatch'
;  view->saveBatch
;  
;END
;
;PRO oxyrisk_restoreBatch, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_restoreBatch'
;  view->restoreBatch
;;view->startBatchMode
;  
;END
;
;
;PRO oxyrisk_dateComboSelection, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_dateComboSelection'
;  view->dateComboSelection
;  
;END
;
;PRO oxyrisk_modeComboSelection, ev
;
;  index=ev.index
;  ;;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_modeComboSelection'
;  view->userSelection, index
;  
;END
;
;PRO oxyrisk_modeMenuSelection, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_modeMenuSelection'
;  view->displayModeSelectionGUI
;  
;END
;
;PRO oxyrisk_batchComposition, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_benchmarkBuildMenuSelection'
;  view->displayCompositeBatchGUI
;  
;END
;
;PRO oxyrisk_displayEntity, ev
;
;  ;help, ev, /str
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_entityMenuSelection'
;  view->displayEntitySelectionGUI
;  
;END
;
;PRO oxyrisk_saveEntity, ev
;
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_entityMenuSave'
;  view->saveEntity
;  
;END
;
;PRO oxyrisk_restoreEntity, ev
;
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_entityMenuLoad'
;  view->restoreEntity
;  
;END
;
;PRO oxyrisk_displayElaboration, ev
;
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_elaborationMenuSelection'
;  view->displayElaborationSelectionGUI
;  
;END
;
;PRO oxyrisk_saveElaboration, ev
;
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_elaborationMenuSave'
;  view->saveElaboration
;  
;END
;
;PRO oxyrisk_restoreElaboration, ev
;
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_elaborationMenuLoad'
;  view->restoreElaboration
;  
;END
;
;PRO oxyrisk_dateMenuSelection, ev
;
;  if size(ev, /TYPE) eq 8 then ev=ev.top
;  widget_control, ev, get_uvalue=view
;  ;doLog,'oxyrisk_dateMenuSelection'
;  view->displayDateSelectionGUI
;  
;END

;PRO oxyrisk_mapDrawMouse, ev
;
;  doLog,"clicking...", ev.press
;  if (ev.press eq 1) then begin
;    doLog,"left"
;    xCoord = ev.x
;    yCoord = ev.y
;    if size(ev, /TYPE) eq 8 then ev=ev.top
;    widget_control, ev, get_uvalue=view
;    view->recognize, xCoord, yCoord
;  endif
;;doLog,'oxyrisk_mapDrawMouse'
;  
;END