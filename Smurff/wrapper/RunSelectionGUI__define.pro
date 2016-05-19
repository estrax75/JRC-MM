FUNCTION RunSelectionGUI::getScreenSize 

 return, self.dimensions

END

FUNCTION RunSelectionGUI::getLangCatLabelFromKey, key, placeHolders=placeHolders 

 return, self.mgr->getLangCatLabelFromKey(key, placeHolders=placeHolders)

END

PRO RunSelectionGUI::convertToImage

  self.mgr->convertToImage
  
END

PRO RunSelectionGUI::saveRequest

  self.mgr->saveRequest
  
END

PRO RunSelectionGUI::restoreRequest

  self.mgr->restoreRequest
  
END

PRO RunSelectionGUI::saveGui

  fs=self.mgr->getFileSystem()
  if (fs->isOSUnix()) then fix_filter='*'+strmid(fs->getRunSelectionDisplayInfoExtension(),1,4) else filter=['*'+fs->getRunSelectionDisplayInfoExtension()]
  if (fs->isOSUnix()) then filter='*'+strmid(fs->getRunSelectionDisplayInfoExtension(),1,4) else fix_filter='*'+fs->getRunSelectionDisplayInfoExtension()
  if (fs->isOSUnix()) then default_extension=strmid(fs->getRunSelectionDisplayInfoExtension(),1,4) else default_extension=fs->getRunSelectionDisplayInfoExtension()
  runSelectionFile=dialog_pickfile(DEFAULT_EXTENSION=default_extension, $
    DIALOG_PARENT=self->getTopBase(), $
    FILTER=filter, FIX_FILTER=fix_filter, $
    GET_PATH=path, PATH=fs->getSaveDir(), $
    TITLE=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:SaveToFileMsg'), /OVERWRITE_PROMPT, /WRITE)
  if runSelectionFile ne '' then begin
    runSelectionFile=fs->removeFileExtension(runSelectionFile)
    runSelectionFile=fs->addFileExtension(runSelectionFile, fs->getRunSelectionDisplayInfoExtension(), extensionSep='')
    self.info->XmlWriteStructList, runSelectionFile
    discard=self->dialogMessage([self.mgr->getLangCatLabelFromKey('RunSelectionGUI:SaveGuiToFileOKMsg', placeHolders=[runSelectionFile])], title=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:SaveGuiToFileOKTitle'), /INFO)
    
  endif
  
END

PRO RunSelectionGUI::restoreGui

  fs=self.mgr->getFileSystem()
  
  if (fs->isOSUnix()) then fix_filter='*'+strmid(fs->getRunSelectionDisplayInfoExtension(),1,4) else filter=['*'+fs->getRunSelectionDisplayInfoExtension()]
  if (fs->isOSUnix()) then filter='*'+strmid(fs->getRunSelectionDisplayInfoExtension(),1,4) else fix_filter='*'+fs->getRunSelectionDisplayInfoExtension()
  if (fs->isOSUnix()) then default_extension=strmid(fs->getRunSelectionDisplayInfoExtension(),1,4) else default_extension=fs->getRunSelectionDisplayInfoExtension()
  runSelectionFile=dialog_pickfile(DEFAULT_EXTENSION=default_extension, $
    DIALOG_PARENT=self->getTopBase(), $
    FILTER=filter, FIX_FILTER=fix_filter, $
    GET_PATH=path, PATH=fs->getSaveDir(), $
    TITLE=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:RestoreFromFileMsg'), /OVERWRITE_PROMPT, /READ, /MUST_EXIST)
  if runSelectionFile ne '' then begin
    result=self.info->fillDataFromXMLFile(runSelectionFile)
    if result eq 1 then begin
      self->configure
      discard=self->dialogMessage([self.mgr->getLangCatLabelFromKey('RunSelectionGUI:RestoreOKMsg', placeHolders=[runSelectionFile])], $
      title=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:RestoreOKTitle'), $
      /INFO)
    endif else discard=self->dialogMessage($
      [self.mgr->getLangCatLabelFromKey('RunSelectionGUI:RestoreKOMsg', placeHolders=[runSelectionFile])], $
      title=[self.mgr->getLangCatLabelFromKey('RunSelectionGUI:RestoreKOTitle')], /WARNING)
  endif
  
END

PRO RunSelectionGUI::doRun

  if self.info->checkIntegrity(self) then begin
    self.mgr->setRunSelectionDisplay, self.info->clone(/DEEP)
    self.mgr->execRequest
  endif
  
  
END

PRO RunSelectionGUI::userRunSelection, code, NORUNUPDATE=NORUNUPDATE, SILENT=SILENT

  widget_control, self.runList, get_uvalue=RUNINDEXES
  selIndex=(where(RUNINDEXES eq code, count))[0]
  widget_control, self.runList, SET_COMBOBOX_SELECT=selIndex
  self.info->setRunCodeSelected, code
  self->updateRunRelated
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self, SILENT=SILENT
  
  
END

PRO RunSelectionGUI::allSourceFileSetting

  if widget_info((*self.sourceFilesMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    widget_control, (*self.sourceFilesMultipleWid)[0], sensitive=1-self.info->getAllSourceFilesFlag()
  endif else begin
    ; buttons
    allBtts=(*self.sourceFilesMultipleWid)
    for i=0, n_elements(allBtts)-1 do widget_control, allBtts[i], sensitive=1-self.info->getAllSourceFilesFlag()
  endelse
  
END

PRO RunSelectionGUI::userSourceFileSelections, indexes, codes, SETALL=SETALL, ALLTEST=ALLTEST, NOSELECTION=NOSELECTION, NORUNUPDATE=NORUNUPDATE

  ; select everytime all the files!
  if keyword_set(SETALL) then begin
    self.info->setAllSourceFilesFlag, 1
    ;allWid=widget_info(self->getTopBase(), FIND_BY_UNAME='RESOLUTION*'+'ALL')
    self->allSourceFileSetting
    widget_control, (*self.sourceFilesMultipleWid)[0], get_uvalue=list
    indexes=indgen(n_elements(list))
    codes=list
  endif
  if ~keyword_set(NOSELECTION) and self.info->getSelectedRunUseInputFilesSelection() then self->updateSourceFilesListSelection, indexes, codes else self.info->setSourceFileSelectedCodes, /VOID
  
END

PRO RunSelectionGUI::updateSourceFilesListSelection, indexes, codes, NOSELECTION=NOSELECTION

  selFileLabel=widget_info(self->getTopBase(), FIND_BY_UNAME='FILEINFOSEL')
  
  selElements=strcompress(n_elements(indexes), /REMOVE)
  if keyword_set(NOSELECTION) or n_elements(indexes) eq 0 then indexes=-1
  
  widget_control, (*self.sourceFilesMultipleWid)[0], set_list_select=indexes
  widget_control, selFileLabel, set_value=selElements
  self.info->setSourceFileSelectedCodes, codes
  
END

PRO RunSelectionGUI::updateSourceFilesList, fileList, codes, NOSELECTION=NOSELECTION, VOID=VOID

  if n_elements(fileList) ne 0 then begin
    showFileList=fileList[0:n_elements(fileList)-1 < 50]
    showCodeList=codes[0:n_elements(fileList)-1 < 50]
  endif
  selFileLabel=widget_info(self->getTopBase(), FIND_BY_UNAME='FILEINFOSEL')
  avFileLabel=widget_info(self->getTopBase(), FIND_BY_UNAME='FILEINFOAVA')
  
  avElements='0'
  selElements='0'
  
  ;if n_elements(fileList) ne 0 then begin
  if ~keyword_set(VOID) then begin
    if ~keyword_set(NOSELECTION) then begin
      widget_control, (*self.sourceFilesMultipleWid)[0], sensitive=1
      widget_control, (*self.sourceFilesMultipleWid)[0], set_value=showFileList
      widget_control, (*self.sourceFilesMultipleWid)[0], set_uvalue=showCodeList
      avElements=strcompress(n_elements(fileList), /REMOVE)
    ;widget_control, avFileLabel, set_value=avElements
    endif else begin
      widget_control, (*self.sourceFilesMultipleWid)[0], sensitive=0
      widget_control, (*self.sourceFilesMultipleWid)[0], set_value=''
      widget_control, (*self.sourceFilesMultipleWid)[0], set_uvalue=''
    endelse
    widget_control, selFileLabel, set_value=selElements
    widget_control, avFileLabel, set_value=avElements
  endif else begin
    widget_control, (*self.sourceFilesMultipleWid)[0], sensitive=0
    widget_control, (*self.sourceFilesMultipleWid)[0], set_value=''
    widget_control, (*self.sourceFilesMultipleWid)[0], set_uvalue=''
  endelse
  
END

;PRO RunSelectionGUI::userRemoveSourceFileFlag, selection
;
;  self.info->setRemoveSourceFilesFlag, selection
;  
;END

;PRO RunSelectionGUI::userDeleteInputFlag, selection
;
;  self.info->setDeleteInputFlag, selection
;  
;END
;
;PRO RunSelectionGUI::userOverwriteResultFlag, selection
;
;  self.info->setOverwriteResultFlag, selection
;  
;END

PRO RunSelectionGUI::userSourceFileAddSelection, code, NORUNUPDATE=NORUNUPDATE

;self.info->addInputFileCodeSelected, code
;if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self

END

PRO RunSelectionGUI::userSourceFileRemoveSelection, code, NORUNUPDATE=NORUNUPDATE

;self.info->removeInputFileCodeSelected, code
;if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self

END

PRO RunSelectionGUI::userROISelections, codes, ALLTEST=ALLTEST, NOSELECTION=NOSELECTION, NORUNUPDATE=NORUNUPDATE

  if keyword_set(ALLTEST) then begin
    self.info->switchAllROIFlag
    ;allWid=widget_info(self->getTopBase(), FIND_BY_UNAME='RESOLUTION*'+'ALL')
    self->allROISetting
  ;widget_control, allWid, sensitive=self.info->getAllResolutionFlag
  endif
  if keyword_set(NOSELECTION) then codes=self.info->getRoiCodesSelected()
  indexes=self.info->getROIIndexesOfCodes(codes, NOSELECTION=NOSELECTION)
  if ~keyword_set(NOSELECTION) then begin
    self->mapROIData, indexes
    self.info->setRoiCodesSelected, codes
  endif
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userROIAddSelection, code, NORUNUPDATE=NORUNUPDATE

  self.info->addRoiCodeSelected, code
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userROIRemoveSelection, code, NORUNUPDATE=NORUNUPDATE

  self.info->removeRoiCodeSelected, code
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userYearSelections, codes, ALLTEST=ALLTEST, NOSELECTION=NOSELECTION, NORUNUPDATE=NORUNUPDATE

  if keyword_set(ALLTEST) then begin
    self.info->switchAllYearFlag
    ;allWid=widget_info(self->getTopBase(), FIND_BY_UNAME='RESOLUTION*'+'ALL')
    self->allYearSetting
  ;widget_control, allWid, sensitive=self.info->getAllResolutionFlag
  endif
  if keyword_set(NOSELECTION) then codes=self.info->getYearCodesSelected()
  indexes=self.info->getYearIndexesOfCodes(codes)
  self->mapYearData, indexes
  self.info->setYearCodesSelected, codes
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userYearAddSelection, code, NORUNUPDATE=NORUNUPDATE

  self.info->addYearCodeSelected, code
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userYearRemoveSelection, code, NORUNUPDATE=NORUNUPDATE

  self.info->removeYearCodeSelected, code
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userMonthSelections, codes, ALLTEST=ALLTEST, NOSELECTION=NOSELECTION, NORUNUPDATE=NORUNUPDATE

  if keyword_set(ALLTEST) then begin
    self.info->switchAllMonthFlag
    ;allWid=widget_info(self->getTopBase(), FIND_BY_UNAME='RESOLUTION*'+'ALL')
    self->allMonthSetting
  ;widget_control, allWid, sensitive=self.info->getAllResolutionFlag
  endif
  if keyword_set(NOSELECTION) then codes=self.info->getMonthCodesSelected()
  indexes=self.info->getMonthIndexesOfCodes(codes)
  self->mapMonthData, indexes
  self.info->setMonthCodesSelected, codes
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userMonthAddSelection, code, NORUNUPDATE=NORUNUPDATE

  self.info->addMonthCodeSelected, code
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userMonthRemoveSelection, code, NORUNUPDATE=NORUNUPDATE

  self.info->removeMonthCodeSelected, code
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userResolutionSelections, codes, ALLTEST=ALLTEST, NOSELECTION=NOSELECTION, NORUNUPDATE=NORUNUPDATE

  if keyword_set(ALLTEST) then begin
    self.info->switchAllResolutionFlag
    ;allWid=widget_info(self->getTopBase(), FIND_BY_UNAME='RESOLUTION*'+'ALL')
    self->allResolutionSetting
  ;widget_control, allWid, sensitive=self.info->getAllResolutionFlag
  endif
  if keyword_set(NOSELECTION) then codes=self.info->getResolutionCodesSelected()
  indexes=self.info->getResolutionIndexesOfCodes(codes)
  self->mapResolutionData, indexes
  self.info->setResolutionCodesSelected, codes
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userResolutionAddSelection, code, NORUNUPDATE=NORUNUPDATE

  self.info->addResolutionCodeSelected, code
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::userResolutionRemoveSelection, code, NORUNUPDATE=NORUNUPDATE

  self.info->removeResolutionCodeSelected, code
  if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::destroySection, widgetIds, UNAME=UNAME

  destroyIds=lonarr(1)
  nElem = n_elements(UNAME) > n_elements(widgetIds)
  if n_elements(UNAME) ne 0 then begin
    for i=0, nElem-1 do destroyIds[i]=widget_info(self.topBase, FIND_BY_UNAME=UNAME[i])
  endif else begin
    destroyIds=widgetIds
  endelse
  for i=0, n_elements(destroyIds)-1 do widget_control, destroyIds[i], /DESTROY
  
END

PRO RunSelectionGUI::updateRoiInfo

  if self.info->getSelectedRunUseROISelection() then begin
    widget_control, self.topBase, MAP=0
    self->destroySection, UNAME='ROIBASE'
    roiBase=widget_base(self.roiBase, xpad=0, ypad=0,space=0, FRAME=0, /COLUMN, UNAME='ROIBASE')
    self->buildROISection, roiBase
    widget_control, self.topBase, MAP=1
  endif else begin
    self->deactivateROISection
  endelse
  
END

PRO RunSelectionGUI::updateSelectionsSensitive

  roiBase=widget_info(self.topBase, FIND_BY_UNAME='ROIBASE')
  widget_control, roiBase, sensitive=self.info->getSelectedRunUseROISelection()
  resolutionBase=widget_info(self.topBase, FIND_BY_UNAME='RESOLUTIONBASE')
  widget_control, resolutionBase, sensitive=self.info->getSelectedRunUseResolutionSelection()
  yearBase=widget_info(self.topBase, FIND_BY_UNAME='YEARBASE')
  widget_control, yearBase, sensitive=self.info->getSelectedRunUseYearSelection()
  monthBase=widget_info(self.topBase, FIND_BY_UNAME='MONTHBASE')
  widget_control, monthBase, sensitive=self.info->getSelectedRunUseMonthSelection()
  
END

PRO RunSelectionGUI::updateRunRelated, NORUNUPDATE=NORUNUPDATE

  description=self.info->getSelectedRunDescription()
  length=(strlen(description))[0]
  newDesc=''
  maxL=80
  if length gt maxL then begin
    for i=0, length-1, maxL do newDesc=[newDesc, strmid(description, i, maxL)]
    newDesc=newDesc[1:*]
  endif else begin
    newDesc=description
  endelse
  ;widget_control, self.runDescriptionText, set_value=newDesc
  widget_control, self.runDescriptionText, set_value=description
  self->updateRoiInfo
  self.info->setRoiCodesSelected, /NOSELECTION
  self->updateSourceFileInfo
  self->updateDestinationFileInfo
  self->updateSelectionsSensitive
  
END

;PRO RunSelectionGUI::userRemoveSourceFilesFlag, selection
;
;  self.info->setRemoveSourceFilesFlag, selection
;  
;END

PRO RunSelectionGUI::userDeleteInputFlag, selection

  widget_control, self.deleteInputFlagButton, SET_BUTTON=selection
  self.info->setDeleteInputFlag, selection
  
END

PRO RunSelectionGUI::userOverwriteResultFlag, selection

  widget_control, self.overwriteResultFlagButton, SET_BUTTON=selection
  self.info->setOverwriteResultFlag, selection
  
END

;user selection & fixed files database version (need to be merged with previous one)
PRO RunSelectionGUI::updateSourceFileInfo, VOID=VOID

  titleWid=widget_info(self->getTopBase(), FIND_BY_UNAME='INPUTFILE*'+'TITLE')
  sourceFolderName=self.info->getSelectedSourceFolderName(/WITHFILTER)
  widget_control, titleWid, set_value=sourceFolderName
  
  ;  inputFilesName=self.info->getFolderContents(inputFolderName, /NOPATH)
  ;  widget_control, (*self.sourceFilesMultipleWid)[0], set_value=inputFilesName
  if self.info->getSelectedRunUseInputFilesSelection() then begin
    fileList=self.info->getSourceFileInfo(/NAME_ORDERED, displayCodes=displayCodes, NOSELECTION=NOSELECTION)
    VOID=NOSELECTION
    self->updateSourceFilesList, fileList, displayCodes, NOSELECTION=NOSELECTION
    if ~(keyword_set(VOID)) then self->userSourceFileSelections, /SETALL else self->userSourceFileSelections, /NOSELECTION
  endif else begin
    self->updateSourceFilesList, /VOID
  endelse
  
END

;run related version
;PRO RunSelectionGUI::updateDestinationFileInfo
;
;  titleWid=widget_info(self->getTopBase(), FIND_BY_UNAME='OUTPUTFILE*'+'TITLE')
;  outputFolderName=self.info->getSelectedOutputFolderName(/WITHFILTER)
;  widget_control, titleWid, set_value=outputFolderName
;
;  outputFilesName=self.info->getFolderContents(outputFolderName, /NOPATH)
;  widget_control, (*self.destFilesMultipleWid)[0], set_value=outputFilesName
;
;END

;user selection & fixed files database version (need to be merged with previous one)
PRO RunSelectionGUI::updateDestinationFileInfo

  titleWid=widget_info(self->getTopBase(), FIND_BY_UNAME='OUTPUTFILE*'+'TITLE')
  outputFolderName=self.info->getSelectedOutputFolderName(/WITHFILTER)
  widget_control, titleWid, set_value=outputFolderName
  
  outputFilesName=self.info->getFolderContents(outputFolderName, /NOPATH)
  widget_control, (*self.destFilesMultipleWid)[0], set_value=outputFilesName
  
END

;PRO RunSelectionGUI::updateDestFileListContent, folderName
;
;
;END

PRO RunSelectionGUI::deactivateROISection

  ;    widget_control, *self.roiMultipleWid, set_value=''
  allBtt=widget_info(self->getTopBase(), FIND_BY_UNAME='ROI*ALL')
  if allBtt gt 1 then widget_control, allBtt, sensitive=0, set_button=0
  ;    ids=*self.roiMultipleWid
  ;for i=0, n_elementswidget_control, *self.roiMultipleWid, sensitive=0
  
  if widget_info((*self.roiMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    ;widget_control, (*self.roiMultipleWid)[0], sensitive=1-self.info->getallROIFlag()
    widget_control, (*self.roiMultipleWid)[0], sensitive=0, set_value=''
  endif else begin
    ; buttons
    allBtts=(*self.roiMultipleWid)
    for i=0, n_elements(allBtts)-1 do widget_control, allBtts[i], sensitive=0, set_button=0
  endelse
;if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::allROISetting, NORUNUPDATE=NORUNUPDATE

  if widget_info((*self.roiMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    widget_control, (*self.roiMultipleWid)[0], sensitive=1-self.info->getallROIFlag()
  endif else begin
    ; buttons
    allBtts=(*self.roiMultipleWid)
    for i=0, n_elements(allBtts)-1 do widget_control, allBtts[i], sensitive=1-self.info->getallROIFlag()
  endelse
;if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::allResolutionSetting, NORUNUPDATE=NORUNUPDATE

  if widget_info((*self.resolutionMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    widget_control, (*self.resolutionMultipleWid)[0], sensitive=self.info->getallResolutionFlag()
  endif else begin
    ; buttons
    allBtts=(*self.resolutionMultipleWid)
    for i=0, n_elements(allBtts)-1 do widget_control, allBtts[i], sensitive=1-self.info->getallResolutionFlag()
  endelse
;if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::allYearSetting, NORUNUPDATE=NORUNUPDATE

  if widget_info((*self.yearMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    widget_control, (*self.yearMultipleWid)[0], sensitive=self.info->getallYearFlag()
  endif else begin
    ; buttons
    allBtts=(*self.yearMultipleWid)
    for i=0, n_elements(allBtts)-1 do widget_control, allBtts[i], sensitive=1-self.info->getAllYearFlag()
  endelse
;if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END

PRO RunSelectionGUI::allMonthSetting, NORUNUPDATE=NORUNUPDATE

  if widget_info((*self.monthMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    widget_control, (*self.monthMultipleWid)[0], sensitive=self.info->getallMonthFlag()
  endif else begin
    ; buttons
    allBtts=(*self.monthMultipleWid)
    for i=0, n_elements(allBtts)-1 do widget_control, allBtts[i], sensitive=1-self.info->getAllMonthFlag()
  endelse
;if ~(keyword_set(NORUNUPDATE)) then self.info->updateRunElements, self
  
END


PRO RunSelectionGUI::mapROIData, indexes

  if widget_info((*self.roiMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    widget_control, (*self.roiMultipleWid)[0], SET_LIST_SELECT=indexes
  endif else begin
    ; buttons
    nElem=n_elements((*self.roiMultipleWid))
    for i=0, nElem-1 do begin
      idx=where(i eq indexes, count)
      widget_control, (*self.roiMultipleWid)[i], set_button=(count > 0)
    endfor
  endelse
  
END

PRO RunSelectionGUI::mapResolutionData, indexes

  if widget_info((*self.resolutionMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    widget_control, (*self.resolutionMultipleWid)[0], SET_LIST_SELECT=indexes
  endif else begin
    ; buttons
    nElem=n_elements((*self.resolutionMultipleWid))
    for i=0, nElem-1 do begin
      idx=where(i eq indexes, count)
      widget_control, (*self.resolutionMultipleWid)[i], set_button=(count > 0)
    endfor
  endelse
  
END

PRO RunSelectionGUI::mapMonthData, indexes

  if widget_info((*self.monthMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    widget_control, (*self.monthMultipleWid)[0], SET_LIST_SELECT=indexes
  endif else begin
    ; buttons
    nElem=n_elements((*self.monthMultipleWid))
    for i=0, nElem-1 do begin
      idx=where(i eq indexes, count)
      widget_control, (*self.monthMultipleWid)[i], set_button=(count > 0)
    endfor
  endelse
  
END

PRO RunSelectionGUI::mapYearData, indexes

  if widget_info((*self.yearMultipleWid)[0], /TYPE) eq 6 then begin
    ; list
    widget_control, (*self.yearMultipleWid)[0], SET_LIST_SELECT=indexes
  endif else begin
    ; buttons
    nElem=n_elements((*self.yearMultipleWid))
    for i=0, nElem-1 do begin
      idx=where(i eq indexes, count)
      widget_control, (*self.yearMultipleWid)[i], set_button=(count > 0)
    endfor
  endelse
  
END

;build
PRO RunSelectionGUI::buildOperationButtonSection, base

  subBase=base
  bttBase=widget_base(subBase, /COLUMN)
  
  mainBttBase1=widget_base(bttBase, YPAD=2, /COLUMN)
  optionBttBase1=widget_base(bttBase, YPAD=2, /COLUMN)
  optionBttBase2=widget_base(bttBase, YPAD=2, /COLUMN)
  optionBttBase3=widget_base(bttBase, YPAD=2, /COLUMN)
  mainBttBase2=widget_base(bttBase, YPAD=2, /COLUMN)
  
  ;bttDims=self->getMainButtonDimension()
  bttDims=self->getMainBigButtonDimension()
  
  runBtt=widget_button(mainBttBase1, value=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:ExecuteBtt'), UNAME='DORUN', $
    event_pro=self.eventprefix+'DoRun', SCR_XSIZE=bttDims[0], SCR_YSIZE=bttDims[1], /ALIGN_CENTER, FONT=self->getButtonLabelFont())
  configBtt=widget_button(mainBttBase1, value=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:ConfigBtt'), UNAME='DISPLAYCONFIG', $
    event_pro=self.eventprefix+'Config', SCR_XSIZE=bttDims[0], SCR_YSIZE=bttDims[1], /ALIGN_CENTER, FONT=self->getButtonLabelFont())

  ;saveCfgBtt=widget_button(optionBttBase1, value='SAVE REQUEST', UNAME='SAVERQS', $
  ;bttText=self->SplitStringInArray(self.mgr->getLangCatLabelFromKey('RunSelectionGUI:SaveRequestBtt'))
  saveCfgBtt=widget_button(optionBttBase1, value=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:SaveRequestBtt'), UNAME='SAVERQS', $
    event_pro=self.eventprefix+'saveRequest', SCR_XSIZE=bttDims[0], SCR_YSIZE=bttDims[1], /ALIGN_CENTER, FONT=self->getButtonLabelFont())
  ;restoreCfgBtt=widget_button(optionBttBase1, value='RESTORE REQUEST', UNAME='RESTORERQS', $
  restoreCfgBtt=widget_button(optionBttBase1, value=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:RestoreRequestBtt'), UNAME='RESTORERQS', $
    event_pro=self.eventprefix+'restoreRequest', SCR_XSIZE=bttDims[0], SCR_YSIZE=bttDims[1], /ALIGN_CENTER, FONT=self->getButtonLabelFont())

  ;saveCfgBtt=widget_button(optionBttBase1, value='SAVE GUI', UNAME='SAVEGUI', $
  saveCfgBtt=widget_button(optionBttBase2, value=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:SaveGuiBtt'), UNAME='SAVEGUI', $
    event_pro=self.eventprefix+'saveGui', SCR_XSIZE=bttDims[0], SCR_YSIZE=bttDims[1], /ALIGN_CENTER, FONT=self->getButtonLabelFont())
  ;restoreCfgBtt=widget_button(optionBttBase1, value='RESTORE GUI', UNAME='RESTOREGUI', $
  restoreCfgBtt=widget_button(optionBttBase2, value=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:RestoreGuiBtt'), UNAME='RESTOREGUI', $
    event_pro=self.eventprefix+'restoreGui', SCR_XSIZE=bttDims[0], SCR_YSIZE=bttDims[1], /ALIGN_CENTER, FONT=self->getButtonLabelFont())
    
  convertToImageBtt=widget_button(optionBttBase3, value=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:ConvertToImageBtt'), UNAME='CONVERTIMG', $
    event_pro=self.eventprefix+'convertToImage', SCR_XSIZE=bttDims[0], SCR_YSIZE=bttDims[1], /ALIGN_CENTER, FONT=self->getButtonLabelFont())

  exitBtt=widget_button(mainBttBase2, value=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:ExitBtt'), UNAME='DISPLAYEXIT', $
    event_pro=self.eventprefix+'destroyWindow', SCR_XSIZE=bttDims[0], SCR_YSIZE=bttDims[1], /ALIGN_CENTER, FONT=self->getButtonLabelFont())
    
END

FUNCTION RunSelectionGUI::buildMultiSelectionList, refBase, wType, title, elementsName, elementsCode, eventName, $
    eventTag, scrXSize, scrYSize, $
    TITLEASTEXT=TITLEASTEXT, ALLOPTION=ALLOPTION, COLUMN=COLUMN, ROW=ROW, FRAME=FRAME, UNAME=UNAME, SCROLL=SCROLL
  ;    eventTag, ALLOPTION=ALLOPTION, COLUMN=COLUMN, ROW=ROW
    
  if n_elements(scrXSize) eq 0 then xSize=self->getListsSectionXSize() else xSize=scrXSize
  if n_elements(scrYSize) eq 0 then ySize=self->getListsSectionYSize() else ySize=scrYSize
  
  if keyword_set(COLUMN)+keyword_set(ROW) eq 0 then COLUMN=1
  multiBase=widget_base(refBase, xpad=0, ypad=0, space=0, ROW=ROW, COLUMN=COLUMN, FRAME=FRAME, UNAME=UNAME, SCROLL=SCROLL, SCR_XSIZE=xSize, SCR_YSIZE=ySize)
  ;multiBase=widget_base(refBase, xpad=0, ypad=0, space=0, ROW=ROW, COLUMN=COLUMN, FRAME=FRAME, UNAME=UNAME, /SCROLL, scr_xsize=xSize, scr_ysize=ySize)
  
  avSelectionNo=n_elements(elementsName)
  
  
  if keyword_set(COLUMN) then begin
    elementXSize=xSize
    elementYSize=ySize/(avSelectionNo+1) ; space for label
    listXSize=elementXSize
    listYSize=elementYSize*avSelectionNo
  endif
  if keyword_set(ROW) then begin
    elementXSize=xSize/(avSelectionNo+1) ;self->getSingleElementMultipleXSize(avSelectionNo+1) ; space for label
    elementYSize=ySize
    listXSize=elementXSize*avSelectionNo
    listYSize=elementYSize
  endif
  
  if not(keyword_set(TITLEASTEXT)) then sectionTitle=widget_label(multiBase, UNAME=eventTag+'TITLE', value=title, $
    scr_xsize=elementXSize, scr_ysize=self->getLabelYSize()*2, /ALIGN_LEFT, font=self.titleFont) else $
    sectionTitle=widget_text(multiBase, value=title, UNAME=eventTag+'TITLE', $
    scr_xsize=elementXSize, scr_ysize=self->getLabelYSize()*2, /ALIGN_LEFT, font=self.titleFont)
    
  if keyword_set(ALLOPTION) then begin
    allNonExclusiveBase = widget_Base(multiBase, UNAME='', $
      XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
      SPACE=0 ,XPAD=0 ,YPAD=0, ROW=ROW, COLUMN=COLUMN, $
      SCR_XSIZE=elementXSize, SCR_YSIZE=elementYSize > self->getLabelYSize())
    eventProName=self.eventPrefix+eventName
    doLog,eventTag+'ALL', level=0
    allBtt= widget_button(allNonExclusiveBase, $
      ;XOFFSET=0 ,YOFFSET=0, VALUE='All', UNAME=eventTag+'ALL', $
      XOFFSET=0 ,YOFFSET=0, VALUE=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:AllOptionsBtt'), UNAME=eventTag+'ALL', $
      UVALUE=elementsCode, SCR_XSIZE=elementXSize, SCR_YSIZE=elementYSize > self->getLabelYSize(), $
      event_pro=eventProName)
    splitBase=widget_base(multiBase, SCR_XSIZE=elementXSize, FRAME=1, YPAD=1)
  endif
  
  eventProName=self.eventPrefix+eventName
  list=lonarr(avSelectionNo)
  if wType eq 'BUTTON'then begin
    ;BUTTON approach
    ;    xSize=self->getMultipleXSize(xSel)
    ;    ySize=self->getMultipleYSize(ySel)
    ;doLog,xSize, ySize
  
    multipleNonExclusiveBase = widget_Base(multiBase, UNAME='', $
      XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE,$
      SPACE=0 ,XPAD=0 ,YPAD=0, ROW=ROW, COLUMN=COLUMN)
    for i=0, avSelectionNo-1 do begin
      ;Use find by uname!!!!
      ;build non exclusive buttons
      list[i]= widget_button(multipleNonExclusiveBase, $
        XOFFSET=0 ,YOFFSET=0, VALUE=elementsName[i], UNAME=eventTag+elementsCode[i], $
        UVALUE=elementsCode[i], SCR_XSIZE=elementXSize, SCR_YSIZE=elementYSize > self->getLabelYSize(), $
        event_pro=eventProName)
    endfor
  endif else begin
    ;    list=widget_list(multiBase, uvalue=elementsCode, uname=eventTag+elementsCode, value=elementsName, $
    list=widget_list(multiBase, uvalue=elementsCode, value=elementsName, $
      SCR_XSIZE=listXSize, SCR_YSIZE=listYSize, $
      event_pro=eventProName, /MULTIPLE)
  ;LIST approach
  endelse
  
  return, list
  
END

PRO RunSelectionGUI::buildROISection, base

  ;elementsCode=self.info->getAllRoiCodes()
  ;elementsName=self.info->getAllRoiNames()
  elementsCode=self.info->getAllRoiCodesBySelectedRun()
  elementsName=self.info->getAllRoiNamesBySelectedRun()
  
  eventTag='ROI*'
  eventName='roi_selection'
  ySize=self->getSelectionListYSize()
  title=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:RoiSectionTitle')
  ;title='ROI'
  ;wType='BUTTON' ; TYPE='LIST'
  wType='LIST'
  ALLOPTION=1
  
  self.roiMultipleWid=ptr_new(self->buildMultiSelectionList(base, wType, title, elementsName, elementsCode, $
    eventName, eventTag, xSize, ySize, ALLOPTION=ALLOPTION, /COLUMN, /FRAME, /SCROLL), /NO_COPY)
;widget_control, base, sensitive=self.info->getSelectedRunUseROISelection()
    
END

PRO RunSelectionGUI::buildResolutionSection, base

  elementsCode=self.info->getAllResolutionCodes()
  elementsName=self.info->getAllResolutionNames()
  eventTag='RESOLUTION*'
  eventName='resolution_selection'
  title=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:ResolutionSectionTitle')
  ySize=self->getSelectionListYSize()
  ;title='Resolution'
  wType='BUTTON' ; TYPE='LIST'
  ALLOPTION=0
  
  self.resolutionMultipleWid=ptr_new(self->buildMultiSelectionList(base, wType, title, elementsName, elementsCode, $
    eventName, eventTag, xSize, ySize, ALLOPTION=ALLOPTION, /FRAME, UNAME='RESOLUTIONBASE', /SCROLL), /NO_COPY)
;widget_control, base, sensitive=self.info->getSelectedRunUseResolutionSelection()
    
END

PRO RunSelectionGUI::buildMonthSection, base

  elementsCode=self.info->getAllMonthCodes()
  elementsName=self.info->getAllMonthNames()
  eventTag='MONTH*'
  eventName='month_selection'
  title=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:MonthSectionTitle')
  ;title='Month'
  ysize=self->getSelectionListYSize()
  wType='BUTTON' ; TYPE='LIST'
  ALLOPTION=1
  
  self.monthMultipleWid=ptr_new(self->buildMultiSelectionList(base, wType, title, elementsName, elementsCode, $
    eventName, eventTag, xSize, ySize, ALLOPTION=ALLOPTION, /COLUMN, /FRAME, UNAME='MONTHBASE', /SCROLL), /NO_COPY)
;widget_control, base, sensitive=self.info->getSelectedRunUseMonthSelection()
    
END

PRO RunSelectionGUI::buildYearSection, base

  elementsCode=self.info->getAllYearCodes()
  elementsName=self.info->getAllYearNames()
  eventTag='YEAR*'
  eventName='year_selection'
  title=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:YearSectionTitle')
  ySize=self->getSelectionListYSize()
  title='Year'
  wType='BUTTON' ; TYPE='LIST'
  ALLOPTION=1
  
  self.yearMultipleWid=ptr_new(self->buildMultiSelectionList(base, wType, title, elementsName, elementsCode, $
    eventName, eventTag, xSize, ySize, ALLOPTION=ALLOPTION, /FRAME, UNAME='YEARBASE', /SCROLL), /NO_COPY)
;widget_control, base, sensitive=self.info->getSelectedRunUseYearSelection()
    
END

;FUNCTION RunSelectionGUI::buildFileList, refBase, title, elementsName, elementsCode, eventName, eventTag, SELECTABLE=SELECTABLE, MULTIPLE=MULTIPLE
;
;  fileListBase = widget_base(refBase, $
;    XOFFSET=0 ,YOFFSET=0, $
;    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN, /ALIGN_CENTER)
;
;  ;fileListTitle=widget_label(fileListBase, value=title, scr_ysize=self->getLabelYSize(), /ALIGN_CENTER, font=self.titleFont)
;  fileListTitle=widget_text(fileListBase, value=title, scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
;
;  return, fileList=widget_list(fileListBase, value=[''], $
;    SCR_XSIZE=self->getModelListBoxXSize(), SCR_YSIZE=self->getModelListBoxYSize(), $
;    event_pro=self.eventPrefix+eventName, MULTIPLE=MULTIPLE, SENSITIVE=SELECTABLE)
;
;END

PRO RunSelectionGUI::buildRunSection, refBase

  runOptionBase=widget_base(refBase,xpad=0, ypad=0,space=0,/COLUMN)
  self->buildRunSelectionSection, runOptionBase
  self->buildFilesInfoSection, runOptionBase
  
END

PRO RunSelectionGUI::buildFilesInfoSection, refBase

  sourceFolderName=self.info->getSelectedSourceFolderName(/WITHFILTER)
  outputFolderName=self.info->getSelectedOutputFolderName(/WITHFILTER)
  
  sourceFilesName=self.info->getFolderContents(sourceFolderName, /NOPATH)
  outputFilesName=self.info->getFolderContents(outputFolderName, /NOPATH)
  
  ;  inputFilesName=self.info->getSelectedInputFiles()
  ;  outputFilesName=self.info->getSelectedOutputFiles()
  
  scrXSize=self->getFileListXSize()
  scrYSize=self->getFileListYSize()
  
  fileListBase=widget_base(refBase,xpad=0, ypad=0,space=0,/ROW)
  leftListBase=widget_base(fileListBase,xpad=0, ypad=0,space=0,/COLUMN)
  
  elementsNames=sourceFilesName
  elementsCode=strcompress(indgen(n_elements(sourceFilesName)), /REMOVE)
  eventTag='INPUTFILE*'
  eventName='inputfile_selection'
  title=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:FileSourceSectionTitle', placeHolders=[sourceFolderName[0]])
  ;title='Source'+ '('+sourceFolderName[0]+')'
  wType='LIST'
  
  self.sourceFilesMultipleWid=ptr_new(self->buildMultiSelectionList(leftListBase, wType, title, elementsNames, elementsCode, $
    eventName, eventTag, scrXSize, scrYSize*2, /COLUMN, /TITLEASTEXT), /NO_COPY)
    
  fileInfoBase=widget_base(leftListBase,xpad=0, ypad=0,space=0,/ROW)
  
  fileSelectedLabel=widget_label(fileInfoBase, SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=scrXSize/3, UNAME='FILEINFOSEL', font=self.titleFont, VALUE='0')
  separatorLabel=widget_label(fileInfoBase, SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=scrXSize/6, font=self.titleFont, VALUE='/')
  fileAvailableLabel=widget_label(fileInfoBase, SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=scrXSize/3, UNAME='FILEINFOAVA', font=self.titleFont, VALUE='0')
  
  elementsNames=outputFilesName
  elementsCode=strcompress(indgen(n_elements(outputFilesName)), /REMOVE)
  eventTag='OUTPUTFILE*'
  eventName='outputfile_selection'
  title=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:FileResultSectionTitle', placeHolders=[outputFolderName[0]])
  ;title='Output'+ '('+outputFolderName[0]+')'
  wType='LIST' ; TYPE='LIST'
  
  self.destFilesMultipleWid=ptr_new(self->buildMultiSelectionList(fileListBase, wType, title, elementsNames, elementsCode, $
    eventName, eventTag, scrXSize, scrYSize*2, /COLUMN, /TITLEASTEXT), /NO_COPY)
    
END

PRO RunSelectionGUI::buildRunSelectionSection, base

  title='Run option'
  
  runSectionBase=widget_base(base, $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  runTitle=widget_label(runSectionBase, value=title, SCR_YSIZE=self->getLabelYSize(), SCR_XSIZE=self->getRunSectionXSize(), /ALIGN_LEFT, font=self.titleFont)
  
  dropBase=widget_base(runSectionBase, $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN)
    
  runSelectionBase=widget_base(dropBase, $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN, /ALIGN_LEFT)
    
  runNames=self.info->getAllRunNames()
  runCodes=self.info->getAllRunCodes()
  
  ;runSelectionTitle=widget_label(runSelectionBase, value=title, scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  ;self.runProcessDropList=widget_droplist(runSelectionBase, value=runNames, $
  self.runList=widget_combobox(runSelectionBase, value=runNames, $
    SCR_XSIZE=self->getRunDropListXSize(), SCR_YSIZE=self->getRunDropListYSize(), $
    event_pro=self.eventPrefix+'runListSelection', UVALUE=runCodes)
    
  optionFlagsBase=widget_base(dropBase, $
    XOFFSET=0 ,YOFFSET=0, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /COLUMN, /ALIGN_LEFT)
    
  overwriteResultFlagSelectionBase = widget_base(optionFlagsBase, $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW, /ALIGN_LEFT)
  ;deleteFlagTitle=widget_label(removeSourceFlagBase, value='Delete input after run', scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  ;doLog,self->getCancelFlagXSize()
  self.overwriteResultFlagButton = widget_button(overwriteResultFlagSelectionBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:OverwriteResultFlagTitle'), event_pro=self.eventPrefix+'overwriteResultFlagButton', $
    ;XOFFSET=0 ,YOFFSET=0, VALUE='Remove input/intermediate files after successful run', event_pro=self.eventPrefix+'removeSourceFilesFlagButton', $
    SCR_YSIZE=self->getLabelYSize() > self->getCancelFlagYSize(), sensitive=1, /ALIGN_LEFT)
  ;SCR_YSIZE=self->getLabelYSize() > self->getCancelFlagYSize(), sensitive=1, SCR_XSIZE=self->getCancelFlagXSize(), /ALIGN_RIGHT)

  deleteInputFlagSelectionBase = widget_base(optionFlagsBase, $
    XOFFSET=0 ,YOFFSET=0, /NONEXCLUSIVE, $
    SPACE=0 ,XPAD=0 ,YPAD=0, /ROW, /ALIGN_LEFT)
  ;deleteFlagTitle=widget_label(removeSourceFlagBase, value='Delete input after run', scr_ysize=self->getLabelYSize(), /ALIGN_LEFT, font=self.titleFont)
  ;doLog,self->getCancelFlagXSize()
  self.deleteInputFlagButton = widget_button(deleteInputFlagSelectionBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE=self.mgr->getLangCatLabelFromKey('RunSelectionGUI:DeleteInputFlagTitle'), event_pro=self.eventPrefix+'deleteInputFlagButton', $
    ;XOFFSET=0 ,YOFFSET=0, VALUE='Remove input/intermediate files after successful run', event_pro=self.eventPrefix+'removeSourceFilesFlagButton', $
    SCR_YSIZE=self->getLabelYSize() > self->getCancelFlagYSize(), sensitive=1, /ALIGN_LEFT)

  self.runDescriptionText = widget_text(runSectionBase, $
    XOFFSET=0 ,YOFFSET=0, VALUE='', SCR_XSIZE=self->getRunDropListXSize()+self->getCancelFlagXSize(), $
    SCR_YSIZE=self->getLabelYSize()*3, sensitive=1, /ALIGN_LEFT, /WRAP, /SCROLL)
    
END

PRO RunSelectionGUI::buildListsSelection, refBase

  ;self.roiBase=widget_base(refBase,xpad=0, ypad=0,space=0, frame=0, /COLUMN)
  listsBase1=widget_base(refBase,xpad=1, ypad=1,space=1,/COLUMN)
  listsBase2=widget_base(refBase,xpad=1, ypad=1,space=1,/COLUMN)
  
  self.roiBase=widget_base(listsBase1,xpad=0, ypad=0,space=0,FRAME=0,/COLUMN)
  roiBase=widget_base(self.roiBase,xpad=0, ypad=0,space=0,FRAME=0,/COLUMN, UNAME='ROIBASE')
  
  self->buildROISection, roiBase
  self->buildResolutionSection, listsBase1
  
  self->buildYearSection, listsBase2
  self->buildmonthSection, listsBase2
  
END

PRO RunSelectionGUI::build

  ;bestDimensions=self->getBestDimensions()
  title=self->getTitle()
  
  base = widget_base(/COLUMN, $
    uvalue=self, TLB_FRAME_ATTR=1, $
    title=title, /TLB_KILL_REQUEST_EVENTS, event_pro=self.eventprefix+'destroyWindow')
  ;xsize=self->getWindowXsize(), ysize=self->getWindowYsize(), $
    
  mainBase = widget_base(base, /ROW)
  
  selectionSectionBase = widget_base(mainbase,xpad=1, ypad=1,space=0,/ROW)
  
  operationlButtonsBase = widget_base(mainbase,xpad=0, ypad=0,space=0,/COLUMN)
  
  
  self->buildListsSelection, selectionSectionBase
  
  self->buildRunSection, selectionSectionBase
  
  self->buildOperationButtonSection, operationlButtonsBase
  
  self->setTopBase, base
  xmanager, 'oxyrisk', base, /JUST_REG
  
END

;geometry
FUNCTION RunSelectionGUI::getLabelYSize

  fs=self.mgr->getFileSystem()
  if (fs->isOSWin()) then return, 20
  if (fs->isOSUnix()) then return, 20
  if (fs->MacOS()) then return, 20
  
END

FUNCTION RunSelectionGUI::getWindowXSize

  return, self->getVisibleXSize()*.50
  
END

FUNCTION RunSelectionGUI::getWindowYSize

  return, self->getVisibleYSize()*.50
  
END

FUNCTION RunSelectionGUI::getSelectionSectionYSize

  return, self->getWindowYSize()*0.8
  
END

FUNCTION RunSelectionGUI::getSelectionSectionXSize

  return, self->getWindowXSize()
  
END

FUNCTION RunSelectionGUI::getButtonSectionXSize

  return, self->getWindowXSize()
  
END

FUNCTION RunSelectionGUI::getButtonSectionYSize

  return, self->getWindowYSize()-self->getSelectionSectionYSize()
  
END

FUNCTION RunSelectionGUI::getListsSectionXSize

  return, self->getSelectionSectionXSize()*.2
  
END

FUNCTION RunSelectionGUI::getListsSectionYSize

  return, self->getSelectionSectionYSize()
  
END

FUNCTION RunSelectionGUI::getRunSectionXSize

  return, self->getWindowXSize()-self->getListsSectionXSize()
  
END

FUNCTION RunSelectionGUI::getRunSectionYSize

  return, self->getListsSectionYSize()
  
END

FUNCTION RunSelectionGUI::getChooseListXSize

  return, self->getListsSectionXSize()*.50
  
END

FUNCTION RunSelectionGUI::getChooseListYSize

  return, self->getListsSectionYSize()*.50
  
END

FUNCTION RunSelectionGUI::getFileListXSize

  return, self->getRunSectionXSize()*.5
  
END

FUNCTION RunSelectionGUI::getFileListYSize

  return, self->getRunSectionYSize()*.85
  
END

FUNCTION RunSelectionGUI::getAvailableYSize

 return, (self->getMainBigButtonDimension())[1]*8

END

FUNCTION RunSelectionGUI::getSelectionListYSize

 return, self->getAvailableYSize()/2

END

FUNCTION RunSelectionGUI::getRunDropListXSize

  return, self->getRunSectionXSize()*.98
  ;return, self->getRunSectionXSize()*.85
  
END

FUNCTION RunSelectionGUI::getRunDropListYSize

  return, self->getRunSectionYSize()-self->getFileListYSize()
  
END

FUNCTION RunSelectionGUI::getCancelFlagXSize

  return, self->getRunSectionXSize()-self->getRunDropListXSize()
  
END

FUNCTION RunSelectionGUI::getCancelFlagYSize

  return, self->getRunDropListYSize()
  
END

FUNCTION RunSelectionGUI::getSingleElementMultipleXSize, elNo

  if n_elements(elNo) eq 0 then splitNo=1 else splitNo=elNo
  return, self->getListsSectionXSize()*(1./splitNo)
  
END

FUNCTION RunSelectionGUI::getSingleElementMultipleYSize, elNo

  if n_elements(elNo) eq 0 then splitNo=1 else splitNo=elNo
  return, self->getListsSectionYSize()*(1./splitNo)
  
END

;FUNCTION RunSelectionGUI::getOperationalButtonXSize
;
;  return, 70
;  
;END

FUNCTION RunSelectionGUI::getOperationalButtonXSize

  return, 30
  
END

PRO RunSelectionGUI::configure

  ; Realize and MoveToCenter is done by superclass
  ; Here only fill & set internal widget
  saveList=self.info->getSourceFileSelectedCodes(NOSELECTION=NOFILESELECTION)
  
  rois=self.info->getRoiCodesSelected(NOSELECTION=NOROISELECTION)
  years=self.info->getYearCodesSelected(NOSELECTION=NOYEARSELECTION)
  resolutions=self.info->getResolutionCodesSelected(NOSELECTION=NORESSELECTION)
  months=self.info->getMonthCodesSelected(NOSELECTION=NOMONTHSELECTION)
  
  self->userRunSelection, self.info->getRunCodeSelected(), /SILENT, /NORUNUPDATE
  ;self->userRemoveSourceFilesFlag, self.info->getRemoveSourceFilesFlag()
  self->userDeleteInputFlag, self.info->getDeleteInputFlag()
  self->userOverwriteResultFlag, self.info->getOverwriteResultFlag()
  
  self.info->setSourceFileSelectedCodes, saveList, VOID=NOFILESELECTION
  indexes=self.info->getSourceFileSelectedIndexes()
  codes=self.info->getSourceFileSelectedCodes()

  ;removeFlag=self.info->getRemoveSourceFilesFlag()
  deleteInputFlag=self.info->getDeleteInputFlag()
  overwriteResultFlag=self.info->getOverwriteResultFlag()
  
  ;self->userSourceFileSelections, indexes, codes
  self->userSourceFileSelections, /SETALL
  ;self->userRemoveSourceFileFlag, removeFlag
  self->userDeleteInputFlag, deleteInputFlag
  self->userOverwriteResultFlag, overwriteResultFlag
  
  allWid=widget_info(self->getTopBase(), FIND_BY_UNAME='ROI*'+'ALL')
  if allWid ne 0 then widget_control, allWid, set_button=self.info->getAllROIFlag()
  
  allWid=widget_info(self->getTopBase(), FIND_BY_UNAME='RESOLUTION*'+'ALL')
  if allWid ne 0 then widget_control, allWid, set_button=self.info->getAllResolutionFlag()
  
  allWid=widget_info(self->getTopBase(), FIND_BY_UNAME='MONTH*'+'ALL')
  if allWid ne 0 then widget_control, allWid, set_button=self.info->getAllMonthFlag()
  
  allWid=widget_info(self->getTopBase(), FIND_BY_UNAME='YEAR*'+'ALL')
  if allWid ne 0 then widget_control, allWid, set_button=self.info->getAllYearFlag()
  
  self->allROISetting
  self->userROISelections, rois;, /NORUNUPDATE
  self->allYearSetting
  self->userYearSelections, years;, /NORUNUPDATE
  self->allMonthSetting
  self->userMonthSelections, months;, /NORUNUPDATE
  self->allResolutionSetting
  self->userResolutionSelections, resolutions;, /NORUNUPDATE
  
END

; MM 22/11/2011 end new code

; constructor/destructor
FUNCTION RunSelectionGUI::init, info, mgr, fonts=fonts

  if not self -> InfoSelectionGUI :: init(info, mgr, fonts=fonts) then return , 0
  return , 1
  
END

PRO RunSelectionGUI::cleanUp

  ptr_free, self.roiMultipleWid
  ptr_free, self.resolutionMultipleWid
  ptr_free, self.yearMultipleWid
  ptr_free, self.monthMultipleWid
  ptr_free, self.sourceFilesMultipleWid
  ptr_free, self.destFilesMultipleWid
  self -> InfoSelectionGUI::cleanUp
  
END

;****************************************************************************************

PRO RunSelectionGUI__Define

  Struct = { RunSelectionGUI , $
    runList: 0l, $
    runDescriptionText: 0l, $
    roiMultipleWid: ptr_new(), $
    resolutionMultipleWid: ptr_new(), $
    yearMultipleWid: ptr_new(), $
    monthMultipleWid: ptr_new(), $
    sourceFilesMultipleWid: ptr_new(), $
    destFilesMultipleWid: ptr_new(), $
    singleButtonFlag: 0b, $
    silentMode: 0, $
    ;removeSourceFlagButton: 0l, $
    deleteInputFlagButton: 0l, $
    overwriteResultFlagButton: 0l, $
    roiBase: 0l, $
    Inherits InfoSelectionGUI $
    }
    
END

;****************************************************************************************

