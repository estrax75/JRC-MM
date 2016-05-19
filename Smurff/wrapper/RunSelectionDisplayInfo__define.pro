FUNCTION RunSelectionDisplayInfo::getVersion

  return, '2.0'
  
END

FUNCTION RunSelectionDisplayInfo::getUseSelection

  return, self.runInfo->getUseSelectionByRunCode(self.runCodeSelected)
  
END

FUNCTION RunSelectionDisplayInfo::getSelectedRunUseYearSelection

  values=self.runInfo->getUseSelectionByRunCode(self.runCodeSelected)
  return, fix(strmid(values, 2, 1)) eq 1
  
END

FUNCTION RunSelectionDisplayInfo::getSelectedRunUseInputFilesSelection

  values=self.runInfo->getUseSelectionByRunCode(self.runCodeSelected)
  return, fix(strmid(values, 4, 1)) eq 1
  
END

FUNCTION RunSelectionDisplayInfo::getSelectedRunUseMonthSelection

  values=self.runInfo->getUseSelectionByRunCode(self.runCodeSelected)
  return, fix(strmid(values, 3, 1)) eq 1
  
END

FUNCTION RunSelectionDisplayInfo::getSelectedRunUseResolutionSelection

  values=self.runInfo->getUseSelectionByRunCode(self.runCodeSelected)
  return, fix(strmid(values, 1, 1)) eq 1
  
END

FUNCTION RunSelectionDisplayInfo::getSelectedRunUseROISelection

  values=self.runInfo->getUseSelectionByRunCode(self.runCodeSelected)
  return, fix(strmid(values, 0, 1)) eq 1
  
END

FUNCTION RunSelectionDisplayInfo::getSourceFileInfo, NAME_ORDERED=NAME_ORDERED, displayCodes=displayCodes, NOSELECTION=NOSELECTION

  codes=self->queryRunElements()
  NOSELECTION=0
  if codes[0] eq -1 then begin
    NOSELECTION=1
    return, [-1]
  endif
  fileList=self.dataRuns->getFilesByRunCodes(codes, NAME_ORDERED=NAME_ORDERED, OUTSORTSUB=OUTSORTSUB)
  if keyword_set(NAME_ORDERED) then displayCodes=codes[OUTSORTSUB] else displayCodes=codes
  return,  fileList
  
END

FUNCTION RunSelectionDisplayInfo::getSourceFileSelectedIndexes

  displayCodes=self->queryRunElements()
  fileList=self.dataRuns->getFilesByRunCodes(displayCodes, /NAME_ORDERED, OUTSORTSUB=OUTSORTSUB)
  displayCodes=displayCodes[OUTSORTSUB]
  selectedCodes=self->getSourceFileSelectedCodes()
  nElem=n_elements(selectedCodes)
  indexes=intarr(nElem)
  for i=0, nElem-1 do indexes[i]=(where(selectedCodes[i] eq displayCodes))[0]
  return, indexes
  
END

PRO RunSelectionDisplayInfo::setSourceFileSelectedIndexes, indexes

  displayCodes=self->queryRunElements()
  if displayCodes ne '-1' then begin
    fileList=self.dataRuns->getFilesByRunCodes(displayCodes, /NAME_ORDERED, OUTSORTSUB=OUTSORTSUB, NOTFOUND=NOTFOUND)
    displayCodes=displayCodes[OUTSORTSUB]
    selectedCodes=displayCodes[indexes]
    self->setSourceFileSelectedCodes, selectedCodes
  endif
  
END

PRO RunSelectionDisplayInfo::setSourceFileSelectedCodes, codes, VOID=VOID

  ptr_free, self.sourceFileSelectedCodes
  ptr_free, self.sourceFileNameSelected
  if n_elements(codes) eq 0 then return
  if codes[0] ne -1 then begin
    doLog,'**********', level=0
    fileList=self.dataRuns->getFilesByRunCodes(codes, /NAME_ORDERED)
    self->setSourceFileNameSelected, fileList
    self.sourceFileSelectedCodes=ptr_new(codes, /NO_COPY)
  endif
  
END

PRO RunSelectionDisplayInfo::setSourceFileNameSelected, list

  ptr_free, self.sourceFileNameSelected
  if strcompress(list[0], /REMOVE) ne '-1' then begin
    self.sourceFileNameSelected=ptr_new(list, /NO_COPY)
  endif
  
END

FUNCTION RunSelectionDisplayInfo::getSourceFileSelectedCodes, NOSELECTION=NOSELECTION

  NOSELECTION=1
  if ptr_valid(self.sourceFileSelectedCodes) then begin
    NOSELECTION=0
    list=*self.sourceFileSelectedCodes
    if list[0] eq 'null' and n_elements(list) eq 1 then return, '-1'
    return, list
  endif
  return, -1
  
END

FUNCTION RunSelectionDisplayInfo::getSourceFileNameSelected, NOSELECTION=NOSELECTION

  NOSELECTION=1
  if ptr_valid(self.sourceFileNameSelected) then begin
    NOSELECTION=0
    list=*self.sourceFileNameSelected
    return, list
  endif
  return, -1
  
END

PRO RunSelectionDisplayInfo::updateRunElements, view, SILENT=SILENT

  prevSilentMode=self.silentMode
  self.silentMode=keyword_set(SILENT)
  codes=self->queryRunElements()
  
  if self->getSelectedRunUseInputFilesSelection() then begin
    if codes[0] eq -1 then begin
      if ~(self.silentMode) then test=view->dialogMessage(view->getLangCatLabelFromKey('DialogMessage:NoSourceFileAvailable'), title=view->getLangCatLabelFromKey('DialogMessageTitle:NoSourceFileAvailable'))
      view->updateSourceFileInfo, /VOID
    endif else begin
      view->updateSourceFileInfo
    endelse
  endif else begin
    view->updateSourceFileInfo, /VOID
  endelse
  self.silentMode=prevSilentMode
  
END

FUNCTION RunSelectionDisplayInfo::queryRunElements, ALLYEAR=ALLYEAR, ALLRESOLUTION=ALLRESOLUTION, $
    ALLROI=ALLROI, ALLMONTH=ALLMONTH, NAME_ORDERED=NAME_ORDERED
    
    
  allCodes=self.dataRuns->getCodes()
  
  if keyword_set(ALLYEAR) then yearCodes=self->getAllYearCodes() else yearCodes=self->getYearCodesSelected()
  if keyword_set(ALLRESOLUTION) then resolutionCodes=self->getAllResolutionCodes() else resolutionCodes=self->getResolutionCodesSelected()
  if keyword_set(ALLROI) then roiCodes=self->getAllROICodes() else roiCodes=self->getRoiCodesSelected()
  if keyword_set(ALLMONTH) then monthCodes=self->getAllMonthCodes() else monthCodes=self->getMonthCodesSelected()
  
  filteredCodes=self.dataRuns->filterByROI(roiCodes, allCodes, NOTFOUND=NOTFOUND)
  if keyword_set(NOTFOUND) then return, ['-1']
  filteredCodes=self.dataRuns->filterByYear(yearCodes, filteredCodes, NOTFOUND=NOTFOUND)
  if keyword_set(NOTFOUND) then return, ['-1']
  filteredCodes=self.dataRuns->filterByMonth(monthCodes, filteredCodes, NOTFOUND=NOTFOUND)
  if keyword_set(NOTFOUND) then return, ['-1']
  filteredCodes=self.dataRuns->filterByResolution(resolutionCodes, filteredCodes, NOTFOUND=NOTFOUND)
  if keyword_set(NOTFOUND) then return, ['-1']
  return, filteredCodes
  
END

FUNCTION RunSelectionDisplayInfo::getAllROIFlag

  return, self.allROIFlag
  
END

PRO RunSelectionDisplayInfo::setAllROIFlag, flag

  self.allROIFlag=flag
  
END

FUNCTION RunSelectionDisplayInfo::getAllSourceFilesFlag

  return, self.allSourceFilesFlag
  
END

PRO RunSelectionDisplayInfo::setAllSourceFilesFlag, flag

  self.allSourceFilesFlag=flag
  
END

PRO RunSelectionDisplayInfo::switchAllSourceFilesFlag

  self.allSourceFilesFlag=1-self.allSourceFilesFlag
  
END

PRO RunSelectionDisplayInfo::switchAllROIFlag

  self.allROIFlag=1-self.allROIFlag
  
END

FUNCTION RunSelectionDisplayInfo::getAllResolutionFlag

  return, self.allResolutionFlag
  
END

PRO RunSelectionDisplayInfo::setAllResolutionFlag, flag

  self.allResolutionFlag=flag
  
END

PRO RunSelectionDisplayInfo::switchAllResolutionFlag

  self.allResolutionFlag=1-self.allResolutionFlag
  
END

FUNCTION RunSelectionDisplayInfo::getAllYearFlag

  return, self.allYearFlag
  
END

PRO RunSelectionDisplayInfo::setAllYearFlag, flag

  self.allYearFlag=flag
  
END

PRO RunSelectionDisplayInfo::switchAllYearFlag

  self.allYearFlag=1-self.allYearFlag
  
END

FUNCTION RunSelectionDisplayInfo::getAllMonthFlag

  return, self.allMonthFlag
  
END

PRO RunSelectionDisplayInfo::setAllMonthFlag, flag

  self.allMonthFlag=flag
  
END

PRO RunSelectionDisplayInfo::switchAllMonthFlag

  self.allMonthFlag=1-self.allMonthFlag
  
END

FUNCTION RunSelectionDisplayInfo::getMonthIndexesOfCodes, codes

  idxs=self.monthInfo->getIndexesOfCodes(codes)
  return, idxs
  
END

FUNCTION RunSelectionDisplayInfo::getResolutionIndexesOfCodes, codes

  idxs=self.resolutionInfo->getIndexesOfCodes(codes)
  return, idxs
  
END

FUNCTION RunSelectionDisplayInfo::getYearIndexesOfCodes, codes

  idxs=self.yearInfo->getIndexesOfCodes(codes)
  return, idxs
  
END

FUNCTION RunSelectionDisplayInfo::getROIIndexesOfCodes, codes, NOSELECTION=NOSELECTION

  idxs=self.roiInfo->getIndexesOfCodes(codes, self.runCodeSelected, NOSELECTION=NOSELECTION)
  return, idxs
  
END

FUNCTION RunSelectionDisplayInfo::getRoiCodesSelected, NOSELECTION=NOSELECTION

  NOSELECTION=1
  if ptr_valid(self.roiCodesSelected) then begin
    list=*self.roiCodesSelected
    if list[0] eq 'null' and n_elements(list) eq 1 then return, '-1'
    NOSELECTION=0
    return, list
  endif
  return, '-1'
  
END

PRO RunSelectionDisplayInfo::setRoiCodesSelected, codes, NOSELECTION=NOSELECTION

  ptr_free, self.roiCodesSelected
  if keyword_set(NOSELECTION) then return
  self.roiCodesSelected=ptr_new(codes, /NO_COPY)
  
END

PRO RunSelectionDisplayInfo::removeROICodeSelected, code

  list=self->getRoiCodesSelected(NOSELECTION=NOSELECTION)
  idxs=where(code ne list, count)
  if count eq 0 then begin
    ptr_free, self.roiCodesSelected
    return
  endif
  self->setRoiCodesSelected, list[idxs]
  
END

PRO RunSelectionDisplayInfo::addRoiCodeSelected, code

  list=self->getRoiCodesSelected(NOSELECTION=NOSELECTION)
  if NOSELECTION then list=code else list=[list, code]
  self->setRoiCodesSelected, list
  
END

FUNCTION RunSelectionDisplayInfo::getYearCodesSelected, NOSELECTION=NOSELECTION

  NOSELECTION=1
  if ptr_valid(self.yearCodesSelected) then begin
    NOSELECTION=0
    list=*self.yearCodesSelected
    return, list
  endif
  return, -1
  
END

PRO RunSelectionDisplayInfo::setYearCodesSelected, codes, NOSELECTION=NOSELECTION

  ptr_free, self.yearCodesSelected
  self.yearCodesSelected=ptr_new(codes, /NO_COPY)
  
END

PRO RunSelectionDisplayInfo::addYearCodeSelected, code

  list=self->getYearCodesSelected(NOSELECTION=NOSELECTION)
  if NOSELECTION then list=code else list=[list, code]
  ;doLog,list
  self->setYearCodesSelected, list
  
END

PRO RunSelectionDisplayInfo::removeYearCodeSelected, code

  list=self->getYearCodesSelected(NOSELECTION=NOSELECTION)
  idxs=where(code ne list, count)
  if count eq 0 then begin
    ptr_free, self.yearCodesSelected
    return
  endif
  ;doLog,list[idxs]
  self->setYearCodesSelected, list[idxs]
  
END

FUNCTION RunSelectionDisplayInfo::getResolutionCodesSelected, NOSELECTION=NOSELECTION

  NOSELECTION=1
  if ptr_valid(self.resolutionCodesSelected) then begin
    NOSELECTION=0
    list=*self.resolutionCodesSelected
    return, list
  endif
  return, -1
  
END

PRO RunSelectionDisplayInfo::setResolutionCodesSelected, codes, NOSELECTION=NOSELECTION

  ptr_free, self.resolutionCodesSelected
  self.resolutionCodesSelected=ptr_new(codes, /NO_COPY)
  
END

PRO RunSelectionDisplayInfo::addResolutionCodeSelected, code

  list=self->getResolutionCodesSelected(NOSELECTION=NOSELECTION)
  if NOSELECTION then list=code else list=[list, code]
  self->setresolutionCodesSelected, list
  
END

PRO RunSelectionDisplayInfo::removeResolutionCodeSelected, code

  list=self->getResolutionCodesSelected(NOSELECTION=NOSELECTION)
  idxs=where(code ne list, count)
  if count eq 0 then begin
    ptr_free, self.resolutionCodesSelected
    return
  endif
  self->setResolutionCodesSelected, list[idxs]
  
END

FUNCTION RunSelectionDisplayInfo::getMonthCodesSelected, NOSELECTION=NOSELECTION

  NOSELECTION=1
  if ptr_valid(self.monthCodesSelected) then begin
    NOSELECTION=0
    list=*self.monthCodesSelected
    return, list
  endif
  return, -1
  
END

PRO RunSelectionDisplayInfo::setMonthCodesSelected, codes, NOSELECTION=NOSELECTION

  ptr_free, self.monthCodesSelected
  self.monthCodesSelected=ptr_new(codes, /NO_COPY)
  
END

PRO RunSelectionDisplayInfo::addMonthCodeSelected, code

  list=self->getmonthCodesSelected(NOSELECTION=NOSELECTION)
  if NOSELECTION then list=code else list=[list, code]
  ;doLog,list
  self->setmonthCodesSelected, list
  
END

PRO RunSelectionDisplayInfo::removeMonthCodeSelected, code

  list=self->getMonthCodesSelected(NOSELECTION=NOSELECTION)
  idxs=where(code ne list, count)
  if count eq 0 then begin
    ptr_free, self.MonthCodesSelected
    return
  endif
  ;doLog,list[idxs]
  self->setMonthCodesSelected, list[idxs]
  
END

FUNCTION RunSelectionDisplayInfo::getRunCodeSelected, NOSELECTION=NOSELECTION

  return, self.runCodeSelected
  
END

PRO RunSelectionDisplayInfo::setRunCodeSelected, code, NOSELECTION=NOSELECTION

  self.runCodeSelected=code
  
END

FUNCTION RunSelectionDisplayInfo::getDeleteInputFlag

  return, self.deleteInputFlag
  
END

PRO RunSelectionDisplayInfo::setDeleteInputFlag, selection

  self.deleteInputFlag=selection
  
END

FUNCTION RunSelectionDisplayInfo::getOverwriteResultFlag

  return, self.OverwriteResultFlag
  
END

PRO RunSelectionDisplayInfo::setOverwriteResultFlag, selection

  self.overwriteResultFlag=selection
  
END

FUNCTION RunSelectionDisplayInfo::getFolderContents, fullPathSearch, NOPATH=NOPATH

  return, self.runInfo->getFolderContents(fullPathSearch, NOPATH=NOPATH)
  
END

FUNCTION RunSelectionDisplayInfo::getSelectedRunDescription

  description=self.runInfo->getDescriptionByRunCode(self.runCodeSelected)
  return, description
  
END

FUNCTION RunSelectionDisplayInfo::getSelectedSourceFolderName, WITHFILTER=WITHFILTER

  folderName=self.runInfo->getInputFolderNameByRunCode(self.runCodeSelected, WITHFILTER=WITHFILTER)
  return, folderName
  
END

FUNCTION RunSelectionDisplayInfo::getSelectedOutputFolderName, WITHFILTER=WITHFILTER

  folderName=self.runInfo->getOutputFolderNameByRunCode(self.runCodeSelected, WITHFILTER=WITHFILTER)
  return, folderName
  
END

FUNCTION RunSelectionDisplayInfo::getAllRoiCodesBySelectedRun

  return, self.roiInfo->getCodesByRun(self.runCodeSelected)
  
END

FUNCTION RunSelectionDisplayInfo::getAllRoiNamesBySelectedRun

  codes=self.roiInfo->getCodesByRun(self.runCodeSelected)
  return, self.roiInfo->getDisplayNamesByCodes(codes)
  
END

FUNCTION RunSelectionDisplayInfo::getAllResolutionCodes

  return, self.resolutionInfo->getCodes()
  
END

FUNCTION RunSelectionDisplayInfo::getAllResolutionNames

  return, self.resolutionInfo->getDisplayNames()
  
END

FUNCTION RunSelectionDisplayInfo::getAllYearCodes

  return, self.yearInfo->getCodes()
  
END

FUNCTION RunSelectionDisplayInfo::getAllYearNames

  return, self.yearInfo->getDisplayNames()
  
END

FUNCTION RunSelectionDisplayInfo::getAllMonthCodes

  return, self.monthInfo->getCodes()
  
END

FUNCTION RunSelectionDisplayInfo::getAllMonthNames

  return, self.monthInfo->getDisplayNames()
  
END

FUNCTION RunSelectionDisplayInfo::getSelectedSourceFiles

  return, self.runInfo->getSourceFolderNameByRunCode(self.runCodeSelected)
  
END

FUNCTION RunSelectionDisplayInfo::getOutputFolderContents

  folderName=self.runInfo->getSourceFolderNameByRunCode(self.runCodeSelected)
  return, self->extractFileInfoFromFolder(folderName)
  
END

FUNCTION RunSelectionDisplayInfo::getOutputFolderName

  folderName=self.runInfo->getOutputFolderNameByRunCode(self.runCodeSelected)
  return, self->extractFileInfoFromFolder(folderName)
  
END

FUNCTION RunSelectionDisplayInfo::getAllRunNames

  return, self.runInfo->getDisplayNames()
  
END

FUNCTION RunSelectionDisplayInfo::getAllRunCodes

  list=self.runInfo->getCodes()
  return, list
  
END

FUNCTION RunSelectionDisplayInfo::checkIntegrity, view

  yearCodes=self->getYearCodesSelected(NOSELECTION=NOSELECTION)
  if self->getSelectedRunUseYearSelection() and NOSELECTION then begin
    aa=view->dialogMessage(view->getLangCatLabelFromKey('DialogMessage:NoYearSelected'), title=view->getLangCatLabelFromKey('DialogMessageTitle:NoYearSelected'))
    return, 0
  endif
  
  monthCodes=self->getMonthCodesSelected(NOSELECTION=NOSELECTION)
  if self->getSelectedRunUseMonthSelection() and NOSELECTION then begin
    aa=view->dialogMessage(view->getLangCatLabelFromKey('DialogMessage:NoMonthSelected'), title=view->getLangCatLabelFromKey('DialogMessageTitle:NoMonthSelected'))
    return, 0
  endif
  
  roiCodes=self->getRoiCodesSelected(NOSELECTION=NOSELECTION)
  if self->getSelectedRunUseRoiSelection() and NOSELECTION then begin
    aa=view->dialogMessage(view->getLangCatLabelFromKey('DialogMessage:NoROISelected'), title=view->getLangCatLabelFromKey('DialogMessageTitle:NoROISelected'))
    return, 0
  endif
  
  resolutionCodes=self->getResolutionCodesSelected(NOSELECTION=NOSELECTION)
  if self->getSelectedRunUseResolutionSelection() and NOSELECTION then begin
    aa=view->dialogMessage(view->getLangCatLabelFromKey('DialogMessage:NoResolutionSelected'), title=view->getLangCatLabelFromKey('DialogMessageTitle:NoResolutionSelected'))
    return, 0
  endif
  
  fileSelected=self->getSourceFileSelectedCodes(NOSELECTION=NOSELECTION)
  
  ; temporary commented to test ocean sat2
  if self->getSelectedRunUseInputFilesSelection() and keyword_set(NOSELECTION) then begin
    aa=view->dialogMessage(view->getLangCatLabelFromKey('DialogMessage:NoSourceFileSelected'), title=view->getLangCatLabelFromKey('DialogMessageTitle:NoSourceFileSelected'))
    return, 0
  endif
  
  return, 1
  
END

PRO RunSelectionDisplayInfo::initValues

  self->setRunCodeSelected, (self->getAllRunCodes())[0]
  self->setSourceFileSelectedIndexes, [0]
  self->setOverwriteResultFlag, 0
  self->setDeleteInputFlag, 0
  
  self->setRoiCodesSelected, self->getAllRoiCodesBySelectedRun()
  self->setYearCodesSelected, self->getAllYearCodes()
  self->setResolutionCodesSelected, self->getAllResolutionCodes()
  self->setMonthCodesSelected, self->getAllMonthCodes()
  
END

FUNCTION RunSelectionDisplayInfo::clone, super, DEEP=DEEP

  if (n_elements(super) eq 1) then clone=super else clone=obj_new('RunSelectionDisplayInfo', self.roiInfo, self.yearInfo, self.runInfo, self.resolutionInfo, self.monthInfo, self.dataRuns)
  if keyword_set(DEEP) then begin
    if ptr_valid(self.sourceFileSelectedCodes) then begin
      list=*self.sourceFileSelectedCodes
      clone.sourceFileSelectedCodes=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.sourceFileNameSelected) then begin
      list=*self.sourceFileNameSelected
      clone.sourceFileNameSelected=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.roiCodesSelected) then begin
      list=*self.roiCodesSelected
      clone.roiCodesSelected=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.yearCodesSelected) then begin
      list=*self.yearCodesSelected
      clone.yearCodesSelected=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.resolutionCodesSelected) then begin
      list=*self.resolutionCodesSelected
      clone.resolutionCodesSelected=ptr_new(list, /NO_COPY)
    endif
    if ptr_valid(self.monthCodesSelected) then begin
      list=*self.monthCodesSelected
      clone.monthCodesSelected=ptr_new(list, /NO_COPY)
    endif
  endif else begin
    clone.sourceFileSelectedCodes=self.sourceFileSelectedCodes
    clone.sourceFileNameSelected=self.sourceFileNameSelected
    clone.roiCodesSelected=self.roiCodesSelected
    clone.yearCodesSelected=self.yearCodesSelected
    clone.resolutionCodesSelected=self.resolutionCodesSelected
    clone.monthCodesSelected=self.monthCodesSelected
  endelse
  clone.runCodeSelected=self.runCodeSelected
  clone.deleteInputFlag=self.deleteInputFlag
  clone.overwriteResultFlag=self.overwriteResultFlag
  clone.allROIFlag=self.allROIFlag
  clone.allResolutionFlag=self.allResolutionFlag
  clone.allYearFlag=self.allYearFlag
  clone.allMonthFlag=self.allMonthFlag
  return, clone
  
END

PRO RunSelectionDisplayInfo::printSelectionAttribs

  list=self->getSourceFileSelectedCodes(NOSELECTION=NOSELECTION1)
  doLog, "self.sourceFileSelectedCodes", LEVEL=4
  if keyword_set(NOSELECTION1) then doLog, "NO SELECTION", LEVEL=4 else doLog, list, LEVEL=4
  doLog, "******************"
  
  list=self->getSourceFileNameSelected(NOSELECTION=NOSELECTION2)
  doLog, "self.sourceFileNameSelected", LEVEL=4
  if keyword_set(NOSELECTION2) then doLog, "NO SELECTION", LEVEL=4 else doLog, list, LEVEL=4
  doLog, "******************", LEVEL=4
  
  list=self->getroiCodesSelected(NOSELECTION=NOSELECTION3)
  doLog, "self.roiCodesSelected", LEVEL=4
  if keyword_set(NOSELECTION3) then doLog, "NO SELECTION", LEVEL=4 else doLog, list, LEVEL=4
  doLog, "******************", LEVEL=4
  
  list=self->getyearCodesSelected(NOSELECTION=NOSELECTION4)
  doLog, "self.yearCodesSelected", LEVEL=4
  if keyword_set(NOSELECTION4) then doLog, "NO SELECTION", LEVEL=4 else doLog, list, LEVEL=4
  doLog, "******************"
  
  list=self->getresolutionCodesSelected(NOSELECTION=NOSELECTION5)
  doLog, "self.resolutionCodesSelected", LEVEL=4
  if keyword_set(NOSELECTION5) then doLog, "NO SELECTION", LEVEL=4 else doLog, list, LEVEL=4
  doLog, "******************"
  
  list=self->getmonthCodesSelected(NOSELECTION=NOSELECTION6)
  doLog, "self.monthCodesSelected", LEVEL=4
  if keyword_set(NOSELECTION6) then doLog, "NO SELECTION", LEVEL=4 else doLog, list, LEVEL=4
  doLog, "******************", LEVEL=4
  
  doLog, "self.runCodeSelected", LEVEL=4
  doLog, self->getrunCodeSelected(), LEVEL=4
  doLog, "******************", LEVEL=4
  
  doLog, "self.deleteInputFlag", LEVEL=4
  doLog, self->getDeleteInputFlag(), LEVEL=4
  doLog, "******************", LEVEL=4

  doLog, "self.overwriteResultFlag", LEVEL=4
  doLog, self->getOverwriteResultFlag(), LEVEL=4
  doLog, "******************", LEVEL=4
  
  doLog, "self.allROIFlag", LEVEL=4
  doLog, self->getallROIFlag(), LEVEL=4
  doLog, "******************", LEVEL=4
  
  doLog, "self.allResolutionFlag", LEVEL=4
  doLog, self->getallResolutionFlag(), LEVEL=4
  doLog, "******************", LEVEL=4
  
  doLog, "self.allYearFlag", LEVEL=4
  doLog, self->getallYearFlag(), LEVEL=4
  doLog, "******************", LEVEL=4
  
  doLog, "self.allMonthFlag", LEVEL=4
  doLog, self->getallMonthFlag(), LEVEL=4
  doLog, "******************", LEVEL=4
  
END

FUNCTION RunSelectionDisplayInfo::init, rois, years, runs, resolutions, months, dataRuns;, dataFileFolderInfos

  ;if not self -> XMLHelper :: init() then return , 0
  ;xml stuff...
  nonSerAttribs=STRUPCASE(['sourceFileName', 'roiinfo', 'yearinfo', 'runinfo', 'resolutioninfo', 'monthinfo', 'dataRuns', 'silentMode'])
  if not self -> SimpleXML :: init(obj_class(self), nonSerAttribs) then return , 0
  ;self->addNonSerializedAttributes, nonSerAttribs
  ;init selection...
  ; not copy, preserve original object
  self.roiInfo=rois
  self.yearInfo=years
  self.runInfo=runs
  self.resolutionInfo=resolutions
  self.monthInfo=months
  self.dataRuns=dataRuns
  self->initValues
  return , 1
  
END

PRO RunSelectionDisplayInfo::cleanUp

  self.roiInfo=obj_new()
  self.yearInfo=obj_new()
  self.runInfo=obj_new()
  self.resolutionInfo=obj_new()
  self.monthInfo=obj_new()
  self.dataRuns=obj_new()
  ; free local pointers
  ptr_free, self.roiCodesSelected
  ptr_free, self.yearCodesSelected
  ptr_free, self.resolutionCodesSelected
  ptr_free, self.monthCodesSelected
  ptr_free, self.sourceFileSelectedCodes
  ptr_free, self.sourceFileNameSelected
  ; destroy local objects
  self -> SimpleXML::cleanUp
  
END

;****************************************************************************************

PRO RunSelectionDisplayInfo__Define

  Struct = { RunSelectionDisplayInfo , $
    roiInfo: obj_new(), $
    yearInfo: obj_new(), $
    runInfo: obj_new(), $
    resolutionInfo: obj_new(), $
    monthInfo: obj_new(), $
    dataRuns: obj_new(), $
    sourceFileSelectedCodes: ptr_new(), $
    sourceFileNameSelected: ptr_new(), $
    roiCodesSelected: ptr_new(), $
    yearCodesSelected: ptr_new(), $
    resolutionCodesSelected: ptr_new(), $
    monthCodesSelected: ptr_new(), $
    runCodeSelected: 0, $
    deleteInputFlag: 0, $
    overwriteResultFlag: 0, $
    allSourceFilesFlag: 0b, $
    allROIFlag: 0b, $
    allResolutionFlag: 0b, $
    allYearFlag: 0b, $
    allMonthFlag: 0b, $
    silentMode: 0, $
    Inherits SimpleXML $
    }
    
END

;****************************************************************************************
