FUNCTION Request::getVersion

  return, '1.0'
  
END

PRO Request::setOutputRoi, value

  self.outputRoi=value
  
END

FUNCTION Request::getOutputRoi

  return, self.outputRoi
  
END

PRO Request::setOverwriteResultFlag, value

  self.overwriteResultFlag=value
  
END

FUNCTION Request::getOverwriteResultFlag

  return, self.overwriteResultFlag
  
END

PRO Request::setDeleteInputFlag, value

  self.deleteInputFlag=value
  
END

FUNCTION Request::getDeleteInputFlag

  return, self.deleteInputFlag
  
END

PRO Request::setAllResolutionFlag, value

  self.allResolutionFlag=value
  
END

FUNCTION Request::getAllResolutionFlag

  return, self.allResolutionFlag
  
END

PRO Request::setAllRoiFlag, value

  self.allRoiFlag=value
  
END

FUNCTION Request::getAllRoiFlag

  return, self.allRoiFlag
  
END

PRO Request::setAllYearFlag, value

  self.allYearFlag=value
  
END

FUNCTION Request::getAllYearFlag

  return, self.allYearFlag
  
END

PRO Request::setAllMonthFlag, value

  self.allMonthFlag=value
  
END

FUNCTION Request::getAllMonthFlag

  return, self.allMonthFlag
  
END

PRO Request::setPeriodType, value

  self.periodType=value
  
END

FUNCTION Request::getPeriodType

  return, self.periodType
  
END

PRO Request::setRunCommand, value

  self.runCommand=value
  
END

FUNCTION Request::getRunCommand

  return, self.runCommand
  
END

PRO Request::setRunCommandType, value

  self.runCommandType=value
  
END

FUNCTION Request::getRunCommandType

  return, self.runCommandType
  
END

PRO Request::setRunCode, value

  self.runCode=value
  
END

FUNCTION Request::getRunCode

  return, self.runCode
  
END

PRO Request::setInputFileFilter, value

  self.inputFileFilter=value
  
END

FUNCTION Request::getInputFileFilter

  return, self.inputFileFilter
  
END

PRO Request::setInputDir, value

  self.inputDir=value
  
END

FUNCTION Request::getInputDir

  return, self.inputDir
  
END

PRO Request::setOutputDir, value

  self.outputDir=value
  
END

FUNCTION Request::getOutputDir

  return, self.outputDir
  
END

PRO Request::setInputFileList, list

  ptr_free, self.inputFileList
  if n_elements(list) ne 0 then self.inputFileList=ptr_new(list, /NO_COPY)
  
END

;FUNCTION Request::getBandToExportList, NOTFOUND=NOTFOUND
;
;  NOTFOUND=0
;  if ptr_valid(self.bandToExportList) then   return, *self.bandToExportList
;  NOTFOUND=1
;  return, [-1]
;
;END
;
;PRO Request::setBandToExportList, list
;
;  ptr_free, self.bandToExportList
;  if n_elements(list) ne 0 then self.bandToExportList=ptr_new(list, /NO_COPY)
;  
;END

FUNCTION Request::getinputParameterList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.inputParameterList) then   return, *self.inputParameterList
  NOTFOUND=1
  return, [-1]

END

PRO Request::setinputParameterList, list

  ptr_free, self.inputParameterList
  if n_elements(list) ne 0 then self.inputParameterList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Request::getoutputParameterList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.outputParameterList) then   return, *self.outputParameterList
  NOTFOUND=1
  return, [-1]

END

PRO Request::setoutputParameterList, list

  ptr_free, self.outputParameterList
  if n_elements(list) ne 0 then self.outputParameterList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Request::getInputFileList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.inputFileList) then   return, *self.inputFileList
  NOTFOUND=1
  return, [-1]
  
END

PRO Request::setroiArchiveList, list

  ptr_free, self.roiArchiveList
  if n_elements(list) ne 0 then self.roiArchiveList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Request::getroiArchiveList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.roiArchiveList) then   return, *self.roiArchiveList
  NOTFOUND=1
  return, [-1]
  
END

PRO Request::setRoiList, list

  ptr_free, self.roiList
  if n_elements(list) ne 0 then self.roiList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Request::getRoiList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.roiList) then return, *self.roiList
  NOTFOUND=1
  return, [-1]
  
END

PRO Request::setResolutionList, list

  ptr_free, self.resolutionList
  if n_elements(list) ne 0 then self.resolutionList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Request::getResolutionList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.resolutionList) then return, *self.resolutionList
  NOTFOUND=1
  return, [-1]
  
END

PRO Request::setYearList, list

  ptr_free, self.yearList
  if n_elements(list) ne 0 then self.yearList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Request::getYearList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.yearList) then   return, *self.yearList
  NOTFOUND=1
  return, [-1]
  
END

PRO Request::setMonthList, list

  ptr_free, self.monthList
  if n_elements(list) ne 0 then self.monthList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Request::getMonthList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.monthList) then   return, *self.monthList
  NOTFOUND=1
  return, [-1]
  
END

PRO Request::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  print, '**** runCode:', self->getRunCode()
  print, '**** runCommand:', self->getRunCommand()
  print, '**** runCommandType:', self->getRunCommandType()
  print, '**** periodType:', self->getPeriodType()
  print, '**** inputFileFilter:', self->getInputFileFilter()
  print, '**** inputParameterList:', self->getInputParameterList()
  print, '**** outputParameterList:', self->getOutParameterList()
  ;print, '**** inputDir:', self->getInputDir()
  ;print, '**** outputDir:', self->getOutputDir()
  ;print, '**** bandToExportList:', self->getBandToExportList()
  print, '**** inputFileList:', self->getInputFileList()
  print, '**** roiList:', self->getRoiList()
  print, '**** roiArchiveList:', self->getRoiArchiveList()
  print, '**** resolutionList:', self->getResolutionList()
  print, '**** monthList:', self->getMonthList()
  print, '**** yearList:', self->getYearList()
  print, '**** allMonthFlag:', self->getAllMonthFlag()
  print, '**** allYearFlag:', self->getAllYearFlag()
  print, '**** allRoiFlag:', self->getAllRoiFlag()
  print, '**** allResolutionFlag:', self->getAllResolutionFlag()
  
END

;*****************************
; constructor/destructor
;*****************************

PRO Request::cleanUp

  ptr_free, self.yearList
  ptr_free, self.monthList
  ptr_free, self.roiArchiveList
  ptr_free, self.roiList
  ptr_free, self.resolutionList
  ptr_free, self.inputParameterList
  ptr_free, self.outputParameterList
  ptr_free, self.inputFileList
  obj_destroy, self.fS
  obj_destroy, self.utility
  self->SimpleXML::cleanup
  
END

FUNCTION Request::init

  if not (self -> SimpleXML :: init(obj_class(self), ['fS', 'utility', 'dtu'])) then return, 0
  self.fS=obj_new('FileSystem', /STAND_ALONE)
  self.utility=obj_new('Utility')
  ;nonSerAttribs=STRUPCASE(['fS', 'utility', 'dtu'])
  ;self->addNonSerializedAttributes, nonSerAttribs
  return, 1
  
END

PRO Request__Define

  Struct = { Request , $
    runCode: 0, $
    runCommand: '', $
    runCommandType: '', $
    periodType: '', $
    inputFileFilter: '', $
    inputParameterList: ptr_new(), $
    outputParameterList: ptr_new(), $
    outputRoi: '', $
    inputFileList: ptr_new(), $
    roiList: ptr_new(), $
    resolutionList: ptr_new(), $
    roiArchiveList: ptr_new(), $
    monthList: ptr_new(), $
    yearList: ptr_new(), $
    deleteInputFlag: 0, $
    overwriteResultFlag: 0, $
    allMonthFlag: 0, $
    allYearFlag: 0, $
    allRoiFlag: 0, $
    allResolutionFlag: 0, $
    ; when you add a new attribute remember getter/setter;
    inputDir: '', $
    outputDir: '', $
    ;bandToExportList: ptr_new(), $
    ;non serializable attribs
    fS: obj_new(), $
    utility: obj_new(), $
    dtu: obj_new(), $
    Inherits SimpleXML $
    }
    
END