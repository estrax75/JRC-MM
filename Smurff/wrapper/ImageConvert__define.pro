FUNCTION ImageConvert::getVersion

  return, '1.0'
  
END

FUNCTION ImageConvert::isCropSet

 return, ~(self.cropLimits eq 'N/A' or self.cropLimits eq '') 

END

FUNCTION ImageConvert::getCropLimits
 
 return, self.cropLimits

END

PRO ImageConvert::setCropLimits, value
 
 self.cropLimits=value

END

FUNCTION ImageConvert::getLookUpTableInfo
 
 return, self.lookUpTableInfo

END

PRO ImageConvert::setLookUpTableInfo, value
 
 self.lookUpTableInfo=value

END

PRO ImageConvert::setExportSize, value

  self.exportSize=value
  
END

FUNCTION ImageConvert::getExportSize

  return, self.exportSize
  
END

PRO ImageConvert::setVariableCode, value

  self.variableCode=value
  
END

FUNCTION ImageConvert::getVariableCode

  return, self.variableCode
  
END

PRO ImageConvert::setToTypeCode, value

  self.toTypeCode=value
  
END

FUNCTION ImageConvert::lookUpTableIsPresent

 lut=self->getLookUpTableCode()
 return, ~(self.lookUpTableCode eq '' or strupcase(self.lookUpTableCode) eq 'N/A') 

END

FUNCTION ImageConvert::getLookUpTableCode

  lutInfos=strupcase(strsplit(self.lookUpTableInfo, ';'))
  lutCode=lutInfos[1]
  if lutCode eq 'AUTO' or lutCode eq 'N/A' then lutCode=''
  return, lutCode
  
END

FUNCTION ImageConvert::getLookUpFileName

  lutInfos=strsplit(self.lookUpTableInfo, ';')
  lutFileName=lutInfos[2]
  return, lutFileName
  
END

FUNCTION ImageConvert::getLookUpShowMode

  lutInfos=strsplit(self.lookUpTableInfo, ';')
  lutShowMode=lutInfos[0]
  return, lutShowMode
  
END

FUNCTION ImageConvert::isLookUpTableEmbedded

 mode=self->getLookUpShowMode()
 return, mode eq 'EMBEDDED' or mode eq 'BOTH'  

END

FUNCTION ImageConvert::isLookUpTableExternal

 mode=self->getLookUpShowMode()
 return, mode eq 'EXTERNAL' or mode eq 'BOTH'  

END

FUNCTION ImageConvert::isLookUpTableShow

 mode=self->getLookUpShowMode()
 return, (self->isLookUpTableExternal() or self->isLookUpTableEmbedded())

END
;PRO ImageConvert::setLookUpTableCode, value
;
;  self.lookUpTableCode=value
;  
;END

FUNCTION ImageConvert::getToTypeCode

  return, self.toTypeCode
  
END

PRO ImageConvert::setFromTypeCode, value

  self.fromTypeCode=value
  
END

FUNCTION ImageConvert::getFromTypeCode

  return, self.fromTypeCode
  
END

PRO ImageConvert::setStatType, value

  self.statType=value
  
END

FUNCTION ImageConvert::getStatType

  return, self.statType
  
END

PRO ImageConvert::setInputFileFilter, value

  self.inputFileFilter=value
  
END

FUNCTION ImageConvert::getInputFileFilter

  return, self.inputFileFilter
  
END

PRO ImageConvert::setInputDir, value

  self.inputDir=value
  
END

FUNCTION ImageConvert::getInputDir

  return, self.inputDir
  
END

PRO ImageConvert::setOutputDir, value

  self.outputDir=value
  
END

FUNCTION ImageConvert::getOutputDir

  return, self.outputDir
  
END

FUNCTION ImageConvert::getInputFileList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.inputFileList) then   return, *self.inputFileList
  NOTFOUND=1
  return, [-1]
  
END

PRO ImageConvert::setInputFileList, list

  ptr_free, self.inputFileList
  if n_elements(list) ne 0 then self.inputFileList=ptr_new(list, /NO_COPY)
  
END

FUNCTION ImageConvert::getLabelList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.labelList) then   return, *self.labelList
  NOTFOUND=1
  return, [-1]

END

PRO ImageConvert::setLabelList, list

  ptr_free, self.labelList
  if n_elements(list) ne 0 then self.labelList=ptr_new(list, /NO_COPY)

END

PRO ImageConvert::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  print, '**** fromTypeCode:', self->getFromTypeCode()
  print, '**** toTypeCode:', self->getToTypeCode()
  print, '**** variableCode:', self->getVariableCode()
  print, '**** statType:', self->getStatType()
  print, '**** inputFileFilter:', self->getInputFileFilter()
  print, '**** inputDir:', self->getInputDir()
  print, '**** outputDir:', self->getOutputDir()
  print, '**** inputFileList:', self->getInputFileList()
  
END

;*****************************
; constructor/destructor
;*****************************

PRO ImageConvert::cleanUp

  ptr_free, self.inputFileList
  ptr_free, self.labelList
  obj_destroy, self.fS
  obj_destroy, self.utility
  self->SimpleXML::cleanup
  
END

FUNCTION ImageConvert::init

  if not (self -> SimpleXML :: init(obj_class(self), ['fS', 'utility', 'dtu'])) then return, 0
  self.fS=obj_new('FileSystem', /STAND_ALONE)
  self.utility=obj_new('Utility')
  return, 1
  
END

PRO ImageConvert__Define

  Struct = { ImageConvert , $
    fromTypeCode: '', $
    toTypeCode: '', $
    ;lookUpTableCode: '', $
    lookUpTableInfo: '', $
    exportSize: '', $
    variableCode: '', $
    statType: '', $
    inputFileFilter: '', $
    inputDir: '', $
    outputDir: '', $
    cropLimits: '', $
    inputFileList: ptr_new(), $
    labelList: ptr_new(), $
    fS: obj_new(), $
    utility: obj_new(), $
    dtu: obj_new(), $
    Inherits SimpleXML $
    }
    
END