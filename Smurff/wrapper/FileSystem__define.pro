PRO  FileSystem::createDir, dirName

 if ~(file_info(dirName)).exists then FILE_MKDIR, dirName 

END

FUNCTION FileSystem::getEnvSeparator

  case self->getOSFamily() of
  
    'Windows': return, '='
    'MacOS': return, '='
    'vms': return, '='
    'unix': return, '='
    
  endcase
  
END

FUNCTION FileSystem::getEnvVarValue, envName, NOTPRESENT=NOTPRESENT

  varList=getenv(/ENV)
  for i=0, n_elements(varList)-1 do begin
    vars=strsplit(varList[i], self->getEnvSeparator(), /EXTRACT, count=count, PRESERVE_NULL=PRESERVE_NULL)
    if strupcase(envName) eq strupcase(vars[0]) then return, vars[1]
  endfor
  ;vars=strsplit(varList, getEnvSeparator, /EXTRACT, count=count, PRESERVE_NULL=PRESERVE_NULL))
  ;idx=(where(strupcase(envName) eq varList))[0]
  ;if idx eq -1 then begin
  NOTPRESENT=-1
  return, ''
  ;endif else begin
  ;  return, ''
  ;endelse
  
END

FUNCTION FileSystem::enviFileExists, fullFileName

  sourceFileName=self->getFileNameInfo(fullFileName, filePath=filePath, extension=extension)
  prefix=self->removeFileExtension(fullFileName)
  
  mainFile=file_search(prefix+'*.envi' , FOLD_CASE=0, count=count1)
  hdrFile=file_search(prefix+'*.hdr' , FOLD_CASE=0, count=count2)
  
  if count1 eq 1 and count2 eq 1 then return, 1 else return ,0
  
END

PRO FileSystem::moveFiles, fileList, targetDir, NOOVERWRITE=NOOVERWRITE

  if keyword_set(NOOVERWRITE) then OVERWRITE=0 else OVERWRITE=1
  for i=0, n_elements(fileList)-1 do begin
    dirPath=self->getFilePath(fileList[i], FILENAME=FILENAME)
    destFile=targetDir+path_sep()+FILENAME
    if (OVERWRITE or ((file_info(destFile)).exists eq 0)) then file_move, fileList[i], destFile, /ALLOW, OVERWRITE=OVERWRITE else doLog, 'skip: '+fileList[i], LEVEL=1
  endfor
  
END

PRO FileSystem::removeEnviFile, fullFileName

  sourceFileName=self->getFileNameInfo(fullFileName, filePath=filePath, extension=extension)
  prefix=self->removeFileExtension(fullFileName)
  
  mainFile=file_search(prefix+'*.envi' , FOLD_CASE=0, count=count1)
  hdrFile=file_search(prefix+'*.hdr' , FOLD_CASE=0, count=count2)
  
  if count1 eq 1 and count2 eq 1 then begin
    file_delete, mainFile, /ALLOW_NONEXISTENT , /QUIET
    file_delete, hdrFile, /ALLOW_NONEXISTENT , /QUIET
  endif
  
END

FUNCTION FileSystem::getEnviDataFileName, fullEnviFileName

  sourceFileName=self->getFileNameInfo(fullEnviFileName, filePath=sfilePath, extension=extension)
  prefix=self->removeFileExtension(sourceFileName)
  dataFile=sfilePath+prefix+'.envi'
  
  return, dataFile

END

PRO FileSystem::copyEnviFile, fullSourceFileName, fullDestFileName, TEMP=TEMP, newFileName=newFileName

  sourceFileName=self->getFileNameInfo(fullSourceFileName, filePath=sfilePath, extension=extension)
  prefix=self->removeFileExtension(sourceFileName)
  
  mainFullFile=file_search(sfilePath+prefix+'*.envi' , FOLD_CASE=0, count=count1)
  hdrFullFile=file_search(sfilePath+prefix+'*.hdr' , FOLD_CASE=0, count=count2)
  mainFile=prefix+'.envi'
  hdrFile=prefix+'.hdr'
  
  if n_elements(fullDestFileName) eq 1 then begin
    destFileName=self->getFileNameInfo(fullDestFileName, filePath=destFilePath)
    destFileName=self->removeFileExtension(destFileName)
  endif else begin
    destFileName=self.utility->getSysTime(/FILECOMPATIBILITY)
  endelse
  
  if keyword_set(TEMP) then destfilePath=self->getTempDir()
  
  mainFile1=destfilePath+path_sep()+destFileName+'.envi'
  hdrFile1=destfilePath+path_sep()+destFileName+'.hdr'
  
  if destFilePath eq sfilePath then begin
    destfilePathMain=destfilePath+mainFile+'_'
    destfilePathHdr=destfilePath+hdrFile+'_'
    fullTempMainFile=destfilePathMain
    fullTempHdrFile=destfilePathHdr
  endif else begin
    destfilePathMain=destfilePath
    destfilePathHdr=destfilePath
    fullTempMainFile=destfilePath+path_sep()+mainFile
    fullTempHdrFile=destfilePath+path_sep()+hdrFile
  endelse
  file_copy, mainFullFile[0], destfilePathMain, /OVERWRITE, /ALLOW
  file_copy, hdrFullFile[0], destfilePathHdr, /OVERWRITE, /ALLOW
  file_move, fullTempMainFile[0], mainFile1, /ALLOW, /OVERWRITE
  file_move, fullTempHdrFile[0], hdrFile1, /ALLOW, /OVERWRITE
  newFileName=destfilePath+path_sep()+destFileName
  
END

pro FileSystem::removeEnviHeaderFile, fileName
  testFile=''
  res1=file_search(fileName+'*.hdr' , FOLD_CASE=0, count=count1)
  if count1 gt 0 then testFile=[testFile, res1]
  res2=file_search(fileName+'*.HDR'  , FOLD_CASE=0, count=count2)
  if count2 gt 0 then testFile=[testFile, res2]
  for i=1, n_elements(testFile)-1 do file_delete, testFile[i]
end

pro FileSystem::correctEnviHeaderFileName, mainFileName

  util=obj_new('Utility')
  folder=self->getFilePath(mainFileName)
  newFName=self->removeFileExtension(mainFileName)
  testFile=''
  res1=file_search(newFName+'*.hdr' , FOLD_CASE=0, count=count1)
  if count1 gt 0 then testFile=[testFile, res1]
  res2=file_search(newFName+'*.HDR'  , FOLD_CASE=0, count=count2)
  if count2 gt 0 then testFile=[testFile, res2]
  for i=1, n_elements(testFile)-1 do begin
    newName=util->STRREPLACE(testFile[i], '.HDR', '.hdr')
    newName=util->STRREPLACE(newName, '.ENVI', '.envi')
    newName=util->STRREPLACE(newName, '.envi.hdr', '.hdr')
    if newName ne testFile[i] then begin
      ;work around for windows...
      file_move, testFile[i], newName+'1', /ALLOW, /OVERWRITE
      file_move, newName+'1', newName, /ALLOW, /OVERWRITE
    endif
  endfor
  obj_destroy, util
  
end

function FileSystem::getOSFamily
  ;+
  ; NAME:
  ; OS_FAMILY
  ; PURPOSE:
  ; Return the current operating system as in !VERSION.OS_FAMILY
  ;
  ; CALLING SEQUENCE
  ; result = OS_FAMILY()
  ; INPUTS:
  ; None
  ; OUTPUTS:
  ; result - scalar string containing one of the four values
  ;   'Windows','MacOS','vms' or 'unix'
  ; NOTES:
  ; OS_FAMILY is assumed to be 'unix' if !VERSION.OS is not 'windows',
  ;   'MacOS' or 'vms'
  ;
  ; To make procedures from IDL V4.0 and later compatibile with earlier
  ; versions of IDL, replace calls to !VERSION.OS_FAMILY with OS_FAMILY().
  ;
  ; PROCEDURES CALLED
  ; function TAG_EXISTS()
  ; REVISION HISTORY:
  ; Written,  W. Landsman
  ;-
  if tag_exist(!VERSION, 'OS_FAMILY') then return, !VERSION.OS_FAMILY
  
  case !VERSION.OS of
  
    'windows': return, 'Windows'
    'MacOS': return, 'MacOS'
    'vms': return, 'vms'
    else: return, 'unix'
    
  endcase
  
END

FUNCTION FileSystem::isOSWin

  return, strlowcase(self->getOSFamily()) eq 'windows'
  
END

FUNCTION FileSystem::isOSUnix

  return, strlowcase(self->getOSFamily()) eq 'unix'
  
END

FUNCTION FileSystem::isOSMac

  return, strlowcase(self->getOSFamily()) eq 'MacOS'
  
END

FUNCTION FileSystem::readCSVFile, fileName, FOOTERDISCARDLINES=FOOTERDISCARDLINES, HEADERDISCARDLINES=HEADERDISCARDLINES, HEADERINFO=HEADERINFO, DISCARDSYMBOLS=DISCARDSYMBOLS, SEPARATOR=SEPARATOR, PRESERVE_NULL=PRESERVE_NULL

  ;  ERROR=0
  ;  catch, error_status
  ;  ;print, systime()
  ;  if error_status NE 0 THEN BEGIN
  ;    ERROR=1
  ;    catch, /CANCEL
  ;    msg='problem with file: <'+fileName+'> check existence or read permission.'
  ;    errMsg=dialog_message(msg, /ERROR)
  ;    message, msg
  ;  endif
  discN=n_elements(discardrow)
  totLines=FILE_LINES(fileName)
  
  openr, unit, fileName, /GET_LUN
  bufferString=''
  i=0
  firstRow=1
  if n_elements(SEPARATOR) ne 0 then separator=SEPARATOR else separator=';'
  if n_elements(DISCARDSYMBOLS) ne 0 then discardSymbols=[discardSymbols, '[', ';', '#'] else discardSymbols=['[', ';', '#']
  check=intarr(n_elements(discardSymbols))
  
  if n_elements(HEADERDISCARDLINES) ne 0 then headerDiscardLines=HEADERDISCARDLINES else headerDiscardLines=-1
  lastLineNumber=-1
  
  if n_elements(FOOTERDISCARDLINES) ne 0 then begin
    footerDiscardLines=FOOTERDISCARDLINES
    lastLineNumber=totLines-footerDiscardLines-headerDiscardLines
  endif else begin
    footerDiscardLines=0
  endelse
  
  totValidLines=totLines-footerDiscardLines-headerDiscardLines
  
  for i=0, headerDiscardLines-1 do readf, unit, bufferString
  
  while not(eof(unit)) do begin
    ;if i lt
    readf, unit, bufferString
    i++
    if i eq lastLineNumber then break
    checkFirst=strmid(bufferString, 0,1)
    for j=0, n_elements(check)-1 do check[j] = (strpos(checkFirst, discardSymbols[j])+1) > 0
    ;check1=(strpos(checkFirst, '[')+1) > 0
    ;check2=(strpos(checkFirst, ';')+1) > 0
    ;check3=(strpos(checkFirst, '#')+1) > 0
    null=strlen(strcompress(bufferString, /REMOVE)) eq 0
    ; #, ; is a comment
    ; void string is discarded
    ; [ header is discarded
    if (total(check)) gt 0 or null then begin
      doLog, 'Discard row', i
      doLog, bufferString
    endif else begin
      info=strsplit(bufferString, separator, /EXTRACT, count=count, PRESERVE_NULL=PRESERVE_NULL)
      if firstRow eq 1 then begin
        firstRow=0
        HEADERINFO=info
        storeData=strarr(n_elements(info), totValidLines)
        k=0
      endif else begin
        storeData[*, k]=strcompress(info, /REMOVE)
        k++
      endelse
    endelse
  endwhile
  if keyword_set(READLASTLINE) then storeData=storeData[*,0:k-1] else storeData=storeData[*,0:k-1] 
  close, unit & free_lun, unit
  return, storeData
  
END

FUNCTION FileSystem::removeFileExtension, fullFileName, extensionSep=extensionSep

  if ~keyword_set(extensionSep) then extension='.' else extension=extensionSep
  extensionPos=strpos(fullFileName, extension, /REVERSE_SEARCH)
  if extensionPos ne -1 then return, strmid(fullFileName, 0, extensionPos) else return, fullFileName
  
END

FUNCTION FileSystem::addFileExtension, fullFileName, suffix, extensionSep=extensionSep, REMOVEPREVIOUS=REMOVEPREVIOUS

  if n_elements(extensionSep) eq 0 then extension='.' else extension=extensionSep
  if keyword_set(REMOVEPREVIOUS) then fileName=self->removeFileExtension(fullFileName, extensionSep=extensionSep) else fileName=fullFileName
  return, fileName+extension+suffix
  
END

FUNCTION FileSystem::getFileExtension, fullFileName

  extensionSep='.'
  fileParts=strsplit(fullFileName, extensionSep, /EXTRACT)
  partsNo=n_elements(fileParts)
  if partsNo gt 1 then extension=fileParts[partsNo-1] else extension=''
  return, extension
  
END

FUNCTION FileSystem::getFilePath, fullFileName, FILENAME=FILENAME

  pathSep=path_sep()
  lastIndex=rstrpos(fullFileName, pathSep)
  ;lastIndex=strmid(a, rstrpos(a, '\')+1, 10)
  filePath=''
  
  if lastIndex ne -1 then begin
    fileName=strmid(fullFileName, lastIndex+1, strlen(fullFileName)-lastIndex)
    filePath=strmid(fullFileName, 0, lastIndex+1)
  endif else begin
    fileName=fullFileName
  endelse
  return, filePath
  
END

FUNCTION FileSystem::getFileNameInfo, fullFileName, filePath=filePath, extension=extension

  filePath=self->getFilePath(fullFileName, FILENAME=FILENAME)
  extension=self->getFileExtension(fullFileName)
  return, FILENAME
  
END

FUNCTION FileSystem::buildFileName, fileName, folder

  lastFolderChar=strmid(folder, strlen(folder)-1, 1)
  if ~(lastFolderChar eq self.oSDirSeparator) then folderName=folder+self.oSDirSeparator else folderName=folder
  fullFileName=folderName+fileName
  return, fullFileName
  
END

FUNCTION FileSystem::convertCompatibleFileName, name

  if (self->isFileNameCompatible(name)) then return, name
  list=self->getInvalidCharList()
  resultString=name
  ;strLength=(strlen(name))[0]
  for j=0, n_elements(list)-1 do resultString=self.utility->replaceChar(resultString, list[j], '_')
  return, resultString
  
END

FUNCTION FileSystem::isFileNameCompatible, aString

  ;testString=byte(aString)
  invalidCharList=self->getInvalidCharList()
  for i=0, n_elements(invalidCharList)-1 do if (strpos(aString, invalidCharList[i]))[0] ne -1 then return, 0
  return, 1
  
END

;FUNCTION FileSystem::isFileNameCompatible, aString
;
;  ;testString=byte(aString)
;  invalidCharList=self->getInvalidCharList()
;  for i=0, n_elements(invalidCharList)-1 do if (strpos(aString, invalidCharList[i]))[0] ne -1 then return, 0
;  return, 1
;
;END

FUNCTION FileSystem::getInvalidCharList

  if ptr_valid(self.invalidCharList) then begin
    list=*self.invalidCharList
    return, list
  endif
  return, '*'
  
END

FUNCTION FileSystem::searchFiles, folderName, ONLYFILES=ONLYFILES, RELATIVE=RELATIVE

  if keyword_set(ONLYFILES) then TEST_REGULAR=1
  files=file_search(folderName, /TEST_REGULAR)
  if keyword_set(RELATIVE) then begin
    for i=0, n_elements(files)-1 do begin
      testFile=strsplit(files[i], path_sep(), /EXTRACT)
      files[i]=testFile[n_elements(testFile)-1]
    endfor
  endif
  return, files
  
END


FUNCTION FileSystem::getRecordSeparator

  return, self.recordSeparator
  
END

PRO FileSystem::writeRecordStream, unit, infoArray

  no=n_elements(infoArray)
  record=''
  for i=0, no-1 do begin
    ; check consistency
    if strpos(infoArray[i], self.recordSeparator) eq -1 then record=record+infoArray[i]+self.recordSeparator else message, 'Try to write wrong parameter in a record'
  endfor
  record=strmid(record, 0, strlen(record)-strlen(self.recordSeparator))
  printf, unit, record
  
END

FUNCTION FileSystem::readRecordStream, unit, columns=columns

  bufferString=''
  readf, unit, bufferString
  void=strlen(strcompress(bufferString)) eq 0
  checkFirst=strmid(bufferString, 0,1)
  check1=(strpos(checkFirst, '[')+1) > 0
  check2=(strpos(checkFirst, ';')+1) > 0
  check3=(strpos(checkFirst, '#')+1) > 0
  null=strlen(strcompress(bufferString, /REMOVE)) eq 0
  ; #, ; is a comment
  ; void string string is discarded
  ; [ header is discarded
  if (check1+check2+check3) gt 0 or null or void then begin
    doLog, 'Discard row:'
    doLog, bufferString
    columns=0
    return, ['']
  endif else begin
    info=strsplit(bufferString, self.recordSeparator, /EXTRACT, /PRESERVE)
    columns=n_elements(info)
    return, info
  endelse
  
END

FUNCTION FileSystem::buildValuesStream, pars

  no=n_elements(pars)
  values=strcompress(randomu(seed, no), /REMOVE)
  vals=''
  for i=0, no-1 do vals=vals+values[i]+';'
  return, vals
  
END

; ****************************
; startup methods
; ****************************
FUNCTION FileSystem::buildMapEntryFromFile, fileName

  ;map all configuration that system need from a configuration file!!!
  ERROR=0
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    msg='Problem with file '+fileName+' check version, contents, existence or read permission.'
    errMsg=dialog_message(msg, /ERROR)
    message, msg
    exit
  endif
  
  bufferString=''
  openr, unit, fileName, /GET_LUN
  
  bufferString=''
  i=0
  confMap=obj_new('ConfMap')
  
  while not(eof(unit)) do begin
    readf, unit, bufferString
    i++
    void=strlen(strcompress(bufferString, /REMOVE)) eq 0
    checkFirst=strmid(bufferString, 0,1)
    check1=(strpos(checkFirst, '[')+1) > 0
    check2=(strpos(checkFirst, ';')+1) > 0
    check3=(strpos(checkFirst, '#')+1) > 0
    null=strlen(checkFirst) eq 0
    if (check1+check2+check3) gt 0 or null or void then begin
      print, 'Discard row', i
      print, bufferString
    endif else begin
      info=strsplit(bufferString, '=', /EXTRACT)
      mapEntry=obj_new('MapEntry', info[0], info[1])
      confMap->addEntry, mapEntry
    endelse
  endwhile
  close, unit & free_lun, unit
  return, confMap
  
END

; load from ini file
FUNCTION FileSystem::loadInitFileData

  fileName=self->getInitFileName()
  self.configurationMap=self->buildMapEntryFromFile(fileName)
  return, self.configurationMap
  
END

; load from xml file
FUNCTION FileSystem::loadInitXMLFileData

  fileName=self->getInitXMLFileName()
  ;self.configurationMap=self->buildMapEntryFromXMLFile(fileName)
  confMap=obj_new('ConfigurationMap')
  confMap->buildMapEntryFromXMLFile, fileName
  self.configurationMap=confMap
  return, confMap
  
END

; load from xml file
FUNCTION FileSystem::loadColorMapXMLFileData

  fileName=self->getColorMapXMLFileName()
  ;self.configurationMap=self->buildMapEntryFromXMLFile(fileName)
  colorMap=obj_new('ColorMap')
  colorMap->buildMapEntryFromXMLFile, fileName
  self.colorMap=colorMap
  return, colorMap
  
END
;***********************
; directory management
;***********************
FUNCTION FileSystem::getHomeDir, WITHSEPARATOR=WITHSEPARATOR

  cd,current=dir
  dir=self.applicationRoot
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FileSystem::getDataDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'data'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FileSystem::getConfigurationDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'configuration'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FileSystem::getLogDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'log'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FileSystem::getTempDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'temp2'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FileSystem::getResourceDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'resource'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FileSystem::getDocsDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'documents'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FileSystem::getHelpDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'help'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FileSystem::getSaveDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'save'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION FileSystem::getDumpDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'dump'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

;**************************
; file/extension management
;**************************
FUNCTION FileSystem::getStartUpFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+'startup.ini'
  return, fileName
  
END

FUNCTION FileSystem::getInitFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+'init.ini'
  return, fileName
  
END

FUNCTION FileSystem::getInitXMLFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+'config.xml'
  return, fileName
  
END

FUNCTION FileSystem::getColorMapXMLFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+'colorMap.xml'
  return, fileName
  
END

;**************************
; get/set
;**************************
FUNCTION FileSystem::getSystemDirSeparator

  return, self.oSDirSeparator
  
END

;from init file
PRO FileSystem::setBrowserApplication, fileName

  self.browserApplication=fileName
  
END
;from init file
PRO FileSystem::setWorkSheetApplication, fileName

  self.workSheetApplication=fileName
  
END
;from init file
PRO FileSystem::setDocReaderApplication, fileName

  self.docReaderApplication=fileName
  
END
;from init file
PRO FileSystem::setPdfReaderApplication, fileName

  self.pdfReaderApplication=fileName
  
END

PRO FileSystem::configure, mainDir, MUSTEXISTS=MUSTEXISTS, NOTFOUND=NOTFOUND

  fileName1=self->getEnvVarValue('IDL_APPS')
  fileName2=mainDir+self.oSDirSeparator+'init.ini'
  
  ERROR=0
  NOTFOUND=0
  lastFile=0
  fileName=fileName1
  catch, error_status
  
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    close, /all
    ;errMsg=dialog_message('Problem with file '+fileName+' check contents, existence or read permission.', /ERROR)
    ;message, 'Problem with file '+fileName+' check contents, existence or read permission.'
    if lastFile ne 1 then begin
      fileName=fileName2
      lastFile=1
    endif else begin
      message='Problem with file '+fileName+' check contents, existence or read permission. FileSystem is not linked with a specific application'
      print, message
      if keyword_set(MUSTEXISTS) then errMsg=dialog_message(message, /ERROR)
      NOTFOUND=1
      return
    endelse
  endif
  
  filename='/home/mariomi/idl_apps.ini'
  openr, unit, filename, /GET_LUN
  
  bufferString=''
  while not(eof(unit)) do begin
    readf, unit, bufferString
    if strpos(bufferString, '=') ne -1 then begin
      candidate=strsplit(bufferString, '=', /EXTRACT)
      if strupcase(strcompress(candidate[0], /REMOVE)) eq strupcase(self.applicationName) then begin
        self.applicationRoot=strcompress(candidate[1], /REMOVE)
        close, unit
        free_lun, unit
        return
      endif
    endif
  endwhile
  NOTFOUND=1
  errMsg=dialog_message('No application root found in '+fileName+' insert a row like applicationName=applicationRoot.', /ERROR)
  
END

PRO FileSystem::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, 'applicationRoot:', self.applicationRoot
  print, 'applicationName:', self.applicationName
  print, 'oSDirSeparator:', self.oSDirSeparator
  print, 'pdfReaderApplication:', self.pdfReaderApplication
  print, 'docReaderApplication:', self.docReaderApplication
  print, 'workSheetApplication:', self.workSheetApplication
  print, 'browserApplication:', self.browserApplication
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

;***********************
; constructor/destructor
;***********************

FUNCTION FileSystem::init, applicationName, mainApplication, applicationRoot=applicationRoot, recordSeparator=recordSeparator, STAND_ALONE=STAND_ALONE, MUSTEXISTS=MUSTEXISTS

  if not self -> Object :: init() then return , 0
  self.oSDirSeparator=path_sep()
  if n_elements(applicationName) ne 0 then self.applicationName=applicationName
  ;self.configurationMap=obj_new('ConfMap')
  if n_elements(mainApplication) eq 1 then self.mainApplication=mainApplication
  if ~keyword_set(STAND_ALONE) then if n_elements(applicationRoot) eq 1 then self.applicationRoot=applicationRoot else self->configure, !DIR, MUSTEXISTS=MUSTEXISTS, NOTFOUND=NOTFOUND
  if keyword_set(MUSTEXISTS) and keyword_set(NOTFOUND) then return, 0
  if n_elements(recordSeparator) eq 1 then self.recordSeparator=recordSeparator else self.recordSeparator=';'
  self.invalidCharList=ptr_new(["!", "@", " ", "#", "$", "*", " "], /NO_COPY)
    self.utility=obj_new('Utility')
    
  return , 1
  
END

PRO FileSystem::cleanUp

  self -> Object :: cleanUp
  self.mainApplication=obj_new()
  self.configurationMap=obj_new()
  ptr_free, self.invalidCharList
  ;obj_destroy, self.configurationMap
  obj_destroy, self.utility
  
END

;****************************************************************************************

PRO FileSystem__Define

  Struct = { FileSystem , $
    applicationRoot: '', $
    applicationName: '', $
    oSDirSeparator: '', $
    recordSeparator: '', $
    invalidCharList: ptr_new(), $
    mainApplication: obj_new(), $
    configurationMap: obj_new(), $
    colorMap: obj_new(), $
    utility: obj_new(), $
    Inherits Object $
  }
  
END

;****************************************************************************************
