;**************************
; file/extension management
;**************************
PRO SmurffFileSystem::writeConfigurationFile, physData, physSummary

  ; Build up configuration file from user ini at every startup of application [UPDATELOOKUP setting]
  confDir=self->getConfigurationDir(/WITH)
  
  names=['YEAR_FILE', $
      'MONTH_FILE', $
      'ROI_FILE', $
      'RESOLUTION_FILE',$
      'PHYSICALDATARUN_FILE']
    
  units=indgen(n_elements(names))+1
  ;test internal parameters
  WRITE=1
  TEST=1
  
  ; Build up Model from all files *.nc/*.hdf in $HOME$/data/models dir ($MODELNAME$_$SCENARIONAME$_2D.cdf or $TYPENAME$_$ROINAME_$GRIDRESOLUTION_$EXTRAINFO_$UNKNOWN_$YEAR_$MONTH
  ; ****************** Model section
  mapEntry=self.configurationMap->getEntryByKey(names[0])
  fileName=mapEntry->getValue()
  thisObj=obj_new(strmid(names[0],0, strlen(names[0])-5), self.mainApplication, confDir+fileName)
  ;thisObj->writeStructList, units[0], confDir+fileName, physSummary.years, WRITE=WRITE
  thisObj->writeXmlFile, confDir+fileName, physSummary.years, WRITE=WRITE
  ;no destroy it is original one mapEntry!  
  ;obj_destroy, mapEntry  
  obj_destroy, thisObj  
  ;self->buildYear, units[selection], testDir+names[selection]+'.dat', physSummary.years, WRITE=WRITE
  
  mapEntry=self.configurationMap->getEntryByKey(names[1])
  fileName=mapEntry->getValue()
  thisObj=obj_new(strmid(names[1],0, strlen(names[1])-5), self.mainApplication, confDir+fileName)
  ;thisObj->writeStructList, units[1], confDir+fileName, physSummary.months, WRITE=WRITE
  thisObj->writeXmlFile, confDir+fileName, physSummary.months, WRITE=WRITE
  ;no destroy it is original one mapEntry!  
  ;obj_destroy, mapEntry  
  obj_destroy, thisObj  

  mapEntry=self.configurationMap->getEntryByKey(names[2])
  fileName=mapEntry->getValue()
  thisObj=obj_new(strmid(names[2],0, strlen(names[2])-5), self.mainApplication, confDir+fileName)
  ;thisObj->writeStructList, units[2], confDir+fileName, physSummary.rois, WRITE=WRITE 
  thisObj->writeXmlFile, confDir+fileName, physSummary.rois, WRITE=WRITE 
  ;;no destroy it is original one mapEntry!  
  ;obj_destroy, mapEntry  
  obj_destroy, thisObj  

  ;selection=(where(names eq 'RESOLUTION'))[0]
  ;self->buildResolution, units[selection], testDir+names[selection]+'.dat', physSummary.resolutions, WRITE=WRITE
  mapEntry=self.configurationMap->getEntryByKey(names[3])
  fileName=mapEntry->getValue()
  thisObj=obj_new(strmid(names[3],0, strlen(names[3])-5), self.mainApplication, confDir+fileName)
  ;thisObj->writeStructList, units[3], confDir+fileName, physSummary.resolutions, WRITE=WRITE
  thisObj->writeXmlFile,  confDir+fileName, physSummary.resolutions, WRITE=WRITE
  ;no destroy it is original one mapEntry!  
  ;obj_destroy, mapEntry  
  obj_destroy, thisObj  

  ;selection=(where(names eq strupcase('PhysicalParameter')))[0]
  ;self->buildPhysicalParameter, units[selection], testDir+names[selection]+'.dat', physData, WRITE=WRITE
  mapEntry=self.configurationMap->getEntryByKey(names[4])
  fileName=mapEntry->getValue()
  thisObj=obj_new(strmid(names[4],0, strlen(names[4])-5), self.mainApplication, confDir+fileName)
  ;thisObj->writeStructList, units[4], confDir+fileName, physData, WRITE=WRITE 
  thisObj->writeXmlFile, confDir+fileName, physData, WRITE=WRITE 
  ;no destroy it is original one mapEntry!  
  ;obj_destroy, mapEntry  
  obj_destroy, thisObj  

END

PRO SmurffFileSystem::lookUpSystemData

  ; build .dat from file list ($HOME$/data/model)

  physParLR=self.configurationMap->getEntryByKey('PHYS_PAR_LO_RES_DIR')
  physParLRDir=physParLR->getValue()

  physParHR=self.configurationMap->getEntryByKey('PHYS_PAR_HI_RES_DIR')
  physParHRDir=physParHR->getValue()
  
  physParDir=physParHRDir+self.oSDirSeparator
  
  physInfo=self->readPhysicalParDataFiles(physParDir)
  self->writeConfigurationFile, physInfo.data, physInfo.summary
  
END

FUNCTION SmurffFileSystem::getPhysicalParDataDir, WITHSEPARATOR=WITHSEPARATOR

  ;application related or get from configuration file
  dir=self->getDataDir(/WITH)
  dir=dir+'phys'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getRunSelectionDisplayInfoExtension

 return, '.rdi'

END

;FUNCTION SmurffFileSystem::getPhysicalParDataExtension
;
;  ;application related or get from configuration file
;  entry=self.configurationMap->getEntryByKey('PHYS_FILE_EXTENSION')
;  return, '.'+entry->getValue()
;  
;END

FUNCTION SmurffFileSystem::getPhysicalParDataFileFilter

  ;application related or get from configuration file
  entry=self.configurationMap->getEntryByKey('PHYS_FILE_EXTENSION')
  return, '*.'+entry->getValue()
  
END

FUNCTION SmurffFileSystem::readPhysicalParDataFiles, dir

  wildCard=self->getPhysicalParDataFileFilter()
  
  filenames=file_search(dir+wildCard)
  
  fInfo=file_info(filenames)
  execDate=fInfo.ctime
  
  pdr=obj_new('PhysicalDataRun', self.mainApplication)
  physInfos=pdr->buildListStruct(/NOFILL)
  obj_destroy, pdr
  
  nElem=n_elements(filenames)
  physInfos=replicate(physInfos, nElem)
  
  for i=0, nElem-1 do begin
    ;remove path
    filename=strsplit(filenames[i], self->getSystemDirSeparator(), /EXTRACT)
    filename=filename[n_elements(filename)-1]
    physInfos[i].filename=filename & physInfos[i].code=strcompress(i, /REMOVE)
    ;remove extension
    name=strsplit(filename, '.', /EXTRACT)
    info=strsplit(name[0], '_', /EXTRACT, /PRESERVE_NULL)
    subst1=where(info eq '', count1)
    subst2=where(info eq 'NA', count2)
    if count1 ne 0 then info[subst1]='N/A' 
    if count2 ne 0 then info[subst2]='N/A' 
    physInfos[i].type=info[0]
    physInfos[i].roi=info[1]
    physInfos[i].resolution=info[2]
    physInfos[i].extraInfo=info[3]
    physInfos[i].unknown=info[4]
    physInfos[i].year=info[5]
    physInfos[i].month=info[6]
    physInfos[i].execDate=systime(0, execDate[i])
    ;doLog, info
  endfor
  typeList=physInfos[UNIQ(physInfos.type, SORT(physInfos.type))].type
  roiList=physInfos[UNIQ(physInfos.roi, SORT(physInfos.roi))].roi
  resolutionList=physInfos[UNIQ(physInfos.resolution, SORT(physInfos.resolution))].resolution
  extraList=physInfos[UNIQ(physInfos.extraInfo, SORT(physInfos.extraInfo))].extraInfo
  unknownList=physInfos[UNIQ(physInfos.unknown, SORT(physInfos.unknown))].unknown
  yearList=physInfos[UNIQ(physInfos.year, SORT(physInfos.year))].year
  monthList=physInfos[UNIQ(physInfos.month, SORT(physInfos.month))].month
  summaryPhysInfo={types:typeList, rois:roiList, resolutions:resolutionList, extras:extraList, unknowns:unknownList, years:yearList, months:monthList}
  physInfo={data:physInfos, summary:summaryPhysInfo}
  
  return, physInfo
  
END


;; MM 11/11/11 new code end here

;***********************
; directory management
;***********************
FUNCTION SmurffFileSystem::getHomeDir, WITHSEPARATOR=WITHSEPARATOR

  cd,current=dir
  ;dir=strcompress(direct,/remove_all)
  ;dir="C:\work\informatica\sviluppo\idl_projects\POMI"
  ;dir="D:\FairModeApp"
  dir=self.applicationRoot
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getDataDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'data'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getConfigurationDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'configuration'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getLogDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'log'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getTempDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'temp'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getResourceDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'resource'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getDocsDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'documents'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getHelpDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'help'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getSaveDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'save'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

FUNCTION SmurffFileSystem::getDumpDir, WITHSEPARATOR=WITHSEPARATOR

  dir=self->getHomeDir(/WITH)
  dir=dir+'dump'
  if keyword_set(WITHSEPARATOR) then dir=dir+self.oSDirSeparator
  return, dir
  
END

;**************************
; file/extension management
;**************************
;FUNCTION SmurffFileSystem::getRequestExtension
;
;  return, '.xml'
;  
;END

FUNCTION SmurffFileSystem::getXMLExtension

  return, '.xml'
  
END

FUNCTION SmurffFileSystem::getRequestExtension

  return, '.rqs'
  
END

FUNCTION SmurffFileSystem::getJRCLogoFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"jrc_logo.jpg"
  return, fileName
  
END

FUNCTION SmurffFileSystem::getIesLogoFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"ies_logo.jpg"
  return, fileName
  
END

FUNCTION SmurffFileSystem::getStartUpFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"startup.ini"
  return, fileName
  
END

FUNCTION SmurffFileSystem::getInitFileName

  fileName=self->getResourceDir(/WITH)
  fileName=fileName+"init.ini"
  return, fileName
  
END

;**************************
; get/set
;**************************
FUNCTION SmurffFileSystem::getLastUsedFileName

  return, self.lastUsedFileName
  
END

PRO SmurffFileSystem::setLastUsedFileName, fileName

  self.lastUsedFileName=fileName
  
END

;***********************
; constructor/destructor
;***********************

FUNCTION SmurffFileSystem::init, applicationName, mainApplication, applicationRoot=applicationRoot, recordSeparator=recordSeparator, MUSTEXISTS=MUSTEXISTS

  if not self -> FileSystem :: init(applicationName, mainApplication, applicationRoot=applicationRoot, recordSeparator=recordSeparator, MUSTEXISTS=MUSTEXISTS) then return , 0
  return , 1
  
END

PRO SmurffFileSystem::cleanUp

  self -> Object :: cleanUp
  obj_destroy, self.utility
  
END

;****************************************************************************************

PRO SmurffFileSystem__Define

  Struct = { SmurffFileSystem , $
    groupCodesPrefix: '', $
    Inherits FileSystem $
    }
    
END

;****************************************************************************************
