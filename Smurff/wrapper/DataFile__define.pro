FUNCTION DataFile::getVersion

  return, '1.0'
  
END

PRO DataFile::setCode, value

  self.code=value
  
END

FUNCTION DataFile::getCode

  return, self.code
  
END

PRO DataFile::setDisplayname, value

  self.displayname=value
  
END

FUNCTION DataFile::getDisplayname

  return, self.displayname
  
END

FUNCTION DataFile::getfiletype

  return, self.filetype
  
END

PRO DataFile::setfiletype, value

  self.filetype=value
  
END

FUNCTION DataFile::getBuildFileNameFunction

 return, self.buildFileNameFunction

END

PRO DataFile::setBuildFileNameFunction, value

 self.buildFileNameFunction=value

END

FUNCTION DataFile::getReadContentsFunction

 return, self.readContentsFunction

END

PRO DataFile::setReadContentsFunction, value

 self.readContentsFunction=value

END

FUNCTION DataFile::getarchiveroot

  return, self.archiveroot
  
END

PRO DataFile::setarchiveroot, value

  self.archiveroot=value
  
END

FUNCTION DataFile::getdescription

  return, self.description
  
END

PRO DataFile::setDescription, value

  self.description=value
  
END

FUNCTION DataFile::getParameterCodeChl

  return, self.parameterCodeChl
  
END

PRO DataFile::setParameterCodeChl, value

  self.parameterCodeChl=value
  
END

FUNCTION DataFile::getOuputDir

  return, self.outputDir
  
END

PRO DataFile::setparametersList, list

  ptr_free, self.parametersList
  self.parametersList=ptr_new(list, /NO_COPY)
  
END

FUNCTION DataFile::getparametersList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.parametersList) then   return, *self.parametersList
  NOTFOUND=1
  return, [-1]
  
END

PRO DataFile::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  print, '**** code:', self->getcode()
  print, '**** displayname:', self->getdisplayname()
  print, '**** filetype:', self->getfiletype()
  print, '**** buildfilenameFunction:', self->getBuildFileNameFunction()
  print, '**** readContentsFunction:', self->getReadContentsFunction()
  print, '**** archiveroot:', self->getarchiveroot()
  print, '**** description:', self->getdescription()
  print, '**** parametersList:', self->getparametersList()
  print, '**** parameterCodeChl:', self->parameterCodeChl()
  
END

;*****************************
; constructor/destructor
;*****************************

PRO DataFile::cleanUp

  ptr_free, self.parametersList
  self->SimpleXML::cleanup
  
END

FUNCTION DataFile::init

  if not (self -> SimpleXML :: init(obj_class(self), [''])) then return, 0
  return, 1
  
END

PRO DataFile__Define

  Struct = { DataFile , $
    code: '', $
    displayName: '', $
    fileType: '', $
    buildFileNameFunction: '', $
    readContentsFunction: '', $
    archiveRoot: '', $
    description: '', $
    parametersList: ptr_new(), $
    parameterCodeChl: '', $
    Inherits SimpleXML $
    }
    
END