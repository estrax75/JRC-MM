FUNCTION GlobModisAInfo::init, application, filename, mode=mode

  if not(self -> GlobInfo::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO GlobModisAInfo::cleanUp

  self -> GlobInfo::cleanUp
  
END

PRO GlobModisAInfo

  Struct = { GlobModisAInfo , $
    Inherits GlobInfo $
    }
    
END