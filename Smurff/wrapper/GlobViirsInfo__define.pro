FUNCTION GlobViirsInfo::init, application, filename, mode=mode

  if not(self -> GlobInfo::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO GlobViirsInfo::cleanUp

  self -> GlobInfo::cleanUp
  
END

PRO GlobViirsInfo

  Struct = { GlobViirsInfo , $
    Inherits GlobInfo $
    }
    
END