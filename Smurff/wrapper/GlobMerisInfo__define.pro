FUNCTION GlobMerisInfo::init, application, filename, mode=mode

  if not(self -> GlobInfo::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO GlobMerisInfo::cleanUp

  self -> GlobInfo::cleanUp
  
END

PRO GlobMerisInfo

  Struct = { GlobMerisInfo , $
    Inherits GlobInfo $
    }
    
END