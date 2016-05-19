FUNCTION GlobOldSeaWiFSInfo::init, application, filename, mode=mode

  if not(self -> GlobInfo::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO GlobOldSeaWiFSInfo::cleanUp

  self -> GlobInfo::cleanUp
  
END

PRO GlobOldSeaWiFSInfo__Define

  Struct = { GlobOldSeaWiFSInfo , $
    Inherits GlobInfo $
    }
    
END