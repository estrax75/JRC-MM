FUNCTION GlobSeaWiFSInfo::init, application, filename, mode=mode

  if not(self -> GlobInfo::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO GlobSeaWiFSInfo::cleanUp

  self -> GlobInfo::cleanUp
  
END

PRO GlobSeaWiFSInfo__Define

  Struct = { GlobSeaWiFSInfo , $
    Inherits GlobInfo $
    }
    
END