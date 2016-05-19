;*****************************
; constructor/destructor
;*****************************

PRO GlobOldSeaWiFSPar::cleanUp

  self->GlobPar::cleanup
  
END

FUNCTION GlobOldSeaWiFSPar::init

  if not (self -> GlobPar :: init()) then return, 0
  return, 1
  
END

PRO GlobOldSeaWiFSPar__Define 

  Struct = { GlobOldSeaWiFSPar , $
    Inherits GlobPar $
    }
    
END