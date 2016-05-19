FUNCTION GlobPar::getVersion

  return, '1.0'
  
END

PRO GlobPar::streamPrint

  self->DataFile::cleanup
  
END

;*****************************
; constructor/destructor
;*****************************

PRO GlobPar::cleanUp

  self->DataFile::cleanup
  
END

FUNCTION GlobPar::init

  if not (self -> DataFile :: init()) then return, 0
  return, 1
  
END

PRO GlobPar__Define

  Struct = { GlobPar , $
    Inherits DataFile $
    }
    
END