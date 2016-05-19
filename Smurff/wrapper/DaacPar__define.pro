FUNCTION DaacPar::getVersion

  return, '1.0'
  
END

PRO DaacPar::streamPrint

  self->DataFile::cleanup
  
END

;*****************************
; constructor/destructor
;*****************************

PRO DaacPar::cleanUp

  self->DataFile::cleanup
  
END

FUNCTION DaacPar::init

  if not (self -> DataFile :: init()) then return, 0
  return, 1
  
END

PRO DaacPar__Define

  Struct = { DaacPar , $
    Inherits DataFile $
    }
    
END