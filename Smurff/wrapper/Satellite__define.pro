FUNCTION Satellite::getVersion

  return, '1.0'
  
END

PRO Satellite::streamPrint

  self->DataFile::cleanup
  
END

;*****************************
; constructor/destructor
;*****************************

PRO Satellite::cleanUp

  self->DataFile::cleanup
  
END

FUNCTION Satellite::init

  if not (self -> DataFile :: init()) then return, 0
  return, 1
  
END

PRO Satellite__Define

  Struct = { Satellite , $
    Inherits DataFile $
    }
    
END