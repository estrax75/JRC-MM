FUNCTION Physical::getVersion

  return, '1.0'
  
END

PRO Physical::streamPrint

  self->DataFile::stresmPrint
  
END

;*****************************
; constructor/destructor
;*****************************

PRO Physical::cleanUp

  self->DataFile::cleanup
  
END

FUNCTION Physical::init

  if not (self -> DataFile :: init()) then return, 0
  return, 1
  
END

PRO Physical__Define

  Struct = { Physical, $
    Inherits DataFile $
    }
    
END