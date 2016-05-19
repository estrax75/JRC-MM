FUNCTION Model::getVersion

  return, '1.0'
  
END

PRO Model::streamPrint

  self->DataFile::stresmPrint
  
END

;*****************************
; constructor/destructor
;*****************************

PRO Model::cleanUp

  self->DataFile::cleanup
  
END

FUNCTION Model::init

  if not (self -> DataFile :: init()) then return, 0
  return, 1
  
END

PRO Model__Define

  Struct = { Model, $
    Inherits DataFile $
    }
    
END