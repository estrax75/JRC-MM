FUNCTION ColorMap::getEntryByCode, code

  return, self->getEntryByKey(code)
  
END

FUNCTION ColorMap::getEntryByKeys, parameterCode, basinCode

  for i=0, self.counter-1 do if (*self.mapEntries)[i]->equalsByKeys(parameterCode, basinCode)  then return, (*self.mapEntries)[i]
  return, obj_new()
  
END

FUNCTION ColorMap::getElementClass

 return, 'ColorScaleInfoEntry'

END

FUNCTION ColorMap::init

  if not self -> ConfigurationMap :: init() then return , 0
  return , 1
  
END

PRO ColorMap::cleanUp

  self -> ConfigurationMap::Cleanup
  
END

;****************************************************************************************

PRO ColorMap__Define

  Struct = { ColorMap , $
    Inherits ConfigurationMap $
    }
    
END

;****************************************************************************************
