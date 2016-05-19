@..\wrapper\structure_definition
FUNCTION L2Meris_Operator::doParCodeCorrection, parCodes

  parTest=strupcase(parCodes)
  parCheckIdx=where(parTest eq 'CHLOR_A', count)
  if count ge 1 then parTest[parCheckIdx]='CHLA'
  parCheckIdx=where(parTest eq 'RRS_555', count)
  if count ge 1 then parTest[parCheckIdx]='RRS_560'
  parCheckIdx=where(parTest eq 'RRS_412', count)
  if count ge 1 then parTest[parCheckIdx]='RRS_413'
  parCheckIdx=where(parTest eq 'ANGSTROM', count)
  if count ge 1 then parTest[parCheckIdx]='ANGSTROM_510'
  parCheckIdx=where(parTest eq 'RRS_670', count)
  if count ge 1 then parTest[parCheckIdx]='RRS_665'
  parCheckIdx=where(parTest eq 'SOLZ', count)
  if count ge 1 then parTest[parCheckIdx]='SUNZENITH'
  parCheckIdx=where(parTest eq 'SOLA', count)
  if count ge 1 then parTest[parCheckIdx]='SUNAZIMUTH'
  parCheckIdx=where(parTest eq 'SENZ', count)
  if count ge 1 then parTest[parCheckIdx]='SATZENITH'
  parCheckIdx=where(parTest eq 'SENA', count)
  if count ge 1 then parTest[parCheckIdx]='SATAZIMUTH'
  
  return, parTest
  
END

PRO L2Meris_Operator::CleanUp

  self->L2_Operator::Cleanup
  
END

FUNCTION L2Meris_Operator::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
  REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY, sensorCode=sensorCode
  
  if not (self -> L2_Operator :: init(application, workingDir, periodType, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  if keyword_set(sensorCode) then self.sensorCode=sensorCode else self.sensorCode='Meris' 
  return, 1
  
END

PRO L2Meris_Operator__Define

  Struct = { L2Meris_Operator , $
    Inherits L2_Operator $
  }
  
END