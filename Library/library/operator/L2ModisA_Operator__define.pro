@..\wrapper\structure_definition
FUNCTION L2ModisA_Operator::doParCodeCorrection, parCodes

  parTest=strupcase(parCodes)
  parCheckIdx=where(parTest eq 'CHLOR_A', count)
  if count ge 1 then parTest[parCheckIdx]='CHLA'
  parCheckIdx=where(parTest eq 'AOT_865', count)
  if count ge 1 then parTest[parCheckIdx]='AOT_869'
  parCheckIdx=where(parTest eq 'RRS_490', count)
  if count ge 1 then parTest[parCheckIdx]='RRS_488'
  parCheckIdx=where(parTest eq 'ANGSTROM', count)
  if count ge 1 then parTest[parCheckIdx]='ANGSTROM_531'
  parCheckIdx=where(parTest eq 'RRS_510', count)
  if count ge 1 then parTest[parCheckIdx]='RRS_531'
  parCheckIdx=where(parTest eq 'RRS_670', count)
  if count ge 1 then parTest[parCheckIdx]='RRS_667'
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

PRO L2ModisA_Operator::CleanUp

  self->L2_Operator::Cleanup
  
END

FUNCTION L2ModisA_Operator::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
  REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY, sensorCode=sensorCode
  
  if not (self -> L2_Operator :: init(application, workingDir, periodType, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  if keyword_set(sensorCode) then self.sensorCode=sensorCode else self.sensorCode='ModisA' 
  return, 1
  
END

PRO L2ModisA_Operator__Define

  Struct = { L2ModisA_Operator , $
    Inherits L2_Operator $
  }
  
END