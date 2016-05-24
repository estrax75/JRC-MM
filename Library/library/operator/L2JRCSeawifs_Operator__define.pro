@..\wrapper\structure_definition
FUNCTION L2JRCSeawifs_Operator::doParCodeCorrection, parCodes

  parTest=strupcase(parCodes)
  parCheckIdx=where(parTest eq 'CHLOR_A', count)
  if count ge 1 then parTest[parCheckIdx]='CHLA'
  parCheckIdx=where(parTest eq 'ANGSTROM', count)
  if count ge 1 then parTest[parCheckIdx]='ANGSTROM_510'

  parCheckIdx=where(parTest eq 'SENZ', count)
  if count ge 1 then parTest[parCheckIdx]='SATZENITH'
  parCheckIdx=where(parTest eq 'SENA', count)
  if count ge 1 then parTest[parCheckIdx]='SATAZIMUTH'
  parCheckIdx=where(parTest eq 'SOLZ', count)
  if count ge 1 then parTest[parCheckIdx]='SUNZENITH'
  parCheckIdx=where(parTest eq 'SOLA', count)
  if count ge 1 then parTest[parCheckIdx]='SUNAZIMUTH'

;  parCheckIdx=where(parTest eq 'SATZENITH', count)
;  if count ge 1 then parTest[parCheckIdx]='SENZ'
;  parCheckIdx=where(parTest eq 'SATAZIMUTH', count)
;  if count ge 1 then parTest[parCheckIdx]='SENA'
;  parCheckIdx=where(parTest eq 'SUNZENITH', count)
;  if count ge 1 then parTest[parCheckIdx]='SOLZ'
;  parCheckIdx=where(parTest eq 'SUNAZIMUTH', count)
;  if count ge 1 then parTest[parCheckIdx]='SOLA'
  
  return, parTest
  
END

PRO L2JRCSeawifs_Operator::CleanUp

  self->L2_Operator::Cleanup
  
END

FUNCTION L2JRCSeawifs_Operator::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
  REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY, sensorCode=sensorCode
  
  if not (self -> L2_Operator :: init(application, workingDir, periodType, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  if keyword_set(sensorCode) then self.sensorCode=sensorCode else self.sensorCode='JRCSeaWiFS' 
  return, 1
  
END

PRO L2JRCSeawifs_Operator__Define

  Struct = { L2JRCSeawifs_Operator , $
    Inherits L2_Operator $
  }
  
END