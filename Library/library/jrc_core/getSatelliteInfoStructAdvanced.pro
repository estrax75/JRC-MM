FUNCTION getSatelliteInfoStructAdvanced

  struct = { siteName:'', $
    sensorName: '', $
    sourceFile: '', $
    assignedExtractionFile: '', $
    straightExtractionFile: '', $
    istatus:0l, $
    orbitNumber:0l, $
    edgeEWflag:0l, $
    edgeNSflag:0l, $
    findPixelistatus:0l, $
    dayOfYear:0l, $
    day:0l, $
    month:0l, $
    year:0l, $
    matchUp1Count:0, $
    nonFlaggedCount:0, $
    matchUpConditions:strarr(5), $
    matchUpResults:bytarr(5), $
    flagsPixel:ptrarr(32), $
    flagsTotal:intarr(32) $
  }
  
  return, struct
  
END