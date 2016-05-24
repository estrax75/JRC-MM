FUNCTION getSatelliteInfoStruct

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
    hour:0l, $
    dayOfYear:0l, $
    day:0l, $
    month:0l, $
    year:0l $
  }
  
  return, struct
  
END