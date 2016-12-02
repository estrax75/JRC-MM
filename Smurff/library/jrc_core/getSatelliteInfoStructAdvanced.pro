; MM ver nov 2016
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
    hour:0., $
    dayOfYear:0l, $
    day:0l, $
    month:0l, $
    year:0l, $
    ;matchUp1Count:0, $
    nonFlaggedCount:0, $
    ;    matchUpConditions:strarr(19), $
    ;    matchUpResults:bytarr(19), $
    ;    plotTitles:strarr(19), $
    matchUpConditions:strarr(25), $
    matchUpResults:bytarr(25), $
    plotTitles:strarr(25), $
    flagsPixel:ptrarr(32), $
    flagsTotal:intarr(32), $
    statDesc:strarr(2), $
    statParameters:strarr(19), $
    statMeans:fltarr(19), $
    statStdDev:fltarr(19) $
    ;    statParameters: ptr_new(), $
    ;    statValues:ptrarr(2) $
    ;    angStat:fltarr(2), $
    ;    aotStat:fltarr(2), $
    ;    chlaStat:fltarr(2), $
    ;    sunZenithStat:fltarr(2) $
  }

  return, struct

END

;FUNCTION getSatelliteInfoStructAdvanced
;
;  struct = { siteName:'', $
;    sensorName: '', $
;    sourceFile: '', $
;    assignedExtractionFile: '', $
;    straightExtractionFile: '', $
;    istatus:0l, $
;    orbitNumber:0l, $
;    edgeEWflag:0l, $
;    edgeNSflag:0l, $
;    findPixelistatus:0l, $
;    hour:0., $
;    dayOfYear:0l, $
;    day:0l, $
;    month:0l, $
;    year:0l, $
;    ;matchUp1Count:0, $
;    nonFlaggedCount:0, $
;;    matchUpConditions:strarr(19), $
;;    matchUpResults:bytarr(19), $
;;    plotTitles:strarr(19), $
;    matchUpConditions:strarr(25), $
;    matchUpResults:bytarr(23), $
;    plotTitles:strarr(23), $
;    flagsPixel:ptrarr(32), $
;    flagsTotal:intarr(32), $
;    statDesc:strarr(2), $
;    statParameters:strarr(19), $
;    statMeans:fltarr(19), $
;    statStdDev:fltarr(19) $
;;    statParameters: ptr_new(), $
;;    statValues:ptrarr(2) $
;;    angStat:fltarr(2), $
;;    aotStat:fltarr(2), $
;;    chlaStat:fltarr(2), $
;;    sunZenithStat:fltarr(2) $
; }
;  
;  return, struct
;  
;END
;FUNCTION getSatelliteInfoStructAdvanced
;
;  struct = { siteName:'', $
;    sensorName: '', $
;    sourceFile: '', $
;    assignedExtractionFile: '', $
;    straightExtractionFile: '', $
;    istatus:0l, $
;    orbitNumber:0l, $
;    edgeEWflag:0l, $
;    edgeNSflag:0l, $
;    findPixelistatus:0l, $
;    hour:0., $
;    dayOfYear:0l, $
;    day:0l, $
;    month:0l, $
;    year:0l, $
;    ;matchUp1Count:0, $
;    nonFlaggedCount:0, $
;    matchUpConditions:strarr(19), $
;    matchUpResults:bytarr(19), $
;    plotTitles:strarr(19), $
;    flagsPixel:ptrarr(32), $
;    flagsTotal:intarr(32), $
;    statDesc:strarr(2), $
;    ;    angStat:fltarr(2), $
;    ;    aotStat:fltarr(2), $
;    ;    chlaStat:fltarr(2), $
;    ;    sunZenithStat:fltarr(2) $
;  }
;
;  return, struct
;
;END