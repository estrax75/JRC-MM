COMMON bigData, data_day, expectedDays, foundDays

FUNCTION getCoeffStruct

  struct = {G0coeffs:fltarr(6), G1coeffs:fltarr(11), $
    G2coeffs:fltarr(11), RahmanCoeffs_BLUE:fltarr(3), $
    RahmanCoeffs_RED:fltarr(3), RahmanCoeffs_NIR:fltarr(3), $
    soilCoeffs: fltarr(2) $
  }

  return, struct

END

FUNCTION getSatInfoStruct

  struct = { sourceFile: '', $
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
    year:0l $
  }

  return, struct

END

FUNCTION getSiteInfoStruct

  struct = { siteCode:'', $
    expected:0l, $
    read:0l, $
    discarded:0l, $
    edgesType1:0l, $
    edgesType2:0l, $
    duplicate:0l $
  }

  return, struct

END

FUNCTION getExtractInfoStruct

  struct = { lat:0., $
    lon:0., $
    n_side:0l, $
    ext:'' $
  }

  return, struct

END

FUNCTION getInvalidStruct

  struct = { invalid_count:0l, $
    sigma_filter:0l, $
    valid_count:0l, $
    expected:0l, $
    found:0 $
  }

  return, struct

END

FUNCTION getExtraSerieInfoStruct, UNDEF=UNDEF

  if keyword_set(UNDEF) then startValue='undef' else startValue=''
  struct = { year:startValue, $
    month:startValue, $
    roiName:startValue, $
    roiBoundary:startValue, $
    parName:startValue, $
    statName:startValue, $
    statValue:startValue, $
    validValues:startValue, $
    totValues:startValue, $
    percValues:startValue $
  }

  return, struct

END

FUNCTION getStatInfoStruct, UNDEF=UNDEF

  if keyword_set(UNDEF) then startValue='undef' else startValue=''
  struct = { year:startValue, $
    month:startValue, $
    roiName:startValue, $
    roiBoundary:startValue, $
    parName:startValue, $
    statName:startValue, $
    statValue:startValue, $
    validValues:startValue, $
    totValues:startValue, $
    percValues:startValue $
  }

  return, struct

END

; *******************************************************
; initialize a structure of "FMParameter" type
; *******************************************************

;Parameter
;Code  PK  #3
;TypeCode  FK  #3
;DisplayName   string20
;MeasureUnit   string10
;Description   string150

FUNCTION getParameterInfoStruct

  struct = { displayName: '', $
    shortName: '', $
    bandName: '', $
    longName: '', $
    measureUnit: '', $
    binsize: 1., $
    cutvalue: -1, $
    description: '' $
  }

  return, struct

END

;FUNCTION getSplitInfoStruct
;
; struct = { splitInfo, $
;      name: '', $
;      index: 0, $
;      borderCoords: fltarr(2,5), $
;      labelPosition: fltarr(2) $
; }
; return, struct
;
;END

;
;FUNCTION getSplitBatchStruct, n_elements
;
; struct = { requests: replicate(getSingleRequestStruct(), n_elements), $
;      splitStyle: "", $
;      graphicType: "" $
; }
; return, struct
;
;END
;
;FUNCTION getSingleRequestStruct, n_elements
;
; struct = { entity: obj_new(), $
;      elaboration: obj_new(), $
;      location: fltarr(4) $
; }
; return, struct
;
;END
;
;; Here new code 2010/05/26
;; "FM" prefix is for FairMode
;
;; Here generic application structures
;
;FUNCTION getDateTimeStruct
;
; dtu=obj_new('DateTimeUtility')
; dts=dtu->getDateTimeStruct()
; obj_destroy, dtu
; return, dts
;
;END
;
;; *******************************************************
;; initialize a structure of "TimeStamp" type
;; *******************************************************
;
;;value
;;template
;
FUNCTION getTimeStampStruct

  dtu=obj_new('DateTimeUtility')
  tss=dtu->getTimeStampStruct()
  obj_destroy, dtu
  return, tss

END
;
;; *******************************************************
;; initialize a structure of "ResultData" type
;; *******************************************************
;
;;observedCode: '', $
;;runCode: '', $
;;modelCode: '', $
;;scenarioCode: '', $
;;parameterCode: '', $
;;observedData: ptr_new(), $
;;runData: ptr_new() $
;
;FUNCTION getResultData
;
; struct = { resultData, $
;      observedCode: '', $
;      runCode: '', $
;      modelCode: '', $
;      scenarioCode: '', $
;      parameterCode: '', $
;      observedMinVal: 0., $
;      observedMaxVal: 0., $
;      runMinVal: 0., $
;      runMaxVal: 0., $
;      runData: ptr_new(), $
;      observedData: ptr_new() $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMMainData" type
;; *******************************************************
;
;;		'MAINCONFIGFILE'
;;		'CATEGORYFILE'
;;		'MODELFILE'
;;		'SCENARIOFILE'
;;		'PARAMETERFILE'
;;		'IDLROUTINEFILE'
;;		'PARAMETERTYPE'
;;;		'AXISTYPE'
;;		'GROUPBYTIME'
;;		'GROUPBYSTAT'
;;		'DIAGRAM'
;;   'SEASON'
;;   'DAYPERIOD'
;
;FUNCTION getFMMainDataStruct
;
; struct = { mainConfig: obj_new(), category: obj_new(), $
; 			model: obj_new(), scenario: obj_new(), $
; 			parameter: obj_new(), idlRoutine: obj_new(), $
;      ;parameterType: obj_new(), axisType: obj_new(), $
;      parameterType: obj_new(), $
; 			groupByTime: obj_new(), groupByStat: obj_new(), $
; 			diagram: obj_new(), season: obj_new(), dayPeriod: obj_new() $
; }
; return, struct
;
;END
;
;; End
;; Here DB (or config files) related structures
;
;; *********UNUSED****************************************
;; initialize a structure of "MainConfig" type
;; *******************************************************
;
;;MAIN_CONFIG
;;Code	PK	#3
;;DisplayName		string20
;;Description		string150
;
;FUNCTION getMainConfig
;
; struct = { code:0, $
; 			displayName:"", $
; 			description: "" $
; }
;
; return, struct
;
;END
;; *******************************************************
;; initialize a structure of "FMModeConfig" type
;; *******************************************************
;
;;MODE_CONFIG
;;Code	PK	#3
;;DisplayName		string20
;;Description		string150
;
;FUNCTION getFMModeConfig
;
; struct = { code:0, $
; 			displayName:"", $
; 			description: "" $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMTimePeriod" type
;; *******************************************************
;
;;TIMEPERIOD
;;Code	PK	#3
;;Display_Name		string20
;;StartValue		string10
;;EndValue		string10
;;Template   string
;;Description   string
;
;FUNCTION getFMTimePeriod
;
; struct = { code:0, $
; 			displayName:'', $
; 			startValue:'', $
; 			endValue: '', $
; 			template: '', $
; 			description: '' $
; }
;
; return, struct
;
;END
;
;;MENUINFO
;;code PK  #3
;;DisplayName    ''
;;FileName    ''
;;IsMenu  0|1
;;Level    0|1|2
;;FatherCode    FK  #3
;
;; *******************************************************
;; initialize a structure of "FMMenuInfo" type
;; *******************************************************
;
;FUNCTION getFMMenuInfo
;
; struct = { code:0, $
;      displayName: '', $
;      fileName: '', $
;      isMenu: 0, $
;      level: 0, $
;      fatherCode: 0 $
; }
;
; return, struct
;
;END
;
;;CATEGORY
;;Code	PK	#3
;;DisplayName		string20
;;Description		string150
;
;; *******************************************************
;; initialize a structure of "FMCategory" type
;; *******************************************************
;
;FUNCTION getFMCategory
;
; struct = { code:0, $
; 			displayName:"", $
; 			description: "" $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMParameter" type
;; *******************************************************
;
;;Parameter
;;Code	PK	#3
;;TypeCode	FK	#3
;;DisplayName		string20
;;MeasureUnit		string10
;;Description		string150
;
;FUNCTION getFMParameter
;
; struct = { code:'', $
; 			typeCode:'', $
; 			displayName: '', $
;			measureUnit: '', $
;			description: '' $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMParameterFile" type
;; *******************************************************
;
;;Parameter
;;name	PK	string
;;typeCode	FK	string
;;measureUnit		string10
;
;FUNCTION getFMParameterFile
;
; struct = { name:'', $
; 			typeCode: '', $
;			measureUnit: '' $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMModel" type
;; *******************************************************
;
;;MODEL
;;Code	PK	#3
;;DisplayName		string20
;;Description		string150
;
;FUNCTION getFMModel
;
; struct = { code:'',$
; 			displayName:'',$
; 			description: ''$
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMScenario" type
;; *******************************************************
;
;;SCENARIO
;;Code	PK	#3
;;DisplayName		string20
;;Description		string150
;
;FUNCTION getFMScenario
;
; struct = { code:'',$
; 			displayName:"", $
; 			description: "" $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMRunFile" type
;; *******************************************************
;
;;RUNFILE
;;stationCode
;;stationName
;;altitude
;;lon
;;lat
;;region
;;stationType
;;areaType
;;siting
;;parameters
;
;FUNCTION getFMRunFile
;
; struct = { model: '', $					; model name/id
;			scenario: '', $					; scenario name/id
;			filename: '', $
;			execDate: '' $
;			;parameters: ptr_new() $			; list of parameters
;}
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMRun" type
;; *******************************************************
;
;;RUN
;;Code	PK	#3
;;ScenarioCode	FK	#3
;;ModelCode	FK	#3
;;AreaCode	FK	#3
;;DisplayName		string20
;;GridXStart		#3,6
;;GridXStep		#3,6
;;GridXStepNumber		#3,6
;;GridYStart		#3,6
;;GridYStep		#3,6
;;GridYStepNumber		#3,6
;;GridVInterpolation	#3,6
;;GridHInterpolation	#3,6
;;ExecutionDate		date
;;FileName		string30
;;Description		string150
;
;FUNCTION getFMRun
;
; struct = { code:0, $
; 			scenarioCode:'', $
; 			modelCode: '', $
; 			areaCode: 0, $
;			displayName: "", $
;			gridXStart:0., $
;			gridXStep:0., $
;			gridXStepNumber:0, $
;			gridYStart:0., $
;			gridYStep:0., $
;			gridYStepNumber:0, $
;			gridVerticalInterpolation:0., $
;			gridHorizontalInterpolation:0., $
;			executionDate:"", $
;			fileName:"", $
;			description:"" $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMMonitor" type
;; *******************************************************
;
;;MONITOR
;;stationCode
;;stationName
;;shortName
;;altitude
;;lon
;;lat
;;GMTLag
;;region
;;stationType
;;areaType
;;siting
;;parameters
;
;FUNCTION getFMMonitor
;
; struct = { stationCode: '', $					; code/id of observation
;      stationName: '', $          ; name to display (menu list)
;      shortName: '', $          ; short name to plot (graphic area)
;			altitude: 0., $						; height above sea
;			lon: 0., $							; only if like ground station (Longitude degrees)
;      lat: 0., $              ; only if like ground station (Latitude degrees)
;      gmtLag: '', $              ; offset from GMT (code like GMT+1, GMT-1...)
;			region: '', $						; Admin
;			stationType: '', $					; background/traffic/industrial/unknown
;			areaType: '', $						; plain/valley/hilly/mountain/coastal
;			siting: '', $						; urban/suburban/rural
;			parameters: '' $				; list of parameters
;}
; ;ptr_free, struct.parameters
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMObserved" type
;; *******************************************************
;
;;OBSERVED
;;Code	PK	#3
;;Name		string50
;;Display_Name   string20
;;Short_Name   string20
;;XGeoLocation		#4,6
;;YGeoLocation		#4,6
;;HeightAboveSea		#4
;;Description		string150
;;country			string20
;;countryGMT			string3
;
;FUNCTION getFMObserved
;
; struct = { code: '', $					; code/id of observation
;			name: '', $					; name of ground station
;      displayName: '', $      ; name to display (menu list)
;      shortName: '', $      ; short name to plot
;			;type: "", $					; ground station/Lidar/other
;			;category: "", $				; region/type of veichle/other if applicable
;			;polyCode: 0b, $				; display polygon code (see getObservedPolygon function).
;			;isStation: 0b, $			; boolean
;			xGeoLocation: 0., $			; only if like ground station (Longitude degrees)
;			yGeoLocation: 0., $			; only if like ground station (Latitude degrees)
;			heightAboveSea: 0, $		; height above sea
;			description: "", $				; !!!
;			country: "", $				; !!!
;			countryGMT: "", $				; !!!
;			;dataFile: "", $				; file contains data
;			;startDate: intarr(6), $		; date of first data
;			;endDate: intarr(6), $		; date of last data
;			parameters: ptr_new() $		; string array [n] of names
;			;measureUnits: ptr_new(), $	; string array [n] of names
;}
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMObservedCategory" type
;; *******************************************************
;
;;OBSERVED_CATEGORY
;;Code	PK	#3
;;ObservedCode	FK	#3
;;CategoryCode	FK	#3
;;Value		string20
;
;FUNCTION getFMObservedCategory
;
; struct = { code:0, $
; 			observedCode:'', $
; 			categoryCode:'', $
;			value: "" $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMObservedParameter" type
;; *******************************************************
;
;;OBSERVED_PARAMETER
;;Code	PK	#3
;;ObservedCode	FK	#3
;;ParameterCode	FK	#3
;;FileName		string30
;;MonitorSampling		#2,1
;;StartAvailability		date
;;EndAvailability		date
;;InternalParameterCode		string10
;;Description		string150
;
;FUNCTION getFMObservedParameter
;
; struct = { code:0, $
; 			observedCode:'', $
; 			parameterCode: 0, $
;			fileName: "", $
;			monitorSampling:"", $ ; time stamp hh24:mi
;			startAvailability: "" , $ ; time stamp is: yyyymmddhh24:mi
;			endAvailability: "" , $ ; time stamp is: yyyymmddhh24:mi
;			internalParameterCode: "" , $
;			description: "" $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMRunParameter" type
;; *******************************************************
;
;;RUN_PARAMETER
;;Code	PK	#3
;;RunCode	FK	#3
;;ParameterCode	FK	#3
;;FileName		string30
;;MonitorSampling		#2,1
;;StartAvailability		date
;;EndAvailability		date
;;InternalParameterCode		string10
;;Description		string150
;
;FUNCTION getFMRunParameter
;
; struct = { code:0, $
; 			runCode:0, $
; 			parameterCode: 0, $
;			fileName: "", $
;			monitorSampling:"", $ ; time stamp hh24:mi
;			startAvailability: "" , $ ; standard time stamp is: yyyymmddhh24:mi
;			endAvailability: "" , $ ; standard time stamp is: yyyymmddhh24:mi
;			internalParameterCode: "" , $
;			description: "" $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMParameterType" type
;; *******************************************************
;
;;PARAMETER_TYPE
;;Code	PK	#3
;;DisplayName		string20
;;Description		string150
;
;FUNCTION getFMParameterType
;
; struct = { code:'', $
; 			displayName: '', $
; 			description: '' $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMGoalsCriteriaOC" type
;; *******************************************************
;
;;GOALS_CRITERIA_OC
;;Code	PK	#3
;;ElaborationCode	FK	#3
;;parameterCode	FK	#3
;;PeriodCode	FK	#3
;;AreaCode		FK	#3
;;Values		list of #5,2
;;Description		string150
;
;FUNCTION getFMGoalsCriteriaOC
;
; struct = { code:0, $
;      parameterCode:'', $
; 			scaleName:'', $
; 			statNickName: '', $
; 			periodName: '', $
;			values:ptr_new(), $
;			description:'' $
; }
; ptr_free, struct.values
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMElaboration" type
;; *******************************************************
;
;;ELABORATION
;;code
;;IDLRoutineCode
;;displayName
;;diagramCode
;;groupByTimeCode
;;groupByStatCode
;;seasonCode
;;dayPeriodCode
;;multipleChoiceFlag
;;numberRefValue
;;goalsCriteriaOCFlag
;;mode
;;description
;FUNCTION getFMElaboration
;
; struct = { code:0, $
;      IDLRoutineCode: 0, $
;      displayName: '', $
; 			diagramCode:'', $
;      groupByTimeCode: '', $
;      groupByStatCode: '', $
;      seasonCode: '', $
;      dayPeriodCode: '', $
;      ;multipleChoiceFlags: bytarr(4), $
;      multipleChoiceFlags: ptr_new(), $
;      numberRefValue: 0, $
;      goalsCriteriaOCFlag: 0, $
;      mode: '', $
;			description: '' $
; }
; ptr_free, struct.multipleChoiceFlags
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMIDLRoutine" type
;; *******************************************************
;
;;IDL_ROUTINE
;;Code	PK	#3
;;RoutineName		string15
;;InParameters		list of string15
;;OutParameters		list of string15
;;Description		string150
;
;FUNCTION getFMIDLRoutine
;
; struct = { code:0, $
; 			routineName: "", $
;; 			inParameters: ptr_new(), $
;;			outParameters: ptr_new(), $
;			description: "" $
; }
;; ptr_free, struct.outParameters, struct.inParameters
;
; return, struct
;
;END
;
;;; *******************************************************
;;; initialize a structure of "FMAxisType" type
;;; *******************************************************
;;
;;;AXIS_TYPE
;;;Code	PK	#3
;;;Name		string20
;;;IDLRoutineCode	FK	#3
;;;Description		string150
;;
;;FUNCTION getFMAxisType
;;
;; struct = { code:0, $
;;      IDLRoutineCode:0, $
;;      multipleChoice:'', $
;; 			description:'', $
;; 			name: '' $
;; }
;;
;; return, struct
;;
;;END
;
;; *******************************************************
;; initialize a structure of "FMGroupByTime" type
;; *******************************************************
;
;;GROUP_BY_TIME
;;Code	PK	#3
;;DisplayName		string10
;;Value			string10
;;TimeStamp		string5
;
;FUNCTION getFMGroupByTime
;
; struct = { code:0, $
; 			displayName: "", $
; 			value: "", $
;			timeStamp: "" $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMMonitoringGroupStat" type
;; *******************************************************
;
;;MONITORING_GROUP_STAT
;;Code PK  #3
;;DisplayName    string10
;;Value    string10
;;Operation    #1
;
;FUNCTION getFMMonitoringGroupStat
;
; struct = { code:0, $
;      displayName:"", $
;      value: "", $
;      operation: "" $
; }
;
; return, struct
;
;END
;; *******************************************************
;; initialize a structure of "FMGroupByStat" type
;; *******************************************************
;
;;GROUP_BY_STAT
;;Code PK  #3
;;DisplayName    string10
;;Value    string10
;;Operation    #1
;
;FUNCTION getFMGroupByStat
;
; struct = { code:0, $
;      displayName:"", $
;      value: "", $
;      operation: "" $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "FMDiagram" type
;; *******************************************************
;
;;DIAGRAM
;;Code	PK	#3
;;Display_Name		string20
;;;AxysCodes list of codes
;;MaxMultipleChoice int
;;Extra		string150
;
;FUNCTION getFMDiagram
;
; struct = { code:0, $
; 			displayName:'', $
;      plotRoutine:'', $
;      ;axisCodes:ptr_new(), $
;      maxMultipleChoice:0,$
; 			description: '' $
; }
;
;; ptr_free, struct.axisCodes
; return, struct
;
;END
;
;; End DB related (or file) structures
;
;; *******************************************************
;; facility access procedure to a variable of "VParameterData" type
;; *******************************************************
;
;PRO initVParameterData, vParameterStruct, dataFile=dataFile, $
;	vectorFlag=vectorFlag, id=id, name=name, zFlag=zFlag, $
;	type=type, measureUnit=measureUnit, obsName=obsName, $
;	fact2=fact2, configurationFile=configurationFile, $
;	frameNumber=frameNumber, sampleFreq=sampleFreq, $
;	rawData=rawData, startDate=startDate, endDate=endDate
;
; if n_elements(dataFile) ne 0 then vParameterStruct.dataFile=dataFile
; if n_elements(configurationFile) ne 0 then vParameterStruct.configurationFile=configurationFile
; if n_elements(vectorFlag) ne 0 then vParameterStruct.vectorFlag=vectorFlag
; if n_elements(id) ne 0 then vParameterStruct.id=id
; if n_elements(name) ne 0 then vParameterStruct.name=name
; if n_elements(type) ne 0 then vParameterStruct.type=type
; if n_elements(zFlag) ne 0 then vParameterStruct.zFlag=zFlag
; if n_elements(measureUnit) ne 0 then vParameterStruct.measureUnit=measureUnit
; if n_elements(obsName) ne 0 then vParameterStruct.obsName=obsName
; if n_elements(fact2) ne 0 then begin
;	;ptr_free, vParameterStruct.fact2
;	vParameterStruct.fact2=ptr_new(fact2, /NO_COPY)
; endif
; if n_elements(frameNumber) ne 0 then vParameterStruct.frameNumber=frameNumber
; if n_elements(sampleFreq) ne 0 then vParameterStruct.sampleFreq=sampleFreq
; if n_elements(rawData) ne 0 then begin
;	;ptr_free, vParameterStruct.rawData
;	vParameterStruct.rawData=ptr_new(rawData, /NO_COPY)
; endif
; if n_elements(startDate) eq 6 then vParameterStruct.startDate=startDate
; if n_elements(endDate) eq 6 then vParameterStruct.endDate=endDate
; heap_gc
;
;END
;
;; *******************************************************
;; facility access procedure to a variable of "VMainData" type
;; *******************************************************
;
;PRO initVMainData, vDataStruct, meteoData=meteoData, $
;	chemData=chemData, aeroData=aeroData, $
;	observedData=observedData, obsAvailablePars=obsAvailablePars, $
;	parameterData=parameterData, meteoModelList=meteoModelList, $
;	chemModelList=chemModelList, aeroModelList=aeromodelList, $
;	controlWindow=controlWindow, dataSelection=dataSelection
;
; if n_elements(meteoData) ne 0 then begin
;	;ptr_free,vDataStruct.meteoData
;	vDataStruct.meteoData=ptr_new(meteoData, /NO_COPY)
; endif
; if n_elements(chemData) ne 0 then begin
;	;ptr_free,vDataStruct.chemData
;	vDataStruct.chemData=ptr_new(chemData, /NO_COPY)
; endif
; if n_elements(aeroData) ne 0 then begin
;	;ptr_free,vDataStruct.aeroData
;	vDataStruct.aeroData=ptr_new(aeroData, /NO_COPY)
; endif
; if n_elements(observedData) ne 0 then begin
;	;ptr_free,vDataStruct.observedData
;	vDataStruct.observedData=ptr_new(observedData, /NO_COPY)
; endif
; if n_elements(obsAvailablePars) ne 0 then begin
;	;ptr_free,vDataStruct.obsAvailablePars
;	vDataStruct.obsAvailablePars=ptr_new(obsAvailablePars, /NO_COPY)
; endif
; if n_elements(controlWindow) ne 0 then begin
;	;ptr_free,vDataStruct.controlWindow
;	vDataStruct.controlWindow=ptr_new(controlWindow, /NO_COPY)
; endif
; if n_elements(dataSelection) ne 0 then begin
;	;ptr_free,vDataStruct.dataSelection
;	vDataStruct.dataSelection=ptr_new(dataSelection, /NO_COPY)
; endif
; if n_elements(meteoModelList) ne 0 then begin
;	;ptr_free,vDataStruct.meteoModelList
;	vDataStruct.meteoModelList=ptr_new(meteoModelList, /NO_COPY)
; endif
; if n_elements(chemModelList) ne 0 then begin
;	;ptr_free,vDataStruct.chemModelList
;	vDataStruct.chemModelList=ptr_new(chemModelList, /NO_COPY)
; endif
; if n_elements(aeroModelList) ne 0 then begin
;	;ptr_free,vDataStruct.aeroModelList
;	vDataStruct.aeroModelList=ptr_new(aeroModelList, /NO_COPY)
; endif
;
; heap_gc
;
;END
;
;; *******************************************************
;; facility access procedure to a variable of "SAObservedDataStruct" type
;; *******************************************************
;
;PRO initSAObservedData, sAObservedData, id=id, $
;	name=name, type=type, category=category, isStation=isStation, $
;	xGeoLocation=xGeoLocation, yGeoLocation=yGeoLocation, comments=comments, $
;	params=params, measureUnits=measureUnits, xDataPos=xDataPos, $
;	yDataPos=yDataPos, zDataPos=zDataPos, dataFile=dataFile, $
;	timeDate=timeDate, values=values, validity=validity, $
;	height=height, polyCode=polyCode
;
; if n_elements(id) ne 0 then sAObservedData.id=id
; if n_elements(name) ne 0 then sAObservedData.name=name
; if n_elements(type) ne 0 then sAObservedData.type=type
; if n_elements(polyCode) ne 0 then sAObservedData.polyCode=polyCode
; if n_elements(category) ne 0 then sAObservedData.category=category
; if n_elements(isStation) ne 0 then sAObservedData.isStation=isStation
; if n_elements(xGeoLocation) ne 0 then sAObservedData.xGeoLocation=xGeoLocation
; if n_elements(yGeoLocation) ne 0 then sAObservedData.yGeoLocation=yGeoLocation
; if n_elements(height) ne 0 then sAObservedData.height=height
; if n_elements(comments) ne 0 then sAObservedData.comments=comments
; if n_elements(dataFile) ne 0 then sAObservedData.dataFile=dataFile
; if n_elements(params) ne 0 then begin
;	;ptr_free, sAObservedData.params
;	sAObservedData.params=ptr_new(params, /NO_COPY)
; endif
; if n_elements(measureUnits) ne 0 then begin
;	;ptr_free, sAObservedData.measureUnits
;	sAObservedData.measureUnits=ptr_new(measureUnits, /NO_COPY)
; endif
; if n_elements(xDataPos) ne 0 then begin
;	;ptr_free, sAObservedData.xDataPos
;	sAObservedData.xDataPos=ptr_new(xDataPos, /NO_COPY)
; endif
; if n_elements(yDataPos) ne 0 then begin
;	;ptr_free, sAObservedData.yDataPos
;	sAObservedData.yDataPos=ptr_new(yDataPos, /NO_COPY)
; endif
; if n_elements(zDataPos) ne 0 then begin
;	;ptr_free, sAObservedData.zDataPos
;	sAObservedData.zDataPos=ptr_new(zDataPos, /NO_COPY)
; endif
; if n_elements(timeDate) ne 0 then begin
;	;ptr_free, sAObservedData.timeDate
;	sAObservedData.timeDate=ptr_new(timeDate, /NO_COPY)
; endif
; if n_elements(startDate) ne 0 then begin
;	;ptr_free, sAObservedData.timeDate
;	sAObservedData.startDate=ptr_new(startDate, /NO_COPY)
; endif
; if n_elements(endDate) ne 0 then begin
;	;ptr_free, sAObservedData.timeDate
;	sAObservedData.endDate=ptr_new(endDate, /NO_COPY)
; endif
; if n_elements(values) ne 0 then begin
;	;ptr_free, sAObservedData.values
;	sAObservedData.values=ptr_new(values, /NO_COPY)
; endif
; if n_elements(validity) ne 0 then begin
;	;ptr_free, sAObservedData.validity
;	sAObservedData.validity=ptr_new(validity, /NO_COPY)
; endif
; heap_gc
;
;END
;
;; *******************************************************
;; facility access procedure to a variable of "SAParameterData" type
;; *******************************************************
;
;PRO initSAParameterData, sAparameterStruct, dataFile=dataFile, $
;	vectorFlag=vectorFlag, id=id, name=name, zFlag=zFlag, $
;	type=type, measureUnit=measureUnit, obsName=obsName, $
;	fact2=fact2, xDimension=xDimension, yDimension=yDimension, $
;	zDimension=zDimension, frameNumber=frameNumber, $
;	sampleFreq=sampleFreq, rawData=rawData, secondaryRawData=secondaryRawData, $
;	xGeoLoc=xGeoLoc, yGeoLoc=yGeoLoc, zGeoLoc=zGeoLoc, $
;	topography=topography, sourceCoordSystem=sourceCoordSystem, $
;	xMeasureUnit=xMeasureUnit, yMeasureUnit=yMeasureUnit, $
;	zMeasureUnit=zMeasureUnit, startDate=startDate
;
; if n_elements(dataFile) ne 0 then sAparameterStruct.dataFile=dataFile
; if n_elements(vectorFlag) ne 0 then sAparameterStruct.vectorFlag=vectorFlag
; if n_elements(id) ne 0 then sAparameterStruct.id=id
; if n_elements(name) ne 0 then sAparameterStruct.name=name
; if n_elements(type) ne 0 then sAparameterStruct.type=type
; if n_elements(zFlag) ne 0 then sAparameterStruct.zFlag=zFlag
; if n_elements(measureUnit) ne 0 then sAparameterStruct.measureUnit=measureUnit
; if n_elements(obsName) ne 0 then sAparameterStruct.obsName=obsName
; if n_elements(fact2) ne 0 then begin
;	;ptr_free, sAparameterStruct.fact2
;	sAparameterStruct.fact2=ptr_new(fact2, /NO_COPY)
; endif
; if n_elements(xDimension) ne 0 then sAparameterStruct.xDimension=xDimension
; if n_elements(yDimension) ne 0 then sAparameterStruct.yDimension=yDimension
; if n_elements(zDimension) ne 0 then sAparameterStruct.zDimension=zDimension
; if n_elements(frameNumber) ne 0 then sAparameterStruct.frameNumber=frameNumber
; if n_elements(sampleFreq) ne 0 then sAparameterStruct.sampleFreq=sampleFreq
; if n_elements(rawData) ne 0 then begin
;	;ptr_free, sAparameterStruct.rawData
;	sAparameterStruct.rawData=ptr_new(rawData, /NO_COPY)
; endif
; if n_elements(secondaryRawData) ne 0 then begin
;	;ptr_free, sAparameterStruct.rawData
;	sAparameterStruct.secondaryRawData=ptr_new(secondaryRawData, /NO_COPY)
; endif
; if n_elements(xGeoLoc) ne 0 then begin
;	;ptr_free, sAparameterStruct.xGeoLoc
;	sAparameterStruct.xGeoLoc=ptr_new(xGeoLoc, /NO_COPY)
; endif
; if n_elements(yGeoLoc) ne 0 then begin
;	;ptr_free, sAparameterStruct.yGeoLoc
;	sAparameterStruct.yGeoLoc=ptr_new(yGeoLoc, /NO_COPY)
; endif
; if n_elements(zGeoLoc) ne 0 then begin
;	;ptr_free, sAparameterStruct.zGeoLoc
;	sAparameterStruct.zGeoLoc=ptr_new(zGeoLoc, /NO_COPY)
; endif
; if n_elements(topography) ne 0 then begin
;	;ptr_free, sAparameterStruct.topography
;	sAparameterStruct.topography=ptr_new(topography, /NO_COPY)
; endif
; ;if n_elements(ixyzsys) ne 0 then sAparameterStruct.ixyzsys=ixyzsys
; if n_elements(sourceCoordSystem) ne 0 then sAparameterStruct.sourceCoordSystem=sourceCoordSystem
; if n_elements(xMeasureUnit) ne 0 then sAparameterStruct.xMeasureUnit=xMeasureUnit
; if n_elements(yMeasureUnit) ne 0 then sAparameterStruct.yMeasureUnit=yMeasureUnit
; if n_elements(zMeasureUnit) ne 0 then sAparameterStruct.zMeasureUnit=zMeasureUnit
; if n_elements(startDate) eq 6 then sAparameterStruct.startDate=startDate
; heap_gc
;
;END
;
;; *******************************************************
;; facility access procedure to a variable of "SAMainData" type
;; *******************************************************
;
;PRO initSAMainData, sAdataStruct, meteoData=meteoData, $
;	chemData=chemData, aeroData=aeroData, landUseData=landUseData, $
;	observedData=observedData, sliceInfo=sliceInfo, rawModelData=rawModelData, $
;	parameterData=parameterData, startDate=startDate, endDate=endDate, $
;	controlWindow=controlWindow, windParameterData=windParameterData, $
;	obsAvailablePars=obsAvailablePars
;
; if n_elements(meteoData) ne 0 then begin
;	;ptr_free,sAdataStruct.meteoData
;	sAdataStruct.meteoData=ptr_new(meteoData, /NO_COPY)
; endif
; if n_elements(chemData) ne 0 then begin
;	;ptr_free,sAdataStruct.chemData
;	sAdataStruct.chemData=ptr_new(chemData, /NO_COPY)
; endif
; if n_elements(aeroData) ne 0 then begin
;	;ptr_free,sAdataStruct.aeroData
;	sAdataStruct.aeroData=ptr_new(aeroData, /NO_COPY)
; endif
; if n_elements(obsAvailablePars) ne 0 then begin
;	;ptr_free,sAdataStruct.obsAvailablePars
;	sAdataStruct.obsAvailablePars=ptr_new(obsAvailablePars, /NO_COPY)
; endif
; if n_elements(landUseData) ne 0 then begin
;	;ptr_free,sAdataStruct.landUseData
;	sAdataStruct.landUseData=ptr_new(landUseData, /NO_COPY)
; endif
; if n_elements(observedData) ne 0 then begin
;	;ptr_free,sAdataStruct.observedData
;	sAdataStruct.observedData=ptr_new(observedData, /NO_COPY)
; endif
; if n_elements(sliceInfo) ne 0 then begin
;	;ptr_free,sAdataStruct.sliceInfo
;	sAdataStruct.sliceInfo=ptr_new(sliceInfo, /NO_COPY)
; endif
; if n_elements(rawModelData) ne 0 then begin
;	;ptr_free,sAdataStruct.rawModelData
;	sAdataStruct.rawModelData=ptr_new(rawModelData, /NO_COPY)
; endif
; if n_elements(parameterData) ne 0 then begin
;	;ptr_free,sAdataStruct.parameterData
;	sAdataStruct.parameterData=ptr_new(parameterData, /NO_COPY)
; endif
; if n_elements(windParameterData) ne 0 then begin
;	;ptr_free,sAdataStruct.windParameterData
;	sAdataStruct.windParameterData=ptr_new(windParameterData, /NO_COPY)
; endif
; if n_elements(controlWindow) ne 0 then begin
;	;ptr_free,sAdataStruct.controlWindow
;	sAdataStruct.controlWindow=ptr_new(controlWindow, /NO_COPY)
; endif
; if n_elements(startDate) eq 4 then sAdataStruct.startDate=startDate
; if n_elements(endDate) eq 4 then sAdataStruct.endDate=endDate
; heap_gc
;
;END
;
;; *******************************************************
;; initialize a structure of "ProjectionData" type
;; *******************************************************
;
;FUNCTION getProjectionDataStruct
;
; struct = { destCode: 0, $
; 			destName: "", $
; 			sourceCode: 0, $
; 			sourceName: "" $
; }
;
; return, struct
;
;END
;
;
;; *******************************************************
;; initialize a structure of "SAParameterData" type
;; *******************************************************
;
;FUNCTION getSAParameterDataStruct
;
; struct = { SAParameterDataStruct, dataFile: "", vectorFlag: "", $
; 			id: 0l, name: "", type: "", $
;			measureUnit: "", zFlag: "", $
;			obsName: "", fact2: ptr_new(),  $
;			xDimension: 0l, yDimension: 0l, $
;			zDimension: 0l, frameNumber: 0l, $
;			sampleFreq: "",	rawData: ptr_new(), $
;			xGeoLoc: ptr_new(), yGeoLoc: ptr_new(), $
;			zGeoLoc: ptr_new(), topography: ptr_new(), sourceCoordSystem: 0, $
;			xMeasureUnit: "", yMeasureUnit: "", $
;			zMeasureUnit: "", secondaryRawData: ptr_new(), $
;			startDate: intarr(6), modelName:"" $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "SAWindData" type
;; *******************************************************
;
;FUNCTION getSAWindDataStruct
;
; struct = { SAWindDataStruct, inherits SAParameterDataStruct, $
;			uComponent: ptr_new(), vComponent: ptr_new(), $
;			wComponent: ptr_new(), deltaTimeMeteo: 0 $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "SAObservedData" type
;; *******************************************************
;
;FUNCTION getSAObservedDataStruct
;
; struct = { id: 0, $					; id of observation
;			name: "", $					; name of ground station/Lidar/sonda
;			type: "", $					; ground station/Lidar/other
;			category: "", $				; region/type of veichle/other if applicable
;			polyCode: 0b, $				; display polygon code (see getObservedPolygon function).
;			isStation: 0b, $			; boolean
;			xGeoLocation: 0., $			; only if like ground station
;			yGeoLocation: 0., $			; only if like ground station
;			height: 0., $				; height above sea
;			comments: "", $				; !!!
;			dataFile: "", $				; file contains data
;			startDate: intarr(6), $		; date of first data
;			endDate: intarr(6), $		; date of last data
;			params: ptr_new(), $		; string array [n] of names
;			measureUnits: ptr_new(), $	; string array [n] of names
;			xDataPos: ptr_new(), $		; float array [m] x position of observation
;			yDataPos: ptr_new(), $		; float array [m] y position of observation
;			zDataPos: ptr_new(), $		; float array [m] z position of observation
;			timeDate: ptr_new(), $		; float array [m] date of observation [yyyymmddhhmiss]
;			values: ptr_new(), $		; float array [n,m] of observation values
;			validity: ptr_new() $		; float array [n,m] of observation values
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "SAConfigFile" type
;; *******************************************************
;
;FUNCTION getSAConfigFileStruct
;
; struct = { meteoConfFile: "", meteoDataFile: "", $
;			chemConfFile: "", chemDataFile: "", $
;			aeroConfFile: "", aeroDataFile: "", $
;			landuseConfFile: "", landuseDataFile: "", $
;			obserConfFile: "", obserDataFile: "", $
;			dataFileSelection: bytarr(5), $
;			startDate: intarr(4), endDate: intarr(4), $
;			hasDate: 0b, fileType: bytarr(5), fileSuffix:'' $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "SAParameterConfData" type
;; *******************************************************
;
;FUNCTION getSAParameterConfDataStruct
;
; struct = { confFileName: "", dataFileName: "", $
; 			parNumber: 0, type: "", parName: ptr_new(), $
; 			parSample: ptr_new(), zParFlag: ptr_new(), $
; 			parMeasureUnit: ptr_new(), parFactor1: ptr_new(), $
; 			parFactor2: ptr_new(), parCompare: ptr_new() $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "SAWindowControl" type
;; *******************************************************
;
;FUNCTION getSAWindowControlStruct
;
; struct = { prevParBttId: 0l, prevModeBtt: 0l, $
;			prevSliceBtt: 0l, prevObsBtt: 0l, $
;			displayOptionStatus: bytarr(6), $
;			displayOptBtt: lonarr(6), interactionMode: 0b, $
;			zoomInfo: fltarr(10), freeSliceInfo: fltarr(10), $
;			systemP: replicate(!P, 2), systemX: replicate(!X,2), $
;			systemY: replicate(!Y,2), systemZ: replicate(!Z,2), $
;			prevGraphXLabel: strarr(2), prevGraphXMU: "", $
;			prevGraphElemNum: 0l, graphOverplotNum: 0, $
;			graphOverplotTitles: strarr(6), prevGraphTitle: strarr(2), $
;			prevStationData: fltarr(2), graphRangeYInfo:fltarr(3), $
;			obsSelectedIdxs: ptr_new(), prevObsData:fltarr(3), $
;			colorInfo: ptr_new(), fixedScaleColor: 0b, autoColorTableUpdate:0b, $
;			graphRangeXInfo:fltarr(3), obsAllLevelFlag: 0b, $
;			displayObsIndexes: ptr_new(), projectionInfo: ptr_new() $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "SAMainData" type
;; *******************************************************
;
;FUNCTION getSAMainDataStruct
;
; struct = { meteoData: ptr_new(), chemData: ptr_new(), $
; 			aeroData: ptr_new(), observedData: ptr_new(), $
; 			landUseData: ptr_new(), sliceInfo: ptr_new(), $
; 			parameterData: ptr_new(), startDate: intarr(4), $
; 			endDate: intarr(4), controlWindow: ptr_new(), $
; 			windParameterData: ptr_new(), obsAvailablePars: ptr_new() $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "VMainData" type
;; *******************************************************
;
;FUNCTION getVMainDataStruct
;
; struct = { meteoData: ptr_new(), chemData: ptr_new(), $
; 			aeroData: ptr_new(), observedData: ptr_new(), $
; 			dataSelection: ptr_new(), obsAvailablePars: ptr_new(), $
; 			aeroModelList: ptr_new(), controlWindow: ptr_new(), $
; 			meteoModelList: ptr_new(), chemModelList: ptr_new() $
; 	}
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "VParameterData" type
;; *******************************************************
;
;FUNCTION getVParameterDataStruct
;
; struct = { VParameterDataStruct, dataFile: "", $
;			configurationFile: "", vectorFlag: "", $
; 			id: 0l, name: "", type: "", $
;			measureUnit: "", zFlag: "", $
;			obsName: "", fact2: ptr_new(),  $
;			frameNumber: 0l, $
;			sampleFreq: "",	rawData: ptr_new(), $
;			startDate: intarr(6), endDate: intarr(6), $
;			modelName: "" $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "VObsParameterData" type
;; *******************************************************
;
;FUNCTION getVObsParameterDataStruct
;
; struct = { VObsParameterDataStruct, $
; 			id: 0l, name: "", type: "", $
; 			measureUnit: "", sampleFreq: "" $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "VWindowControl" type
;; *******************************************************
;
;FUNCTION getVWindowControlStruct
;
; struct = { prevParBttId: 0l, hourTypeIndex: 0b, yearTypeIndex: 0b, $
;			prevStatsBtt: 0l, prevObsBtt: 0l, averageHourValue: 0b, $
;			displayOptionStatus: bytarr(6), prevBlock9Btt:0l, $
;			displayOptBtt: lonarr(6), thresholdIndex: fltarr(2), $
;			modelSelection: ptr_new(), $
;			prevGraphXLabel: strarr(2), prevGraphXMU: "", $
;			prevGraphElemNum: 0l, graphOverplotNum: 0, $
;			graphOverplotTitles: strarr(6), prevGraphTitle: strarr(2), $
;			prevStationData: fltarr(2), graphRangeInfo:fltarr(3), $
;			mapDraw: 0l $
; }
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "VDataSelection" type
;; *******************************************************
;
;FUNCTION getVDataSelectionStruct
;
; struct = { averageValue: 0, hourType: 0b, $
; 			yearType: 0, thresholdInfo: fltarr(2), $
;			modelSelected: ptr_new(), startDate: intarr(4), $
;			endDate: intarr(4) , statsIndex: 0b, $
;			obsSelectedIdxs: ptr_new(), displayOption: 0b, $
;			statsName: '', obsParameter: ptr_new(), $
;			observed9Block: 0, addModelMean: 0b, $
;			saveOnFile: 0b, saveFileName: "", binSize:0, binNumber:0 $
; }
;
; return, struct
;
;END
;
;; *******************************************************
;; initialize a structure of "VBarPlot" type
;; *******************************************************
;
;FUNCTION getVBarPlotStruct
;
; struct = { stdDevValue:0., $
; 			corrValue:0., biasValue: 0., $
;			meanValue: 0., nmseValue:0, $
;			color: 0ll , name: "" $
; }
;
; return, struct
;
;END
