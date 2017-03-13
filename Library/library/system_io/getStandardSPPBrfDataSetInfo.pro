function getStandardSPPBrfDataSetInfo

  infoHeader=getJRCHeader_v1_5()

  INT_NAN=2^15
  INT_MAX=2^15-1
  UINT_MAX=2u^16-1
  BYTE_NAN=0
  RADIANCE_MAX=1000

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]

  bandNames=['featureId', 'start_time', 'stop_time', $
    'SREFL_CH1', 'SREFL_CH2', $
    'l1_flags', 'latitude', 'longitude', $
    'SZEN', 'VZEN', 'RELAZ', $
    'RADIANCE_CH1', 'RADIANCE_CH2']

  ;featureId start_time:time stop_time:time  SREFL_CH1:float SREFL_CH2:float l1_flags:short  latitude:float  longitude:float SZEN:float  VZEN:float  RELAZ:float RADIANCE_CH1:float  RADIANCE_CH2:float

  ;  bandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'REL_PHI', $
  ;    'LDTR_FLAG', 'Q1', 'Q2']

  bandLongNames=['Sequential Id', 'Start Time', 'End Time', $
    'Bidirectional Reflectance Factor Band 1', 'Bidirectional Reflectance Factor Band 2', $
    'QA', 'latitude', 'longitude', $
    'Solar Zenith Angle', 'View Zenith Angle', 'Relative Azimuth Angle', $
    'RADIANCE_CH1', 'RADIANCE_CH2']

  bandStandardNames=['SEQUENTIAL_ID', 'START_TIME', 'STOP_TIME', $
    'BRF_BAND_1', 'BRF_BAND_2', $
    'QA', 'Latitude', 'Longitude', $
    'SZEN', 'VZEN', 'REL_PHI', $
    'RADIANCE_CH1', 'RADIANCE_CH2']

  bandSlopes=[1, 1, 1,$
    1, 1, $
    1, 1, 1, $
    1, 1, 1, $
    1, 1]

  bandMeasureUnits=['-','Time', 'Time', $
    '-', '-', $
    '-','degree','degree', $
    'degree','degree', 'degree', $
    '-', '-']

  bandIntercepts=fltarr(n_elements(bandSlopes))

  ; band data type coding:
  ;  BYTE=bandDataType[v] eq 1, SHORT=bandDataType[v] eq 2, $
  ;    LONG=bandDataType[v] eq 3, $
  ;    UBYTE=bandDataType[v] eq 16, USHORT=bandDataType[v] eq 12, $
  ;    ULONG=bandDataType[v] eq 13, $
  ;    FLOAT=bandDataType[v] eq 4, DOUBLE=bandDataType[v] eq 5, $
  ;    STRING=bandDataType[v] eq 7, UINT64=bandDataType[v] eq 14, $
  bandDataType=[7,7,7,$
    4,4,$
    2,4,4,$
    4,4,4, $
    4,4]

  minMaxs=fltarr(n_elements(bandDataType), 2)
  scaledMinMaxs=minMaxs
  nanList=fltarr(n_elements(bandDataType))

  minMaxs[*,*]=-1
  minMaxs[0,*]=GENERIC_DATA_RANGE
  scaledMinMaxs[0,*]=minMaxs[0,*]/bandSlopes[0]
  nanList[0]=INT_NAN

  minMaxs[1,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[1,*]=minMaxs[1,*]/bandSlopes[1]
  nanList[1]=INT_NAN

  minMaxs[2,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[2,*]=minMaxs[2,*]/bandSlopes[3]
  nanList[2]=INT_NAN

  minMaxs[3,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[3,*]=minMaxs[3,*]/bandSlopes[3]
  nanList[3]=INT_NAN

  minMaxs[4,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[4,*]=minMaxs[4,*]/bandSlopes[4]
  nanList[4]=INT_NAN

  minMaxs[5,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[5,*]=minMaxs[5,*]/bandSlopes[5]
  nanList[5]=INT_NAN

  minMaxs[6,*]=ANGLES_DATA_RANGE1
  scaledMinMaxs[6,*]=minMaxs[6,*]/bandSlopes[6]
  nanList[6]=INT_NAN

  minMaxs[7,*]=ANGLES_DATA_RANGE1
  scaledMinMaxs[7,*]=minMaxs[7,*]/bandSlopes[7]
  nanList[7]=INT_NAN

  minMaxs[8,*]=ANGLES_DATA_RANGE2
  scaledMinMaxs[8,*]=minMaxs[8,*]/bandSlopes[8]
  nanList[8]=INT_NAN

  minMaxs[9,*]=ANGLES_DATA_RANGE2
  scaledMinMaxs[9,*]=minMaxs[9,*]/bandSlopes[9]
  nanList[9]=INT_NAN

  minMaxs[10,*]=ANGLES_DATA_RANGE2
  scaledMinMaxs[10,*]=minMaxs[10,*]/bandSlopes[10]
  nanList[10]=INT_NAN

  ;check... setting???
  minMaxs[*,*]=-1
  minMaxs[11,*]=[0,RADIANCE_MAX]
  scaledMinMaxs[11,*]=minMaxs[11,*]/bandSlopes[11]
  nanList[11]=INT_NAN

  minMaxs[*,*]=-1
  minMaxs[12,*]=[0,RADIANCE_MAX]
  scaledMinMaxs[12,*]=minMaxs[12,*]/bandSlopes[12]
  nanList[12]=INT_NAN

  return, { $
    header: infoHeader, $
    bandNames: bandNames, $
    bandLongNames:bandLongNames, $
    bandStandardNames:bandStandardNames, $
    bandMeasureUnits: bandMeasureUnits, $
    bandDataTypes: bandDataType, $
    bandSlopes: bandSlopes, $
    bandIntercepts: bandIntercepts, $
    minMaxs:minMaxs, $
    scaledMinMaxs:scaledMinMaxs, $
    nanS:nanList $
  }

end