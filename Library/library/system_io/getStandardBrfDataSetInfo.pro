function getStandardBrfDataSetInfo

  infoHeader=getJRCHeader_v1_5()

  INT_NAN=2^15
  INT_MAX=2^15-1
  UINT_MAX=2u^16-1
  BYTE_NAN=0

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]

  bandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
    'TS', 'TV', 'REL_PHI', $
    'LDTR_FLAG', 'Q1', 'Q2']

  bandLongNames=['Bidirectional Reflectance Factor Band 1', 'Bidirectional Reflectance Factor Band 2', 'Uncertainties of Bidirectional Reflectance Factor Band 1', 'Uncertainties of Bidirectional Reflectance Factor Band 2', $
    'Solar Zenith Angle', 'View Zenith Angle', 'Relative Azimuth Angle', $
    'LDTR_FLAG', 'Internal JRC Flag (1) for BRFs calculations for Band 1', 'Internal JRC Flag (2) for BRFs calculations for Band 1']

  bandStandardNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
    'TS', 'TV', 'PHI', $
    'LDTR_FLAG', 'Q1', 'Q2']

  bandSlopes=[10e-05, 10e-05, 10e-05, 10e-05,$
    10e-03, 10e-03, 10e-03, $
    1, 1, 1]

  bandMeasureUnits=['-','-', '-', '-', $
    'degree','degree','degree', $
    '-','-', '-' $
    ]

  bandIntercepts=fltarr(n_elements(bandSlopes))

  ; band data type coding:
  ;  BYTE=bandDataType[v] eq 1, SHORT=bandDataType[v] eq 2, $
  ;    LONG=bandDataType[v] eq 3, $
  ;    UBYTE=bandDataType[v] eq 16, USHORT=bandDataType[v] eq 12, $
  ;    ULONG=bandDataType[v] eq 13, $
  ;    FLOAT=bandDataType[v] eq 4, DOUBLE=bandDataType[v] eq 5, $
  ;    STRING=bandDataType[v] eq 7, UINT64=bandDataType[v] eq 14, $
  bandDataType=[2,2,2,2,$
    2,2,2,$
    12,1,1]

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
  scaledMinMaxs[2,*]=minMaxs[2,*]/bandSlopes[2]
  nanList[2]=INT_NAN

  minMaxs[3,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[3,*]=minMaxs[3,*]/bandSlopes[3]
  nanList[3]=INT_NAN

  minMaxs[4,*]=ANGLES_DATA_RANGE1
  scaledMinMaxs[4,*]=minMaxs[4,*]/bandSlopes[4]
  nanList[4]=INT_NAN

  minMaxs[5,*]=ANGLES_DATA_RANGE1
  scaledMinMaxs[5,*]=minMaxs[5,*]/bandSlopes[5]
  nanList[5]=INT_NAN

  minMaxs[6,*]=ANGLES_DATA_RANGE2
  scaledMinMaxs[6,*]=minMaxs[6,*]/bandSlopes[6]
  nanList[6]=INT_NAN

  ;  minMaxs[7,*]=[1, 14]
  ;  nanList[7]=BYTE_NAN

  minMaxs[7,*]=[0, UINT_MAX]
  scaledMinMaxs[7,*]=minMaxs[7,*]/bandSlopes[7]
  nanList[7]=INT_NAN

  ;manual setting???
  minMaxs[8,*]=[1,2]
  scaledMinMaxs[8,*]=0
  nanList[8]=BYTE_NAN

  ;manual setting???
  minMaxs[9,*]=[1,2]
  scaledMinMaxs[9,*]=0
  nanList[9]=BYTE_NAN


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