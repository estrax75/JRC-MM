function getStandardBrfDataSetInfo

  INT_NAN=-9999
  BYTE_NAN=255

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]

  bandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
    'TS', 'TV', 'PHI', $
    'LDTR_FLAG', 'Q1', 'Q2']

  bandLongNames=['Bidirectional Reflectance Factor Band 1', 'Bidirectional Reflectance Factor Band 2', 'Uncertainties of Bidirectional Reflectance Factor Band 1', 'Uncertainties of Bidirectional Reflectance Factor Band 2', $
    'Solar Zenith Angle', 'View Zenith Angle', 'Relative Azimuth Angle', $
    'LDTR_FLAG', 'Internal JRC Flag (1) for BRFs calculations for Band 1', 'Internal JRC Flag (2) for BRFs calculations for Band 1']
  
  bandSlopes=[10e-05, 10e-05, 10e-05, 10e-05,$
    10e-03, 10e-03, 10e-03, $
    1, 1, 1]
    
  bandMeasureUnits=['-','-', '-', '-', $
    'degree','degree','degree', $
    '-','-', '-' $
    ]
  
  bandIntercepts=fltarr(n_elements(bandSlopes))
  bandDataType=[2,2,2,2,$
    2,2,2,$
    2,1,1]

  minMaxs=fltarr(n_elements(bandDataType), 2)
  nanList=fltarr(n_elements(bandDataType))

  minMaxs[*,*]=-1
  minMaxs[0,*]=GENERIC_DATA_RANGE
  nanList[0]=INT_NAN

  minMaxs[1,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[1]=INT_NAN

  minMaxs[2,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[2]=INT_NAN

  minMaxs[3,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[3]=INT_NAN

  minMaxs[4,*]=ANGLES_DATA_RANGE1
  nanList[4]=INT_NAN

  minMaxs[5,*]=ANGLES_DATA_RANGE1
  nanList[5]=INT_NAN

  minMaxs[6,*]=ANGLES_DATA_RANGE2
  nanList[6]=INT_NAN

;  minMaxs[7,*]=[1, 14]
;  nanList[7]=BYTE_NAN

  minMaxs[7,*]=[-32640, 32640]
  nanList[7]=INT_NAN

  ;manual setting???
  minMaxs[8,*]=[1,2]
  nanList[8]=BYTE_NAN

  ;manual setting???
  minMaxs[9,*]=[1,2]
  nanList[9]=BYTE_NAN

  return, { $
    version: '1.1', $
    versionDate: '2016/06/06', $
    bandNames: bandNames, $
    bandLongNames:bandLongNames, $
    bandMeasureUnits: bandMeasureUnits, $
    bandDataTypes: bandDataType, $
    bandSlopes: bandSlopes, $
    bandIntercepts: bandIntercepts, $
    minMaxs:minMaxs, $
    nanS:nanList $
  }

end