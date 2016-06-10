function getStandardBrfDataSetInfo

  INT_NAN=-9999
  BYTE_NAN=255

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]

  bandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
    'TS', 'TV', 'PHI', $
    'LDTR_FLAG', 'Q1', 'Q2']
  bandLongNames=bandNames
  
  bandSlopes=[10e-05, 10e-05, 10e-05, 10e-05,$
    10e-03, 10e-03, 10e-03, $
    1, 1, 1]
    
  bandMeasureUnits=['-','-', '-', '-', $
    'deg','deg','deg', $
    '-', '-', '-' $
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

  ;manual setting???
  ;minMaxs[10,*]=[tempMin>0.,tempMax]
  ;nanList[10]=INT_NAN

  ;manual setting???
  ;minMaxs[11,*]=[tempMin>0.,tempMax]
  ;nanList[11]=INT_NAN


  ;manual setting???
  ;nanList[12]=INT_NAN

  return, { $
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