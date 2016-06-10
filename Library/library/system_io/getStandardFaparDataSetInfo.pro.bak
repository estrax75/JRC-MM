function getStandardFaparDataSetInfo, Type=Type

  INT_NAN=-9999
  BYTE_NAN=255

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]

  bandNames=['FAPAR','Sigma_FAPAR', $
    'RECTIFIED RED', 'Sigma_RECTIFIED_RED', $
    'RECTIFIED NIR', 'Sigma_RECTIFIED_NIR', $
    'JRC_MASK', $
    'TS', 'TV', 'PHI', $
    'BRF_TOC_RED', 'BRF_TOC_NIR', $
    'JRC_FLAG']

  bandLongNames=bandNames

  bandSlopes=[1, 1, $
    10e-05, 10e-05, $
    10e-05, 10e-05, $
    1, $
    10e-03, 10e-03, 10e-03,$
    10e-05, 10e-05,$
    1]

  bandMeasureUnits=['-','-', $
    '-', '-', $
    '-', '-', $
    '-', $
    'deg','deg','deg', $
    '-', '-', $
    '-']

  bandIntercepts=fltarr(n_elements(bandSlopes))

  bandDataType=[1,1,$
    2,2,$
    2,2,$
    2,$
    2,2,2, $
    2,2,$
    1]

  minMaxs=fltarr(n_elements(bandDataType), 2)
  nanList=fltarr(n_elements(bandDataType))

  minMaxs[*,*]=-1
  minMaxs[0,*]=GENERIC_DATA_RANGE
  nanList[0]=BYTE_NAN

  minMaxs[1,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[1]=BYTE_NAN

  minMaxs[2,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[2]=INT_NAN

  minMaxs[3,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[3]=INT_NAN

  minMaxs[4,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[4]=INT_NAN

  minMaxs[5,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[5]=INT_NAN

  ;manual setting???
  ;tempMin=min(output.flag, max=tempMax)
  ;minMaxs[6,*]=[tempMin, tempMax]
  nanList[6]=INT_NAN

  minMaxs[7,*]=ANGLES_DATA_RANGE1
  nanList[7]=INT_NAN

  minMaxs[8,*]=ANGLES_DATA_RANGE1
  nanList[8]=INT_NAN

  minMaxs[9,*]=ANGLES_DATA_RANGE2
  nanList[9]=INT_NAN

  ;manual setting???
  ;minMaxs[10,*]=[tempMin>0.,tempMax]
  nanList[10]=INT_NAN

  ;manual setting???
  ;minMaxs[11,*]=[tempMin>0.,tempMax]
  nanList[11]=INT_NAN


  ;manual setting???
  nanList[12]=INT_NAN


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