function getStandardFaparDataSetInfo, Type=Type

  INT_NAN=-9999
  BYTE_NAN=255

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]

  bandNames=['FAPAR','Sigma_FAPAR', $
    'RECTIFIED_BAND_1', 'Sigma_RECTIFIED_BAND_1', $
    'RECTIFIED_BAND_2', 'Sigma_RECTIFIED_BAND_2', $
    'LDTR_FLAG', $
    'TS', 'TV', 'PHI', $
    'BRF_TOC_BAND_1', 'BRF_TOC_BAND_2', $
    'JRC_FLAG']

  bandLongNames=['Fraction of Absorbed Photosynthetically Active Radiation','Uncertainties of FAPAR', $
    'Rectified Reflectance in Band 1', 'Uncertainties of Rectified Reflectance in Band 1', $
    'Rectified Reflectance in Band 2', 'Uncertainties of Rectified Reflectance in Band 2', $
    'LDTR_FLAG', $
    'Solar Zenith Angle', 'View Zenith Angle', 'Relative Azimuth Angle', $
    'Surface Bidirectional Reflectance Factor Band 1', 'Surface Bidirectional Reflectance Factor Band 2', $
    'JRC_FLAG']

  bandSlopes=[1, 1, $
    10e-05, 10e-05, $
    10e-05, 10e-05, $
    2, $
    10e-03, 10e-03, 10e-03,$
    10e-05, 10e-05,$
    1]

  bandMeasureUnits=['-','-', $
    '-', '-', $
    '-', '-', $
    '-', $
    'degree','degree','degree', $
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
  minMaxs[6,*]=[-32640, 32640]
  nanList[6]=-1

  minMaxs[7,*]=ANGLES_DATA_RANGE1
  nanList[7]=INT_NAN

  minMaxs[8,*]=ANGLES_DATA_RANGE1
  nanList[8]=INT_NAN

  minMaxs[9,*]=ANGLES_DATA_RANGE2
  nanList[9]=INT_NAN

  minMaxs[10,*]=GENERIC_DATA_RANGE
  nanList[10]=INT_NAN

  minMaxs[11,*]=GENERIC_DATA_RANGE
  nanList[11]=INT_NAN

  minMaxs[12,*]=[0b,14b]
  nanList[12]=BYTE_NAN


  return, { $
    versionNumber: '1.1', $
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