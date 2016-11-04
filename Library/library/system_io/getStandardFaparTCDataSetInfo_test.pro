function getStandardFaparTCDataSetInfo_test

  infoHeader=getJRCHeader_v1_3()

  INT_NAN=2^15
  INT_MAX=2^15-1
  UINT_MAX=2u^16-1
  BYTE_NAN1=0
  BYTE_NAN2=255

  GENERIC_DATA_RANGE=[0., 1.]
  DAY_DATA_RANGE=[0, 31]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]


  bandNames=['Day','Number_of_Day', $, $
    'FAPAR','Dev_Temp_FAPAR','Sigma_FAPAR', $
    'RECTIFIED_RED','Dev_Temp_Red', 'Sigma_RECTIFIED_RED', $
    'RECTIFIED_NIR','Dev_Temp_Nir', 'Sigma_RECTIFIED_NIR', $
    'BRF_TOC_BAND_1', 'BRF_TOC_BAND_2', $
    'JRC_FLAG',$
    'TS', 'TV']

  bandLongNames=['Day', 'Number_of_day', $
    'Fraction of Absorbed Photosynthetically Active Radiation', 'Temporal Deviation of FAPAR', 'Uncertainties of FAPAR', $
    'Rectified Reflectance in Band 1','Temporal Deviation of Band 1', 'Uncertainties of Rectified Reflectance in Band 1', $
    'Rectified Reflectance in Band 2','Temporal Deviation of Band 2', 'Uncertainties of Rectified Reflectance in Band 2', $
    'Surface Bidirectional Reflectance Factor Band 1', 'Surface Bidirectional Reflectance Factor Band 2', $
    'JRC_FLAG', $
    'Solar Zenith Angle', 'View Zenith Angle']

  bandStandardNames=['Day','Number_of_Day', $, $
    'FAPAR','Dev_Temp_FAPAR','Sigma_FAPAR', $
    'RECTIFIED_RED','Dev_Temp_Red', 'Sigma_RECTIFIED_RED', $
    'RECTIFIED_NIR','Dev_Temp_Nir', 'Sigma_RECTIFIED_NIR', $
    'Surface Bidirectional Reflectance Factor Band 1', 'Surface Bidirectional Reflectance Factor Band 2', $
    'JRC_FLAG',$
    'TS', 'TV']

;  'sdn_parameter_urn'
;  'sdn_parameter_name'
;  'sdn_uom_urn'
;  'sdn_uom_name'
  
  ;  bandNames=['FAPAR','Sigma_FAPAR', $
  ;    'RECTIFIED_RED', 'Sigma_RECTIFIED_RED', $
  ;    'RECTIFIED_NIR', 'Sigma_RECTIFIED_NIR', $
  ;    'Number_of_Day', $
  ;    'JRC_FLAG']

  bandSlopes=[1,1,$
    1, 1, 1,$
    10e-05, 10e-05, 10e-05, $
    10e-05, 10e-05, 10e-05, $
    10e-05, 10e-05,$
    1,$
    10e-03, 10e-03]

  bandMeasureUnits=['n_a','n_a', $
    'n_a', 'n_a', 'n_a', $
    'n_a', 'n_a', 'n_a', $
    'n_a', 'n_a', 'n_a', $
    '-', '-', $
    'n_a', $
    'degree','degree']

  bandIntercepts=fltarr(n_elements(bandSlopes))

  ; band data type coding:
  ;  BYTE=bandDataType[v] eq 1, SHORT=bandDataType[v] eq 2, $
  ;    LONG=bandDataType[v] eq 3, $
  ;    UBYTE=bandDataType[v] eq 16, USHORT=bandDataType[v] eq 12, $
  ;    ULONG=bandDataType[v] eq 13, $
  ;    FLOAT=bandDataType[v] eq 4, DOUBLE=bandDataType[v] eq 5, $
  ;    STRING=bandDataType[v] eq 7, UINT64=bandDataType[v] eq 14, $
  bandDataTypes=[16,16,$
    16,16,16,$
    2,2,2,$
    2,2,2,$
    2,2,$
    2, $
    2,2]

  minMaxs=fltarr(n_elements(bandDataTypes), 2)
  scaledMinMaxs=minMaxs
  nanList=fltarr(n_elements(bandDataTypes))

  minMaxs[*,*]=-1
  ;'Day','Number_of_Day', $, $
  minMaxs[0,*]=DAY_DATA_RANGE
  scaledMinMaxs[0,*]=DAY_DATA_RANGE
  nanList[0]=BYTE_NAN2

  minMaxs[1,*]=DAY_DATA_RANGE
  scaledMinMaxs[1,*]=DAY_DATA_RANGE
  nanList[1]=BYTE_NAN1

  ;'FAPAR','Dev_Temp','Sigma_FAPAR', $
  minMaxs[2,*]=GENERIC_DATA_RANGE
  scaledMinMaxs[2,*]=[1,255]
  nanList[2]=BYTE_NAN1

  minMaxs[3,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[3,*]=[1,255]
  nanList[3]=BYTE_NAN1

  minMaxs[4,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[4,*]=[1,255]
  nanList[4]=BYTE_NAN1

  ;'RECTIFIED_RED','Dev_Temp_Red', 'Sigma_RECTIFIED_RED', $
  minMaxs[5,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[5,*]=[0,INT_MAX]
  nanList[5]=INT_NAN

  minMaxs[6,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[6,*]=[0,INT_MAX]
  nanList[6]=INT_NAN

  minMaxs[7,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[7,*]=[0,INT_MAX]
  nanList[7]=INT_NAN

  ;'RECTIFIED_NIR','Dev_Temp_Nir', 'Sigma_RECTIFIED_NIR', $
  minMaxs[8,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[8,*]=[0,INT_MAX]
  nanList[8]=INT_NAN

  minMaxs[9,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[9,*]=[0,INT_MAX]
  nanList[9]=INT_NAN

  minMaxs[10,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[10,*]=[0,INT_MAX]
  nanList[10]=INT_NAN

  minMaxs[11,*]=GENERIC_DATA_RANGE
  scaledMinMaxs[11,*]=[0, INT_MAX]
  nanList[11]=INT_NAN

  minMaxs[12,*]=GENERIC_DATA_RANGE
  scaledMinMaxs[12,*]=[0, INT_MAX]
  nanList[12]=INT_NAN

  ;'JRC_FLAG', $
  minMaxs[13,*]=[0,14]
  scaledMinMaxs[13,*]=[0,14]
  nanList[13]=255

  minMaxs[14,*]=ANGLES_DATA_RANGE1
  scaledMinMaxs[14,*]=minMaxs[14,*]/bandSlopes[14]
  nanList[14]=INT_NAN

  minMaxs[15,*]=ANGLES_DATA_RANGE1
  scaledMinMaxs[15,*]=minMaxs[15,*]/bandSlopes[15]
  nanList[15]=INT_NAN

  return, { $
    header: infoHeader, $
    bandNames: bandNames, $
    bandLongNames:bandLongNames, $
    bandStandardNames:bandStandardNames, $
    bandMeasureUnits: bandMeasureUnits, $
    bandDataTypes: bandDataTypes, $
    bandSlopes: bandSlopes, $
    bandIntercepts: bandIntercepts, $
    minMaxs:minMaxs, $
    scaledMinMaxs:scaledMinMaxs, $
    nanS:nanList $
  }

end