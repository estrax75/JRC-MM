function getStandardFaparTCDataSetInfo

  infoHeader=getJRCHeader_v1_5()

  INT_NAN=2^15
  INT_MAX=2^15-1
  ;Shift Nan to first negative int because -9999 is a valid angle...
  UINT_MAX=2u^16-1
  BYTE_NAN1=0
  BYTE_NAN2=255

  GENERIC_DATA_RANGE=[0., 1.]
  DAY_DATA_RANGE=[1, 31]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]


  bandNames=['Day','Number_of_Day', $, $
    'FAPAR','Dev_Temp_FAPAR','Sigma_FAPAR', $
    'RECTIFIED_RED','Dev_Temp_Red', 'Sigma_RECTIFIED_RED', $
    'RECTIFIED_NIR','Dev_Temp_Nir', 'Sigma_RECTIFIED_NIR', $
    'JRC_FLAG']

  bandLongNames=['Representative day in the period', 'Number_of_day', $
    'Fraction of Absorbed Photosynthetically Active Radiation', 'Temporal Deviation of FAPAR', 'Uncertainties of FAPAR', $
    'Rectified Reflectance in Band 1','Temporal Deviation of Band 1', 'Uncertainties Band 1', $
    'Rectified Reflectance in Band 2','Temporal Deviation of Band 2', 'Uncertainties Band 2', $
    'JRC_FLAG']

  bandStandardNames=['Day','Number_of_Day', $, $
    'FAPAR','Dev_Temp_FAPAR','Sigma_FAPAR', $
    'RECTIFIED_RED','Dev_Temp_Red', 'Sigma_RECTIFIED_RED', $
    'RECTIFIED_NIR','Dev_Temp_Nir', 'Sigma_RECTIFIED_NIR', $
    'JRC_FLAG']

  bandSlopes=[1,1,$
    1, 1, 1,$
    10e-05, 10e-05, 10e-05, $
    10e-05, 10e-05, 10e-05, $
    1]

  bandMeasureUnits=['n_a','n_a', $
    'n_a', 'n_a', 'n_a', $
    'n_a', 'n_a', 'n_a', $
    'n_a', 'n_a', 'n_a', $
    'n_a']

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
    2]

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
  scaledMinMaxs[2,*]=[0,254];[1,255]
  nanList[2]=BYTE_NAN2;BYTE_NAN1

  minMaxs[3,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[3,*]=[0,254];[1,255]
  nanList[3]=BYTE_NAN2;BYTE_NAN1

  minMaxs[4,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[4,*]=[0,254];[1,255]
  nanList[4]=BYTE_NAN2;BYTE_NAN1

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

  ;'JRC_FLAG', $
  minMaxs[11,*]=[0,14]
  scaledMinMaxs[11,*]=[0,14]
  nanList[11]=255

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