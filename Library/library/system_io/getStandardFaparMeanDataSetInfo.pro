function getStandardFaparMeanDataSetInfo

  infoHeader=getJRCHeader_v1_5()

  INT_NAN=2^15
  INT_MAX=2^15-1
  UINT_MAX=2u^16-1
  BYTE_NAN1=0
  BYTE_NAN2=255

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]


  bandNames=strupcase(['Day','Number_of_Day', $, $
    'FAPAR','Dev_Temp','Sigma_FAPAR', $
    'RECTIFIED_RED','Dev_Temp_Red', 'Sigma_RECTIFIED_RED', $
    'RECTIFIED_NIR','Dev_Temp_Nir', 'Sigma_RECTIFIED_NIR', $
    'JRC_FLAG', $
    'TOC_RED','TOC_NIR'])

  bandLongNames=['Day', 'Number_of_day', $
    'Fraction of Absorbed Photosynthetically Active Radiation','Uncertainties of FAPAR', $
    'Rectified Reflectance in Band 1','Temporal Deviation of Band 1', 'Uncertainties of Rectified Reflectance in Band 1', $
    'Rectified Reflectance in Band 2','Temporal Deviation of Band 2', 'Uncertainties of Rectified Reflectance in Band 2', $
    'JRC_FLAG', $
    'Top of Canopy for Band 1', 'Top of Canopy for Band 2']

  bandStandardNames=['Day', 'Number_of_day', $
    'Fraction of Absorbed Photosynthetically Active Radiation','Uncertainties of FAPAR', $
    'Rectified Reflectance in Band 1','Temporal Deviation of Band 1', 'Uncertainties of Rectified Reflectance in Band 1', $
    'Rectified Reflectance in Band 2','Temporal Deviation of Band 2', 'Uncertainties of Rectified Reflectance in Band 2', $
    'JRC_FLAG', $
    'Top of Canopy for Band 1', 'Top of Canopy for Band 2']

;  'sdn_parameter_urn'
;  'sdn_parameter_name'
;  'sdn_uom_urn'
;  'sdn_uom_name'
  
  ;  bandNames=['FAPAR','Sigma_FAPAR', $
  ;    'RECTIFIED_RED', 'Sigma_RECTIFIED_RED', $
  ;    'RECTIFIED_NIR', 'Sigma_RECTIFIED_NIR', $
  ;    'Number_of_Day', $
  ;    'JRC_FLAG']


  bandLongNames=bandNames
  bandSlopes=[1,1,$
    1, 1, 1,$
    10e-05, 10e-05, 10e-05, $
    10e-05, 10e-05, 10e-05, $
    1,$
    10e-05, 10e-05]

  bandMeasureUnits=['-','-', $
    '-', '-', '-', $
    '-', '-', '-', $
    '-', '-', '-', $
    '-', $
    '-', '-']

  bandIntercepts=fltarr(n_elements(bandSlopes))

  ; band data type coding:
  ;  BYTE=bandDataType[v] eq 1, SHORT=bandDataType[v] eq 2, $
  ;    LONG=bandDataType[v] eq 3, $
  ;    UBYTE=bandDataType[v] eq 16, USHORT=bandDataType[v] eq 12, $
  ;    ULONG=bandDataType[v] eq 13, $
  ;    FLOAT=bandDataType[v] eq 4, DOUBLE=bandDataType[v] eq 5, $
  ;    STRING=bandDataType[v] eq 7, UINT64=bandDataType[v] eq 14, $
  bandDataTypes=[1,1,$
    16,16,16,$
    12,12,12,$
    12,12,12,$
    2,$
    12,12]

  minMaxs=fltarr(n_elements(bandDataTypes), 2)
  nanList=fltarr(n_elements(bandDataTypes))

  ;'Day','Number_of_Day', $, $
  minMaxs[0,*]=[0,31]
  nanList[0]=0

  minMaxs[1,*]=[0,31]
  nanList[1]=0

  ;'FAPAR','Dev_Temp','Sigma_FAPAR', $
  minMaxs[*,*]=-1
  minMaxs[2,*]=GENERIC_DATA_RANGE
  nanList[2]=BYTE_NAN

  minMaxs[3,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[3]=BYTE_NAN

  minMaxs[4,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[4]=BYTE_NAN

  ;'RECTIFIED_RED','Dev_Temp_Red', 'Sigma_RECTIFIED_RED', $
  minMaxs[5,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[5]=INT_NAN

  minMaxs[6,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[6]=INT_NAN

  minMaxs[7,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[7]=INT_NAN

  ;'RECTIFIED_NIR','Dev_Temp_Nir', 'Sigma_RECTIFIED_NIR', $
  minMaxs[8,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[8]=INT_NAN

  minMaxs[9,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[9]=INT_NAN

  minMaxs[10,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[10]=INT_NAN

  ;'JRC_FLAG', $
  minMaxs[11,*]=[0,14]
  nanList[11]=INT_NAN

  ;'TOC_RED','TOC_RED', $
  minMaxs[12,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[12]=INT_NAN
  
  minMaxs[13]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[13]=INT_NAN

  ;'LDTR_FLAG'
  ;nanList[14]=INT_NAN
  avgIdx=[2,5,8,11]
  bandNames=bandNames[avgIdx]
  bandLongNames=bandLongNames[avgIdx]
  bandMeasureUnits=bandMeasureUnits[avgIdx]
  bandDataTypes=bandDataTypes[avgIdx]
  bandSlopes=bandSlopes[avgIdx]
  bandIntercepts=bandIntercepts[avgIdx]
  nanList=nanList[avgIdx]
  minMaxs=minMaxs[[avgIdx],*]

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
    nanS:nanList $
  }

end