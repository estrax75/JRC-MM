function getStandardFaparTCDataSetInfo_new

  ;  data_tc= {Composite, day: bytarr(7200,3600), $
  ;    nday: bytarr(7200,3600), $
  ;    fapar: fltarr(7200,3600), $
  ;    dev_temp: fltarr(7200,3600), $
  ;    sigma: fltarr(7200,3600), $
  ;    red: fltarr(7200,3600), $
  ;    dev_red_temp: fltarr(7200,3600), $
  ;    sigma_red:fltarr(7200,3600), $
  ;    nir: fltarr(7200,3600), $
  ;    dev_nir_temp: fltarr(7200,3600), $
  ;    sigma_nir: fltarr(7200,3600), $
  ;    flag: bytarr(7200,3600), $
  ;    toc_red: fltarr(7200,3600), $
  ;    toc_nir: fltarr(7200,3600), $
  ;    qa: bytarr(7200,3600)}

  INT_NAN=-9999
  BYTE_NAN=255

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]


  bandNames=['Day','Number_of_Day', $, $
    'FAPAR','Dev_Temp','Sigma_FAPAR', $
    'RECTIFIED_RED','Dev_Temp_Red', 'Sigma_RECTIFIED_RED', $
    'RECTIFIED_NIR','Dev_Temp_Nir', 'Sigma_RECTIFIED_NIR', $
    'JRC_FLAG', $
    'TOC_RED','TOC_RED', $
    'LDTR_FLAG']

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
    10e-05, 10e-05, $
    1]

  bandMeasureUnits=['-','-', $
    '-', '-', '-', $
    '-', '-', '-', $
    '-', $
    '-', '-', $
    '-']

  bandIntercepts=fltarr(n_elements(bandSlopes))
  bandDataTypes=[1,1,$
    1,1,1,$
    2,2,2,$
    2,2,2,$
    2,$
    2,2,$
    1]

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
  minMaxs[12]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[12]=INT_NAN
  
  minMaxs[13]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[13]=INT_NAN

  ;'LDTR_FLAG'
  nanList[14]=INT_NAN


  return, { $
    versionNumber: '1.1', $
    versionDate: '2016/06/06', $
    bandNames: bandNames, $
    bandLongNames:bandLongNames, $
    bandMeasureUnits: bandMeasureUnits, $
    bandDataTypes: bandDataTypes, $
    bandSlopes: bandSlopes, $
    bandIntercepts: bandIntercepts, $
    minMaxs:minMaxs, $
    nanS:nanList $
  }

end