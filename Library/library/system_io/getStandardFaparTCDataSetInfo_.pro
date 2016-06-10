function getStandardFaparTCDataSetInfo_

  INT_NAN=-9999
  BYTE_NAN=255

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]


  bandNames=['FAPAR','Sigma_FAPAR', 'Temporal_Deviation_FAPAR', $
    'RECTIFIED_RED', 'Sigma_RECTIFIED_RED', 'Temporal_Deviation_Red', $
    'RECTIFIED_NIR', 'Sigma_RECTIFIED_NIR', 'Temporal_Deviation_NIR', $
    'DOY', 'Number_of_Day', $
    'JRC_FLAG', $
    'BRF_TOC_RED', 'BRF_TOC_NIR', $
    'JRC_LDTR']
  
  bandLongNames=bandNames
  bandSlopes=[1, 1, 1,$
    10e-05, 10e-05,10e-05, $
    10e-05, 10e-05,10e-05, $
    1, 1,$
    1,$
    10e-05, 10e-05,$
    1]

  bandMeasureUnits=['-','-','-', $
    '-', '-', '-', $
    '-', '-', '-', $
    '-', $
    '-','-','-', $
    '-', '-', $
    '-']

  bandIntercepts=fltarr(n_elements(bandSlopes))
  bandDataTypes=[1,1,1,$
    2,2,2,$
    2,2,2,$
    2,2,$
    1, $
    2,2,$
    2]

  minMaxs=fltarr(n_elements(bandDataTypes), 2)
  nanList=fltarr(n_elements(bandDataTypes))

  minMaxs[*,*]=-1
  minMaxs[0,*]=GENERIC_DATA_RANGE
  nanList[0]=BYTE_NAN

  minMaxs[1,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[1]=BYTE_NAN

  minMaxs[2,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[2]=BYTE_NAN

  minMaxs[3,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[3]=INT_NAN

  minMaxs[4,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[4]=INT_NAN

  minMaxs[5,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[5]=INT_NAN

  minMaxs[6,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[6]=INT_NAN

  minMaxs[7,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[7]=INT_NAN

  minMaxs[8,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[8]=INT_NAN

  ;manual setting???
  ;tempMin=min(output.flag, max=tempMax)
  minMaxs[9,*]=[0, 366]
  nanList[9]=INT_NAN

  minMaxs[10,*]=[0, 31]
  nanList[10]=INT_NAN

  minMaxs[11,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[11]=INT_NAN

  minMaxs[12,*]=GENERIC_DATA_RANGE;minMax[0,*]
  nanList[12]=INT_NAN

  ;manual setting???
  ;minMaxs[10,*]=[tempMin>0.,tempMax]
  nanList[13]=INT_NAN

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