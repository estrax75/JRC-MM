function getStandardFaparTCDataSetInfo

  INT_NAN=-9999
  BYTE_NAN=255

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]


  bandNames=['FAPAR','Sigma_FAPAR', $
    'RECTIFIED_RED', 'Sigma_RECTIFIED_RED', $
    'RECTIFIED_NIR', 'Sigma_RECTIFIED_NIR', $
    'Number_of_Day', $
    'JRC_FLAG']

  
  bandLongNames=bandNames
  bandSlopes=[1, 1,$
    10e-05, 10e-05, $
    10e-05, 10e-05, $
    1,$
    1]

  bandMeasureUnits=['-','-', $
    '-', '-', $
    '-', '-', $
    '-', $
    '-']

  bandIntercepts=fltarr(n_elements(bandSlopes))
  bandDataTypes=[1,1,$
    2,2,$
    2,2,$
    2,$
    1]

  minMaxs=fltarr(n_elements(bandDataTypes), 2)
  nanList=fltarr(n_elements(bandDataTypes))

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

  minMaxs[6,*]=[0,31]
  nanList[6]=0

  nanList[7]=INT_NAN

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