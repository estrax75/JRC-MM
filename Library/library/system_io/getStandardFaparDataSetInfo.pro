function getStandardFaparDataSetInfo, Type=Type

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

  bandNames=['FAPAR','Sigma_FAPAR', $
    'Rectified_BAND_1', 'Sigma_RECTIFIED_BAND_1', $
    'Rectified_BAND_2', 'Sigma_RECTIFIED_BAND_2', $
    'LDTR_FLAG', $
    'TS', 'TV', 'PHI', $
    'BRF_TOC_BAND_1', 'BRF_TOC_BAND_2', $
    'JRC_FLAG','FAPAR_real']


  bandLongNames=['Fraction of Absorbed Photosynthetically Active Radiation','Uncertainties of FAPAR', $
    'Rectified Reflectance in Band 1', 'Uncertainties of Band 1', $
    'Rectified Reflectance in Band 2', 'Uncertainties of Band 2', $
    'LDTR_FLAG', $
    'Solar Zenith Angle', 'View Zenith Angle', 'Relative Azimuth Angle', $
    'Surface Bidirectional Reflectance Factor Band 1', 'Surface Bidirectional Reflectance Factor Band 2', $
    'JRC_FLAG','Fraction of Absorbed Photosynthetically Active Radiation']

  bandStandardNames=['Fraction of Absorbed Photosynthetically Active Radiation','Uncertainties of FAPAR', $
    'Rectified Reflectance in Band 1', 'Uncertainties of Band 1', $
    'Rectified Reflectance in Band 2', 'Uncertainties of Band 2', $
    'LDTR_FLAG', $
    'Solar Zenith Angle', 'View Zenith Angle', 'Relative Azimuth Angle', $
    'Surface Bidirectional Reflectance Factor Band 1', 'Surface Bidirectional Reflectance Factor Band 2', $
    'JRC_FLAG','Fraction of Absorbed Photosynthetically Active Radiation']

  ; this is only a sample... overwrite externally (slope = 1./250 or 1./254) 
  bandSlopes=[1, 1, $
    10e-05, 10e-05, $
    10e-05, 10e-05, $
    1, $
    10e-03, 10e-03, 10e-03,$
    10e-05, 10e-05,$
    1, 1]

  bandMeasureUnits=['-','-', $
    '-', '-', $
    '-', '-', $
    '-', $
    'degree','degree','degree', $
    '-', '-', $
    '-','-']

  bandIntercepts=fltarr(n_elements(bandSlopes))

  ; band data type coding:
  ;  BYTE=bandDataType[v] eq 1, SHORT=bandDataType[v] eq 2, $
  ;    LONG=bandDataType[v] eq 3, $
  ;    UBYTE=bandDataType[v] eq 16, USHORT=bandDataType[v] eq 12, $
  ;    ULONG=bandDataType[v] eq 13, $
  ;    FLOAT=bandDataType[v] eq 4, DOUBLE=bandDataType[v] eq 5, $
  ;    STRING=bandDataType[v] eq 7, UINT64=bandDataType[v] eq 14, $
  bandDataTypes=[16,16,$
    2,2,$
    2,2,$
    12,$
    2,2,2, $
    2,2,$
    16,4]

  minMaxs=fltarr(n_elements(bandDataTypes), 2)
  scaledMinMaxs=minMaxs
  nanList=fltarr(n_elements(bandDataTypes))

  ; fapar (byte)
  minMaxs[*,*]=-1
  minMaxs[0,*]=GENERIC_DATA_RANGE
  scaledMinMaxs[0,*]=[0,254];[1,255]
  nanList[0]=BYTE_NAN2;BYTE_NAN1

  ; fapar - sigma (byte)
  minMaxs[1,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[1,*]=[0,254];[1,255]
  nanList[1]=BYTE_NAN2;BYTE_NAN1

  ; rectified band 1 (signed int)
  minMaxs[2,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[2,*]=[0,INT_MAX]
  nanList[2]=INT_NAN

  ; rectified band 1 - sigma (signed int)
  minMaxs[3,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[3,*]=[0,INT_MAX]
  nanList[3]=INT_NAN

  ; rectified band 2 (signed int)
  minMaxs[4,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[4,*]=[0,INT_MAX]
  nanList[4]=INT_NAN

  ; rectified band 2 - sigma (signed int)
  minMaxs[5,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[5,*]=[0, INT_MAX]
  nanList[5]=INT_NAN

  ;manual setting???
  ;tempMin=min(output.flag, max=tempMax)
  ; rectified band 2 - sigma (signed int)
  ; MM check file/original values
  ;minMaxs[6,*]=[-32640, 32640] ; [0, 65000]
  minMaxs[6,*]=[0u, UINT_MAX] ; [0, 65000]
  nanList[6]=-1

  ;  'TS'
  minMaxs[7,*]=ANGLES_DATA_RANGE1 ; To check
  scaledMinMaxs[7,*]=minMaxs[7,*]/bandSlopes[7]
  nanList[7]=INT_NAN

  ; 'TV'
  minMaxs[8,*]=ANGLES_DATA_RANGE1
  scaledMinMaxs[8,*]=minMaxs[8,*]/bandSlopes[8]
  nanList[8]=INT_NAN

  ; 'PHI'
  minMaxs[9,*]=ANGLES_DATA_RANGE2
  scaledMinMaxs[9,*]=minMaxs[9,*]/bandSlopes[9]
  nanList[9]=INT_NAN

  minMaxs[10,*]=GENERIC_DATA_RANGE
  scaledMinMaxs[10,*]=minMaxs[10,*]/bandSlopes[10]
  nanList[10]=INT_NAN

  minMaxs[11,*]=GENERIC_DATA_RANGE
  scaledMinMaxs[11,*]=minMaxs[11,*]/bandSlopes[11]
  nanList[11]=INT_NAN

  minMaxs[12,*]=[0b,14b]
  scaledMinMaxs[12,*]=[0,15]
  nanList[12]=255

  minMaxs[13,*]=[0.,1.]
  scaledMinMaxs[13,*]=[0.,1.]
  nanList[13]=-1



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
    scaledMinMaxs: scaledMinMaxs, $
    nanS:nanList $
  }

end