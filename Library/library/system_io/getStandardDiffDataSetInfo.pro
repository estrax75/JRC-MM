function getStandardDiffDataSetInfo

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

  bandNames=['Difference', 'Fapar1', 'Fapar2']

  bandLongNames=['Difference between two products', 'First FAPAR', 'Second FAPAR']

  bandStandardNames=['Difference between two products', 'First FAPAR', 'Second FAPAR']
  
  bandSlopes=[.0001,.0001,.0001]
    
  bandMeasureUnits=['-','-','-']
  
  bandIntercepts=fltarr(n_elements(bandSlopes))

  ; band data type coding:
  ;  BYTE=bandDataType[v] eq 1, SHORT=bandDataType[v] eq 2, $
  ;    LONG=bandDataType[v] eq 3, $
  ;    UBYTE=bandDataType[v] eq 16, USHORT=bandDataType[v] eq 12, $
  ;    ULONG=bandDataType[v] eq 13, $
  ;    FLOAT=bandDataType[v] eq 4, DOUBLE=bandDataType[v] eq 5, $
  ;    STRING=bandDataType[v] eq 7, UINT64=bandDataType[v] eq 14, $
  bandDataTypes=[2,2,2]

  minMaxs=fltarr(n_elements(bandDataTypes), 2)
  scaledMinMaxs=minMaxs
  nanList=fltarr(n_elements(bandDataTypes))

  minMaxs[0,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[0,*]=minMaxs[0,*]/bandSlopes[0]
  nanList[0]=INT_NAN

  minMaxs[1,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[1,*]=minMaxs[1,*]/bandSlopes[1]
  nanList[1]=INT_NAN

  minMaxs[2,*]=GENERIC_DATA_RANGE;minMax[0,*]
  scaledMinMaxs[2,*]=minMaxs[2,*]/bandSlopes[2]
  nanList[2]=INT_NAN
  
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