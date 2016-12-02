function getStandardDiffDataSetInfo

  infoHeader=getJRCHeader_v1_3()
  
  INT_NAN=2^15
  INT_MAX=2^15-1
  UINT_MAX=2u^16-1
  BYTE_NAN=0

  GENERIC_DATA_RANGE=[0., 1.]
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
  bandDataType=[2,2,2]

  minMaxs=fltarr(n_elements(bandDataType), 2)
  nanList=fltarr(n_elements(bandDataType))
  nanList[*]=INT_NAN

  return, { $
    header: infoHeader, $
    bandNames: bandNames, $
    bandLongNames:bandLongNames, $
    bandStandardNames:bandStandardNames, $
    bandMeasureUnits: bandMeasureUnits, $
    bandDataTypes: bandDataType, $
    bandSlopes: bandSlopes, $
    bandIntercepts: bandIntercepts, $
    minMaxs:minMaxs, $
    nanS:nanList $
  }

end