function getStandardDiffDataSetInfo

  infoHeader=getJRCHeader()
  infoHeader.title='Difference'
  
  INT_NAN=-9999
  BYTE_NAN=255

  GENERIC_DATA_RANGE=[0., 1.]
  ANGLES_DATA_RANGE1=[0., 90.]
  ANGLES_DATA_RANGE2=[-180., 180.]

  bandNames=['Difference', 'Fapar1', 'Fapar2']

  bandLongNames=['Difference between two products', 'First FAPAR', 'Second FAPAR']

  bandStandardNames=['Difference between two products', 'First FAPAR', 'Second FAPAR']
  
  bandSlopes=[.0001,.0001,.0001]
    
  bandMeasureUnits=['-','-','-']
  
  bandIntercepts=fltarr(n_elements(bandSlopes))
  ;bandIntercepts[0]=1.
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