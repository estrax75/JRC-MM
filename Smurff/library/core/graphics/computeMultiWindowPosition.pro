FUNCTION computeMultiWindowPosition, index, storeWindowsNumber=storeWindowsNumber, storeRefMainDimension=storeRefMainDimension, storeGridStyle=storeGridStyle

  common computePosition_CB, windowNumber, refMainDimension, gridStyle
  
  if n_elements(storeWindowsNumber) eq 1 then begin
    windowNumber=storeWindowsNumber
  endif
  if n_elements(storeRefMainDimension) eq 4 then begin
    refMainDimension=storeRefMainDimension
  endif
  if n_elements(storeGridStyle) eq 3 then begin
    gridStyle=storeGridStyle
  endif
  
  if n_elements(index) ne 1 then return, 0
  
  refFullNormSize=[0.05, 0.08, 0.98, 0.98]
  
  ;infoDim=size(gridStyle, /DIM)
  
  rows=gridStyle[0]
  columns=gridStyle[1]
  order=gridStyle[2]
  
  if index lt 1 or index gt windowNumber then message, 'This window is out of range [0 to windowsNumber]'
  
  COLUMNFIXED=rows eq 0
  ROWFIXED=columns eq 0

  if keyword_set(COLUMNFIXED) then begin
    colIndex=(index-1) mod columns
    rowIndex=(index-1)/columns
    totRows=windowNumber
    totColumns=1
  endif

  if keyword_set(ROWFIXED) then begin
    colIndex=(index-1) mod rows
    rowIndex=(index-1)/rows
    totRows=1
    totColumns=windowNumber
  endif

  doLog, index, rowIndex, colIndex
  
  rowSize=refFullNormSize[2]-refFullNormSize[0]
  rowOffset=refFullNormSize[0]
  
  colSize=refFullNormSize[3]-refFullNormSize[1]
  colOffset=refFullNormSize[1]
  
  rowPos=[float(rowOffset)+rowSize/totRows*rowIndex, rowOffset+rowSize/totRows*(rowIndex+1)]
  colPos=[float(colOffset)+colSize/totColumns*colIndex, colOffset+colSize/totColumns*(colIndex+1)]
  
  return, [colPos[0], rowPos[0], colPos[1], rowPos[1]]
  ;return, [0,0,0,0]
  
END