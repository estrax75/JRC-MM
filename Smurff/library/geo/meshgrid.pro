function meshgrid, rowList, colList, rowLimit=rowLimit, colLimit=colLimit, rowStep=rowStep, colStep=colStep

  ;  if n_elements(rowStep) eq 1 then rowStepL=rowStep else rowStepL=1
  ;  if n_elements(colStep) eq 1 then colStepL=colStep else colStepL=1
  ;
  ;  if n_elements(rowLimit) eq 2 then rowStepL=rowStep else rowStepL=1
  ;  if n_elements(colLimit) eq 2 then colStepL=colStep else colStepL=1
  ;
  ;  rowDim=(abs(rowLimit[1]-rowLimit[0]))/rowStepL
  ;  colDim=(abs(colLimit[1]-colLimit[0]))/colStepL
  ;
  ;  rows = rowLimit[0]+findgen(rowDim)*float(stepLon)
  ;  cols = latlim[0]+findgen(colDim)*float(stepLat)
  ;
  ;  meshRows = lon #(intarr(rowDim)+1)
  ;  meshCols = lat ##(intarr(colDim)+1)

  ;type=size(rowList, /TYPE)
  
  rowNumber=n_elements(rowList)
  colNumber=n_elements(colList)

  ;meshRows=make_array(rowNumber, colNumber, TYPE=type)
  ;meshCols=make_array(rowNumber, colNumber, TYPE=type)
  
  meshRows = rowList #(intarr(colNumber)+1)
  meshCols = colList ##(intarr(rowNumber)+1)
  
  return, {meshRows:meshRows, meshCols:meshCols}
  
end