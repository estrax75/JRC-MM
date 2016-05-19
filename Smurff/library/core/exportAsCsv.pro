PRO exportAsCsv, fileName, mainTitle, headers, measureUnits, timeColumn, dataColumns, separator=separator, appendLines=appendLines

  openw, destUnit, fileName, /GET_LUN
  doLog, 'fileName:'+fileName
  printf, destUnit, mainTitle
  doLog, mainTitle
  if n_elements(separator) ne 1 then sep=';' else sep=separator
  firstLine='x axis label'+sep+headers[0]+' ['+measureUnits[0]+']'+sep
  for i=1, n_elements(headers)-1 do firstLine=firstLine+headers[i]+' ['+measureUnits[i]+']'+sep
  firstLine=strmid(firstLine, 0, strlen(firstLine)-1)
  dims=size(dataColumns, /DIM)
  printf, destUnit, firstLine
  doLog, firstLine
  for i=0, dims[1]-1 do begin
    line=timeColumn[i]+sep
    for j=0, dims[0]-1 do line=line+strcompress(dataColumns[j,i], /REMOVE)+sep
    line=strmid(line, 0, strlen(line)-1)
    printf, destUnit, line
    doLog, line
  endfor
  for i=0, n_elements(appendLines)-1 do printf, destUnit, appendLines[i]
  doLog, fileName+' written', LEVEL=4
  free_lun, destUnit & close, destUnit
  
END