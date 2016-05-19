pro writeTestCase, passedBands, usedBands, result, algoName, rrsSet, outTextInputFile

  COMMON hdInfo, bandInfo

 outTextInputFile=outTextInputFile+'.txt' 
 fullHeaderInfo=bandInfo+','+'log10processedBand1'+','+'log10processedBand2'+','+'log10processedBand3'+','+'log10processedBand4'+','+'exitValue'+','+'exitNovelty'
 openW, lun, outTextInputFile, /GET_LUN
 printf, lun, 'algo_name: '+algoName
 printf, lun, 'rrsset: '+rrsSet
 printf, lun, fullHeaderInfo
 
 nElem=n_elements(passedBands[0,*])
 for i=0l, nElem-1 do begin
  buildRow=''
  nSubElem=n_elements(passedBands[0:5,0])
  for j=0, nSubElem-1 do buildRow=buildRow+','+strcompress(passedBands[j,i])
  nSubElem=n_elements(usedBands[*,0])
  for j=0, nSubElem-1 do buildRow=buildRow+','+strcompress(usedBands[j,i])
  nSubElem=n_elements(result[*,0])
  for j=0, nSubElem-1 do buildRow=buildRow+','+strcompress(result[j,i])
  buildRow=strcompress(strmid(buildRow, 1, strlen(buildRow)), /REMOVE)
  printf, lun, buildRow
 endfor
 close, lun & free_lun, lun

end
