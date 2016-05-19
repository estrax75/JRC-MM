pro doHistogram, values, tempdir, roiCode, day, month, year, cutValue=cutvalue, title=title, binsize=binsize

  validIdxs=where(finite(values), vCount)
  if n_elements(binsize) eq 1 then if binsize eq -1 then DelIdlVar, binsize
  if n_elements(cutvalue) eq 1 then if cutvalue eq -1 then DelIdlVar, cutvalue
  if vCount ne 0 then begin
    vals=values[validIdxs]
    if n_elements(binsize) ne 1 then binsize=10
    valIdxs=where(vals ne -9999, m999Count)
    if (m999Count ne 0) then begin
      vals=vals[valIdxs]
      minVals=min(vals, max=maxVals)
      avg=total(vals)/n_elements(vals)
      stdDv=stddev(vals)
      count=n_elements(vals)
      if n_elements(cutvalue) eq 1 then idx=where(vals le cutValue, count) else idx=indgen(count)
      vals=vals[idx]
      fname=strcompress(roiCode)+'_'+strtrim(year)+'_'+strtrim(month)+'_'+strcompress(day, /REMOVE)+'.eps'
      if ~keyword_set(title) then title=fName+' (binsize='+strcompress(binsize, /remove)+')'
      if count gt 0 then begin
        ;binSize=(maxVals-minVals)/10
        ;tempDir=mainApp->getKeyValue('TEMP_DIR')
        if avg gt 20 then prefix='BIG_' else prefix=''
        outFileName=tempDir+path_sep()+prefix+fname
        ;if binsize ge 0 and uniq(vals[sort(vals)]) gt 2 then begin
        if n_elements(uniq(vals[sort(vals)])) gt 2 then begin
          ;doLog, binsize, , LEVEL=4
          cgPS_Open, filename=outFileName
          cgHistoplot, vals, binsize=binSize, /FILL, title=title;, min=minVals
          cgText, 0.7, 0.1, /NORM, $
            'min = '+strcompress(minVals, /REMOVE), TT_FONT='Helvetica'
          cgText, 0.7, 0.3, /NORM, $
            'max = '+strcompress(maxVals, /REMOVE), TT_FONT='Helvetica'
          cgText, 0.7, 0.5, /NORM, $
            'avg = '+strcompress(avg, /REMOVE), TT_FONT='Helvetica'
          cgText, 0.7, 0.7, /NORM, $
            'stddev = '+strcompress(stdDv, /REMOVE), TT_FONT='Helvetica'
          cgPS_Close
        endif else begin
          doLog, 'Data too close to plot an histogram!'
        endelse
      endif
    endif
  endif else begin
    doLog, 'No data... No Histogram!'
  endelse
  
end