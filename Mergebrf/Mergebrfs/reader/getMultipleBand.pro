FUNCTION getMultipleBand, refDir, refFile, bandList, APPLYCONVERSION=APPLYCONVERSION, RETRIEVEFILLDATA=RETRIEVEFILLDATA

  bandNo=n_elements(bandList)
  bands=ptrarr(bandNo)
  slopes=ptrarr(bandNo)
  offsets=ptrarr(bandNo)
  fillDatas=ptrarr(bandNo)
  apply=intarr(bandNo)
  apply[*]=0
  if n_elements(APPLYCONVERSION) gt 0 then begin
    if n_elements(APPLYCONVERSION) eq 1 then apply[*]=APPLYCONVERSION else if n_elements(APPLYCONVERSION) ne bandNo then message, 'wrong usage of APPLYCONVERSION keyword...' else apply=APPLYCONVERSION  
  endif
  for i=0, n_elements(bandList)-1 do begin
    read_data, refDir, refFile, bandList[i], thisBand, thisSlope, thisOffset, thisFillData
    if apply[i] then band=thisBand*thisSlope+thisOffset else band=thisBand
    bands[i]=ptr_new(band, /NO_COPY)
    if n_elements(thisSlope) ne 0 then slopes[i]=ptr_new(thisSlope, /NO_COPY)
    if n_elements(thisOffset) ne 0 then offsets[i]=ptr_new(thisOffset, /NO_COPY)
    if n_elements(thisFillData) ne 0 then fillDatas[i]=ptr_new(thisFillData, /NO_COPY)
  endfor

  RETURN, {bandList:bandList, bands:bands, slopes:slopes, offsets:offsets, fillDatas:fillDatas}

END