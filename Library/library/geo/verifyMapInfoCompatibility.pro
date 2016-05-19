function verifyMapInfoCompatibility, mapInfos, MAX_DIFFERENCE_PERCENT=MAX_DIFFERENCE_PERCENT, averagePs=averagePs, averageMc=averageMc

  if keyword_set(MAX_DIFFERENCE_PERCENT) then maxDiff=MAX_DIFFERENCE_PERCENT else maxDiff=0.01
  i=0
  
  if tag_exist(mapInfos[i].proj, 'PE_COORD_SYS_CODE') then lastPECoordSys=mapInfos[i].proj.PE_COORD_SYS_CODE else lastPECoordSys=mapInfos[i].proj.name 
  statsPs = fltarr(3,2)
  statsMc = fltarr(3,2)
  
  statsPs[0,0]=min(mapInfos[*].ps[0], max=maxPs)
  statsPs[1,0]=maxPs
  statsPs[2,0]=mean(mapInfos[*].ps[0])

  statsPs[0,1]=min(mapInfos[*].ps[1], max=maxPs)
  statsPs[1,1]=maxPs
  statsPs[2,1]=mean(mapInfos[*].ps[1])
  
  statsMc[0,0]=min(mapInfos[*].mc[2], max=maxMc)
  statsMC[1,0]=maxMc
  statsMC[2,0]=mean(mapInfos[*].mc[2])

  statsMc[0,1]=min(mapInfos[*].mc[3], max=maxMc)
  statsMc[1,1]=maxMc
  statsMc[2,1]=mean(mapInfos[*].mc[3])
  diff=fltarr(4)

  for i=0, n_elements(mapInfos)-1 do begin
    if tag_exist(mapInfos[i].proj, 'PE_COORD_SYS_CODE') then thisPECoordSys=mapInfos[i].proj.PE_COORD_SYS_CODE else thisPECoordSys=mapInfos[i].proj.name 
    if thisPECoordSys ne lastPECoordSys then begin
      message,'different projection for this file'
    endif
    lastPECoordSys=thisPECoordSys
    
    diff[0]=abs(mapInfos[i].ps[0]-statsPs[2,0])/statsPs[2,0]
    diff[1]=abs(mapInfos[i].ps[1]-statsPs[2,1])/statsPs[2,1]
    diff[2]=abs(mapInfos[i].mc[2]-statsMc[2,0])/statsMc[2,0]
    diff[3]=abs(mapInfos[i].mc[3]-statsMc[2,1])/statsMc[2,1]
    
    indx=where(diff ge maxDiff, count)
    if count gt 0 then return, 0
    doLog, '**file:', i, ' OK**', level=0
    
  endfor
  averagePs=reform(statsPs[2,0:1])
  averageMc=reform(statsMc[2,0:1])
  
  return, 1
  
end