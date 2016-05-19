pro removeEnviFiles, envifiles

  for i=0, n_elements(envifiles)-1 do begin
    ENVI_OPEN_FILE, envifiles[i], r_fid=fid
    envi_file_mng, id=fid, /remove, /delete, /no_warning
;    lastPointPos=strpos(envifiles[i], '.', /REVERSE_SEARCH)
;    if lastPointPos ne -1 then baseFileName=strmid(envifiles[i], 0, lastPointPos) else baseFileName=envifiles[i]
;    if baseFileName ne '' then begin
;      allFiles=file_search(baseFileName+'*.*', count=count)
;      if count ne 0 then file_delete, allFiles
;    endif
  endfor
  
end