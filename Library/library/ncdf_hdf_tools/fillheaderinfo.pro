pro fillHeaderInfo, fileId, header, TYPE=TYPE, $
  time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End, $
  Satellite=Satellite, id=id, date_created=date_created, cdr_name=cdr_name
  

  ; title='JRC daily FAPAR with AVHRR'
  ; technique='JRC FAPAR TOC algorithm - see QA4ECV ATBD'
  ; mainParameter='-'
  ; source='Inputs BRF AVHRR computed from NOAA datasets'
  ; versionNumber='1.0'
  ; versionDate='21-09-2016'
  ; author=''
  ; institute='European Commission - Joint Research Center'
  ; conventions='CF-4'  ; verifica
  ;date='-'
  ;telephone='-'
  ;facsimile='-'
  ;internet='-'
  ;post_processing='Normalized surface reflectance to BRF'
  tagList=strupcase(tag_names(header))

  if ~(keyword_set(TYPE)) then TYPE='NC'
  idx1=where(tagList eq strupcase('time_Coverage_Start'), cntStart)
  idx2=where(tagList eq strupcase('time_Coverage_End'), cntEnd)
  idx3=where(tagList eq strupcase('Satellite'), cntSat)
  idx4=where(tagList eq strupcase('Id'), cntId)
  idx5=where(tagList eq strupcase('date_created'), cntDate)
  idx6=where(tagList eq strupcase('cdr_name'), cntCDRName)
  
  if n_elements(time_Coverage_Start) and cntStart eq 1 then header.(idx1)=[header.(idx1)[0], time_Coverage_Start]
  if n_elements(time_Coverage_End) and cntEnd eq 1 then header.(idx2)=[header.(idx2)[0], time_Coverage_End]
  if n_elements(Satellite) and cntSat eq 1 then header.(idx3)=[header.(idx3)[0], Satellite]
  if n_elements(id) and cntId eq 1 then header.(idx4)=[header.(idx4)[0], Id]
  if n_elements(date_created) and cntDate eq 1 then header.(idx5)=[header.(idx5)[0], date_created]
  if n_elements(cdr_name) and cntCDRName eq 1 then header.(idx5)=[header.(idx5)[0], date_created]

  if strupcase(TYPE) eq 'NC' then begin
    NCDF_ATTPUT, fileid, /GLOBAL, 'Conventions', 'CF-1.6'
    NCDF_ATTPUT, fileid, /GLOBAL, 'Metadata_Conventions', 'CF-1.6, Unidata Dataset Discovery v1.0'
    NCDF_ATTPUT, fileid, /GLOBAL, 'standard_name_vocabulary', 'CF Standard Name Table (v25, 05 July 2013)'
  endif
  

  for i=0, n_elements(tagList)-1 do begin
    if strupcase(TYPE) eq 'NC' then NCDF_ATTPUT, fileid, /GLOBAL, (header.(i))[0], (header.(i))[1]
    if strupcase(TYPE) eq 'HDF' then HDF_SD_ATTRSET, fileid, (header.(i))[0], (header.(i))[1]
  endfor

end