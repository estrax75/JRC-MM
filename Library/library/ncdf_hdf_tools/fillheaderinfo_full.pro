pro fillheaderinfo_full, fileId, header, TYPE=TYPE

  title=''
  source='Inputs BRF AVHRR computed from NOAA datasets'
  versionNumber='1.0'
  versionDate='21-09-2016'
  institute='European Commission - Joint Research Center'
  conventions='CF-4'  ; verifica 
  post_processing='Normalized surface reflectance to BRF'

  if n_elements(header) eq 1 then begin
    avTags=tag_names(header)
    thisTag=(where(avTags eq strupcase('Conventions')))[0]
    if thisTag ne -1 then conventions=header.(thisTag)
    thisTag=(where(avTags eq strupcase('title')))[0]
    if thisTag ne -1 then title=header.(thisTag)
    thisTag=(where(avTags eq strupcase('technique')))[0]
    if thisTag ne -1 then technique=header.(thisTag)
    thisTag=(where(avTags eq strupcase('mainParameter')))[0]
    if thisTag ne -1 then mainParameter=header.(thisTag)
    thisTag=(where(avTags eq strupcase('source')))[0]
    if thisTag ne -1 then source=header.(thisTag)
    thisTag=(where(avTags eq strupcase('versionNumber')))[0]
    if thisTag ne -1 then versionNumber=header.(thisTag)
    thisTag=(where(avTags eq strupcase('versionDate')))[0]
    if thisTag ne -1 then versionDate=header.(thisTag)
    thisTag=(where(avTags eq strupcase('author')))[0]
    if thisTag ne -1 then author=header.(thisTag)
    thisTag=(where(avTags eq strupcase('institute')))[0]
    if thisTag ne -1 then institute=header.(thisTag)
    thisTag=(where(avTags eq strupcase('date')))[0]
    if thisTag ne -1 then date=header.(thisTag)
    thisTag=(where(avTags eq strupcase('telephone')))[0]
    if thisTag ne -1 then telephone=header.(thisTag)
    thisTag=(where(avTags eq strupcase('facsimile')))[0]
    if thisTag ne -1 then facsimile=header.(thisTag)
    thisTag=(where(avTags eq strupcase('internet')))[0]
    if thisTag ne -1 then internet=header.(thisTag)
    thisTag=(where(avTags eq strupcase('date')))[0]
    if thisTag ne -1 then post_processing=header.(thisTag)
  endif

  if ~(keyword_set(TYPE)) then TYPE='NC' 
  if strupcase(TYPE) eq 'NC' then begin
    NCDF_ATTPUT, fileid, /GLOBAL,'Conventions',conventions, /STRING
    NCDF_ATTPUT, fileid, /GLOBAL,'title',title, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'technique',technique, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'mainParameter',mainParameter, /STRING
    NCDF_ATTPUT, fileid, /GLOBAL,'source',source, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'versionNumber',versionNumber, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'versionDate',versionDate, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'author',author, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'institute',institute, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'date',date, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'telephone',telephone, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'facsimile',facsimile, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'internet',internet, /STRING
    ;NCDF_ATTPUT, fileid, /GLOBAL,'date',date, /STRING
  endif else begin
    HDF_SD_ATTRSET, fileid,'title',title
    HDF_SD_ATTRSET, fileid,'technique',technique
    HDF_SD_ATTRSET, fileid,'mainParameter',mainParameter
    HDF_SD_ATTRSET, fileid,'source',source
    HDF_SD_ATTRSET, fileid,'versionNumber',versionNumber
    HDF_SD_ATTRSET, fileid,'versionDate',versionDate
    HDF_SD_ATTRSET, fileid,'author',author
    HDF_SD_ATTRSET, fileid,'institute',institute
    HDF_SD_ATTRSET, fileid,'Conventions',conventions
    HDF_SD_ATTRSET, fileid,'date',date
    HDF_SD_ATTRSET, fileid,'telephone',telephone
    HDF_SD_ATTRSET, fileid,'facsimile',facsimile
    HDF_SD_ATTRSET, fileid,'internet',internet
    HDF_SD_ATTRSET, fileid,'date',date
  endelse

end