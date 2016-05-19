FUNCTION buildRqsFile, fileTemplateName, years=years, months=months, rois=rois, $
    firstYear=firstYear, $
    allyearflag=allyearflag, allmonthFlag=allmonthFlag, allroiflag=allroiflag, $
    overwriteresultflag=overwriteresultflag, onlyLast=onlyLast, processAll=processAll, $
    OXYRISK_ROI_FILENAME=OXYRISK_ROI_FILENAME
    
    
  if keyword_set(onlyLast) and keyword_set(processAll) then message, 'Set ONLY one exclusive flag (onlyLast OR processAll) not both.'
  if keyword_set(onlyLast) then begin
    get_current_date, year=year, month=month
    years=year
    years=string(format='(I4)', years)
    month=month eq 12 ? 1 : (month-1)
    ; standard settings
    months=string(format='(I02)', month)
    allroiflag=1
    overwriteresultflag=1
  endif
  if keyword_set(processAll) then begin
    get_current_date, year=year, month=month
    if ~keyword_set(firstYear) then firstYear=2002
    lastYear=year
    years=indgen(lastYear-firstYear+1)+fix(firstYear)
    years=string(format='(I4)', years)
    fYears=''
    for i=0, n_elements(years)-1 do fYears=fYears+years[i]+';'
    years=strmid(fYears, 0, strlen(fYears)-1)
    
    ; standard settings
    months=indgen(12)+1
    months=string(format='(I02)', months)
    fMonths=''
    for i=0, n_elements(months)-1 do fMonths=fMonths+months[i]+';'
    months=strmid(fMonths, 0, strlen(fMonths)-1)
    allroiflag=1
  endif
  utility=obj_new('Utility')
  fs=obj_new('FileSystem', /STAND_ALONE)
  sourceFileName=fs->getFileNameInfo(fileTemplateName, filePath=filePath, extension=extension)
  destFile=filePath+utility->getSysTime(/FILECOMPATIBILITY)+'.'+extension
  ; fill roi & roi archive - load ROI object...
  openr, unit, fileTemplateName, /GET_LUN
  openw, destUnit, destFile, /GET_LUN
  bufferString=''
  replacing=['yearlist', 'monthlist', 'roilist']
  values=[keyword_set(years) ? years : '', $
    keyword_set(months) ? months : '', $
    keyword_set(rois) ? rois : '' $
    ]
  types=['7', $
    '7', $
    '7' $
    ]
    
  if n_elements(rois) ne 0 then begin
    if n_elements(OXYRISK_ROI_FILENAME) ne 1 then OXYRISK_ROI_FILENAME=getenv('OXYRISK_ROI_FILENAME')
    if OXYRISK_ROI_FILENAME eq '' then OXYRISK_ROI_FILENAME='E:\data\mariomi\application\oxyrisk\configuration\roi.xml'
    roiInfo=obj_new('ROI')
    if ~roiInfo->fillDataFromXMLFile(OXYRISK_ROI_FILENAME) then begin
      return, 0
      message, 'Set roi file using os env variable OR by specifing full path filename with keyword -OXYRISK_ROI_FILENAME-)'
    endif
    roiCodeList=strsplit(rois, ';', /EXTRACT)
    archiveCodes=roiInfo->getArchiveRoiCodesSelectedByCodes(roiCodeList)
    fArchiveCodes=''
    for i=0, n_elements(archiveCodes)-1 do fArchiveCodes=fArchiveCodes+archiveCodes[i]+';'
    archiveCodes=strmid(fArchiveCodes, 0, strlen(fArchiveCodes)-1)
    replacing=[replacing, 'roiarchivelist']
    types=[types, '7']
    values=[values, archiveCodes]
  endif
  if keyword_set(allyearflag) then begin
    replacing = [replacing,  'allyearflag']
    values=[values, '1']
    types=[types, '2']
  endif
  if keyword_set(allmonthFlag) then begin
    replacing = [replacing, 'allmonthFlag']
    values=[values, '1']
    types=[types, '2']
  endif
  if keyword_set(allroiflag) then begin
    replacing = [replacing, 'allroiflag']
    values=[values, '1']
    types=[types, '2']
  endif
  if keyword_set(overwriteresultflag) then begin
    replacing = [replacing, 'overwriteresultflag']
    values=[values, '1']
    types=[types, '2']
  endif
  
  
  while not(eof(unit)) do begin
    ;if i lt
    readf, unit, bufferString
    check=(where (strupcase(replacing[*]) eq strupcase(strmid(bufferString,1,strlen(bufferString)-2)), found))[0] ;remove first < and last > from bufferString...
    if found eq 1 then replaceXMLBlock, unit, destUnit, replacing[check], values[check], types[check] else printf, destUnit, bufferString
  endwhile
  close, unit & free_lun, unit
  close, destUnit & free_lun, destUnit
  obj_destroy, utility
  obj_destroy, fs
  return, destFile
;file_delete, destFile
  
END