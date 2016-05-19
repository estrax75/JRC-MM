FUNCTION extractParameterStruct, parameterFullString, SEPARATOR=SEPARATOR

  COMMON smurffCB, mainApp
  
  if n_elements(SEPARATOR) ne 1 then sepChar='$' else sepChar=SEPARATOR
  
  parInfo=strsplit(parameterFullString, sepChar, /EXTRACT)
  minTags=4
  
  tagsNo=n_elements(parInfo)
  
  if tagsNo ge minTags then begin
    ;PP from JRC new computation$PP$mean$buildEmisPPVarFileName$FALSE
    parLabel=parInfo[0]
    parId=parInfo[1]
    parStat=parInfo[2]
    parFunction=parInfo[3]
    
    if tagsNo gt minTags then extractFlag=mainApp->isTrue(parInfo[4]) else extractFlag=1
    if tagsNo gt minTags+1  then exportMapFlag=mainApp->isTrue(parInfo[5]) else exportMapFlag=0
    if tagsNo gt minTags+2  then hideFlag=fix(strupcase(parInfo[6]) eq 'HIDE') else hideFlag=0
    
    doLog, 'parLabel: '+strcompress(parLabel, /REMOVE_ALL), 'parId: '+strcompress(parId, /REMOVE_ALL), 'parStat: '+strcompress(parStat, /REMOVE_ALL), 'parFunction: '+strcompress(parFunction, /REMOVE_ALL), 'extractFlag: '+strcompress(extractFlag, /REMOVE_ALL), 'exportMapFlag: '+strcompress(exportMapFlag, /REMOVE_ALL), 'hideFlag: '+strcompress(hideFlag, /REMOVE_ALL), LEVEL=4
    
    return, {label:parLabel, id:parId, stat:parStat, getFunction:parFunction, extractFlag:extractFlag, exportMapFlag:exportMapFlag, hideFlag:hideFlag}
  endif
  
  message, ['Wrong parameter string in xml file ', parameterFullString+'.','Expected:','paraLabel$parId$parStat$parFunction[$extractFlag, exportMapFlag, hideFlag]']
  
END