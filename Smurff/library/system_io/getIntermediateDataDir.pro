;+
; :Author: mariomi
;-
;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    dataDir
;
;
;
; :Author: mariomi
;-
function getIntermediateDataDir, dataDir

  COMMON smurffCB, mainApp

  fs=mainApp->getFileSystem()
  intermediateWrite=mainApp->isTrue(mainApp->getKeyValue('INTERMEDIATE_FULL_WRITE'))
  intermediateDataDir=''
  doLog, dataDir, LEVEL=4
  fs=mainApp->getFileSystem()
  if intermediateWrite then begin
    intermediateDataDir=mainApp->getKeyValue('TEMP_DIR')
    if dataDir ne '' then begin
      intermediateDataDir=dataDir+path_sep()+'extra_data'
      fs->createDir, intermediateDataDir
    endif
  endif
  return, intermediateDataDir

end
