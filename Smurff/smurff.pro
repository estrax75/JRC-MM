; for 18n look "IDLffLangCat"
; single element xml read bug on XMLHelper/SimpleXML

PRO optimizerL2Files

 merDir='E:\projects\idl\smurff\data\global\calibration\meris\timeseries\daily\1km\L2\raw'
 modDir='E:\projects\idl\smurff\data\global\calibration\modisa\timeseries\daily\1km\L2\raw'
 swfDir='E:\projects\idl\smurff\data\global\calibration\seawifs\timeseries\daily\1km\L2\raw'
 vrsDir='E:\projects\idl\smurff\data\global\calibration\viirs\timeseries\daily\1km\L2\raw'
 
 swflist=file_search(swfDir, '*.sav')
 infos=FILE_INFO(swflist)
 idx=where(infos.size gt 10000, count)
 swflist=swflist[idx]
 
 for i=0, count-1 do begin
   restore, swflist[i]
   save, data, filename=swflist[i]
 endfor
 
 vrslist=file_search(vrsDir, '*.sav')
 
 for i=0, n_elements(vrslist) -1 do begin
   restore, swflist[i]
   save, data, filename=vrslist[i]
 endfor

 modlist=file_search(modDir, '*.sav')
 infos=FILE_INFO(modlist)
 idx=where(infos.size gt 10000, count)
 modlist=modlist[idx]

 for i=count-1, 0, -1  do begin
   restore, modlist[i]
   save, data, filename=modlist[i]
 endfor

 merlist=file_search(merDir, '*.sav')
 infos=FILE_INFO(merlist)
 idx=where(infos.size gt 10000, count)
 merlist=merlist[idx]
 
 for i=0, count-1 do begin
  restore, merlist[i]
  save, data, filename=merlist[i]
 endfor

END

PRO smurff_batch

  appMainName='smurff'
  ;appMainName='oxyrisk'
  ;mainPath='E:\data\mariomi\application'
  mainPath='/exports/local1/mariomi/application'
  fileList=['A1_partial_run.rqs', 'A2_partial_run.rqs', 'A3_full_run.rqs', 'B1_full_from_2010.rqs', 'B2_full_from_2010.rqs', $
  'C1_partial_run.rqs', 'C2_full_run.rqs', 'D1_partial_run.rqs', 'D2_full_run.rqs', 'D3_partial_run.rqs', 'D4_full_run.rqs', 'D5_partial_run.rqs', $
  'D6_partial_run.rqs', 'D7_full_run.rqs', 'D8_full_run.rqs', 'E1_partial_run.rqs', 'E2_full_run.rqs', 'E3_partial_run.rqs', 'E4_full_run.rqs', $
  'E5_partial_run.rqs', 'E6_ful_run.rqs', 'E7_full_run.rqs', 'E8_full_run.rqs']
  ;oxyriskapp, [mainPath+path_sep()+appMainName+path_sep()+'save'+path_sep()+'oc2_test_ow.rqs', mainPath+path_sep()+appMainName+path_sep()+'save'+path_sep()+'biomap_partial.rqs']
  smurff, [mainPath+path_sep()+appMainName+path_sep()+'save'+path_sep()+fileList[16]]
  ;smurff, [mainPath+path_sep()+appMainName+path_sep()+'save'+path_sep()+'run_all.rqs']
  
END

PRO smurff_convert

  appMainName='smurff'
  ;appMainName='oxyrisk'
  ;mainPath='E:\data\application\'
  mainPath='/exports/local1/mariomi/application'
  smurff, [mainPath+path_sep()+appMainName+path_sep()+'save'+path_sep()+'test1.rqs', mainPath+path_sep()+appMainName+path_sep()+'save'+path_sep()+'test2.rqs', mainPath+path_sep()+appMainName+path_sep()+'save'+path_sep()+'test3.rqs']
  ;smurff, [mainPath+path_sep()+appMainName+path_sep()+'save'+path_sep()+'run_all.rqs']
  
END

PRO smurff, batchFileNames, root=root, CONVERT=CONVERT

  COMMON smurffCB, mainApp
  compile_opt  strictarr
  
  ;!P.COLOR=0
  ENVI, /restore_base_save_files
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  ;oxyApp=obj_new('OxyApplication', root=root)
  mainApp=obj_new('SmurffApplication', root=root)
  if obj_valid(mainApp) then begin
    startUpMessage='Application started!!!'
    if n_elements(batchFileNames) eq 0 then a=dialog_message(startUpMessage, /INFORMATION) else oxyApp->logMessage, startUpMessage
    mainApp->startUp
    if n_elements(batchFileNames) eq 0 then begin
      mainApp->display
    endif else begin
     doLog, systime(0), LEVEL=4
     doLog, '***start batch job***', LEVEL=4
     doLog, batchFileNames, LEVEL=4
     if keyword_set(CONVERT) then mainApp->runImageConvertFromFile, batchFileNames else mainApp->runRequestFromFile, batchFileNames
     doLog, systime(0), LEVEL=4
     doLog, '***end of batch job***', LEVEL=4
    endelse
  endif
  
END

PRO buildTestConfiguration, WRITE=WRITE

  doLog, "++++++++++Buildings test files...++++++++++++++++++++++++++++++++++++++++++++++++"
  fsm=obj_new('OxyFileSystemManager')
  fsm->test_BuildInternalSystemConfig, WRITE=WRITE
  fsm->test_BuildResourceConfigFiles, WRITE=WRITE
  obj_destroy, fsm
  
END
