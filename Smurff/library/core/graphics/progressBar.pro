PRO initProgressBar, nLoops, title=title, numberUpdates=numberUpdates

  COMMON smurffCB, mainApp
  COMMON progBarCB, progressbar, updateFreq, loops
  
  if ~keyword_set(numberUpdates) then numberUpdates=20
  if nLoops lt 20 then updateFreq=1 else updateFreq=fix(nLoops/float(numberUpdates))
  loops=nLoops
  if obj_valid(progressbar) then closeProgressBar
  progressbar = Obj_New('PROGRESSBARBTT',/noCancel,title=title,xsize=250,ysize=20,$
    /NODRAW,/TRUE,BUTTONSAMPLE=[250,0,0], BUTTONNUMBER=numberUpdates, screensize=mainApp->getScreenSize())
  progressbar -> Start
  progressbar -> Update, 100./numberUpdates
  
END

PRO updateProgressBar, progCount

  COMMON progBarCB, progressbar, updateFreq, loops
  
  updateOk=(progCount mod updateFreq) eq 0
  IF updateOk THEN BEGIN
    percent=float(progCount)/loops*100
    progressbar -> Update, percent
  ENDIF
;        doLog, count, '/', nloop
  
END

PRO closeProgressBar

  COMMON progBarCB, progressbar, updateFreq, loops
  if obj_valid(progressbar) then progressbar -> Destroy
  
END
