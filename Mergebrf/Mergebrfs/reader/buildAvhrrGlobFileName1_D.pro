;GLOBAL_P17_GEOG_0.05DEG_182-182_99.NOAA-14
function buildAvhrrGlobFileName1_D, sensor, missionCode, year, month, day, archivedir, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR, YEARFOLDER=YEARFOLDER

  fileType='.hdf'
  twoDigityear=strmid(string(year, format="(I4)"), 2,2)

  if keyword_set(JULDAY) then fileName=string(format='(A, "_", I03, "-", I03, "_", A, ".", A, A)', sensor, day, day, twoDigityear, missionCode, fileType) else $
    fileName=string(format='(A, "_", I02, I02, "-", I02, I02, "_", A, ".", A, A)', sensor, day, month, day, month, twoDigityear, missionCode, fileType)
  ;  sjday = julday(date,1,year) - julday(1,1,year) + 1;
  ;  ejday = julday(date+1,1,year) - julday(1,1,year);
  ;  fileName=string(format='(A, "_", A, "_", A, I4, I03, I4, I03,"_", A,"_",A, ".nc")', prefix, periodType, parameter, year, sjday, year, ejday, sensor, roi)

  if keyword_set(YEARFOLDER) then yFolder=path_sep()+string(year, format='(I4)') else yFolder=''
  if keyword_set(FULLPATH) then return, archiveDir+yFolder+path_sep()+fileName else return, fileName
  ;if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName

end