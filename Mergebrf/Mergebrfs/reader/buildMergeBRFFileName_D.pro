function buildMergeBRFFileName_D, sensor, missionCode, year, month, day, archivedir, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR

  ;AVHRR-Land_v004_AVH09C1_NOAA-16_20030101_c20140421105754
  fileType='.nc'

  if keyword_set(JULDAY) then fileName=string(format='(A, A, "-", I4, I03, "_c", A)', sensor, missionCode, year, day, fileType) else $
    fileName=string(format='(A, A, "-", I4, I02, I02, "_c", A)', sensor, missionCode, year, day, month, fileType)

  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+string(year, format='(I4)')+path_sep()+fileName else return, fileName

end