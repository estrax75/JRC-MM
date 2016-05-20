function buildAvhrrLandFileName_D, sensor, missionCode, year, month, day, archivedir, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR

  ;AVHRR-Land_v004_AVH09C1_NOAA-16_20030101_c20140421105754
  fileType=".nc"
  expectedRetrieveData=""
  for i=1, 14 do expectedRetrieveData=expectedRetrieveData+"?"

  expectedRetrieveOrbitCode=''
  for i=1, 2 do expectedRetrieveOrbitCode=expectedRetrieveOrbitCode+'?'

  if keyword_set(JULDAY) then fileName=string(format='(A, "*", A, "-", A, "??_", I4, I03, "_c", A, A)', sensor, missionCode, expectedRetrieveOrbitCode, year, day, expectedRetrieveData, fileType) else $
    fileName=string(format='(A, "*", A, "-??_", I4, I02, I02, "_c", A, A)', sensor, missionCode, year, month, day, expectedRetrieveData, fileType)

  if keyword_set(YEARFOLDER) then yFolder=path_sep()+string(year, format='(I4)') else yFolder=''
  if keyword_set(FULLPATH) then return, archiveDir+yFolder+path_sep()+fileName else return, fileName
  ;if keyword_set(FULLPATH) then return, archiveDir+path_sep()+string(year, format='(I4)')+path_sep()+fileName else return, fileName

end