function build_Land_Avh_FileName_D, sensor, missionCode, year, month, day, archivedir, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR, YEARFOLDER=YEARFOLDER, MISSIONNUMBER=MISSIONNUMBER

  ;AVH09C1.A2003001.N16.004.2014111105754
  ;AVHRR-Land_v004_AVH09C1_NOAA-14_20000701_c20140210122521.nc

  fileType='*'
  expectedRetrieveTimeStamp=''
  for i=1, 15 do expectedRetrieveTimeStamp=expectedRetrieveTimeStamp+'?'

  expectedRetrieveOrbitCode=''
  for i=1, 2 do expectedRetrieveOrbitCode=expectedRetrieveOrbitCode+'?'
  if n_elements(MISSIONNUMBER) ne 0 then expectedRetrieveOrbitCode=string(MISSIONNUMBER, format='(I02)')

  expectedRetrieveOrbitSuffix=''
  for i=1, 3 do expectedRetrieveOrbitSuffix=expectedRetrieveOrbitSuffix+'?'

  if keyword_set(JULDAY) then fileName=string(format='(A, "_", A, "-", I02, "_", I4, I03, A, ".", A)', sensor, missionCode, missionNumber, year, day, expectedRetrieveOrbitCode, expectedRetrieveOrbitSuffix, expectedRetrieveTimeStamp, fileType) else $
    fileName=string(format='(A, "_", A, "-", I02, "_", I4, I02, I02, "_", A, ".", A)', $
      sensor, missionCode, missionNumber, year, month, day, expectedRetrieveTimeStamp, fileType)
  ;  sjday = julday(date,1,year) - julday(1,1,year) + 1;
  ;  ejday = julday(date+1,1,year) - julday(1,1,year);
  ;  fileName=string(format='(A, "_", A, "_", A, I4, I03, I4, I03,"_", A,"_",A, ".nc")', prefix, periodType, parameter, year, sjday, year, ejday, sensor, roi)

  if keyword_set(YEARFOLDER) then yFolder=path_sep()+string(year, format='(I4)') else yFolder=''
  if keyword_set(FULLPATH) then return, archiveDir+yFolder+path_sep()+fileName else return, fileName
  ;if keyword_set(FULLPATH) then return, archiveDir+path_sep()+string(year, format='(I4)')+path_sep()+fileName else return, fileName

end