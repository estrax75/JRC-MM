function build_JRC_SPP_AVH_Daily_FileName, sensor, missionCode, year, month, day, archivedir, fileType, FULLPATH=FULLPATH, $
  JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR, YEARFOLDER=YEARFOLDER, MISSIONNUMBER=MISSIONNUMBER

  ;AVH09C1.A2003001.N16.004.2014111105754
  fullSensor=sensor+'????'
  if n_elements(fileType) eq 0 then ifileType='*' else ifileType=fileType 
  expectedRetrieveTimeStamp=''
  for i=1, 14 do expectedRetrieveTimeStamp=expectedRetrieveTimeStamp+'?'

  expectedRetrieveOrbitCode=''
  for i=1, 2 do expectedRetrieveOrbitCode=expectedRetrieveOrbitCode+'?'
  if n_elements(MISSIONNUMBER) ne 0 then missionName='N'+string(MISSIONNUMBER, format='(I02)')

  expectedRetrieveOrbitSuffix=''
  for i=1, 3 do expectedRetrieveOrbitSuffix=expectedRetrieveOrbitSuffix+'?'

  if keyword_set(JULDAY) then fileName=string(format='(A, ".A", I4, I03, ".", A, ".", A, ".", A, ".", A)', fullSensor, year, day, missionName, expectedRetrieveOrbitSuffix, expectedRetrieveTimeStamp, ifileType) else $
    fileName=string(format='(A, ".A", I4, I02, I02, ".", A, ".", A, ".", A, ".", A)', fullSensor, year, month, day, missionName, expectedRetrieveOrbitSuffix, expectedRetrieveTimeStamp, ifileType)
  ;  sjday = julday(date,1,year) - julday(1,1,year) + 1;
  ;  ejday = julday(date+1,1,year) - julday(1,1,year);
  ;  fileName=string(format='(A, "_", A, "_", A, I4, I03, I4, I03,"_", A,"_",A, ".nc")', prefix, periodType, parameter, year, sjday, year, ejday, sensor, roi)

  if keyword_set(YEARFOLDER) then yFolder=path_sep()+string(year, format='(I4)') else yFolder=''
  if keyword_set(FULLPATH) then return, archiveDir+yFolder+path_sep()+fileName else return, fileName
  ;if keyword_set(FULLPATH) then return, archiveDir+path_sep()+string(year, format='(I4)')+path_sep()+fileName else return, fileName

end