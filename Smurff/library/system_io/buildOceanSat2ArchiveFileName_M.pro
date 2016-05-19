;% modified - deleted .bz2
function buildOceanSat2ArchiveFileName_M, year, date, periodType, roi, sensor, parameter, archivedir, FULLPATH=FULLPATH, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND, OUTFILEDIR=OUTFILEDIR
    
  ;global SST_MODIS_DIR
    
  ;E:\data\mariomi\application\oxyrisk\input\oceansat2\monthly\O2_01APR2011_004_000_GAN_L3B_CL_M.hdf
  NOTFOUND=0
  NOTFOUND1=0 & NOTFOUND2=0 & NOTFOUND3=0 & NOTFOUND4=0
  utils=obj_new('utility')
  month=date
  archiveRoot=archivedir
  
  monthName=utils->convertMonthNumberToName(month)
  obj_destroy, utils
  
  pathSep = path_sep()
  
  oceanSatFilenames=strarr(4)
  yearStr=string(format='(I4)', year)
  
  ; AO: Aerosol optical depth at 865 nM
  oceanSatPatt0 = string(format='("O2_01", A, I4, "_", I03, "*L3B*AO*")', monthName, year, month)
  ; DA: Diffuse attenuation coefficients k 490 nM
  oceanSatPatt1 = string(format='("O2_01", A, I4, "_", I03, "*L3B*DA*")', monthName, year, month)
  ; CL: Chlorophyll Mapped
  oceanSatPatt2 = string(format='("O2_01", A, I4, "_", I03, "*L3B*CL*")', monthName, year, month)
  ; SE: Sediment Mapped
  oceanSatPatt3 = string(format='("O2_01", A, I4, "_", I03, "*L3B*SE*")', monthName, year, month)
  
  oceanSatDir = archiveRoot + pathSep
  
  oceanSatFilenames[0] = file_search(oceanSatDir+oceanSatPatt0, count=countOceanSatFilename0);
  oceanSatFilenames[1] = file_search(oceanSatDir+oceanSatPatt1, count=countOceanSatFilename1);
  oceanSatFilenames[2] = file_search(oceanSatDir+oceanSatPatt2, count=countOceanSatFilename2);
  oceanSatFilenames[3] = file_search(oceanSatDir+oceanSatPatt3, count=countOceanSatFilename3);
  
  if countOceanSatFilename0 ne 1 then begin
    doLog, string(format='("Can''t find OceanSat2 (Aerosol Optical) file for month: ", I3, "  year: ", I4)', month, year), level=2
    NOTFOUND1=1
    NOTFOUND++
  ;return,''
  endif
  
  if countOceanSatFilename1 ne 1 then begin
    doLog, string(format='("Can''t find OceanSat2 (Diffuse Attenuation) file for month: ", I3, "  year: ", I4)', month, year), level=2
    NOTFOUND2=1
    NOTFOUND++
  ;return,''
  endif
  
  if countOceanSatFilename2 ne 1 then begin
    doLog, string(format='("Can''t find OceanSat2 (Chlorophyll) file for month: ", I3, "  year: ", I4)', month, year), level=2
    NOTFOUND3=1
    NOTFOUND++
  ;return,''
  endif
  
  if countOceanSatFilename3 ne 1 then begin
    doLog, string(format='("Can''t find OceanSat2 (Sediment) file for month: ", I3, "  year: ", I4)', month, year), level=2
    NOTFOUND4=1
    NOTFOUND++
  ;return,''
  endif
  
  if NOTFOUND eq 4 then NOTFOUND=1 else NOTFOUND=0
  return, oceanSatFilenames
  
END