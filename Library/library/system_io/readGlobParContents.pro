;function [sst, ix_sst, sst_lat, sst_lon] = read_AVHRR_sst_qual(sst_filename,sst_qual_filename,map_win,avhrr_qual)
function readGlobParContents, globfilenames, varList, outBandNames, convFuncList, roi, tempDir, targetMapInfo, ignoreValue, $
NOTFOUND=NOTFOUND, NOLATCORRECTION=NOLATCORRECTION, NOLONCORRECTION=NOLONCORRECTION

  COMMON smurffCB, mainApp
  
  utils=mainApp->getUtility()
  fS=mainApp->getFileSystem()
  
  ;avhrrQual=float(mainApp->getKeyValue('AVHRR_QUAL'))
  
  tempfName=utils->getSysTime(/FILECOMPATIBILITY)
  enviFileName = tempDir+path_sep()+tempfName+'_all_glob.envi'
  
  ;matlab version
  ;daacSlope = 0.0012;
  ;test version
  globSlope = 1.00;
  globIntercept = 0;
  globValidRange = [0,254];
  ;daacLongName = 'Photosynthetically Available Radiation';
  ;daacUnits = 'Einstein m^-2 Day';
  outOfRangeValue = 65534 ;-32767.??
  
  globInfo=findFileForDataSetName(globfilenames, varList[0])
  ;jrcInfoQualSst=findFileForDataSetName(sstfilenames[1], varList[1])
  
  ENVI_OPEN_DATA_FILE, globInfo.fileName, r_fid=fidglob, /HDF_SD, HDFSD_DATASET=globInfo.dataSetIdx
  ENVI_FILE_QUERY, fidGlob, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
  
  globData = ENVI_GET_DATA(fid=fidglob, dims=dims, pos=0)
  
  qualityCheck = where(globData le -32767., count, complement=goodIdxs, ncomplement=goodIdxsno);
  resData=fltarr(ns,nl,/NOZERO)
  if count ne 0 then resData[qualityCheck]=ignoreValue ; or sstData[qualityCheck]=!VALUES.F_NAN
  if goodIdxsno ne 0 then resData[goodIdxs]=1.*globData[goodIdxs] * globSlope + globIntercept
  ;daacQualData=0
  globData=0
  dt=4 ; Convert to float
  
  openw, lun, enviFileName, /GET_LUN
  writeu, lun, resData
  free_lun, lun
  
  globMapSize = [ns, nl];
  globMapWin = [-180.0,180.0,90,-90];
  
  stepLon = (ABS(globMapWin[1]-globMapWin[0]))/(ns-1)
  stepLat = (ABS(globMapWin[2]-globMapWin[3]))/(nl-1)
  
  ; set the map info to the regular grid in ENVI file
  ps = [stepLon, stepLat]
  ;mc = [0.5D, 0.5D, globMapWin[0], globMapWin[2]]
  mc = [0.0D, 0.0D, globMapWin[0], globMapWin[2]]
  mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)
  ;bNumber=1
  ;bNames='daacPar'
  bNumber=n_elements(outBandNames)
  bNames=outBandNames
  
  ; setup the header in the ENVI file
  envi_setup_head_oxy,  $
    FNAME=enviFileName,$
    NS=ns,$
    NL=nl, $
    DATA_IGNORE_VALUE=ignoreValue, $
    NB=bNumber, $
    DATA_TYPE=dt, $
    FILE_TYPE=0, $
    INTERLEAVE=0, $
    R_FID=sstId, $
    MAP_INFO=mapinfo, $
    /WRITE,$
    /OPEN, $
    BNAMES=bNames
    
  return, {enviDataFile:enviFileName, year:'', month:'', jday:''}
  
end