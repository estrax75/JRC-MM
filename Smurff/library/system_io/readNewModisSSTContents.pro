;function [sst, ix_sst, sst_lat, sst_lon] = read_AVHRR_sst_qual(sst_filename,sst_qual_filename,map_win,avhrr_qual)
;newFileName A20143352014365.L3m_MO_PAR_par_4km
;new var name "par"
function readNewModisSSTContents, sstfilenames, varList, outBandNames, convFuncList, roi, tempDir, targetMapInfo, ignoreValue

  COMMON smurffCB, mainApp
  
  utils=mainApp->getUtility()
  fS=mainApp->getFileSystem()
  
  tempfName=utils->getSysTime(/FILECOMPATIBILITY)
  enviFileName = tempDir+path_sep()+tempfName+'_all_sst.envi'
  
  modisInfoSst=findFileForDataSetName(sstfilenames, varList[0])
  modisQualInfoSst=findFileForDataSetName(sstfilenames, varList[1])
  ;65535 Uint Nan??
  ENVI_OPEN_DATA_FILE, modisInfoSst.fileName, r_fid=fidSst, /HDF_SD, HDFSD_DATASET=modisInfoSst.dataSetIdx
  ENVI_FILE_QUERY, fidSst, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
  sstData = ENVI_GET_DATA(fid=fidSst, dims=dims, pos=0)
  doLog, callingRoutine=callingRoutine, /STACK
  doLog, callingRoutine, modisInfoSst.fileName, LEVEL=4
  
  ENVI_OPEN_DATA_FILE, modisQualInfoSst.fileName, r_fid=fidQualSst, /HDF_SD, HDFSD_DATASET=modisQualInfoSst.dataSetIdx
  ENVI_FILE_QUERY, fidQualSst, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
  sstQualData = ENVI_GET_DATA(fid=fidSst, dims=dims, pos=0)
  doLog, callingRoutine=callingRoutine, /STACK
  doLog, callingRoutine, modisQualInfoSst.fileName, LEVEL=4
  
  sstSlope = 0.000717;
  sstIntercept = -2.000000;
  sstValidRange = [1, 32767];
  sstLongName = 'Temperature';
  sstUnits = 'degC';
  
  sstQualSlope = 1;
  sstQualIntercept = 1;
  ;sstQualValidRange = [0, 2];
  sstQualNotValid= 65535
  sstQualLongName = 'SST Flags';
  sstQualUnits = '';
  
  qualityCheck = where(sstData eq sstQualNotValid, count, complement=goodIdxs, ncomplement=goodIdxsno);
  
  ;Unsigned Int to float... flag -9999 NaN
  resData=fltarr(ns,nl,/NOZERO)
  if count ne 0 then resData[qualityCheck]=ignoreValue ; or sstData[qualityCheck]=!VALUES.F_NAN
  if goodIdxsno ne 0 then resData[goodIdxs]=1.*sstData[goodIdxs] * sstSlope + sstIntercept
  sstQualData=0
  sstData=0
  dt=4 ; Convert to float
  
  openw, lun, enviFileName, /GET_LUN
  writeu, lun, resData
  free_lun, lun
  
  sstMapSize = [ns, nl];
  sstMapWin = [-180.0,180.0,90,-90];
  
  stepLon = (ABS(sstMapWin[1]-sstMapWin[0]))/(ns-1)
  stepLat = (ABS(sstMapWin[2]-sstMapWin[3]))/(nl-1)
  
  ; set the map info to the regular grid in ENVI file
  ps = [stepLon, stepLat]
  mc = [0.5D, 0.5D, sstMapWin[0], sstMapWin[2]]
  mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)
  ;bNumber=1
  ;bNames='sst'
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
    
  ENVI_FILE_MNG, id=fidSst, /REMOVE, /DELETE
  ENVI_FILE_MNG, id=fidQualSst, /REMOVE, /DELETE
  ;file_delete, modisInfoSst.fileName
  return, {enviDataFile:enviFileName, year:'', month:'', jday:''}
  
end