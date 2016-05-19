;function [sst, ix_sst, sst_lat, sst_lon] = read_AVHRR_sst_qual(sst_filename,sst_qual_filename,map_win,avhrr_qual)
function readAvhrrContents, sstfilenames, varList, outBandNames, convFuncList, roi, tempDir, targetMapInfo, ignoreValue

  COMMON smurffCB, mainApp
  
  utils=mainApp->getUtility()
  fS=mainApp->getFileSystem()
  
  avhrrQual=float(mainApp->getKeyValue('AVHRR_QUAL'))
  
  tempfName=utils->getSysTime(/FILECOMPATIBILITY)
  enviFileName = tempDir+path_sep()+tempfName+'_all_sst.envi'
  
  sstSlope = 0.075;
  sstIntercept = -3;
  
  jrcInfoSst=findFileForDataSetName(sstfilenames, varList[0])
  jrcInfoQualSst=findFileForDataSetName(sstfilenames, varList[1])
  
  ENVI_OPEN_DATA_FILE, jrcInfoSst.fileName, r_fid=fidSst, /HDF_SD, HDFSD_DATASET=jrcInfoSst.dataSetIdx
  ENVI_OPEN_DATA_FILE, jrcInfoQualSst.fileName, r_fid=fidSstQual, /HDF_SD, HDFSD_DATASET=jrcInfoQualSst.dataSetIdx
  ENVI_FILE_QUERY, fidSst, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
  
  sst_qual_slope = 1;
  sst_qual_intercept = 1;
  sst_qual_valid_range = [0,8];
  sst_qual_long_name = 'SST Flags';
  sst_qual_units = '';
  
  sstData = ENVI_GET_DATA(fid=fidSst, dims=dims, pos=0)
  sstQualData = ENVI_GET_DATA(fid=fidSstQual, dims=dims, pos=0)
  
  qualityCheck = where(sstQualData lt avhrrQual, count, complement=goodIdxs, ncomplement=goodIdxsno);
  
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
;  bNumber=1
;  bNames='sst'
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