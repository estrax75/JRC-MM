;function [sst, ix_sst, sst_lat, sst_lon] = read_AVHRR_sst_qual(sst_filename,sst_qual_filename,map_win,avhrr_qual)
function readGlobSeaWiFSParContents, globfilenames, varList, outBandNames, convFuncList, roi, tempDir, targetMapInfo, ignoreValue, $
NOTFOUND=NOTFOUND, NOLATCORRECTION=NOLATCORRECTION, NOLONCORRECTION=NOLONCORRECTION

  COMMON smurffCB, mainApp
  
  utils=mainApp->getUtility()
  fS=mainApp->getFileSystem()
  
  ;use Fred function(s) to read data...
  ;avhrrQual=float(mainApp->getKeyValue('AVHRR_QUAL'))
  
  tempfName=utils->getSysTime(/FILECOMPATIBILITY)
  enviFileName = tempDir+path_sep()+tempfName+'_all_glob.envi'
  
  resData=read_nc4_dataset(globfilenames[0],varList[0],start=start,count=count)
  ;globInfo=findFileForDataSetName(globfilenames, varList[0])
  ;jrcInfoQualSst=findFileForDataSetName(sstfilenames[1], varList[1])
  
  ;ENVI_OPEN_DATA_FILE, globInfo.fileName, r_fid=fidglob, /HDF_SD, HDFSD_DATASET=globInfo.dataSetIdx
  ;ENVI_FILE_QUERY, fidGlob, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
  
  ;globData = ENVI_GET_DATA(fid=fidglob, dims=dims, pos=0)
  
  qualityCheck = where(resData.data le resData.badv, count, complement=goodIdxs, ncomplement=goodIdxsno);
  dims=size(resData.data, /DIM)
  ns=dims[0] & nl=dims[1] 
  globData=fltarr(nl,ns,/NOZERO)
  
  if count ne 0 then globData[qualityCheck]=ignoreValue ; or sstData[qualityCheck]=!VALUES.F_NAN
  if goodIdxsno ne 0 then globData[goodIdxs]=1.*resData.data[goodIdxs] * resData.scale + resData.offset
  dt=4 ; Convert to float
  
  openw, lun, enviFileName, /GET_LUN
  writeu, lun, globData
  free_lun, lun
  
  ;globMapSize = [nl, ns];
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