;function [sst, ix_sst, sst_lat, sst_lon] = read_AVHRR_sst_qual(sst_filename,sst_qual_filename,map_win,avhrr_qual)
function readOceanSat2Contents, oceanSatFileNames, varList, convFuncList, tempDir, targetMapInfo, ignoreValue

  COMMON smurffCB, mainApp
  
  utils=mainApp->getUtility()
  fS=mainApp->getFileSystem()
  
  tempfName=utils->getSysTime(/FILECOMPATIBILITY)
  
  valIdxs = where(oceanSatFileNames ne '', count)
  
  if count ne 0 then begin
    os2Clo=findFileForDataSetName(oceanSatFileNames[valIdxs], varList[0])
    os2Aod=findFileForDataSetName(oceanSatFileNames[valIdxs], varList[1])
    os2tsm=findFileForDataSetName(oceanSatFileNames[valIdxs], varList[2])
    os2dac=findFileForDataSetName(oceanSatFileNames[valIdxs], varList[3])
  endif else begin
    return, {enviDataFile:'', year:'', month:'', jday:''}
  endelse
  
  if os2Clo.found+os2Aod.found+os2tsm.found+os2dac.found ne 0 then begin
    enviFileName = tempDir+path_sep()+tempfName+'_all_os2.envi'
    openw, lun, enviFileName, /GET_LUN
    bandNames=['']
    if os2Clo.found then begin
      cloIgnoreValue=0.
      ns=os2Clo.ns & nl=os2Clo.nl & dt=os2Clo.dataType
      addBandToEnviFile, os2Clo.fileName, os2Clo.dataSetIdx, lun, cloIgnoreValue, ignoreValue
      bandNames=[bandNames, varList[0]]
    endif
    if os2Aod.found then begin
      aodIgnoreValue=0.
      ns=os2Aod.ns & nl=os2Aod.nl  & dt=os2Aod.dataType
      addBandToEnviFile, os2Aod.fileName, os2Aod.dataSetIdx, lun, aodIgnoreValue, ignoreValue
      bandNames=[bandNames, varList[1]]
    endif
    if os2tsm.found then begin
      tsmIgnoreValue=0.
      ns=os2tsm.ns & nl=os2tsm.nl  & dt=os2tsm.dataType
      addBandToEnviFile, os2tsm.fileName, os2tsm.dataSetIdx, lun, tsmIgnoreValue, ignoreValue
      bandNames=[bandNames, varList[2]]
    endif
    if os2dac.found then begin
      dacIgnoreValue=0.
      ns=os2dac.ns & nl=os2dac.nl  & dt=os2dac.dataType
      addBandToEnviFile, os2dac.fileName, os2dac.dataSetIdx, lun, dacIgnoreValue, ignoreValue
      bandNames=[bandNames, varList[3]]
    endif
    bandNames=bandNames[1:*]
    
    doLog, callingRoutine=callingRoutine, /STACK
    doLog, callingRoutine, enviFileName, LEVEL=4
    close, lun
    free_lun, lun
    
    os2MapSize = [ns, nl];
    os2MapWin = [-180.0,180.0,90,-90];
    
    stepLon = (ABS(os2MapWin[1]-os2MapWin[0]))/(ns-1)
    stepLat = (ABS(os2MapWin[2]-os2MapWin[3]))/(nl-1)
    
    ; set the map info to the regular grid in ENVI file
    ps = [stepLon, stepLat]
    mc = [0.5D, 0.5D, os2MapWin[0], os2MapWin[2]]
    mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)
    
    bNumber=n_elements(bandNames)
    
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
      BNAMES=bandNames
  endif
  
  return, {enviDataFile:enviFileName, year:'', month:'', jday:''}
  
end