@readsingleband.pro
pro test_baltic_chl

  ENVI, /restore_base_save_files
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  maindir='E:\data\mariomi\application\smurf\data\timeseries\monthly\output_vs_e_regional\baltic_gof_analysis'
  months=['06', '07', '08']
  years=['2003', '2004', '2005', '2006']
  cropfileName='crop_BALT_GOF'
  fullfileName='full_BALT'
  savFile='balticsea_GULF_OF_FINLAND'
  dataSavFileSuffix='_data.sav'
  statsSavFileSuffix='_stats.sav'
  searchSourcePath='*MO.BALT'
  bandToSearch='chl_oc3-Mean'
  ;bandToSearch='chl_oc3-Mean'
  
  plotArray=fltarr(n_elements(years), n_elements(months),2)
  for i=0, n_elements(years)-1 do begin
    for j=0, n_elements(months)-1 do begin
      test_dir=maindir+path_sep()+years[i]+'_'+months[j]
      
      sourceFiles=file_search(test_dir+path_Sep()+searchSourcePath, count=count);
      sourceDataInfo=readSingleBand(bandToSearch, sourceFiles, mask)
      ;help, sourceDataInfo, /STRUCT
      ENVI_OPEN_DATA_FILE, test_dir+path_sep()+cropfileName+'.envi', r_fid=fid;, /HDF_SD, HDFSD_DATASET=sourceDataSetIndex
      ENVI_FILE_QUERY, fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
      cropData = ENVI_GET_DATA(fid=fid, dims=dims, pos=0)
      cropMapInfo = envi_get_map_info(FID=fid)
      print, 'cropMapInfo-->', cropMapInfo
      ENVI_FILE_MNG, id=fid, /REMOVE
      
      ENVI_OPEN_DATA_FILE, test_dir+path_sep()+fullfileName+'.envi', r_fid=fid;, /HDF_SD, HDFSD_DATASET=sourceDataSetIndex
      ENVI_FILE_QUERY, fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
      fullData = ENVI_GET_DATA(fid=fid, dims=dims, pos=0)
      fullMapInfo = envi_get_map_info(FID=fid)
      print, 'fullMapInfo-->', fullMapInfo
      ENVI_FILE_MNG, id=fid, /REMOVE
      
      flagIdxs=where(sourceDataInfo.data le 0 and fullData le 0, flagCount)
      if count ne 0 then begin
        sourceDataInfo.data[flagIdxs]=!VALUES.F_NAN
        fullData[flagIdxs]=!VALUES.F_NAN
      endif
      print, 'full difference? --->', total(fullData, /NAN), total(sourceDataInfo.data, /NAN)
      restore, test_dir+path_sep()+months[j]+'_'+years[i]+'_'+savFile+dataSavFileSuffix
      ;restore "band" cropped matrix
      
      ;restore "statres" {count:int, statvalue:float}
      flagIdxs=where(cropData le 0, flagCount)
      if flagCount gt 0 then begin
        cropData[flagIdxs]=!VALUES.F_NAN
        band[flagIdxs]=!VALUES.F_NAN
      endif
      print, 'crop difference? --->', total(cropData, /NAN), total(band, /NAN)
      help, statres, /STR
      restore, test_dir+path_sep()+months[j]+'_'+years[i]+'_'+savFile+statsSavFileSuffix
      plotArray[i,j,0]=statres.statValue
      plotArray[i,j,1]=mean(cropData, /NAN)
      
    endfor
    
  endfor
  
  ;for i=0, 1 do print, plotArray[*,*,i]
  yrange=[0., max(plotArray)]
  
  ;linestyles=indgen(n_elements(years)) mod 6
  ;plot, reform(plotArray[0,*,0]), yrange=yrange, psym=symbols[0];, linestyle=linestyles[0];, color=colors[0]
  ;for i=0, n_elements(years)-1 do oplot, reform(plotArray[i,*,0]),  psym=symbols[i];, linestyle=linestyles[i];, color=colors[i]
  nSeries=n_elements(years)
  pSyms=indgen(nSeries)+1
  colors=['Light Sea Green', 'Orange', 'Red', 'Red']
  ;colors=['red', 'dodger blue', 'Forest Green', 'Rosy Brown', 'Dark Salmon', 'Violet', 'Black']
  
  ;lineStyles=indgen(nSeries) mod 5
  lineStyles=[0,0,1,0]
  
  for i=0, nSeries-1 do begin
    y=reform(plotArray[i,*,0])
    cgPlot, y, overplot=i ne 0, LINESTYLE=lineStyles[i], psym=0, color=colors[i], $
      title=mainTitle, POSITION=POSITION, YRANGE=YRANGE, ytitle=yMeasureUnit, XSTYLE=1, $
      XTICK_GET=xticks, YMARGIN=[9,2], CHARSIZE=charsize, XTICKS=1, XMINOR=n_elements(y)-1
    ;, XTICKINTERVAL=XTICKINTERVAL
      
    cgPlot, y, /overplot, psym=pSyms[i], color=colors[i], POSITION=POSITION, XSTYLE=1
    
  endfor
  cgLegend, Title=years, PSym=pSyms, $
    LineStyle=lineStyles, Color=colors, Location=[0.15, 0.85], $
    /Box, /Background, BG_Color='gray', Length=0.075, /Center_Sym, SymSize=2
  label='MODIS chla (oc3_Mean) -Gulf of Finland- months: '
  for i=0, n_elements(months)-1 do label=label+months[i]+'-'
  label=strmid(label, 0, strlen(label)-1)
  cgText, 0.5, 0.92, /NORM, $
    label, ALIGN=0.5, TT_FONT='Helvetica'
end