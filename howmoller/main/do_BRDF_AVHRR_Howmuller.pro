pro do_BRDF_AVHRR_Howmuller, month, year, sourceParameter, parameter, formatType, inputBaseDir, outputBaseDir, $
  eps=EPS, TC_TYPE=TC_TYPE, TA_TYPE=TA_TYPE, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX
  ;
  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  inputBaseDir=ST_fileSystem->adjustDirSep(inputBaseDir, /ADD)
  outputBaseDir=ST_fileSystem->adjustDirSep(outputBaseDir, /ADD)
  
  noaanumber=getAVHRRNOAANumber(year, undef)
  if n_elements(noaanumber) gt 1 then noaanumber=noaanumber[MISSIONOVERLAPINDEX]
  noaanumber=noaanumber[0]

  tInfo=getTimeDerivedInfo(year, month, TA_TYPE, TC_TYPE, xticks_c=xticks_c, xtickname_c=xtickname_c)

  first=tInfo.first
  last=tInfo.last
  ;level=tInfo.level
  ;extraPath=tInfo.extraPath

  instrument='AVH'
  version='N'+string(noaanumber, format='(I02)');version='001'
  indicator='LAN'
  spatialResolution='0005D'
  level='L2'

  outDir=outputBaseDir+'HM'+path_sep()

  ;hmFile=outputBaseDir+'HM'+path_sep()+fName+'_hm'
  ;savFile=outputBaseDir+'HM'+path_sep()+fName+'_idl'

  timeLapse=n_elements(first)
  toalong=dblarr(4,timeLapse,3600)
  toalong(*,*,*)=-10.0
  num1=fltarr(timeLapse,3600)
  num2=fltarr(timeLapse,3600)
  parListToRead=['BRF_BAND_1','BRF_BAND_2','SIGMA_BRF_BAND_1','SIGMA_BRF_BAND_2','LDTR_FLAG']
  parListToWrite=['BRF_BAND_1','BRF_BAND_2','SIGMA_BRF_BAND_1','SIGMA_BRF_BAND_2']
  
  savFileName=getHowMollerSavFile(parameter, year, month, TC_TYPE, noaanumber, sourceParameter)
  savFileName=outDir+savFileName
  print, 'sav file -->', savFileName

  ;savFileName=getHowMollerSavFile(parameter, year, month, TC_TYPE, noaanumber[0], sourceParameter)
  ;fullFileName=outDir+savFileName

  for timecount=0, timelapse-1 do begin
    startDay=first[timecount]
    endDay=last[timecount]
    ; find AVHRR file
    ;
    ;thisfile_ldtr='AVHRR_NOA'+noaanumber+'_'+year+month+startDay+'000000_'+year+month+endDay+'000000_'+LTYPE+'_MUL_000001_900S900N1800W1800E_PLC_0005D_BRF.NC'
    ;AVH09C1_GEOG_0.05DEG_1991_09_28_N11_BRF.nc
    
    ;resFileHDFInfo=build_JRC_BRDF_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
    ;  product, version, 'HDF',  indicator=indicator, level, projection=projection)
    thisFile=searchProductFile('BRDF', instrument, spatialResolution, indicator, year, month, startDay, endDay, noaanumber, level, version, formatType, inputBaseDir)
    if  thisFile.found then begin
      dims=[0,1]
      print, '** reading --> '+thisFile.fullfilename

      if strupcase(formatType) eq 'NC' then begin
        ncdfread,  thisFile.fullfilename,parListToRead[0], avhrr_b1, slope_b1, intercept_b1, dims
        ncdfread,  thisFile.fullfilename,parListToRead[1], avhrr_b2, slope_b2, intercept_b2, dims
        ncdfread,  thisFile.fullfilename,parListToRead[2],sigma_b1, slope_sb1, intercept_sb1, dims
        ncdfread,  thisFile.fullfilename,parListToRead[3],sigma_b2, slope_sb2, intercept_sb2, dims
        ncdfread,  thisFile.fullfilename,parListToRead[4], cloud, slope_f, intercept_f, dims
      endif
      
      if strupcase(formatType) eq 'HDF' then begin
        ST_operator->readHdfFullInfoData, thisFile.fullfilename, parListToRead[0], avhrr_b1, slope_b1, intercept_b1, b1_fillvalue, ERROR=ERROR, /APPLY
        ST_operator->readHdfFullInfoData, thisFile.fullfilename, parListToRead[1], avhrr_b2, slope_b2, intercept_b2, b2_fillvalue, ERROR=ERROR, /APPLY
        ST_operator->readHdfFullInfoData, thisFile.fullfilename, parListToRead[2], sigma_b1, slope_sb1, intercept_sb1, sigma1_fillvalue, ERROR=ERROR
        ST_operator->readHdfFullInfoData, thisFile.fullfilename, parListToRead[3], sigma_b2, slope_sb2, intercept_sb2, sigma2_fillvalue, ERROR=ERROR
        ST_operator->readHdfFullInfoData, thisFile.fullfilename, parListToRead[4], cloud, slope_f, intercept_f, dims, ERROR=ERROR
      endif

      ;window, 0, xsize=720, ysize=360
      ;tv, congrid(bytscl(avhrr_b1*slope_b1+intercept_b1, min=0., max=1.5), 720,360)
      ;window, 1, xsize=720, ysize=360
      ;tv, congrid(bytscl(avhrr_b2*slope_b2+intercept_b2, min=0., max=1.5), 720,360)
      ;window, 2, xsize=720, ysize=360
      ;tv, congrid(bytscl(sigma_b1*slope_sb1+intercept_sb1, min=0., max=1.5), 720,360)
      ;window, 3, xsize=720, ysize=360
      ;tv, congrid(bytscl(sigma_b2*slope_sb2+intercept_sb2, min=0., max=1.5), 720,360)

      ;ncdfread,  diravhrr+file_ldtr,'TS', avhrr_sun, slope_sun, intercept_sun, dims
      ;ncdfread,  diravhrr+file_ldtr,'Sigma_FAPAR', sigma_avhrr_fapar, slope_fapar, intercept_fapar, dims
      rr1=cgi_map_bitwise_flag(cloud,1)
      mm=1.0

      PPMSA_ALBEDOCOLOR
      ;
      ;
      ;stop

      for lon=0, 3600-1 DO BEGIN
        bug1=avhrr_b1(*,lon)*slope_b1+intercept_b1
        bug2=avhrr_b2(*,lon)*slope_b2+intercept_b2
        bug3=sigma_b1(*,lon)*slope_sb1+intercept_sb1
        bug4=sigma_b2(*,lon)*slope_sb2+intercept_sb2
        ;
        idxlon1=where(bug1 ge 0. and bug3 ge 0.0)
        idxlon2=where(bug2 ge 0. and bug4 ge 0.0)

        if idxlon1(0) ge 0 then begin
          toalong(0,timecount,lon)=mean(bug1(idxlon1))
          toalong(2,timecount,lon)=2.*mean(bug3(idxlon1))
          num1(timecount,lon)=N_elements(idxlon1)/7200.0*100.0
        endif
        if idxlon2(0) ge 0 then begin
          toalong(1,timecount,lon)=mean(bug2(idxlon2))
          toalong(3,timecount,lon)=2.*mean(bug4(idxlon2))
          num2(timecount,lon)=N_elements(idxlon2)/7200.0*100.0
        endif
      endfor
      print, '** done **--> '+thisFile.fullfilename
    endif

  endfor

  ;save, toalong, num1, num2, $
  ;  filename='/local0/gobrona/save/Global/Save'+parameter+'_'+year+month+'_avhrr_BRFs'+noaanumber+'.sav'
  save, toalong, num1, num2, $
    filename=savFileName, /COMPRESS


  ;print, 'Max', m
  ;
  ;
  min1=0.0
  min2=0.00
  max1=timeLapse*8
  max2=180.0
  nx=360
  nyear=1
  xB = 10
  yB = 35
  xs = 55
  ys = 45
  yL=nx*2.0
  xL=31*nyear
  ;
  xSize1 = double(xs + xL + xB)
  ySize1 = double(ys + yL + yB)

  set_plot,'x'
  resX=!d.x_px_cm
  resY=!d.y_px_cm
  print, resX, resY
  resX=43.75
  resY=43.75
  ;stop
  pos=[xs, ys, xs+xL, ys+yL]/[xSize1, ySize1, xSize1, ySize1]
  PPMSA_ALBEDOCOLOR
  latitude = (INDGEN(3600)+1) * 1.0

  titlefig=['Band 1', 'Band 2', '2 x Sigma Band 1', '2 Sigma Band 2']
  namefig=['Band_1', 'Band_2', 'Sigma_Band_1', 'Sigma_Band_2']

  for i=0, 3 do begin

    set_plot,'ps'

    epsFileName=getHowMollerEpsFile(parameter, namefig[i], '_Global_lat_daily_brftoc_avhrr_', month, year, noaanumber)
    epsFileName=outDir+epsFileName+'.eps'
    ;fname=parameter+namefig(i)+'_Global_lat_daily_brftoc_avhrr_'+month+year+noaanumber+'.eps'
    SUB=parameter+' '+titlefig[i]+' AVHRR TOC '+' NOAA '+strcompress(noaanumber, /REMOVE)
    DEVICE,FILENAME=epsFileName,XSIZE=19.625000,YSIZE=20.000000, bits_per_pixel=8,/portrait, $
      /color,ENCAPSULATED=1

    lev=[0.01,0.02,0.05,0.07, 0.1,0.15,0.2,0.25,0.30,0.40,0.50,0.60,0.70,0.80,0.90,0.95, 1.00]
    xnam=[' ','.01',' ','.02',' ','.05',' ','.07',' ','0.10',' ','.15',' ','.20',' ',$
      '0.25', ' ', '.30',' ','.40',' ','.50',' ','.60',' ','.70',' ','.80',' ','.90',' ','.95',' ','1.0',' ']


    ;nummean=fltarr(3600,2)
    ;
    ;for l=0, 3600-1 do begin
    ;idx=where(num(*,l) gt 0.)
    ;	res=moment(num(idx,l), sdev=sdev)
    ;	nummean(l,0)=res(0)
    ;	nummean(l,1)=sdev
    ;endfor

    time=findgen(timeLapse)
    Contour, toalong(i,*,*),time,latitude,levels = lev, c_charsize=1.5, $
      c_color=[2,20,30,40,55,60,70,80,100,110,130,150,170,190,210,235,256], $
      /fill,xstyle=1,ystyle=1,ytitle='Latitude', $
      subtitle=SUB,$
      charsize=1.,charthick=1., title='Longitude Average '+strcompress(month, /REMOVE)+' '+strcompress(year, /REMOVE), $
      BACKGROUND = 16777215, color=0,ymargin=[10,8], xrange =[0,30], yr=[0,3600], $
      ;xticks=11, xtickname=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
      xticks=4, xtickname=['01','05','10','15',strcompress(string(timeLapse),/remove_all)],$
      yticks=4, ytickname=['-90','-45','0','45','90'] ;,'Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    cols=[2,20,30,40,55,60,70,80,100,110,130,150,170,190,210,235,256] ; les valeurs pour chaque couleur
    ncon=n_elements(cols)

    contour,rotate([1,1]#(indgen(ncon)*3),3),indgen(ncon)*3,[0,1],/cell_fill,$
      /closed,levels=indgen(ncon),c_colors=cols,/noerase,$
      xrange=[0,ncon],ystyle=4, pos=[0.15,0.05,0.8,0.10],$
      xticks=ncon*2, xtickname=xnam


    device,/close
  endfor
  if !VERSION.OS_FAMILY eq 'unix' then set_plot, 'X'
  ;
  ;
  ;
  ;


end