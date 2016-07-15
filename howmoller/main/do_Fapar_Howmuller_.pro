;do_Fapar_Howmuller, 1, 1999, 14, 'FAPAR', 'band1_2','NC', '/space3/storage/products/results/FAPAR', '/space3/storage/products/results/FAPAR', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY', /nodirbuild
;do_Fapar_Howmuller, 2, 2003, 16, 'FAPAR', 'band1_2','NC', '/space3/storage/products/results/FAPAR', '/space3/storage/products/results/FAPAR', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY', /nodirbuild
pro do_Fapar_Howmuller_, month, year, NOAANUMBER, sourceParameter, parameter, $
  inputFormatType, inputBaseDir, outputBaseDir, $
  eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, nodirbuild=nodirbuild
  ;
  ; QA4EVC daily BRFs
  ;
  if ~obj_valid(utils) then utils=obj_new('Utility')
  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)

  NOAANUMBER=string(NOAANUMBER, format='(I02)')
  year=string(year, format='(I04)')
  month=string(month, format='(I02)')

  tc_dir=''
  ta_dir=''

  if n_elements(TC_TYPE) eq 0 then TC_TYPE='DAILY'
  if TA_TYPE eq 'TC' then ta_dir='TC'
  if TA_TYPE eq 'MEAN' then ta_dir='TC'
  if TA_TYPE eq 'NONE' then ta_dir=''
  level='L2'

  if TC_TYPE eq 'DAILY' then begin
    dayofMonth=utils->calcDayOfMonth([year,month,1,0])
    first=indgen(dayofMonth)+1
    last=first
    xticks_c=4
    xtickname_c=['01','05','10','15',strcompress(string(20),/remove_all)]
    tc_dir=TC_TYPE
  endif
  if TC_TYPE eq '5D' then begin
    first=[01,06,11,16,21,26]
    last=[5,10,15,20,25,utils->calcDayOfMonth([year,month,1,0])]
    xticks_c=3
    xtickname_c=[string(first[0], format='(I02)')+string(last[0], format='(I02)'), $
      string(first[2], format='(I02)')+string(last[2], format='(I02)'), $
      string(first[4], format='(I02)')+string(last[4], format='(I02)')]
    tc_dir=TC_TYPE
  endif
  if TC_TYPE eq '10D' then begin
    first=[01,11,21]
    last=[10,20,utils->calcDayOfMonth([year,month,1,0])]
    xticks_c=3
    xtickname_c=[string(first[0], format='(I02)')+string(last[0], format='(I02)'), $
      string(first[1], format='(I02)')+string(last[1], format='(I02)'), $
      string(first[2], format='(I02)')+string(last[2], format='(I02)')]
    tc_dir=TC_TYPE
  endif
  if TC_TYPE eq '16D' then begin
    first=[01,17]
    last=[16,utils->calcDayOfMonth([year,month,1,0])]
    xticks_c=2
    xtickname_c=[string(first[0], format='(I02)')+string(last[0], format='(I02)'), $
      string(first[1], format='(I02)')+string(last[1], format='(I02)')]
    tc_dir=TC_TYPE
  endif
  if TC_TYPE eq 'MONTHLY' then begin
    first=[01]
    last=[utils->calcDayOfMonth([year,month,1,0])]
    xticks_c=2
    xtickname_c=[string(first[0], format='(I02)')+string(last[0], format='(I02)'), $
      string(first[0], format='(I02)')+string(last[0], format='(I02)')]
    tc_dir=TC_TYPE
  endif
  extraPath=''
  if ta_dir ne '' then extraPath=path_sep()+ta_dir 
  if tc_dir ne '' then extraPath=path_sep()+tc_dir
  if ~(KEYWORD_SET(nodirbuild)) then inputDir=inputBaseDir+extraPath else inputDir=inputBaseDir
  outDir=outputBaseDir+extraPath+path_sep()+'HM'+path_sep()

  parListToRead=['FAPAR','BRF_BAND_2','Sigma_FAPAR','SIGMA_BRF_BAND_2', 'LDTR_FLAG']
  parListToWrite=['FAPAR','BRF_BAND_2','Sigma_FAPAR','SIGMA_BRF_BAND_2']
  sensor='AVH09C1'
  missionCode='N'

  timeLapse=n_elements(first)
  toalong=dblarr(n_elements(parListToWrite),timeLapse,3600)
  toalong(*,*,*)=-10.0
  num1=fltarr(timeLapse,3600)
  num2=fltarr(timeLapse,3600)
  for timecount=0, timeLapse-1 do begin
    startDay=string(first[timecount], format='(I02)')
    endDay=string(last[timecount], format='(I02)')
    yearDay=utils->calcDayOfYear([long(year),long(month),long(startDay),0])+1
    print, 'working on', year, month, startDay, '...'
    ;thisfile_ldtr=getFaparFile(noaanumber, year, month, startDay, endDay, LTYPE)

    ; subscribe with [0] 'cause reader needs only single element scalar parameter
    res_ldtr=(file_search(diravhrr,thisfile_ldtr, count=count, /FULLY_QUALIFY_PATH))[0]
    if  count eq 1 then begin
      dims=[0,1]
      print, '** reading --> '+res_ldtr

      if inputFormatType eq 'NC' then begin
        ncdfread,  res_ldtr,parListToRead[0], fapar, slope_fapar, intercept_fapar, dims
        ncdfread,  res_ldtr,parListToRead[1], b2, slope_b2, intercept_b2, dims
        ncdfread,  res_ldtr,parListToRead[2], sigma_fapar, slope_fapar_b2, intercept_fapar_b2, dims
        ncdfread,  res_ldtr,parListToRead[3], sigma_b2, slope_sigma_b2, intercept_sigma_b2, dims
	    ncdfread,  res_ldtr,parListToRead[4], cloud, slope_f, intercept_f, dims
      endif
      if inputFormatType eq 'HDF' then begin
        operatorObj->readHdfFullInfoData,res_ldtr,parListToRead[0], fapar, slope_fapar, intercept_fapar, dims
        operatorObj->readHdfFullInfoData,res_ldtr,parListToRead[1], b2, slope_b2, intercept_b2, dims
        operatorObj->readHdfFullInfoData,res_ldtr,parListToRead[2], sigma_fapar, slope_fapar_b2, intercept_fapar_b2, dims
        operatorObj->readHdfFullInfoData,res_ldtr,parListToRead[3], sigma_b2, slope_sigma_b2, intercept_sigma_b2, dims
	    operatorObj->readHdfFullInfoData,res_ldtr,parListToRead[4], cloud, slope_f, intercept_f, dims
      endif

      rr1=cgi_map_bitwise_flag(cloud,1)
      mm=1.0

      FAPARCOLOR
      ;
      ;
      ;stop

      for lon=0, 3600-1 DO BEGIN
        bug1=fapar(*,lon)*slope_fapar+intercept_fapar
        bug2=b2(*,lon)*slope_b2+intercept_b2
        bug3=sigma_fapar(*,lon)*slope_sigma_fapar+intercept_sigma_fapar
        bug4=sigma_b2(*,lon)*slope_sigma_b2+intercept_sigma_b2
        ;
        idxlon1=where(bug1 ge 0. and bug3 ge 0.0 and rr1 eq 0.0)
        idxlon2=where(bug2 ge 0. and bug4 ge 0.0)

        if idxlon1(0) ge 0 then begin
          toalong(0,timecount,lon)=mean(bug1(idxlon1))
          toalong(1,timecount,lon)=2.*mean(bug3(idxlon1))
          num1(timecount,lon)=N_elements(idxlon1)/7200.0*100.0
        endif
        if idxlon2(0) ge 0 then begin
        	toalong(1,d,lon)=mean(bug2(idxlon2))
        	toalong(3,d,lon)=2.*mean(bug4(idxlon2))
        	num2(d,lon)=N_elements(idxlon2)/7200.0*100.0
        endif

      endfor
      print, '** done **--> '+res_ldtr

    endif

  endfor

  fullFileName=getHowMollerSavFile(outDir, parameter, year, month, TC_TYPE, noaanumber, sourceParameter)
  print, 'sav file -->', fullFileName
  save, toalong, num1, num2, $
    filename=fullFileName

  ;print, 'Max', m
  ;
  ;
  min1=0.0
  min2=0.00
  ;max1=nday*8
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
  FAPARCOLOR
  latitude = (INDGEN(3600)+1) * 1.0

  titlefig=['FAPAR','2 x Sigma','Band 2','2 x Sigma Band 2'] ;[ Band 1', 'Band 2', '2 x Sigma Band 1', '2 Sigma Band 2']
  namefig=['FAPAR', 'SigmaFAPAR','Band_2', 'SigmaBand2'] ; , 'Sigma_Band_1', 'Sigma_Band_2']

  for i=0, n_elements(parListToWrite)-1 do begin

    set_plot,'ps'

    fname=parameter+namefig(i)+'_Global_lat_daily_avhrr_'+TC_TYPE+'_'+month+year+noaanumber+'.eps'
    SUB=titlefig(i)+' AVHRR '+' NOAA '+noaanumber
    DEVICE,FILENAME=fname,XSIZE=19.625000,YSIZE=20.000000, bits_per_pixel=8,/portrait, $
      /color,ENCAPSULATED=1

    lev=[0.01,0.02,0.05,0.07, 0.1,0.15,0.2,0.25,0.30,0.40,0.50] ;,0.60,0.70,0.80,0.90,0.95, 1.00]
    xnam=[' ','.01',' ','.02',' ','.05',' ','.07',' ','0.10',' ','.15',' ','.20',' ',$
      '0.25', ' ', '.30',' ','.40',' ','.50',' '] ;,' ','.60',' ','.70',' ','.80',' ','.90',' ','.95',' ','1.0',' ']


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
      c_color=lev(*)*500.0, $
      /fill,xstyle=1,ystyle=1,ytitle='Latitude', col=255, $
      subtitle=SUB,$
      charsize=1.,charthick=1., title='Longitude Average '+month+' '+year, $
      BACKGROUND = 16777215, ymargin=[10,8], xrange =[0,30], yr=[0,3600], $
      ;xticks=11, xtickname=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
      xticks=xticks_c, xtickname=xtickname_c,$
      yticks=4, ytickname=['-90','-45','0','45','90'] ;,'Jun','Jul','Aug','Sep','Oct','Nov','Dec']

    cols=lev(*)*500.0 ; les valeurs pour chaque couleur
    ncon=n_elements(cols)

    contour,rotate([1,1]#(indgen(ncon)*3),3),indgen(ncon)*3,[0,1],/cell_fill,$
      /closed,levels=indgen(ncon),c_colors=cols,/noerase,$
      xrange=[0,ncon],ystyle=4, pos=[0.15,0.05,0.8,0.10],col=255, $
      xticks=ncon*2, xtickname=xnam


    device,/close
  endfor
  print, '**** done ****'
  ;
  ;
  ;
  ;


end