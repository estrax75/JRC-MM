;do_faparComp_Howmuller, 1, 1999, 14, 'FAPAR_DIFF', 'FAPAR_DIFF', 'HDF', '/space3/storage/products/AVHRR_LDTR', '/space3/storage/products/results/FAPAR/COMPARISONS', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY'
;do_faparComp_Howmuller, 7, 2003, 16, 'FAPAR_DIFF', 'FAPAR_DIFF', 'NC', '/space3/storage/products/results/FAPAR/COMPARISONS', '/space3/storage/products/results/FAPAR/COMPARISONS', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY'
;do_faparComp_Howmuller, 7, 2003, 16, 'FAPAR_DIFF', 'FAPAR_DIFF', 'NC', '/space3/storage/products/results/FAPAR/COMPARISONS', '/space3/storage/products/results/FAPAR/COMPARISONS', eps=EPS, TA_TYPE='MEAN', TC_TYPE='MONTHLY'
pro do_faparComp_Howmuller, month, year, NOAANUMBER, sourceParameter, parameter, $
  inputFormatType, inputBaseDir, outputBaseDir, $
  eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, nodirbuild=nodirbuild
  ;
  ; QA4EVC daily BRFs
  ;
  if ~obj_valid(utils) then utils=obj_new('Utility')
  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)

  NOAANUMBER=string(NOAANUMBER, format='(I02)')

  tInfo=getTimeDerivedInfo(year, month, TA_TYPE, TC_TYPE, xticks_c=xticks_c, xtickname_c=xtickname_c)

  year=string(year, format='(I04)')
  month=string(month, format='(I02)')

  first=tInfo.first
  last=tInfo.last
  level=tInfo.level
  extraPath=tInfo.extraPath

  if ~(KEYWORD_SET(nodirbuild)) then inputDir=inputBaseDir+tInfo.extraPath else inputDir=inputBaseDir
  outDir=outputBaseDir+path_sep()+tInfo.extraPath+path_sep()+'HM'+path_sep()

  parListToRead=['Difference','fapar_'+TC_TYPE+'SWF','fapar_'+TC_TYPE+'AVHRR']
  parListToWrite=['Difference','fapar_'+TC_TYPE+'SWF','fapar_'+TC_TYPE+'AVHRR']

  timeLapse=n_elements(first)
  toalong=dblarr(n_elements(parListToWrite),timeLapse,3600)
  toalong(*,*,*)=-10.0
  num1=fltarr(timeLapse,3600)
  num2=fltarr(timeLapse,3600)
  num3=fltarr(timeLapse,3600)

  for timecount=0, timelapse-1 DO BEGIN
    startDay=string(first[timecount], format='(I02)')
    endDay=string(last[timecount], format='(I02)')
    yearDay=utils->calcDayOfYear([long(year),long(month),long(startDay),0])+1
    print, 'working on', year, month, startDay, '...'
    thisfile_ldtr=buildDiffFAPARFileName_TC_('AVHRR_Vs_SWF', resolutions, year, month, first[timecount], missionName, missionCode, mainVarName, level, startDay=first[timecount], endDay=last[timecount])
    thisfile_ldtr=fsObj->addFileExtension(thisfile_ldtr, inputFormatType)
    ; subscribe with [0] 'cause reader needs only single element scalar parameter
    res_ldtr=(file_search(inputBaseDir,thisfile_ldtr, count=count, /FULLY_QUALIFY_PATH))[0]
    if count eq 0 then begin
      testDir=inputBaseDir+path_sep()+tInfo.extraPath
      fullFileN=testDir+path_sep()+thisfile_ldtr
      res_ldtr=(file_search(testDir,thisfile_ldtr, count=count, /FULLY_QUALIFY_PATH))[0]
    endif

    if  count eq 1 then begin
      dims=[0,1]
      print, '** reading --> '+res_ldtr

      if inputFormatType eq 'NC' then begin
        ncdfread,  res_ldtr,parListToRead[0], diff, slope_diff, intercept_diff, dims, diff_fillvalue
        ncdfread,  res_ldtr,parListToRead[1], fapar1, slope_fapar1, intercept_fapar1, dims, fapar1_fillvalue
        ncdfread,  res_ldtr,parListToRead[2], fapar2, slope_fapar2, intercept_fapar2, dims, fapar2_fillvalue
      endif
      if inputFormatType eq 'HDF' then begin
        operatorObj->readHdfFullInfoData, res_ldtr, parListToRead[0], diff, slope_diff, intercept_diff, diff_fillvalue, ERROR=ERROR
        operatorObj->readHdfFullInfoData, res_ldtr, parListToRead[1], fapar1, slope_fapar1, intercept_fapar1, fapar1_fillvalue, ERROR=ERROR
        operatorObj->readHdfFullInfoData, res_ldtr, parListToRead[2], fapar2, slope_fapar2, intercept_fapar2, fapar2_fillvalue, ERROR=ERROR
      endif
      ;rr1=cgi_map_bitwise_flag(cloud,1)
      mm=1.0
      ;
      ;
      ;stop

      for lon=0, 3600-1 DO BEGIN
        bug1=diff(*,lon)*slope_diff[0]+intercept_diff[0]
        bug2=fapar1(*,lon)*slope_fapar1[0]+intercept_fapar1[0]
        bug3=fapar2(*,lon)*slope_fapar2[0]+intercept_fapar2[0]
        ;
        idxlon1=where(bug1 ge -2. and bug1 le 2. and diff(*,lon) ne diff_fillvalue)
        idxlon2=where(bug2 ge 0. and fapar1(*,lon) ne fapar1_fillvalue ne fapar1_fillvalue)
        idxlon3=where(bug3 ge 0. and fapar2(*,lon) ne fapar2_fillvalue ne fapar2_fillvalue)

        if idxlon1(0) ge 0 then begin
          toalong(0,timecount,lon)=mean(bug1(idxlon1))
          ;sigma...
          ;toalong(2,timecount,lon)=2.*mean(bug3(idxlon1))
          num1(timecount,lon)=N_elements(idxlon1)/7200.0*100.0
        endif
        if idxlon2(0) ge 0 then begin
          toalong(1,timecount,lon)=mean(bug2(idxlon2))
          ;sigma...
          ;toalong(3,timecount,lon)=2.*mean(bug4(idxlon2))
          num2(timecount,lon)=N_elements(idxlon2)/7200.0*100.0
        endif
        if idxlon3(0) ge 0 then begin
          toalong(2,timecount,lon)=mean(bug3(idxlon3))
          ;sigma...
          ;toalong(2,timecount,lon)=2.*mean(bug3(idxlon1))
          num3(timecount,lon)=N_elements(idxlon3)/7200.0*100.0
        endif
      endfor
      print, '** done **--> '+res_ldtr
    endif

  endfor

  fullFileName=getHowMollerSavFile(outDir, parameter, year, month, TC_TYPE, noaanumber, sourceParameter)
  print, 'sav file -->', fullFileName
  save, toalong, num1, num2, num3, $
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

  if !VERSION.OS_FAMILY eq 'unix' then set_plot, 'x' else set_plot, 'win'
  resX=43.75
  resY=43.75
  ;stop
  pos=[xs, ys, xs+xL, ys+yL]/[xSize1, ySize1, xSize1, ySize1]
  loadct, 33;PPMSA_ALBEDOCOLOR
  latitude = (INDGEN(3600)+1) * 1.0

  ;titlefig=['Band 1', 'Band 2', '2 x Sigma Band 1', '2 Sigma Band 2']
  ;namefig=['Band_1', 'Band_2', 'Sigma_Band_1', 'Sigma_Band_2']
  titlefig=['Difference ','SWF', 'AVHRR']
  namefig=['Difference', 'SWF', 'AVHRR']

  if timeLapse eq 1 then begin
    ;just for testing...
    ;fake second dimension
    contourData=fltarr(n_elements(parListToWrite),2,3600)
    contourData[*,0,*]=toalong(*,0,*)
    contourData[*,1,*]=toalong(*,0,*)
  endif else begin
    contourData=toalong(i,*,*)
  endelse

  for i=0, n_elements(parListToWrite)-1 do begin

    if i eq 0 then begin
      ;17 / 35
      ;lev=[0.01,0.02,0.05,0.07, 0.1,0.15,0.2,0.25,0.30,0.40,0.50,0.60,0.70,0.80,0.90,0.95, 1.00]
      lev=[0.02,0.05, 0.1,0.2,0.50,0.70,0.90, 1.00]/2
      xnamP='.'+string(lev*100, "(I02)")
      levNeg=reverse(lev)
      xnamN='-.'+string(levNeg*100, "(I02)")
      levNeg=-levNeg
      lev=[levNeg, 0., lev]
      xNam=[xnamN, '0.0', xnamP]
      xFullNam=strarr(n_elements(lev)*2+1)
      xFullNam[*]=' '
      for j=1, n_elements(xFullNam)-2, 2 do xFullNam[j]=xnam[j/2]
      xnam=xFullNam
      loadct,33
      print, 'diff lev n.', n_elements(lev)
      print, 'diff xnam n.', n_elements(xnam)
    endif else begin
      lev=[0.01,0.02,0.05,0.07, 0.1,0.15,0.2,0.25,0.30,0.40,0.50,0.60,0.70,0.80,0.90,0.95, 1.00]
      xnam=[' ','.01',' ','.02',' ','.05',' ','.07',' ','0.10',' ','.15',' ','.20',' ',$
        '0.25', ' ', '.30',' ','.40',' ','.50',' ','.60',' ','.70',' ','.80',' ','.90',' ','.95',' ','1.0',' ']
      print, 'abs fapar lev n.', n_elements(lev)
      print, 'abs diff xnam n.', n_elements(xnam)
      FAPARCOLOR
    endelse

    set_plot,'ps'

    fname=parameter+namefig(i)+'_Global_lat_daily_fapar_'+month+year+noaanumber+'.eps'
    SUB=parameter+' '+titlefig(i)+' FAPAR '
    DEVICE,FILENAME=outDir+fname,XSIZE=19.625000,YSIZE=20.000000, bits_per_pixel=8,/portrait, $
      /color,ENCAPSULATED=1

    ;    xnam=[' ','.01',' ','.02',' ','.05',' ','.07',' ','0.10',' ','.15',' ','.20',' ',$
    ;      '0.25', ' ', '.30',' ','.40',' ','.50',' ','.60',' ','.70',' ','.80',' ','.90',' ','.95',' ','1.0',' ']


    ;nummean=fltarr(3600,2)
    ;
    ;for l=0, 3600-1 do begin
    ;idx=where(num(*,l) gt 0.)
    ;	res=moment(num(idx,l), sdev=sdev)
    ;	nummean(l,0)=res(0)
    ;	nummean(l,1)=sdev
    ;endfor

    time=findgen(timeLapse)
    Contour, contourData(i,*,*),time,latitude,levels = lev, c_charsize=1.5, $
      c_color=[2,20,30,40,55,60,70,80,100,110,130,150,170,190,210,235,256], $
      /fill,xstyle=1,ystyle=1,ytitle='Latitude', $
      subtitle=SUB,$
      charsize=1.,charthick=1., title='Longitude Average '+month+' '+year, $
      BACKGROUND = 16777215, color=255,ymargin=[10,8], xrange =[0,30], yr=[0,3600], $
      ;xticks=11, xtickname=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
      xticks=4, xtickname=['01','05','10','15',strcompress(string(timeLapse),/remove_all)],$
      yticks=4, ytickname=['-90','-45','0','45','90'] ;,'Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    cols=[2,20,30,40,55,60,70,80,100,110,130,150,170,190,210,235,256] ; les valeurs pour chaque couleur
    ncon=n_elements(cols)

    contour,rotate([1,1]#(indgen(ncon)*3),3),indgen(ncon)*3,[0,1],/cell_fill,$
      /closed,levels=indgen(ncon),c_colors=cols,/noerase,$
      xrange=[0,ncon],ystyle=4, pos=[0.15,0.05,0.8,0.10],$
      xticks=ncon*2, xtickname=xnam, color=255, charsize=0.8


    device,/close
  endfor
  print, '...done'
  ;
  ;
  ;
  ;


end