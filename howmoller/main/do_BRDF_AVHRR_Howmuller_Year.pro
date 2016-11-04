;doHowmullerYear, 2003, 16, 'fapar', TC_TYPE='DAILY'
;doHowmullerYear, 2003, 16, 'fapar', TC_TYPE='10D'
;do_BRF_AVHRR_Howmuller_Year, 2003, 16, 'band1_2', TC_TYPE=TC_TYPE
PRO do_BRF_AVHRR_Howmuller_Year, year, sourceParameter, parameter, parSelector, inputDir, outputDir, noaanumber, $
  eps=EPS, TA_TYPE=TA_TYPE, TC_TYPE=TC_TYPE, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX


  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons
  ;month=indgen(12)+1

  inputDir=ST_fileSystem->adjustDirSep(inputDir, /ADD)
  outputDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)

  if n_elements(noaanumber) eq 0 then begin
    noaanumber=getAVHRRNOAANumber(year, undef)
    if n_elements(noaanumber) gt 1 then noaanumber=noaanumber[MISSIONOVERLAPINDEX]
    noaanumber=noaanumber[0]
  endif else begin
    noaanumber=fix(noaanumber)
  endelse
  print, year, noaanumber

  tInfo=getTimeDerivedInfo(year, month, TA_TYPE, TC_TYPE, xticks_c=xticks_c, xtickname_c=xtickname_c)

  ;  if n_elements(TC_TYPE) eq 0 then TC_TYPE='DAILY'
  ;  ; DAILY as default
  ;  if TC_TYPE eq 'DAILY' then begin
  ;    dayofMonth=utils->calcDayOfMonth([year,month,1,0])
  ;    first=indgen(dayofMonth)+1
  ;    last=first
  ;    diravhrr='/space3/storage/products/results/FAPAR/DAILY/type1/'
  ;  endif
  ;  if TC_TYPE eq '5D' then begin
  ;    diravhrr='/space3/storage/products/results/FAPAR/TC/type1/5DAYS/'
  ;    first=[01,06,11,16,21,26]
  ;    last=[5,10,15,20,25,utils->calcDayOfMonth([year,month,1,0])]
  ;  endif
  ;  if TC_TYPE eq '10D' then begin
  ;    diravhrr='/space3/storage/products/results/FAPAR/TC/type1/10DAYS/'
  ;    first=[01,11,21]
  ;    last=[10,20,utils->calcDayOfMonth([year,month,1,0])]
  ;  endif
  ;  if TC_TYPE eq '16D' then begin
  ;    diravhrr='/space3/storage/products/results/FAPAR/TC/type1/16DAYS/'
  ;    first=[01,17]
  ;    last=[16,utils->calcDayOfMonth([year,month,1,0])]
  ;  endif
  ;  if TC_TYPE eq 'MONTHLY' then begin
  ;    diravhrr='/space3/storage/products/results/FAPAR/TC/type1/MONTHLY/'
  ;    first=[01]
  ;    last=[utils->calcDayOfMonth([year,month,1,0])]
  ;  endif

  first=tInfo.first
  last=tInfo.last
  ;level=tInfo.level
  ;extraPath=tInfo.extraPath

  timeLapse=n_elements(first)
  destDim=ST_utils->calcDays(year, first)
  ;n_elements(first)
  if parameter eq 'BRFs' then maxk=3 else maxk=1
  ;if parSelector eq 'band1_2' then maxk=3 else maxk=1
  for k=0, maxk do begin

    tot=dblarr(destDim,3600)
    tot(*,*)=0.0
    ;
    i=0
    ; for all the months in a year
    for m=0, timeLapse-1 do begin

      ;res= file_search('/local0/gobrona/save/Global/','Save'+parameter+'_'+year+month(m)+'_avhrr_BRFs'+noaanumber+'.sav')
      ;fileName='Save'+parameter+'_'+year+month(m)+'_'+TC_TYPE+'_avhrr_BRFs'+noaanumber+'.sav'
      ;res= file_search('/space3/storage/products/results/FAPAR/analysis/',fileName, count=count)
      month=first[m]
      savFileName=getHowMollerSavFile(parameter, year, month, 'DAILY', noaanumber, sourceParameter)
      savFileName=inputDir+savFileName

      fInfo=file_info(savFileName)

      if fInfo.size ne 0 then begin
        print, 'restore:', savFileName
        restore, savFileName
        ss=size(toalong)
        tot[i:i+ss(2)-1, *] = toalong[k, *,*]
        print, '*****'
        monthDays=ss(2)
      endif else begin
        print, 'skip:', savFileName 
        monthDays=ST_utils->calcDays(year, month)
      endelse
      print, monthDays 
      i=i+monthDays
    endfor
    print, i
    ;
    min1=0.0
    min2=0.00
    max1=timeLapse*8
    max2=180.0
    nx=360
    ;nyear=12
    xB = 10
    yB = 35
    xs = 55
    ys = 45
    yL=nx*2.0
    xL=timeLapse
    ;
    xSize1 = double(xs + xL + xB)
    ySize1 = double(ys + yL + yB)

    set_plot,'x'
    resX=!d.x_px_cm
    resY=!d.y_px_cm
    ;print, resX, resY
    resX=43.75
    resY=43.75
    ;stop
    pos=[xs, ys, xs+xL, ys+yL]/[xSize1, ySize1, xSize1, ySize1]
    latitude = (INDGEN(3600)+1) * 1.0

    if parSelector eq 'BRF' then begin
      PPMSA_ALBEDOCOLOR
      titlefig=['Band 1', 'Band 2', '2 x Sigma Band 1', '2 Sigma Band 2']
      namefig=['Band_1', 'Band_2', 'Sigma_Band_1', 'Sigma_Band_2']
      pencolor=1
    endif else begin
      FAPARCOLOR
      titlefig=['FAPAR','2 x Sigma']
      namefig=['FAPAR', 'SigmaFAPAR']
      pencolor=0
    endelse

    set_plot,'ps'

    fname=getHowMollerSavFile(namefig[k], year, undefined, 'newYEAR', noaanumber, sourceParameter, parSelector)
    ;fname=parameter+namefig[k]+'_Global_lat_daily_brftoc_avhrr_'+noaanumber+year+'_'+TC_TYPE+'_all.eps'
    ;baseFile=ST_fileSystem->getFileNameInfo(fname, filePath=filePath, extension=extension)
    fname=ST_fileSystem->removeFileExtension(fname)
    fname=ST_fileSystem->addFileExtension(fname, 'eps')

    SUB=parSelector+' '+titlefig[k]+' AVHRR TOC '+' NOAA '+ string(noaanumber, format='(I02)')
    fullfName=outputDir+fname
    DEVICE,FILENAME=fullfName,XSIZE=19.625000,YSIZE=20.000000, bits_per_pixel=8,/portrait, $
      /color,ENCAPSULATED=1

    if parSelector eq 'BRF' then lev=[0.01,0.02,0.05,0.07, 0.1,0.15,0.2,0.25,0.30,0.40,0.50,0.60,0.70,0.80,0.90,0.95, 1.00] else $
      lev=[0.01,0.02,0.05,0.07, 0.1,0.15,0.2,0.25,0.30,0.40,0.50]
    if parSelector eq 'BRF' then xnam=[' ','.01',' ','.02',' ','.05',' ','.07',' ','0.10',' ','.15',' ','.20',' ',$
      '0.25', ' ', '.30',' ','.40',' ','.50',' ','.60',' ','.70',' ','.80',' ','.90',' ','.95',' ','1.0',' '] else $
      xnam=[' ','.01',' ','.02',' ','.05',' ','.07',' ','0.10',' ','.15',' ','.20',' ',$
      '0.25', ' ', '.30',' ','.40',' ','.50',' ']


    ;nummean=fltarr(3600,2)
    ;
    ;for l=0, 3600-1 do begin
    ;idx=where(num(*,l) gt 0.)
    ;	res=moment(num(idx,l), sdev=sdev)
    ;	nummean(l,0)=res(0)
    ;	nummean(l,1)=sdev
    ;endfor
    if parSelector eq 'BRF' then cc_color=[2,20,30,40,55,60,70,80,100,110,130,150,170,190,210,235,256] else cc_color=lev*500.0
    time=findgen(i)
    Contour, tot(0:i-1,*),time,latitude,levels = lev, c_charsize=1.5, $
      c_color=cc_color,$
      /fill,xstyle=1,ystyle=1,ytitle='Latitude', $
      subtitle=SUB,$
      charsize=1.,charthick=1., title='Longitude Average '+string(year, format='(I4)') , $
      BACKGROUND = 16777215, color=pencolor,ymargin=[10,8], xrange =[0,i], yr=[0,3600], $
      xticks=12, xtickname=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec',' '],$
      ;xticks=4, xtickname=['01','05','10','15','31'],$
      yticks=4, ytickname=['-90','-45','0','45','90'] ;,'Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    cols=cc_color ; les valeurs pour chaque couleur
    ncon=n_elements(cols)

    contour,rotate([1,1]#(indgen(ncon)*3),3),indgen(ncon)*3,[0,1],/cell_fill,$
      /closed,levels=indgen(ncon),c_colors=cols,/noerase,$
      xrange=[0,ncon],ystyle=4, pos=[0.15,0.05,0.8,0.10],$
      xticks=ncon*2, xtickname=xnam, color=pencolor

    device,/close
  endfor
  set_plot, 'X'
  print, '**** done ****'

END

