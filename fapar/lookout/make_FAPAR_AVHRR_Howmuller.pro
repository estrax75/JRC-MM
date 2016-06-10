pro makeit_, month, parameter, year, NOAANUMBER, eps=EPS
;
; QA4EVC daily BRFs
; 
NOAANUMBER=string(NOAANUMBER, format='(I02)')
year=string(year, format='(I04)')
month=string(month, format='(I02)')
;day=string(day, format='(I02)')

diravhrr='/space3/storage/products/results/FAPAR/type1/' 
;
file_ldtr_all='AVHRR_NOA'+noaanumber+'_'+year+month+'??000000_'+year+month+'??000000_L2_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO.NC'
;
;
res_ldtr_all=file_search(diravhrr,file_ldtr_all)
nday=N_elements(res_ldtr_all)
print, nday
;
;
;
toalong=dblarr(2,nday,3600)
toalong(*,*,*)=-10.0
num1=fltarr(nday,3600)
num2=fltarr(nday,3600)
m=0

for d=0, nday-1 DO BEGIN
		;
		; find AVHRR file
		;
		file_ldtr= res_ldtr_all(d)
		print, file_ldtr
		if  strlen(file_ldtr) gt 1 then begin	
			dims=[0,1]
			ncdfread,  file_ldtr,'FAPAR', avhrr_b1, slope_b1, intercept_b1, dims
			;window, 0, xsize=720, ysize=360
			;tv, congrid(bytscl(avhrr_b1*slope_b1+intercept_b1, min=0., max=1.5), 720,360)
			;ncdfread,  file_ldtr,'BRF_BAND_2', avhrr_b2, slope_b2, intercept_b2, dims
			;window, 1, xsize=720, ysize=360
			;tv, congrid(bytscl(avhrr_b2*slope_b2+intercept_b2, min=0., max=1.5), 720,360)
		
			ncdfread,  file_ldtr,'Sigma_FAPAR',sigma_b1, slope_sb1, intercept_sb1, dims
			;window, 2, xsize=720, ysize=360
			;tv, congrid(bytscl(sigma_b1*slope_sb1+intercept_sb1, min=0., max=1.5), 720,360)
		
			;ncdfread,  file_ldtr,'SIGMA_BRF_BAND_2',sigma_b2, slope_sb2, intercept_sb2, dims
			;window, 3, xsize=720, ysize=360
			;tv, congrid(bytscl(sigma_b2*slope_sb2+intercept_sb2, min=0., max=1.5), 720,360)
		
			;ncdfread,  diravhrr+file_ldtr,'TS', avhrr_sun, slope_sun, intercept_sun, dims
			;ncdfread,  diravhrr+file_ldtr,'Sigma_FAPAR', sigma_avhrr_fapar, slope_fapar, intercept_fapar, dims
			ncdfread,  file_ldtr,'LDTR_FLAG', cloud, slope_f, intercept_f, dims
			
			rr1=cgi_map_bitwise_flag(cloud,1)
			mm=1.0
		
			FAPARCOLOR
			;
			;
			;stop
		
			for lon=0, 3600-1 DO BEGIN
				bug1=avhrr_b1(*,lon)*slope_b1+intercept_b1
				;bug2=avhrr_b2(*,lon)*slope_b2+intercept_b2
				bug3=sigma_b1(*,lon)*slope_sb1+intercept_sb1
				;bug4=sigma_b2(*,lon)*slope_sb2+intercept_sb2
				;
				idxlon1=where(bug1 ge 0. and bug3 ge 0.0 and rr1 eq 0.0) 
				;idxlon2=where(bug2 ge 0. and bug4 ge 0.0)		
			
				if idxlon1(0) ge 0 then begin
					toalong(0,d,lon)=mean(bug1(idxlon1))
					toalong(1,d,lon)=2.*mean(bug3(idxlon1))
					num1(d,lon)=N_elements(idxlon1)/7200.0*100.0
				endif
				;if idxlon2(0) ge 0 then begin
				;	toalong(1,d,lon)=mean(bug2(idxlon2))
				;	toalong(3,d,lon)=2.*mean(bug4(idxlon2))
				;	num2(d,lon)=N_elements(idxlon2)/7200.0*100.0
				;endif

			endfor 
			m=m+1
		endif

endfor

;save, toalong, num1, num2, $
;filename='/local0/gobrona/save/Global/Save'+parameter+'_'+year+month+'_avhrr_BRFs'+noaanumber+'.sav'
;print, '/local0/gobrona/save/Global/Save'+parameter+'_'+year+month+'_avhrr_BRFs'+noaanumber+'.sav'
save, toalong, num1, num2, $
  filename='/space3/storage/products/results/temp/Save'+parameter+'_'+year+month+'_avhrr_BRFs'+noaanumber+'.sav'
print, '/space3/storage/products/results/temp/Save'+parameter+'_'+year+month+'_avhrr_BRFs'+noaanumber+'.sav'
	

print, 'Max', m
;
;
min1=0.0
min2=0.00
max1=nday*8
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

titlefig=['FAPAR','2 x Sigma'] ;[ Band 1', 'Band 2', '2 x Sigma Band 1', '2 Sigma Band 2']
namefig=['FAPAR', 'SigmaFAPAR'] ; , 'Sigma_Band_1', 'Sigma_Band_2']

for i=0, 1 do begin

set_plot,'ps'

fname=parameter+namefig(i)+'_Global_lat_daily_avhrr_'+month+year+noaanumber+'.eps'
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

time=findgen(nday)
Contour, toalong(i,*,*),time,latitude,levels = lev, c_charsize=1.5, $
c_color=lev(*)*500.0, $
/fill,xstyle=1,ystyle=1,ytitle='Latitude', col=255, $
subtitle=SUB,$
charsize=1.,charthick=1., title='Longitude Average '+month+' '+year, $
BACKGROUND = 16777215, ymargin=[10,8], xrange =[0,30], yr=[0,3600], $
;xticks=11, xtickname=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
xticks=4, xtickname=['01','05','10','15',strcompress(string(nday),/remove_all)],$
yticks=4, ytickname=['-90','-45','0','45','90'] ;,'Jun','Jul','Aug','Sep','Oct','Nov','Dec']

cols=lev(*)*500.0 ; les valeurs pour chaque couleur
ncon=n_elements(cols)

contour,rotate([1,1]#(indgen(ncon)*3),3),indgen(ncon)*3,[0,1],/cell_fill,$
/closed,levels=indgen(ncon),c_colors=cols,/noerase,$
xrange=[0,ncon],ystyle=4, pos=[0.15,0.05,0.8,0.10],col=255, $
xticks=ncon*2, xtickname=xnam


device,/close
endfor
;
;
;
;


end

