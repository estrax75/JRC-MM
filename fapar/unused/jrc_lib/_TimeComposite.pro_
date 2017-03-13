pro call_read_all, sensor, platform, year, month, dir_in, ff_time, val_time, filename
;
;
; read_data, ff(id_file),'FAPAR', fpar_avhrr, slope_fapara, offset_fapara, fillvala, /info
; 
;

faparTCDSInfo=getStandardFaparTCDataSetInfo()
attribut=faparTCDSInfo.bandNames
;
val_day= {ExpId_t, day: 1, $
	  fapar: fltarr(7200,3600), $
	  sigma: fltarr(7200,3600), $
	  red: fltarr(7200,3600), $
	  sigma_red:fltarr(7200,3600), $ 
	  nir: fltarr(7200,3600), $
  	  sigma_nir: fltarr(7200,3600), $ 
          flag: bytarr(7200,3600), $
	  toc_red: fltarr(7200,3600), $
	  toc_nir: fltarr(7200,3600), $
	  qa: bytarr(7200,3600)}
;
val_time=replicate(val_day, N_elements(ff_time))
; 
file_name_struc=TAG_NAMES(val_day)
;
for t=0, N_elements(ff_time)-1 do begin 
 		extr=strsplit(ff_time(t),'/',/extract)
		print, 'Read daily file:', extr(N_elements(extr)-1)
		doy=strmid(ff_time(t), strlen(dir_in)+strlen(sensor+'_'+platform+'_'+year+month),2)
		val_time(t).day=doy
		for a=0, N_elements(attribut)-1 do begin
             	;
read_data, dir_in, extr(N_elements(extr)-1), attribut(a), data, slope_data, offset_data, fill ;, /info
;stop
		if a eq 0 then val_time(t).fapar=data
		if a eq 1 then val_time(t).sigma=data
		if a eq 2 then val_time(t).red=data
		if a eq 3 then val_time(t).sigma_red=data
		if a eq 4 then val_time(t).nir=data
		if a eq 5 then val_time(t).sigma_nir=data
		if a eq 6 then val_time(t).flag=data
		if a eq 7 then val_time(t).toc_red=data
		if a eq 8 then val_time(t).toc_nir=data
		if a eq 9 then val_time(t).qa=data
if a eq 2 then begin
dayData=readFapar(filename)
faparcolor
;
img=data
idx=where(data lt 0.0)
img(idx)=0.0
window,0, xsize=720*2, ysize=360*2, title='Red daily'
tv, reverse(congrid(img*250, 720*2, 360*2), 2)
stop
endif
;if a eq 6 then begin
;loadct,12
;
;window,1, xsize=720*2, ysize=360*2, title='FLAG daily'
;img=data-data
;idx=where(data eq 0)
;img(idx)=100
;tvscl, reverse(congrid(data, 720*2, 360*2), 2)
;endif

	endfor

endfor
;
;
;  
;
end


PRO MakeIt, sensor, platform, year, ten=TEN, mois=MOIS
;
;
;
;
;
month=['01'] ;,'02','03','04','05','06','07','08','09','10','11','12']
;
dir_in='/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/'+sensor+'/FAPAR/'
dirout='/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/'+sensor+'/FAPAR_TC/'
for m=0, N_elements(month)-1 do begin 
;
; look for files within the month
;
	IF KEYWORD_SET(ten) then begin
	first=['01','11','21']
	last=['10','20','31']
	endif
	IF KEYWORD_SET(mois) then begin
	first=['01']
	last=['31']
	endif
;
	
	file_name=sensor+'_'+platform+'_'+year+month(m)+'*'
	end_file='L2_MUL_000009_900S900N1800W1800E_PLC_0005D_PRO.HDF'
	expidfile = dir_in + file_name+end_file

	ff = FILE_SEARCH(expidfile, COUNT=cnt)
	doys=intarr(cnt)
;
	for d=0,N_elements(ff)-1  do begin 
		doys_filename=strmid(ff(d), strlen(dir_in)+strlen(sensor+'_'+platform+'_'+year+month(m)),2)
		doys(d)=float(doys_filename)
	endfor
	for t=0, N_elements(first)-1 do begin
		id_file=where(doys ge float(first(t)) and doys le last(t))
		print, ff(id_file)
	;
	; read files requested in the period 
	;
	call_read_all, sensor, platform, year, month(m), dir_in, ff(id_file), val_time 
	;
	; call time composite program
	;stop
	;endfor
	;
	call_composite, val_time, val_comp
	;
	new_file=sensor+'_'+platform+'_'+year+month(m)+first(t)+'000000_'+$
					 year+month(m)+last(t)+'000000_'+$
					 'L3_MUL_000009_900S900N1800W1800E_PLC_0005D_PRO.HDF'
	; save the results
	;
	sdid_outfile1 = HDF_SD_START(dirout+new_file, /CREATE)
    	a=size(val_comp.fapar)
    	sdid_fpar = HDF_SD_CREATE(sdid_outfile1, 'FAPAR', [a(1),a(2)], /float)
    	sdid_uncert = HDF_SD_CREATE(sdid_outfile1, 'Sigma FAPAR', [a(1),a(2)], /float)	
    	sdid_delta = HDF_SD_CREATE(sdid_outfile1, 'Temporal Deviation FAPAR', [a(1),a(2)], /float)	
    	sdid_red = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED RED', [a(1),a(2)], /float)
    	sdid_uncert_red = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED RED', [a(1),a(2)], /float)
    	sdid_delta_red = HDF_SD_CREATE(sdid_outfile1, 'Temporal Deviation Red', [a(1),a(2)], /float)	
    	sdid_nir = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED NIR', [a(1),a(2)], /float)
    	sdid_uncert_nir = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED NIR', [a(1),a(2)], /float)
    	sdid_delta_nir = HDF_SD_CREATE(sdid_outfile1, 'Temporal Deviation NIR', [a(1),a(2)], /float)	
    	sdid_day = HDF_SD_CREATE(sdid_outfile1, 'DOY', [a(1),a(2)], /byte)
    	sdid_nday = HDF_SD_CREATE(sdid_outfile1, 'Number of Day', [a(1),a(2)], /byte)
    	sdid_flag = HDF_SD_CREATE(sdid_outfile1, 'FLAG', [a(1),a(2)], /byte)
    	sdid_redtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC RED', [a(1),a(2)], /float)
    	sdid_nirtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC NIR', [a(1),a(2)], /float)
    	sdid_qa =  HDF_SD_CREATE(sdid_outfile1, 'JRC QA', [a(1),a(2)], /byte)
;
; data_tc= {Composite, day: bytarr(7200,3600), $
;	  nday: bytarr(7200,3600), $
;	  fapar: fltarr(7200,3600), $
;	  dev_temp: fltarr(7200,3600), $
;	  sigma: fltarr(7200,3600), $
;	  red: fltarr(7200,3600), $
;	  dev_red_temp: fltarr(7200,3600), $
;	  sigma_red:fltarr(7200,3600), $ 
;	  nir: fltarr(7200,3600), $
;	  dev_nir_temp: fltarr(7200,3600), $ 
; 	  sigma_nir: fltarr(7200,3600), $ 
;         flag: bytarr(7200,3600), $
;	  toc_red: fltarr(7200,3600), $
;	  toc_nir: fltarr(7200,3600), $
;	  qa: bytarr(7200,3600)}
;
    	HDF_SD_ADDDATA, sdid_fpar, val_comp.fapar
    	HDF_SD_ADDDATA, sdid_uncert, val_comp.sigma
    	HDF_SD_ADDDATA, sdid_delta, val_comp.dev_temp
    	HDF_SD_ADDDATA, sdid_red, val_comp.red
    	HDF_SD_ADDDATA, sdid_uncert_red, val_comp.sigma_red
	HDF_SD_ADDDATA, sdid_delta_red, val_comp.dev_red_temp
    	HDF_SD_ADDDATA, sdid_nir, val_comp.nir
    	HDF_SD_ADDDATA, sdid_uncert_nir, val_comp.sigma_nir 
	HDF_SD_ADDDATA, sdid_delta_nir, val_comp.dev_nir_temp
	HDF_SD_ADDDATA, sdid_day, val_comp.day
 	HDF_SD_ADDDATA, sdid_nday,val_comp.nday
    	HDF_SD_ADDDATA, sdid_flag, val_comp.flag
    	HDF_SD_ADDDATA, sdid_redtoc, val_comp.toc_red
    	HDF_SD_ADDDATA, sdid_nirtoc, val_comp.toc_nir
   	HDF_SD_ADDDATA, sdid_qa, val_comp.qa
    	HDF_SD_END, sdid_outfile1
	;	
	endfor
;	
endfor
;
end
