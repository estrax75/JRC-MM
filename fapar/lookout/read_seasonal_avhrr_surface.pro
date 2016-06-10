;FUNCTION cgi_map_bitwise_flag, statusmap, bitposition
;return, BYTE((statusmap AND (2UL^bitposition))/(2UL^bitposition))
;END
;
PRO GETDATA, sd_id, SDSNo, name, array, dims, chanid, fillvalue

	chanid=0

	sds_id = HDF_SD_SELECT(SD_ID,SDSNo)
	HDF_SD_GETDATA, SDS_ID, array

	HDF_SD_GETINFO, sds_id, LABEL=l, UNIT=u, FORMAT=f, $ 
		COORDSYS=c,NDIMS=ndims, DIMS=dims, TYPE=ty,NAME=name 
	PRINT,name
	;find the id of the band_names
	!QUIET=1
	dindex = HDF_SD_ATTRFIND(sds_id, 'band_names')	
	!QUIET=0
   IF dindex GT 0 THEN BEGIN				;if Earth View SD is selected
	HDF_SD_ATTRINFO, sds_id, dindex, DATA=chanid
	chanid=STR_SEP(chanid, ",")
	;chanid=FIX(chanid[0])			;find the first channel in the band_names
   ENDIF ELSE BEGIN
	;if cannot find band names,
	;then simply use 1,2,...
	IF ndims GT 2 THEN $
	chanid=STRING(INDGEN(dims[ndims-1])+1)
	chanid=STRCOMPRESS(chanid,/REMOVE_ALL)
   ENDELSE
	;---find the id of the fill value---
	!QUIET=1
	dindex = HDF_SD_ATTRFIND(sds_id, '_FillValue')	 
   	IF dindex GT 0 THEN $
	HDF_SD_ATTRINFO, sds_id, dindex, DATA=fillvalue
	!QUIET=0	
	;------------------------------------
RETURN
END
;
;
;
pro read_data, dir, filename, name, array, slope, offset, fillvalue, info=INFO
;
;
; Inputs:
; dir=directory of the files
; filename = name file
; name = name of the field 
;   i.e. 'FAPAR ????' 
;   
; to know ALL name just call the routine with /INFO
;
;
;print, 'Read ', name 
;dir='/home/cecchgu/MODIS_FAPAR/FAPAR_MODIS/'
;filename=dir+'MODIS_250_127_1_955_2003.hdf'
;
;dir='/net/froz/exports/vol07/cecchgu/MODIS2010/FAPAR/FAPAR_MODIS/'

;file=dir+filename ;'MODIS_250_guido_122_2010.hdf'
;print, file
;stop
;
; open the hdf file
;
print, dir+filename
sd_id=HDF_SD_START(dir+Filename,/READ)
;
; 
; when /info is on - read all names attributes and print them .... 
;  
;
;
IF KEYWORD_SET(INFO) THEN BEGIN
;Take the number of data sets, SDS, and assign
;  to the variable datasets:
HDF_SD_FILEINFO, sd_id, datasets, attributes
FOR I=0,datasets-1 DO begin 
sds_id=HDF_SD_SELECT(sd_id,I) 			
HDF_SD_GETINFO,sds_id,DIMS=dims,NDIMS=ndims, NAME=name,NATTS=natts,TYPE=ty
;
;
PRINT,FORMAT='(I0,".",3A0,'+STRING(ndims)+'(I0,:,"x"),$)', I, "Short name:  ", name , ", size: " , dims
PRINT,", type: ",ty
   FOR J=0,natts-1 DO BEGIN		
   HDF_SD_ATTRINFO,sds_id,J,NAME=name,DATA=attr_dat
		PRINT,FORMAT='(A0,":  ",5(A0))',name,attr_dat
		attr_dat=""
	name=""
  ENDFOR
;endif
;
ENDFOR

;stop
ENDIF       ;;;; INFO 
 
;stop
print,'Field is:' + name
;stop
SDSNo = HDF_SD_NAMETOINDEX(sd_id, name) ;find the SDS offset number
print, SDSNo
IF SDSNo NE -1 THEN GETDATA, sd_id, SDSNo, name, array, dims, chanid, fillvalue
sds_id = HDF_SD_SELECT(SD_ID, SDSNo)
aindex = HDF_SD_ATTRFIND(sds_id, 'scale_factor')	

;stop
IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=Slope else begin
aindex = HDF_SD_ATTRFIND(sds_id, 'slope') 
IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=Slope else begin 
aindex = HDF_SD_ATTRFIND(sds_id, 'reflectance_scales')
IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=Slope
	endelse
endelse

aindex = HDF_SD_ATTRFIND(sds_id, 'add_offset')	
IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=offset else begin
aindex = HDF_SD_ATTRFIND(sds_id, 'intercept') 
IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=offset else begin
aindex = HDF_SD_ATTRFIND(sds_id, 'reflectance_offsets')	
IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=offset	
    	endelse	
endelse

aindex = HDF_SD_ATTRFIND(sds_id, '_FillValue')	
IF aindex GE 0 THEN HDF_SD_ATTRINFO, sds_id, aindex, DATA=fillvalue

;print, slope,offset
;image=FLOAT(array)
;image[*,*]=slope*image[*,*]+offset
;stop
;window, /free, xsize = dims(0)/10, ysize = dims(1)/10
;tvscl, reverse(congrid(array/slope(0)+offset(0), dims(0)/10, dims(1)/10),2)
;stop
HDF_SD_END, sd_id
;stop
END
;

pro make_
dir='/net/froz/exports/vol07/cecchgu/AVHRR/'
;dirseawifs='/net/froz/exports/vol12/data/fapar_products/jrc/GLOBAL_PLC_0.005D/'
;file_jrc='SEA01_ORB01_19990101000000_19990101000000_L2_MUL_000006_900S900N1800W1800E_PLC_2170M_PRO.HDF'
file_eric='AVH13C1.A1999150.N14.003.2010013190758.hdf'
file_eric2='AVH09C1.A1999150.N14.003.2010013190758.hdf'

;pro read_data, dir, filename, name, array, info=INFO
;
;read_data, dir, file_eric,'NDVI', red_avhrr , /info
;read_data, dir, file_eric,'SREFL_CH2', nir_avhrr 

read_data, dir, file_eric,'QA', flag_avhrr1 
read_data, dir, file_eric2,'QA', flag_avhrr2

;read_data, dirseawifs, file_jrc,'Mean:BRF_Rec_R', red_seawifs  
;read_data, dirseawifs, file_jrc,'Mean:BRF_Rec_N', nir_seawifs  

;
; cloud 
;
for k =0, 16 do begin 
rr1=cgi_map_bitwise_flag(flag_avhrr1,k )
;rr11=cgi_map_bitwise_flag(flag_avhrr2,k)

window,k, xsize=720, ysize=360, title=string(k)
tvscl, reverse(congrid(rr1,720, 360),2)
;window,3, xsize=720, ysize=360, title='9 ndvi'
;tvscl, reverse(congrid(rr11,720, 360),2)

endfor
stop
end
