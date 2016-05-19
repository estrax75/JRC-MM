
; Procedures needed:

; .comp /home/melinfr/OC/CAL/VRS/read_nc4_data
; .run /home/melinfr/OC/CAL/VRS/ExtractSubset

; .run /home/melinfr/OC/CAL/VRS/GetVRSInfo

; .run /home/melinfr/OC/CAL/VRS/FindPixel

; Execution
; .run /home/melinfr/OC/CAL/VRS/ExtractData

PRO _vrs_ExtractData,k_site,option,n_side,in_dir,out_dir,root

; Site chosen for extraction of data.

; MSEA
IF ( k_site EQ 1 ) THEN BEGIN
 sitelat = 34.
 sitelon = 25.
 ext = "msea"
ENDIF


; Size of extraction (extraction of a square of n_side x n_side pixels) with n_side = 2n_e +1

n_e = FIX ( (n_side-1)/2 + 0.1)

; type of the files to be read
findvariable = STRCOMPRESS(in_dir+root,/REMOVE_ALL)

; List of files.
filename=FINDFILE(findvariable, count=NbFiles)

print,'Number of files: ',NbFiles

; Loop over the files.
FOR f=0,NbFiles-1 DO BEGIN
;FOR f=459,459 DO BEGIN

    print,filename[f]
    
; Define output    

    ifile = STRMID(STRCOMPRESS(filename[f],/REMOVE_ALL),STRLEN(in_dir), $
                   STRLEN(STRCOMPRESS(filename[f],/REMOVE_ALL))-STRLEN(in_dir))

    ofile = STRMID(STRCOMPRESS(filename[f],/REMOVE_ALL),STRLEN(in_dir), $
                   STRLEN(STRCOMPRESS(filename[f],/REMOVE_ALL))-STRLEN(in_dir) -5) ; remove additional extensions
                   
    ofile = STRCOMPRESS(out_dir+ofile+"."+ext,/REMOVE_ALL)

; Get all SeaWiFS information useful for processing: time, location, size,...        
    GetVRSInfo,filename[f],SatStruct,FStruct,latitude,longitude
    
    SatStruct.Source = ifile

    nline = SatStruct.NumberLine
    nelem = SatStruct.NumberPixel
	
; Find nearest pixel to searched location.
    vrs_FindPixel,SatStruct,latitude,longitude,sitelat,sitelon,n_side,line,elem,istatus

;IF ( 1 EQ 0 ) THEN BEGIN

;print,line,elem
; Check if the pixel is not close to edges 
    IF ( istatus EQ 1 AND ( line LE n_e OR line GE nline-n_e OR elem LE n_e OR elem GE nelem-1-n_e ) ) THEN istatus = -1


; Go ahead only if a contiguous square of pixels is available around the site.
IF ( istatus EQ 1 ) THEN BEGIN

; Get the geophysical values.
     vrs_SelectData,filename[f],SatStruct,FStruct,latitude,longitude,line,elem,n_side,SatData,option

     SatData.Latitude = sitelat
     SatData.Longitude = sitelon

; Write the various variables into a ascii file.
;     WriteData,ofile,SatData,option

STOP
ENDIF

;ENDIF
ENDFOR
; End of loop over files.



END
