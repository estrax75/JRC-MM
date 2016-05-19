pro makeitglob, sensor, noaanumber, year, month, day
  ;
  ;
  ; sensor = avhrr or modis
  ;
  ;
  output={  ExpId_t, $
    fpar: fltarr(7200,3600), $
    sigma: fltarr(7200,3600), $
    red: fltarr(7200,3600), $
    sigma_red:fltarr(7200,3600), $
    nir: fltarr(7200,3600), $
    sigma_nir: fltarr(7200,3600), $
    flag: bytarr(7200,3600), $
    toc_red: fltarr(7200,3600), $
    toc_nir: fltarr(7200,3600), $
    qa: bytarr(7200,3600),$
    sza: bytarr(7200,3600)}
  ;
  ; initialization
  ;
  output.fpar(*,*)=255
  output.sigma(*,*)=255
  output.red(*,*)=255
  output.sigma_red(*,*)=255
  output.nir(*,*)=255
  output.sigma_nir(*,*)=255
  ;
  ;
  ;
  ;if noaanumber eq '16' then dirroot='/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/AVHRR/AVHRR/'
  ;if noaanumber eq '14' then dirroot='/net/netseA2/vol/vol24_h07/archive-from-oldstorage/froz/vol07/gobrona/LDTR/AVHRR/BRDF/'
  ;dirout='/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/AVHRR/FAPAR/'

  if noaanumber eq '16' then dirroot='E:\mariomi\Documents\projects\LDTR\data\input\AVHRR\2003\'
  if noaanumber eq '14' then dirroot='E:\mariomi\Documents\projects\LDTR\data\input\BRDF\1999\'
  dirout='E:\mariomi\Documents\projects\LDTR\data\output\FAPAR_COEFF'

  ;
  ;====================================================================
  ;
  ; should be only once for all outside the program
  ;
  ; retrieve the scale_factor and offset
  ;
  if noaanumber eq '16' then begin
    file_sds='GLOBAL_L3_GEOG_0.05DEG_001-001_03.NOAA-16.hdf'
    read_data, dirroot, file_sds,'RHO1', red_avhrr, slope_red, offset_red
    read_data, dirroot, file_sds,'RHO2', nir_avhrr, slope_nir, offset_nir
    ;
    read_data, dirroot, file_sds,'TS', ts_avhrr, slope_ts, offset_ts
    read_data, dirroot, file_sds,'TV', tv_avhrr, slope_tv, offset_tv
    read_data, dirroot, file_sds,'PHI', phi_avhrr, slope_phi, offset_phi
  endif
  ;
  ;
  ;=================================================================================
  if sensor eq 'AVHRR' then $
    new_file='AVHRR_NOA'+noaanumber+'_'+'20'+year+month+DAY+'000000_'+'20'+year+month+DAY+$
    '000000_L2_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO.HDF'

  if sensor eq 'MODIS' then $
    new_file='MOD01_TER01_'+year+month+DAY+'000000_'+year+month+DAY+$
    '000000_L2_MUL_000008_900S900N1800W1800E_PLC_0005D_PRO.HDF'

  ;
  avhrrdays=julday(month,day,year)-julday(01,01,year)+1
  print, avhrrdays
  ;
  if avhrrdays le 9 then nameavhrrdays='00'+strcompress(string(avhrrdays),/remove_all)
  if avhrrdays ge 10 and avhrrdays le 99 then nameavhrrdays='0'+strcompress(string(avhrrdays),/remove_all)
  if avhrrdays ge 100 then nameavhrrdays=strcompress(string(avhrrdays),/remove_all)

  ;
  if sensor eq 'AVHRR' then begin
    dir=dirroot ;+'/'+sensor+'/BRDF'
    print, nameavhrrdays
    file_eric=file_search(dir,'GLOBAL_L3_GEOG_0.05DEG_'+nameavhrrdays+'-'+nameavhrrdays+'_'+year+'.NOAA-'+noaanumber+'_BRF.hdf')
    ;stop
    ;file_eric=file_search(dir,'GLOBAL_P17_GEOG_0.05DEG_'+nameavhrrdays+'-'+nameavhrrdays+'_'+year+'.NOAA-'+noaanumber+'.hdf')
    print, dir
    ;print,'GLOBAL_P17_GEOG_0.05DEG_'+nameavhrrdays+'-'+nameavhrrdays+'_'+year+'.NOAA-'+noaanumber+'.hdf'
    print, file_eric
    ;stop
    if strlen(file_eric) ge 1 then begin
      file_eric=STRMID(file_eric(0),strlen(dir),strlen(file_eric(0))-strlen(dir))
      print, 'process the file :', file_eric
      ;
      ;
      ;Variable Name = RHO1
      ;Variable Name = RHO2
      ;Variable Name = T3
      ;Variable Name = T4
      ;Variable Name = T5
      ;Variable Name = RHO3
      ;Variable Name = TOA1
      ;Variable Name = TOA2
      ;Variable Name = WV
      ;Variable Name = O3
      ;Variable Name = TIME
      ;Variable Name = LAT
      ;Variable Name = LON
      ;Variable Name = TS
      ;Variable Name = TV
      ;Variable Name = PHI
      ;Variable Name = AOT1
      ;Variable Name = AOT2
      ;Variable Name = QC
      ;Variable Name = ORBDATE
      ;Variable Name = n
      ;Variable Name = N_MAPPING

      read_data, dir, file_eric,'RHO1', red_avhrr, slope_red, offset_red
      ;, /info
      ;
      read_data, dir, file_eric,'RHO2', nir_avhrr, slope_nir, offset_nir
      ;
      read_data, dir, file_eric,'TS', ts_avhrr, slope_ts, offset_ts
      ;
      read_data, dir, file_eric,'TV', tv_avhrr, slope_tv, offset_tv
      ;
      read_data, dir, file_eric,'PHI', phi_avhrr, slope_phi, offset_phi
      ;
      read_data, dir, file_eric,'QC', qc_avhrr, slope_qc, offset_qc		; QC
      ;
      read_data, dir, file_eric,'QA', brdf_qa_avhrr, slope_qa, offset_qa		; BRDF_QA
      ;
      ;
      ;	10      Band 1 BRDF corrected;
      ;	       1 -- yes
      ;	       0 -- no
      ;	5      Band 2 cosp
      ;
      ;
      ;	       1 -- yes
      ;	       0 -- no
      ;
      ;qa_avhrr=qc_avhrr
    endif
    ;
  endif else begin
    file_eric=file_search(dir+sensor+'/BRDF/','MOD09CMG.A'+year+nameavhrrdays+'*.hdf')
    ;
    if strlen(file_eric) ge 1 then begin
      file_eric=STRMID(file_eric(0),strlen(dir+sensor+'/BRDF/'),strlen(file_eric(0))-strlen(dir+sensor+'/BRDF/'))
      print, 'process the file :', file_eric
      ;
      ;
      ; MODIS
      ;
      ;Variable Name = Coarse Resolution Surface Reflectance Band 1
      ;Variable Name = Coarse Resolution Surface Reflectance Band 2
      ;Variable Name = Coarse Resolution Surface Reflectance Band 3
      ;Variable Name = Coarse Resolution Surface Reflectance Band 4
      ;Variable Name = Coarse Resolution Surface Reflectance Band 5
      ;Variable Name = Coarse Resolution Surface Reflectance Band 6
      ;Variable Name = Coarse Resolution Surface Reflectance Band 7
      ;Variable Name = Coarse Resolution Solar Zenith Angle
      ;Variable Name = Coarse Resolution View Zenith Angle
      ;Variable Name = Coarse Resolution Relative Azimuth Angle
      ;Variable Name = Coarse Resolution Ozone
      ;Variable Name = Coarse Resolution Brightness Temperature Band 20
      ;Variable Name = Coarse Resolution Brightness Temperature Band 21
      ;Variable Name = Coarse Resolution Brightness Temperature Band 31
      ;Variable Name = Coarse Resolution Brightness Temperature Band 32
      ;Variable Name = Coarse Resolution Granule Time
      ;Variable Name = Coarse Resolution Band 3 Path Radiance
      ;Variable Name = Coarse Resolution QA
      ;Variable Name = Coarse Resolution Internal CM
      ;Variable Name = Coarse Resolution State QA
      ;Variable Name = Coarse Resolution Number Mapping
      ;Variable Name = number of 500m pixels averaged b3-7
      ;Variable Name = number of 500m rej. detector
      ;Variable Name = number of 250m pixels averaged b1-2
      ;Variable Name = n pixels averaged
      ;Variable Name = Coarse Resolution AOT Model Residual Values
      ;Variable Name = Coarse Resolution AOT at 550 nm
      ;Variable Name = Coarse Resolution Atmospheric Optical Depth Model
      ;Variable Name = Coarse Resolution Water Vapor
      ;Variable Name = Coarse Resolution Air Temperature (2m)
      ;Variable Name = Coarse Resolution Atmospheric Optical Depth QA
      ;Variable Name = Coarse Resolution Number Mapping AOT
      ;
      read_data, dir+sensor+'/BRDF/', file_eric,'Coarse Resolution Surface Reflectance Band 1', red_avhrr, slope_red, 	offset_red, fill_red

      ;
      read_data, dir+sensor+'/BRDF/', file_eric,'Coarse Resolution Surface Reflectance Band 2', nir_avhrr , slope_nir, offset_nir, fill_nir
      ;
      ;read_data, dir+sensor+'/BRDF/', file_eric,'Coarse Resolution Surface Reflectance Band 3', blue_avhrr , slope_nir, offset_nir, fill_nir
      ;
      read_data, dir+sensor+'/BRDF/', file_eric,'Coarse Resolution Solar Zenith Angle', ts_avhrr, slope_ts, offset_ts , fill_ts
      ;
      read_data, dir+sensor+'/BRDF/', file_eric,'Coarse Resolution View Zenith Angle', tv_avhrr , slope_tv, offset_tv, fill_tv
      ;
      read_data, dir+sensor+'/BRDF/', file_eric,'Coarse Resolution Relative Azimuth Angle', phi_avhrr, slope_phi, offset_phi, fill_phi
      ;
      read_data, dir+sensor+'/BRDF/', file_eric,'Coarse Resolution State QA', qa_avhrr, slope_qa, offset_qa, fill_qa
    endif else  begin
      print,'no file', dir+sensor+'/BRDF/','MOD09CMG.A'+year+nameavhrrdays+'*.hdf'
      error=error+1
      ;stop
    endelse
  endelse

  if strlen(file_eric) ge 1 then begin

    sizemat=size(red_avhrr)
    ;
    ; START
    ;
    ;
    reflectance=DBLARR(sizemat(1),sizemat(2),2)
    ;
    reflectance(*,*,0)=red_avhrr(*,*)*slope_red(0)+offset_red(0)
    reflectance(*,*,1)=nir_avhrr(*,*)*slope_nir(0)+offset_nir(0)
    ;
    angles=DBLARR(sizemat(1),sizemat(2),4)
    ;
    ;
    ; verify WITH eRIC AND mARTIN IF CORRECT
    ;
    angles(*,*,0)=ts_avhrr(*,*)*slope_ts(0)+offset_ts(0)
    angles(*,*,1)=tv_avhrr(*,*)*slope_tv(0)+offset_tv(0)
    ;
    output.sza=ts_avhrr(*,*)
    ;
    RELAZ=phi_avhrr(*,*)*slope_phi(0)+offset_phi(0)
    SIN_REL_AZ = sin(RELAZ*!pi/180.)
    COS_REL_AZ = cos(RELAZ*!pi/180.)
    GOOD_REL_AZ = atan(SIN_REL_AZ, COS_REL_AZ)
    angles(*,*,2)=good_rel_az*180./!pi
    ;
    ;
    ;
    angles(*,*,3)=0.0
    ;
    ;
    flagstoc= bytarr(7200, 3600)

    red = [0,0,1,0,0.66,0.68,0.,0.55,0.00,0.00,0.85,0,00,0.]
    gre = [1,0,0,0,0.00,0.00,0.,0.55,0.77,0.66,0.00,0.00,0.]
    blu = [0,0,0,1,0.00,1.00,1.,1.00,1.00,0.55,0.80,0.77,0.]
    TVLCT, red*255, gre*255, blu*255

    flag2 = CheckDataTOC(red_avhrr*slope_red(0)+offset_red(0), nir_avhrr*slope_nir(0)+offset_nir(0))
    flagstoc=flag2
    ;
    ;stop
    ;
    ;flag_angles = CheckAngle(angles(*,*,0), angles(*,*,1))
    ;window, 10, xsize=720, ysize=360, title='FPAR Angles'+SENSOR
    ;tv, reverse(congrid(flag_angles, 720,360),2)
    ;

    red_avhrr=0.
    nir_avhrr=0.
    ts_avhrr=0.
    tv_avhrr=0.
    phi_avhrr=0.
    ;
    ;=======================================================================
    ;
    ;	AOT
    ;
    ;=========================================================================
    ;read_data, dirroot, file_eric,'AOT', AOT, slope_aot, offset_aot

    ;window, 12, xsize=720, ysize=360, title='AAOT '+SENSOR
    ;tv, reverse(congrid(bytscl(aot*slope_aot(0)+offset_aot(0), min=0., max =0.8), 720,360),2)

    ;stop
    ;
    ;	0      Band 1 BRDF corrected;
    ;	       1 -- yes
    ;	       0 -- no
    ;	5      Band 2 BRDF corrected;
    ;	       1 -- yes
    ;	       0 -- no
    ;
    ;
    ;=======================================================================================

    rrq1=cgi_map_bitwise_flag(brdf_qa_avhrr,0)
    rrq2=cgi_map_bitwise_flag(brdf_qa_avhrr,5)
    ;
    idx_nocorr=where (rrq1 eq 0 or rrq2 eq 0)
    ;window,0, xsize=720*2, ysize=360*2
    ;tvscl, reverse(congrid(rrq1, 720*2, 360*2), 2)
    ;stop

    ;=====================================================================================
    ;
    ;
    ;1      Pixel is cloudy;
    ;   		1 -- yes
    ;	       0 -- no

    rr1=cgi_map_bitwise_flag(qc_avhrr,1)
    ;
    ;
    ; 2      Pixel contains cloud shadow;
    ;	       1 -- yes
    ;	       0 -- no
    ;
    rr2=cgi_map_bitwise_flag(qc_avhrr,2)
    ;
    ;  9 channel 2 value is invalid 1 = yes, 0 = no
    ;  8 Channel 1 value is invalid 1 = yes, 0 = no
    ;
    rr21=cgi_map_bitwise_flag(qc_avhrr,9)
    rr22=cgi_map_bitwise_flag(qc_avhrr,8)
    ;
    ;
    idxbad=where(rr21 eq 1 or rr22 eq 1)
    ;
    ; 3      Pixel is over water;
    ;	       1 -- yes
    ;	       0 -- no
    rr3=cgi_map_bitwise_flag(qc_avhrr,3)
    ;
    ;
    ;
    ;
    idx_mask = where(rr1 eq 1) ; or rr2 eq 1)		;----> cloud
    idx_mask2 = where(rr21 eq 1 or rr22 eq 1)		;----> invalid
    IDX_SEA=  where(rr3 eq 1)




    flag2(idxbad)=7.0		;	< bad


    S=SIZE(FLAG2)
    MASK_AVHRR=BYTARR(S(1),S(2))
    ;
    MASK_AVHRR(IDX_MASK)=2		;----> cloud
    ;MASK_AVHRR(IDX_MASK2)=2
    ;MASK_AVHRR(idx_nocorr)= 130
    ;flag2(idx_mask)=2.0

    MASK_AVHRR(IDX_SEA)= 3
    flag2(idx_sea)=3.0
    ;window, 11, xsize=720, ysize=360, title='QA '+SENSOR
    ;tv, reverse(congrid(MASK_avhrr, 720,360),2)

    ;MASK_AVHRR(idx_nocorr)= 0
    ;window, 11, xsize=720*2, ysize=360*2, title='QA '+SENSOR
    ;tv, reverse(congrid(MASK_avhrr, 720*2,360*2),2)
    ;stop
    ;
    ;window, 12, xsize=720*2, ysize=360*2, title='FLAG '+SENSOR
    ;tv, reverse(congrid(flag2, 720*2,360*2),2)
    ;
    ;stop
    ;
    ;
    if sensor eq 'AVHRR' then begin
      sza=angles[*,*,0] ;/180.*!pi
      vza=angles[*,*,1] ;/180.*!pi
      saa=angles[*,*,2] ;/180.*!pi
      vaa=angles[*,*,3] ;/180.*!pi
      ;
      ;
      red=reflectance(*,*,0)
      nir=reflectance(*,*,1)

      ;
      ; use nasa formulae for toc band ... 0.05ρ+0.005
      ;
      D_BRF_ToA_RED=2.*(red*0.05+0.005)
      D_BRF_ToA_NIR=2.*(nir*0.05+0.005)
      ;
      ;
      ;
      ; make the process only over vegetated areas
      ;
      idx=where(flag2 eq 0)
      if idx(0) ge 0 then begin

        FAPAR,'AVHRR'+noaanumber,sza(idx),vza(idx),saa(idx),vaa(idx),$
          red(idx),red(idx),nir(idx),$
          D_BRF_ToA_RED(idx),D_BRF_ToA_RED(idx),D_BRF_ToA_NIR(idx), $
          rhoRED, rhoNIR, D_rhoRED, D_rhoNIR, $
          D_rhotildeBLUE, D_rhotildeRED, D_rhotildeNIR, VI, D_VI, /TOC
        ;
        output.fpar(idx)=vi
        output.sigma(idx)=d_vi
        output.red(idx)=rhoRED
        output.nir(idx)=rhoNIR
        output.sigma_red(idx)=D_rhoRED
        output.sigma_nir(idx)=D_rhoNIR
        ;
        ;
        ;
        ;window,3
        ;plot, [0.,1.],[0.,1.], psym=1, yr=[0.,1.], xr=[0.,1.]
        ;for k=0, N_elements(idx)-1, 50 do plots, red(idx(k)), nir(idx(k)), psym=4, col=output.fpar(idx(k))*250.0
        ;stop

        idx_neg=where(output.fpar le 0.0)
        output.fpar(idx_neg)=0.0
        idx_big=where(output.fpar ge 1.0 and output.fpar le 10.0)
        output.fpar(idx_big)=1.0
        flag2(idx_neg)=4.0
        flag2(idx_big)=6.0
        FAPARCOLOR
        ;window, 0, xsize=720*2, ysize=360*2, title='FPAR '+SENSOR
        ;tv, reverse(congrid(output.fpar*250, 720*2,360*2),2)
        ;
        ;window, 1, xsize=720*2, ysize=360*2, title='New FLAG '+SENSOR
        ;tvscl, reverse(congrid(flag2, 720*2,360*2),2)
        ;stop
        ;
        ;	window, 1, xsize=720, ysize=360, title='Dev FPAR '+SENSOR
        ;    	tv, reverse(congrid(output.sigma*2.*250., 720,360),2)
        ;
        ;	window, 2, xsize=720, ysize=360, title='Rectified Red '+SENSOR
        ;    	tv, reverse(congrid(bytscl(output.sigma_red, min=0., max=0.1), 720,360),2)
        ;


        ;img=fltarr(7200,3600)
        ;img(idx)=rhoRED
        ;
        ;	window, 2, xsize=720, ysize=360, title='Rectified Red '+SENSOR
        ;    	tvscl, reverse(congrid(img, 720,360),2)
        ;
        ;
      endif
    endif
    ;
    ;
    ;stop
    ; make the process of rectifed only over bare soil
    ;
    idx_soil=where(flag2 eq 4)

    if idx_soil(0) ge 0 then begin
      ;
      ;
      ; BUG HERE AS DEPEND ON ANGLES ...
      ;
      sza=angles[*,*,0] ;/180.*!pi
      vza=angles[*,*,1] ;/180.*!pi
      saa=angles[*,*,2] ;/180.*!pi
      vaa=angles[*,*,3] ;/180.*!pi


      Rectified,'AVHRR'+noaanumber,sza(idx_soil),vza(idx_soil),saa(idx_soil),vaa(idx_soil),$
        red(idx_soil),red(idx_soil),nir(idx_soil),$
        D_BRF_ToA_RED(idx_soil),D_BRF_ToA_RED(idx_soil),D_BRF_ToA_NIR(idx_soil), $
        rhoRED_S, rhoNIR_S, D_rhoRED_S, D_rhoNIR_S, $
        D_rhotildeBLUE_S, D_rhotildeRED_S, D_rhotildeNIR_S, vi_s, d_vis, /TOC
      ;;
      output.fpar(idx_soil)=0.0
      output.sigma(idx_soil)=0.0
      output.red(idx_soil)=rhoRED_S
      output.nir(idx_soil)=rhoNIR_S
      output.sigma_red(idx_soil)=D_rhoRED_S
      output.sigma_nir(idx_soil)=D_rhoNIR_S
      ;
      ;img=fltarr(7200,3600)
      ;img(idx_soil)=rhoRED_S
      ;

      ;
      ;stop
    endif
    ;
    PPMSA_ALBEDOCOLOR
    ;	window, 4, xsize=720, ysize=360, title='Rectified Red '+SENSOR
    ;    	tv, reverse(congrid(bytscl(output.red, min=0., max=0.6), 720,360),2)
    output.flag=flag2
    ;
    idx_1 = where (flag2 eq 1)
    idx_2 = where (flag2 eq 2)
    idx_3 = where (flag2 eq 3)
    ;
    ;idx_4 = where (flag_angles eq 1)
    ;idx_5 = where (flag_angles eq 2)
    ;
    ;
    if idx_3(0) ge 0 then begin
      output.fpar(idx_3)=253
      output.sigma(idx_3)=253
      output.red(idx_3)=253
      output.nir(idx_3)=253
      output.sigma_red(idx_3)=253
      output.sigma_nir(idx_3)=253
    endif
    ;
    if idx_2(0) ge 0 then begin
      output.fpar(idx_2)=254
      output.sigma(idx_3)=254
      output.red(idx_3)=254
      output.nir(idx_3)=254
      output.sigma_red(idx_3)=254
      output.sigma_nir(idx_3)=254
    endif
    ;
    if idx_1(0) ge 0 then begin
      output.fpar(idx_1)=255
      output.sigma(idx_3)=255
      output.red(idx_3)=255
      output.nir(idx_3)=255
      output.sigma_red(idx_3)=255
      output.sigma_nir(idx_3)=255
    endif

    ;
    ;if idx_4(0) ge 0 then img(idx_4)=-9996.0
    ;
    ;if idx_5(0) ge 0 then img(idx_5)=-9995.0
    ;
    ;
    ;

    print,'Write the results in ',dirout+new_file
    ;
    idxneg=where(output.fpar lt 0.0)
    if idxneg(0) ge 0 then begin
      output.fpar(idxneg)=0.0
      output.flag(idxneg)= 9
    endif
    idxbig=where(output.fpar gt 1.0 and output.fpar le 250.0)
    if idxbig(0) ge 0 then begin
      output.fpar(idxbig)=1.0
      output.flag(idxbig)= 10
    endif
    ;
    idxneg1=where(output.red lt 0.0)
    if idxneg1(0) ge 0 then begin
      output.red(idxneg1)=0.0
      output.flag(idxneg1)= 11
    endif
    idxbig1=where(output.red gt 1.0 and output.red le 250.0)
    if idxbig1(0) ge 0 then begin
      output.red(idxbig1)=1.0
      output.flag(idxbig1)= 12
    endif
    ;
    idxneg2=where(output.nir lt 0.0)
    if idxneg2(0) ge 0 then begin
      output.nir(idxneg2)=0.0
      output.flag(idxneg2)= 13
    endif
    idxbig2=where(output.nir gt 1.0 and output.nir le 250.0)
    if idxbig2(0) ge 0 then begin
      output.nir(idxbig2)=1.0
      output.flag(idxbig2)= 14
    endif
    ;
    ;idx_4 = where (flag_angles eq 1)
    ;idx_5 = where (flag_angles eq 2)
    ;
    ;
    ;if idx_3(0) ge 0 then img(idx_3)=0.0
    ;
    ;if idx_2(0) ge 0 then img(idx_2)=-9998.0
    ;
    ;if idx_1(0) ge 0 then img(idx_1)=-9997.0
    ;
    ;if idx_4(0) ge 0 then img(idx_4)=-9996.0
    ;
    ;if idx_5(0) ge 0 then img(idx_5)=-9995.0
    ;
    ;
    ; create output file
    ;
    ;
    print, dirout+new_file
    hdffilename=dirout+new_file
    ;tempDir='E:\mariomi\Documents\temp\'
    ncfilename=dirout+new_file+'.nc'
    ;stop
;    sdid_outfile1 = HDF_SD_START(dirout+new_file, /CREATE)
;    a=size(output.fpar)
;    sdid_fpar = HDF_SD_CREATE(sdid_outfile1, 'FAPAR', [a(1),a(2)], /float)
;    sdid_uncert = HDF_SD_CREATE(sdid_outfile1, 'Sigma FAPAR', [a(1),a(2)], /float)
;    sdid_red = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED RED', [a(1),a(2)], /float)
;    sdid_uncert_red = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED RED', [a(1),a(2)], /float)
;    sdid_nir = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED NIR', [a(1),a(2)], /float)
;    sdid_uncert_nir = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED NIR', [a(1),a(2)], /float)
;    sdid_flag = HDF_SD_CREATE(sdid_outfile1, 'FLAG', [a(1),a(2)], /byte)
;    sdid_redtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC RED', [a(1),a(2)], /float)
;    sdid_nirtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC NIR', [a(1),a(2)], /float)
;    sdid_qa =  HDF_SD_CREATE(sdid_outfile1, 'JRC QA', [a(1),a(2)], /byte)
;    HDF_SD_ADDDATA, sdid_fpar, output.fpar
;    HDF_SD_ADDDATA, sdid_uncert, output.sigma
;    HDF_SD_ADDDATA, sdid_red, output.red
;    HDF_SD_ADDDATA, sdid_uncert_red, output.sigma_red
;    HDF_SD_ADDDATA, sdid_nir, output.nir
;    HDF_SD_ADDDATA, sdid_uncert_nir, output.sigma_nir
;    HDF_SD_ADDDATA, sdid_flag, output.flag
;    HDF_SD_ADDDATA, sdid_redtoc, reflectance(*,*,0)
;    HDF_SD_ADDDATA, sdid_nirtoc, reflectance(*,*,1)
;    HDF_SD_ADDDATA, sdid_qa, MASK_avhrr
;    HDF_SD_END, sdid_outfile1
    bandNames=['FAPAR','Sigma FAPAR', 'RECTIFIED RED', 'Sigma RECTIFIED RED', 'RECTIFIED NIR', 'Sigma RECTIFIED NIR', 'FLAG', 'BRF TOC RED', 'BRF TOC NIR', 'JRC QA']
    bandunits=['FAPAR mu','Sigma FAPAR mu', 'RECTIFIED RED mu', 'Sigma RECTIFIED RED mu', 'RECTIFIED NIR mu', 'Sigma RECTIFIED NIR mu', 'FLAG mu', 'BRF TOC RED mu', 'BRF TOC NIR mu', 'JRC QA mu']
    bandValues=[ptr_new(output.fpar, /NO_COPY), $
      ptr_new(output.sigma, /NO_COPY), $
      ptr_new(output.red, /NO_COPY), $
      ptr_new(output.sigma_red, /NO_COPY), $
      ptr_new(output.nir, /NO_COPY), $
      ptr_new(output.sigma_nir, /NO_COPY), $
      ptr_new(output.flag, /NO_COPY), $
      ptr_new(reform(reflectance(*,*,0)), /NO_COPY), $
      ptr_new(reform(reflectance(*,*,1)), /NO_COPY), $
      ptr_new(MASK_avhrr, /NO_COPY)]

    write_hdf, hdffilename, bandNames, bandValues, boundary, /COMPRESS
    write_georef_ncdf, ncfilename, bandNames, bandunits, bandValues, boundaryInfo, /COMPRESS
    ;
    PPMSA_ALBEDOCOLOR
    ;window, 5, xsize=720*2, ysize=360*2, title='Rectified nir '+SENSOR
    ;tv, reverse(congrid(bytscl(output.nir,min=0.,max=0.8), 720*2,360*2),2)

    ;window, 6, xsize=720*2, ysize=360*2, title='Rectified Red '+SENSOR
    ;tv, reverse(congrid(bytscl(output.red, min=0.,max=0.8), 720*2,360*2),2)

    faparcolor
    ;window, 7, xsize=720*2, ysize=360*2, title='FAPAR '+SENSOR
    ;tv, reverse(congrid(output.fpar*250.0, 720*2,360*2),2)

    ;stop

  endif
  ;

  ;

  ;stop
  ;
end

