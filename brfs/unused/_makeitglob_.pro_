;pro makeitglob_, sensor, noaanumber, year, month, day, refDir, outDir
@/reader/compile
function makeitglob_, file1, file2, year, month, dayNum
  ;
  ;
  ; sensor = avhrr or modis
  ;
  ;
  ; check "flat way" to read files
  myVar='SREFL_CH1'

  aa=readStandardNcContents(file1, myVar, outputVarList, conv_functions, tempDir, ignoreValue, outFuncList, NOTFOUND=NOTFOUND)
  aa=read_nc4_dataset(file1, myVar, start=start, count=count)
  aa=readSingleBand(myVar, file1)
  aa=myreaderObj->readNcdfVar(file1, myVar)
  
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
  ; BRF from NASA -
  ; data will changed when top secret program arrive
  ;
  ; ask/get/find monica data...

  ; merge BRF missing Qa,Qc... with the info coming from
  ; /space3/storage/products/AVHRR_LDTR
  ; (day by day) (year=2003) (noaa=16)
  ; MM 20150404 : test on local files
  if noaanumber eq '16' then dirroot='E:\mariomi\Documents\projects\LRM\input\AVHRR\'; '/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/AVHRR/AVHRR/'
  if noaanumber eq '14' then dirroot='E:\mariomi\Documents\projects\LRM\input\BRDF\';'/net/netseA2/vol/vol24_h07/archive-from-oldstorage/froz/vol07/gobrona/LDTR/AVHRR/BRDF/'
  dirout='E:\mariomi\Documents\projects\LRM\output\FAPAR';'/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/AVHRR/FAPAR/'
  ;
  ;====================================================================
  ;
  ; should be only once for all outside the program
  ;
  ; retrieve the scale_factor and offset
  ;
  ;
  ;
  ;=================================================================================
  ;Av09 get QA, QC
  ;Brf/netsea2 get all
  if sensor eq 'AVHRR' then $
    new_file='AVHRR_NOA'+noaanumber+'_'+'20'+year+month+DAY+'000000_'+'20'+year+month+DAY+$
    '000000_L2_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO.HDF'
  ;new file name
  ;000000_L2_AVR_000001_900S900N1800W1800E_PLC_0005D_PRO.HDF'

  if sensor eq 'MODIS' then $
    new_file='MOD01_TER01_'+year+month+DAY+'000000_'+year+month+DAY+$
    '000000_L2_MUL_000008_900S900N1800W1800E_PLC_0005D_PRO.HDF'

  ;
  avhrrdays=julday(month,day,year)-julday(01,01,year)+1
  print, avhrrdays
  ;
  ;if avhrrdays le 9 then nameavhrrdays='00'+strcompress(string(avhrrdays),/remove_all)
  ;if avhrrdays ge 10 and avhrrdays le 99 then nameavhrrdays='0'+strcompress(string(avhrrdays),/remove_all)
  ;if avhrrdays ge 100 then nameavhrrdays=strcompress(string(avhrrdays),/remove_all)
  ; MM
  ; replace with one more easy to do the same thing...
  nameavhrrdays=string(avhrrdays, format='(I03)')

  ;
  if sensor eq 'AVHRR' then begin
    dir=refDir ;+'/'+sensor+'/BRDF'
    fileName='GLOBAL_L3_GEOG_0.05DEG_'+nameavhrrdays+'-'+nameavhrrdays+'_'+year+'.NOAA-'+noaanumber+'_BRF.hdf'
    file_eric=file_search(dir, fileName, count=ericFound)
    if ericFound eq 1 then begin
      print, 'process the file :', file_eric

      ; MM more compact
      bandList=['RHO1', 'RHO2', 'TS', 'TV', 'PHI', 'QC', 'QA']
      bands=getMultipleBand(refDir, refFile, bandList, APPLYCONVERSION=[1,1,1,1,1,0,0])
      red_avhrr=*bands[0]
      nir_avhrr=*bands[1]
      ts_avhrr=*bands[2]
      tv_avhrr=*bands[3]
      phi_avhrr=*bands[4]
      qc_avhrr=*bands[5]    ; QC
      brdf_qa_avhrr=*bands[6]    ; BRDF_QA
    endif
    ;
  endif else begin
    thisDir=dir+sensor+'/BRDF/'
    fileName='MOD09CMG.A'+year+nameavhrrdays+'*.hdf'
    file_eric=file_search(thisDir,fileName, count=ericFound)
    ;
    if ericFound eq 1 then begin
      ; MODIS
      bandList=['Coarse Resolution Surface Reflectance Band 1', 'Coarse Resolution Surface Reflectance Band 2', 'Coarse Resolution Solar Zenith Angle', 'Coarse Resolution View Zenith Angle', $
        'Coarse Resolution Relative Azimuth Angle', 'Coarse Resolution State QA']
      bands=getMultipleBand(thisDir, fileName, bandList, APPLYCONVERSION=[1,1,1,1,1,0])
      red_avhrr=*(bands[0])
      nir_avhrr=*(bands[1])
      ts_avhrr=*(bands[2])
      tv_avhrr=*(bands[3])
      phi_avhrr=*(bands[4])
    endif else  begin
      print,'no file', dir+sensor+'/BRDF/','MOD09CMG.A'+year+nameavhrrdays+'*.hdf'
      error=error+1
      message, 'no file', dir+sensor+'/BRDF/','MOD09CMG.A'+year+nameavhrrdays+'*.hdf'
    endelse
  endelse

  if ericFound eq 1 then begin

    sizemat=size(red_avhrr)
    ; START
    reflectance=DBLARR(sizemat(1),sizemat(2),2)
    ;
    ;reflectance(*,*,0)=red_avhrr(*,*)*slope_red(0)+offset_red(0)
    ;reflectance(*,*,1)=nir_avhrr(*,*)*slope_nir(0)+offset_nir(0)
    ; conversion already apply..
    reflectance[*,*,0]=red_avhrr[*,*]
    reflectance[*,*,1]=nir_avhrr[*,*]
    ;
    angles=DBLARR(sizemat[1],sizemat[2],4)
    ;
    ;angles[*,*,0]=ts_avhrr[*,*]*slope_ts(0)+offset_ts(0)
    ;angles[*,*,1]=tv_avhrr[*,*]*slope_tv(0)+offset_tv(0)
    ; conversion already apply...
    angles[*,*,0]=ts_avhrr[*,*]
    angles[*,*,1]=tv_avhrr[*,*]
    ;
    output.sza=ts_avhrr[*,*]
    ;
    ; conversion already apply...
    RELAZ=phi_avhrr[*,*]
    SIN_REL_AZ = sin(RELAZ*!pi/180.)
    COS_REL_AZ = cos(RELAZ*!pi/180.)
    GOOD_REL_AZ = atan(SIN_REL_AZ, COS_REL_AZ)
    angles[*,*,2]=good_rel_az*180./!pi
    ;
    ;
    ;
    angles[*,*,3]=0.0
    ;
    ;
    flagstoc= bytarr(7200, 3600)

    red = [0,0,1,0,0.66,0.68,0.,0.55,0.00,0.00,0.85,0,00,0.]
    gre = [1,0,0,0,0.00,0.00,0.,0.55,0.77,0.66,0.00,0.00,0.]
    blu = [0,0,0,1,0.00,1.00,1.,1.00,1.00,0.55,0.80,0.77,0.]
    TVLCT, red*255, gre*255, blu*255

    flag2 = CheckDataTOC(red_avhrr, nir_avhrr)
    flagstoc=flag2
    ;
    ;flag_angles = CheckAngle(angles(*,*,0), angles(*,*,1))
    ;window, 10, xsize=720, ysize=360, title='FPAR Angles'+SENSOR
    ;tv, reverse(congrid(flag_angles, 720,360),2)
    ;

    ; recover memory...
    red_avhrr=0.
    nir_avhrr=0.
    ts_avhrr=0.
    tv_avhrr=0.
    phi_avhrr=0.

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

    flag2[idxbad]=7.0		;	< bad

    S=SIZE(FLAG2)
    MASK_AVHRR=BYTARR(S[1],S[2])
    ;
    MASK_AVHRR[IDX_MASK]=2		;----> cloud

    MASK_AVHRR[IDX_SEA]= 3
    flag2[idx_sea]=3.0
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
      red=reflectance[*,*,0]
      nir=reflectance[*,*,1]

      ;
      ; use nasa formulae for toc band ... 0.05ρ+0.005
      ;
      ; sigma (brf)
      D_BRF_ToA_RED=2.*(red*0.05+0.005)
      D_BRF_ToA_NIR=2.*(nir*0.05+0.005)
      ;
      ;
      ;
      ; make the process only over vegetated areas
      ;
      idx=where(flag2 eq 0, count=count)
      if count ne 0 then begin

        ;FAPAR,'AVHRR'+noaanumber,sza(idx),vza(idx),saa(idx),vaa(idx),$
        Rectified,'AVHRR'+noaanumber,sza[idx],vza[idx],saa[idx],vaa[idx],$
          red[idx],red[idx],nir[idx],$
          D_BRF_ToA_RED[idx],D_BRF_ToA_RED[idx],D_BRF_ToA_NIR[idx], $
          rhoRED, rhoNIR, D_rhoRED, D_rhoNIR, $
          D_rhotildeBLUE, D_rhotildeRED, D_rhotildeNIR, VI, D_VI, /TOC
        ;
        output.fpar[idx]=vi
        output.sigma[idx]=d_vi
        output.red[idx]=rhoRED
        output.nir[idx]=rhoNIR
        output.sigma_red[idx]=D_rhoRED
        output.sigma_nir[idx]=D_rhoNIR
        ;
        ;
        ;
        ;window,3
        ;plot, [0.,1.],[0.,1.], psym=1, yr=[0.,1.], xr=[0.,1.]
        ;for k=0, N_elements(idx)-1, 50 do plots, red(idx(k)), nir(idx(k)), psym=4, col=output.fpar(idx(k))*250.0
        ;stop

        idx_neg=where(output.fpar le 0.0, count=count)
        output.fpar[idx_neg]=0.0
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


      Rectified,'AVHRR'+noaanumber,sza[idx_soil],vza[idx_soil],saa(idx_soil),vaa[idx_soil],$
        red[idx_soil],red[idx_soil],nir[idx_soil],$
        D_BRF_ToA_RED[idx_soil],D_BRF_ToA_RED[idx_soil],D_BRF_ToA_NIR[idx_soil], $
        rhoRED_S, rhoNIR_S, D_rhoRED_S, D_rhoNIR_S, $
        D_rhotildeBLUE_S, D_rhotildeRED_S, D_rhotildeNIR_S, vi_s, d_vis, /TOC
      ;;
      output.fpar[idx_soil]=0.0
      output.sigma[idx_soil]=0.0
      output.red[idx_soil]=rhoRED_S
      output.nir[idx_soil]=rhoNIR_S
      output.sigma_red[idx_soil]=D_rhoRED_S
      output.sigma_nir[idx_soil]=D_rhoNIR_S
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
    idx_1 = where (flag2 eq 1, count=count1)
    idx_2 = where (flag2 eq 2, count=count2)
    idx_3 = where (flag2 eq 3, count=count3)
    ;
    ;idx_4 = where (flag_angles eq 1)
    ;idx_5 = where (flag_angles eq 2)
    ;
    ;
    if count3 ne 0 then begin
      output.fpar[idx_3]=253
      output.sigma[idx_3]=253
      output.red[idx_3]=253
      output.nir[idx_3]=253
      output.sigma_red[idx_3]=253
      output.sigma_nir[idx_3]=253
    endif
    ;
    if count2 ge 0 then begin
      output.fpar[idx_2]=254
      output.sigma[idx_3]=254
      output.red[idx_3]=254
      output.nir[idx_3]=254
      output.sigma_red[idx_3]=254
      output.sigma_nir[idx_3]=254
    endif
    ;
    if count1 ge 0 then begin
      output.fpar[idx_1]=255
      output.sigma[idx_1]=255
      output.red[idx_1]=255
      output.nir[idx_1]=255
      output.sigma_red[idx_1]=255
      output.sigma_nir[idx_1]=255
    endif

    new_file=buildMergeBRFFileName_D('AVHRR_BRF', year, month, dayNum)
    ;print,'Write the results in ',dirout+new_file
    ;
    idxneg=where(output.fpar lt 0.0, count=countNeg)
    if countNeg ne 0 then begin
      output.fpar[idxneg]=0.0
      output.flag[idxneg]= 9
    endif
    idxbig=where(output.fpar gt 1.0 and output.fpar le 250.0, count=countBig)
    if countBig ge 0 then begin
      output.fpar[idxbig]=1.0
      output.flag[idxbig]= 10
    endif
    ;
    idxneg1=where(output.red lt 0.0, count=countNeg1)
    if countNeg1 ne 0 then begin
      output.red[idxneg1]=0.0
      output.flag[idxneg1]= 11
    endif
    idxbig1=where(output.red gt 1.0 and output.red le 250.0, count=countBig1)
    if countBig1 ne 0 then begin
      output.red[idxbig1]=1.0
      output.flag[idxbig1]= 12
    endif
    ;
    idxneg2=where(output.nir lt 0.0, count=countNeg2)
    if countNeg2 ne 0 then begin
      output.nir[idxneg2]=0.0
      output.flag[idxneg2]= 13
    endif
    idxbig2=where(output.nir gt 1.0 and output.nir le 250.0, count=countBig2)
    if countBig2 ge 0 then begin
      output.nir[idxbig2]=1.0
      output.flag[idxbig2]= 14
    endif
    ; create output file
    ;
    ; add global attributes
    ; add field attributes
    ; *add angles
    ;
    print, dirout+new_file
    ;stop
    sdid_outfile1 = HDF_SD_START(dirout+new_file, /CREATE)

    ;ncdf_write_global_attribute, file_id, attname, attvalue

    a=size(output.fpar)
    ;sdid_fpar = HDF_SD_CREATE(sdid_outfile1, 'FAPAR', [a[1],a[2]], /float)
    ;sdid_uncert = HDF_SD_CREATE(sdid_outfile1, 'Sigma FAPAR', [a[1],a[2]], /float)
    ;D_BRF_ToA_RED[idx_soil]
    sdid_red = HDF_SD_CREATE(sdid_outfile1, 'D_BRF_ToA_RED', [a[1],a[2]], /float)
    
    
    sdid_red = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED RED', [a[1],a[2]], /float)
    sdid_uncert_red = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED RED', [a[1],a[2]], /float)
    sdid_nir = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED NIR', [a[1],a[2]], /float)
    sdid_uncert_nir = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED NIR', [a[1],a[2]], /float)
    sdid_flag = HDF_SD_CREATE(sdid_outfile1, 'FLAG', [a[1],a[2]], /byte)
    sdid_redtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC RED', [a[1],a[2]], /float)
    sdid_nirtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC NIR', [a[1],a[2]], /float)
    sdid_qa =  HDF_SD_CREATE(sdid_outfile1, 'JRC QA', [a[1],a[2]], /byte)

    ; MM 20150405
    sdid_sza =  HDF_SD_CREATE(sdid_outfile1, 'SZA', [a[1],a[2]], /float)
    sdid_vza =  HDF_SD_CREATE(sdid_outfile1, 'VZA', [a[1],a[2]], /float)
    sdid_saa =  HDF_SD_CREATE(sdid_outfile1, 'SAA', [a[1],a[2]], /float)
    sdid_vaa =  HDF_SD_CREATE(sdid_outfile1, 'VAA', [a[1],a[2]], /float)
    ; END

    HDF_SD_ADDDATA, sdid_fpar, output.fpar
    HDF_SD_ADDDATA, sdid_uncert, output.sigma
    HDF_SD_ADDDATA, sdid_red, output.red
    HDF_SD_ADDDATA, sdid_uncert_red, output.sigma_red
    HDF_SD_ADDDATA, sdid_nir, output.nir
    HDF_SD_ADDDATA, sdid_uncert_nir, output.sigma_nir
    HDF_SD_ADDDATA, sdid_flag, output.flag
    HDF_SD_ADDDATA, sdid_redtoc, reflectance[*,*,0]
    HDF_SD_ADDDATA, sdid_nirtoc, reflectance[*,*,1]
    HDF_SD_ADDDATA, sdid_qa, MASK_avhrr

    ; MM 20150405
    HDF_SD_ADDDATA, sdid_sza, sza
    HDF_SD_ADDDATA, sdid_vza, vza
    HDF_SD_ADDDATA, sdid_saa, saa
    HDF_SD_ADDDATA, sdid_vaa, vaa
    ; END
    HDF_SD_END, sdid_outfile1
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
  return, new_file
  ;

  ;

  ;stop
  ;
end

