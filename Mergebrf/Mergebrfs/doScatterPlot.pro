pro doScatterPlot, year

  utility=obj_new('Utility')
  operatorObj=obj_new('GenericOperator')
  SENSOR='AVHRR'
  savefile=1
  fullData=1
  sampleDim=50000l

  ; 2003
  if year eq 2003 then begin
    location1='E:\mariomi\Documents\projects\LDTR\data\input\AVHRR\2003\'
    prefix1='GLOBAL_L3_GEOG_0.05DEG_'
    postfix1='*.NOAA-16_BRF.hdf'
    location2='E:\mariomi\Documents\projects\LDTR\data\AVHRR\NOAA\surface_refl\2003\result\'
    prefix2='AVHRR-Land_v004_AVH09C1_NOAA-16_'
    postfix2='*.sav'
    outlocation='E:\mariomi\Documents\projects\LDTR\data\result\2003\'
    NOAAString='NOAA-16'
    year1=03
    year2=2003
    ;months=indgen(12)+1
    testMonths=[1,8]
    testDays=[5,15,25]
  endif

  ; 1999
  if year eq 1999 then begin
    location1='E:\mariomi\Documents\projects\LDTR\data\input\BRDF\1999\'
    prefix1='GLOBAL_P17_GEOG_0.05DEG_'
    postfix1='*.NOAA-14.hdf'
    location2='E:\mariomi\Documents\projects\LDTR\data\AVHRR\NOAA\surface_refl\1999\result\'
    prefix2='AVHRR-Land_v004_AVH09C1_NOAA-14_'
    postfix2='*.sav'
    outlocation='E:\mariomi\Documents\projects\LDTR\data\result\1999\'
    NOAAString='NOAA-14'
    year1=99
    year2=1999
    testMonths=[7]
    testDays=[5,10,15,20,25]
  endif

  monthNum=n_elements(testMonths)
  daysNum=n_elements(testDays)

  for m=0, monthNum-1 do begin
    ;monthDays=utility->calcDayOfMonth([years[y],months[m],1,0])
    for d=0, daysNum-1 do begin
      thisMonth=testMonths[m]
      thisDay=testDays[d]
      yearDay1=utility->calcDayOfYear([year2,thisMonth,thisDay,0])+1

      file1=location1+prefix1+string(yearDay1, format='(I03)')+'-'+string(yearDay1, format='(I03)')+'_'+string(year1, format='(I02)')+postfix1
      ffile1=file_search(file1, count=count1)
      date='('+string(year2, format='(I4)') + ' - ' + string(thisMonth, format='(I02)')+ ' - ' + string(thisDay, format='(I02)')+')'

      yearDay2=utility->calcDayOfYear([year2,thisMonth,thisDay,0])+1
      file2=location2+prefix2+string(year2, format='(I4)')+string(thisMonth, format='(I02)')+string(thisDay, format='(I02)')+postfix2
      ffile2=file_search(file2, count=count2)

      if count1 eq 1 and count2 eq 1 then begin
        filesplitting=strsplit(ffile1, path_sep(), /EXTRACT)
        file1=filesplitting[n_elements(filesplitting)-1]

        restore, ffile2
        ; final_red_avhrr is red_avhrr from GLOBAL_L3_GEOG_0.05DEG_ with slope and offset applied
        ;idx_ok=where(red_brf gt 0.0 and final_red_avhrr gt 0.0 and red_brf lt 1 and final_red_avhrr lt 1 and, countpx, ncomp=no_idxs)

        ; final_red_avhrr is red_avhrr from GLOBAL_L3_GEOG_0.05DEG_ with slope and offset applied
        read_data, location1, file1, 'QC', qc_avhrr, slope_qc, offset_qc   ; QC
        print, 'previous data from...:', file1
        ;read_data, location1, file1, 'RHO1', red_avhrr, slope, offset
        ;read_data, location1, file1, 'RHO2', nir_avhrr, slope, offset
        ;already saved in sav file, reload and check only to be sure...
        ;real_red_avhrr=red_avhrr*0.0001
        ;real_nir_avhrr=nir_avhrr*0.0001

        ; change here max value accepted...
        maxAcceptedValue=1.0
        ;load mask(s) (makeitglob.pro)
        rr1=cgi_map_bitwise_flag(qc_avhrr,1)
        rr3=cgi_map_bitwise_flag(qc_avhrr,3)
        rr21=cgi_map_bitwise_flag(qc_avhrr,9)
        rr22=cgi_map_bitwise_flag(qc_avhrr,8)

        ; flag-out everything is somehow invalid (coming from flagging in makeitglob)...
        no_idxs_scatter_b1=where((red_brf le 0.0 or final_red_avhrr le 0.0) or (red_brf ge maxAcceptedValue or final_red_avhrr ge maxAcceptedValue) or $
          (rr1 eq 1) or (rr21 eq 1 or rr22 eq 1) or (rr3 eq 1) or ~finite(red_brf) or ~finite(final_red_avhrr), countpx, ncomp=countpx2, COMPLEMENT=idx_ok_scatter_b1)
        
        no_idxs_scatter_b2=where((nir_brf le 0.0 or final_nir_avhrr le 0.0) or (nir_brf ge maxAcceptedValue or final_nir_avhrr ge maxAcceptedValue) or $
          (rr1 eq 1) or (rr21 eq 1 or rr22 eq 1) or (rr3 eq 1) or ~finite(nir_brf) or ~finite(final_nir_avhrr), countpx, ncomp=countpx2, COMPLEMENT=idx_ok_scatter_b2)
       
        no_idxs_b1=where((red_brf le 0.0 or final_red_avhrr le 0.0) or (red_brf ge maxAcceptedValue or final_red_avhrr ge maxAcceptedValue) or $
          ~finite(red_brf) or ~finite(final_red_avhrr), countpx, ncomp=countpx2, COMPLEMENT=idx_ok_b1)

        no_idxs_b2=where((nir_brf le 0.0 or final_nir_avhrr le 0.0) or (nir_brf ge maxAcceptedValue or final_nir_avhrr ge maxAcceptedValue) or $
          ~finite(nir_brf) or ~finite(final_nir_avhrr), countpx, ncomp=countpx2, COMPLEMENT=idx_ok_b2)

        idx_sea=where(rr3 eq 1., COMPLEMENT=idx_land, ncomplement=nland)
        ;apply sea/invalid and outlayer numbers mask(s)
        ; band1
        ;red_brf[no_idxs_b1]=!VALUES.F_NAN
        ;final_red_avhrr[no_idxs_b1]=!VALUES.F_NAN
        red_brf[idx_sea]=!VALUES.F_NAN
        final_red_avhrr[idx_sea]=!VALUES.F_NAN
        diffImg=reform(final_red_avhrr-red_brf, 7200, 3600)
        idx_not_show=where(abs(diffImg) le 0.05)
        
        ; Nadine's images
        loadct,12

        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_Band1_previous'+strcompress(date, /REMOVE_ALL) $
        else window, 1, title=date+' previous - band1'
        cgImage, reverse(congrid(bytscl(final_red_avhrr, min=0.,max=1., /NAN), 720,360),2), /NAN
        if keyword_set(saveFile) then cgPS_Close

        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_Band1_new'+strcompress(date, /REMOVE_ALL) $
        else window, 2, title=date+' new - band1'
        cgImage, reverse(congrid(bytscl(red_brf, min=0.,max=1., /NAN), 720,360),2), /NAN
        if keyword_set(saveFile) then cgPS_Close

        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_Band1_previous_diff_new'+strcompress(date, /REMOVE_ALL) $
        else window, 3, title=date+' diff - band1'
        cgImage, reverse(congrid(bytscl(diffImg, min=0.,max=1., /NAN), 720,360),2), /NAN
        if keyword_set(saveFile) then cgPS_Close

        ; Mirko's images
;        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_new_Band1_BRFs'+strcompress(date, /REMOVE_ALL) $
;        else window, 0, xsize=720, ysize=360, title='Band1 BRFs (new) - "sea" flagging: '+SENSOR+date
;        cgImage, reverse(rebin(bytscl(red_brf, min=0.00001, /NAN), 720,360),2)
;        if keyword_set(saveFile) then cgPS_Close
;
;        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_Band1_previous_BRFs'+strcompress(date, /REMOVE_ALL) $
;        else window, 2, xsize=720, ysize=360, title='Band1 BRFsl (previous) - "sea" flagging: '+SENSOR+date
;        cgImage, reverse(rebin(bytscl(final_red_avhrr, min=0.001,max=maxAcceptedValue, /NAN), 720,360),2), /NAN
;        if keyword_set(saveFile) then cgPS_Close

        ; histo density
        if keyword_set(saveFile) then cgPS_Open, outlocation+'histo_dens_Band1_new_BRFs'+strcompress(date, /REMOVE_ALL) $
        else window, 3, xsize=720, ysize=360, title=' Band1 BRFsl density histo (previous) - "sea" flagging: '+SENSOR+date
        cghistoplot, final_red_avhrr[idx_ok_b1], nbins=15, /NAN
        if keyword_set(saveFile) then cgPS_Close

        if keyword_set(saveFile) then cgPS_Open, outlocation+'histo_dens_Band1_previous_BRFs'+strcompress(date, /REMOVE_ALL) $
        else window, 4, xsize=720, ysize=360, title='Band1 BRFsl density histo (new) "sea" flagging: '+SENSOR+date
        cghistoplot, red_brf[idx_ok_b1], nbins=15, /NAN
        if keyword_set(saveFile) then cgPS_Close

        ;if keyword_set(saveFile) then cgPS_Open, outlocation+'scatter_Band1_previous_vs_new'+strcompress(date, /REMOVE_ALL) $
        rp=reform(red_brf[idx_ok_scatter_b1], n_elements(idx_ok_scatter_b1))
        rn=reform(final_red_avhrr[idx_ok_scatter_b1], n_elements(idx_ok_scatter_b1))

        ; scatter
        if keyword_set(saveFile) then filename=outlocation+'scatter_Band1_previous_vs_new'+strcompress(date, /REMOVE_ALL) $
        else window, 5, xsize=550, ysize=550, title='Scatter Band1 BRFs (previous Vs new): '+SENSOR+date
        ;cgScatter2D, rp, rn, xrange=[0,maxAcceptedValue], yrange=[0,maxAcceptedValue],title='Band1 scatter plot (hard flagging)';, /Isotropic
        if keyword_set(saveFile) then print=1 else print=0
        
        plotscat, rp, rn, max1=maxAcceptedValue, max2=maxAcceptedValue, /STAT, title=strcompress(date, /REMOVE_ALL)+' '+NOAAString+' '+'Band1', $
          xtitle='previous BRFs (Band1)',ytitle='new BRFs (Band1)',print=savefile,filename=filename;, xrange=[0,maxAcceptedValue], yrange=[0,maxAcceptedValue],title='Band1 scatter plot (hard flagging)';, /Isotropic
        if keyword_set(saveFile) then cgPS_Close

        nir_brf[idx_sea]=!VALUES.F_NAN
        final_nir_avhrr[idx_sea]=!VALUES.F_NAN
        diffImg=reform(final_nir_avhrr-nir_brf, 7200, 3600)
        idx_not_show=where(abs(diffImg) le 0.05)
        
        ; Nadine's images (band2)
        loadct,12

        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_Band2_previous'+strcompress(date, /REMOVE_ALL) $
        else window, 1, title=date+' previous - band2'
        cgImage, reverse(congrid(bytscl(final_nir_avhrr, min=0.,max=1., /NAN), 720,360),2), /NAN
        if keyword_set(saveFile) then cgPS_Close

        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_Band2_new'+strcompress(date, /REMOVE_ALL) $
        else window, 2, title=date+' new - band2'
        cgImage, reverse(congrid(bytscl(nir_brf, min=0.,max=1., /NAN), 720,360),2), /NAN
        if keyword_set(saveFile) then cgPS_Close

        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_Band2_previous_diff_new'+strcompress(date, /REMOVE_ALL) $
        else window, 3, title=date+' diff - band2'
        cgImage, reverse(congrid(bytscl(diffImg, min=0.,max=1., /NAN), 720,360),2), /NAN
        if keyword_set(saveFile) then cgPS_Close

        ; Mirko's image
;        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_Band2_new_BRFs'+strcompress(date, /REMOVE_ALL)+'_'+strcompress(sampleDim)+'_samplepoints' $
;        else window, 0, xsize=720, ysize=360, title='Band2 BRFs (new) - soft flagging'+' - '+strcompress(sampleDim)+' samplepoints'+' : '+SENSOR+date
;        ;tv, reverse(rebin(bytscl(nir_brf, min=0.001, max=maxAcceptedValue, /NAN), 720,360),2)
;        cgImage, reverse(rebin(bytscl(nir_brf, min=0.001, max=maxAcceptedValue, /NAN), 720,360),2)
;        if keyword_set(saveFile) then cgPS_Close
;
;        ;window, 1, xsize=720, ysize=360, title='Band2 BRFsl (previous): '+SENSOR
;        ;tv, reverse(rebin(bytscl(real_nir_avhrr, min=0.001, max=1.0, /NAN), 720,360),2)
;
;        if keyword_set(saveFile) then cgPS_Open, outlocation+'image_Band2_previous_BRFs'+strcompress(date, /REMOVE_ALL) $
;        else window, 2, xsize=720, ysize=360, title='Band2 BRFsl (previous) - soft flagging: '+SENSOR+date
;        ;tv, reverse(rebin(bytscl(final_nir_avhrr, min=0.001,max=maxAcceptedValue, /NAN), 720,360),2), /NAN
;        cgImage, reverse(rebin(bytscl(final_nir_avhrr, min=0.001,max=maxAcceptedValue, /NAN), 720,360),2), /NAN
;        if keyword_set(saveFile) then cgPS_Close

        ; histo density
        if keyword_set(saveFile) then cgPS_Open, outlocation+'histo_dens_Band2_new_BRFs'+strcompress(date, /REMOVE_ALL) $
        else window, 3, xsize=720, ysize=360, title='Band2 BRFs density histo (previous) soft flagging: '+SENSOR+date
        cghistoplot, final_nir_avhrr[idx_ok_b2], nbins=15, /NAN
        if keyword_set(saveFile) then cgPS_Close

        if keyword_set(saveFile) then cgPS_Open, outlocation+'histo_dens_Band2_previous_BRFs'+strcompress(date, /REMOVE_ALL) $ $
        else window, 4, xsize=720, ysize=360, title='Band2 BRFs density histo (new) soft flagging: '+SENSOR+date
        cghistoplot, nir_brf[idx_ok_b2], nbins=15, /NAN
        if keyword_set(saveFile) then cgPS_Close

        nir_brf[no_idxs_scatter_b2]=!VALUES.F_NAN
        final_nir_avhrr[no_idxs_scatter_b2]=!VALUES.F_NAN

        if keyword_set(fullData) then sampleDim=n_elements(idx_ok_scatter_b2)-1 else sampleDim=(n_elements(idx_ok_scatter_b2)<sampleDim)-1

        np=reform(nir_brf[idx_ok_scatter_b2[0:sampleDim]], sampleDim+1)
        nn=reform(final_nir_avhrr[idx_ok_scatter_b2[0:sampleDim]], sampleDim+1)

        ; scatter
        if keyword_set(saveFile) then filename=outlocation+'scatter_Band2_previous_vs_new'+strcompress(date, /REMOVE_ALL) $
        else window, 5, xsize=550, ysize=550, title='Scatter Band2 BRFs (previous Vs new)'+' - '+strcompress(sampleDim)+' : '+SENSOR+date
        ;cgScatter2D, np, nn, xrange=[0,maxAcceptedValue], yrange=[0,maxAcceptedValue],title='Band2 scatter plot (hard flagging)'
        plotscat, np, nn, max1=maxAcceptedValue, max2=maxAcceptedValue, /STAT, $
          xtitle='previous BRFs (Band2)',ytitle='new BRFs (Band2)',rcol=10, title=strcompress(date, /REMOVE_ALL)+' '+NOAAString+' '+'Band2', $, $
          print=savefile,filename=filename;, xrange=[0,maxAcceptedValue], yrange=[0,maxAcceptedValue],title='Band1 scatter plot (hard flagging)';, /Isotropic
        if keyword_set(saveFile) then cgPS_Close

      endif
      ;do scatter here

    endfor
  endfor

  print, 'done'
  a=dialog_message('done', /INFO)

end