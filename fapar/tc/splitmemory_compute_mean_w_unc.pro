PRO sm_compute_mean_unc, daysNumber, data_day_f, data_tc, nSlice
  ;
  ;
  ;
  ; data_day = products for each day and each pixels
  ;
  ; data_tc = time-composite results
  ;
  ;full data
  data_tc= {Composite1, day: bytarr(7200,3600), $
    nday: bytarr(7200,3600), $
    fapar: fltarr(7200,3600), $
    red: fltarr(7200,3600), $
    nir: fltarr(7200,3600), $
    flag: bytarr(7200,3600) $
  }

  ; initiate flag to sea mask
  ; water/Nan by default
  data_tc.flag[*,*]=3
  validIdx=where(data_day_f.fdir ne '', count)
  data_day_t=data_day_f[validIdx]
  daysNumber=count
  print, 'In pure mean section ...'
  starttime=systime(1)

  xSplitDim=7200/nSlice
  ySplitDim=3600; full dim,

  tt=[0, daysNumber]

  for slice=0, nSlice-1 do begin

    subXStart=slice*xSplitDim & subXEnd=(slice+1)*xSplitDim-1
    subYStart=0 & subYEnd=3600-1
    ;subYStart=slice*xSplitDim & subYEnd=(slice+1)*ySplitDim-1

    ;    fullData={  fapar:fapar, sigma:sigma, $
    ;      red:red, sigma_red:sigma_red, $
    ;      nir:nir, sigma_nir:sigma_nir, $
    ;      qa:qa, $
    ;      ts:ts, tv:tv, phi:phi, $
    ;      toc_red:toc_red, toc_nir:toc_nir, $
    ;      flag:flag, $
    ;      slope_fapar:slope_fapar,slope_sigma:slope_sigma,$
    ;      slope_nir:slope_nir,slope_red:slope_red,$
    ;      slope_qa:slope_qa, slope_flag:slope_flag, $
    ;      slope_toc_nir:slope_toc_nir,slope_toc_red:slope_toc_red,$
    ;      offset_fapar:offset_fapar,offset_sigma:offset_sigma,$
    ;      offset_nir:offset_nir,offset_red:offset_red,$
    ;      offset_toc_nir:offset_toc_nir, offset_toc_red:offset_toc_red,$
    ;      offset_qa:offset_qa, offset_flag:offset_flag, $
    ;      day:qa, valid:0}

    ; initialize this slice (overwriting previous...)
    data_day_split={  fapar:fltarr(xSplitDim,ySplitDim), $
      red:fltarr(xSplitDim,ySplitDim), $
      nir:fltarr(xSplitDim,ySplitDim), $
      flag:bytarr(xSplitDim,ySplitDim), valid:0}

    data_tc_split={  fapar:fltarr(xSplitDim,ySplitDim), $
      red:fltarr(xSplitDim,ySplitDim), $
      nir:fltarr(xSplitDim,ySplitDim), $
      flag:bytarr(xSplitDim,ySplitDim), valid:0}

    data_day_split=replicate(data_day_split, daysNumber)

    for t=0, tt[1]-1 do begin
      ;restore, data_day_f[t].data_file
      if data_day_f[t].fid gt 0 then faparData=read_AVHRR_FAPAR(data_day_f[t].fDir, data_day_f[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=data_day_f[t].fid) $
        else faparData=read_AVHRR_FAPAR(data_day_f[t].fDir, data_day_f[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=fid) 
      ;if fid ne -1 then data_day_f[t].fid=fid
      print, 'reading day...', t+1, '/', tt[1] 
      data_day_split[t].fapar=faparData.fapar
      data_day_split[t].red=faparData.red
      data_day_split[t].nir=faparData.nir
      data_day_split[t].flag=faparData.flag
      idx_mask = where(data_day_split[t].flag eq 0.0 and data_day_split[t].fapar gt 0.0 and $
        data_day_split[t].red gt 0.0 and data_day_split[t].red lt 1.0 and $
        data_day_split[t].nir gt 0.0 and data_day_split[t].nir lt 1.0, ncomplement=ncomplement, complement=complement)
      data_day_split[t].fapar[complement]=!VALUES.F_NAN
      data_day_split[t].nir[complement]=!VALUES.F_NAN
      data_day_split[t].red[complement]=!VALUES.F_NAN
      faparData=0
    endfor
    ;
    ;
    ;==========================================================================================
    n_elems=n_elements(data_day_split[0].red)
    nPics=n_elements(data_day_split)
    ;percs=(dindgen(10)*7200*3600)/(7200d*3600)*10
    ;percs=(dindgen(10)/10.*7200/nSlice*3600/nSlice)
    for i=0l, n_elems-1 do begin
      data_tc_split.fapar[i]=mean(data_day_split[*].fapar[i], /NAN)
      data_tc_split.nir[i]=mean(data_day_split[*].nir[i], /NAN)
      data_tc_split.red[i]=mean(data_day_split[*].red[i], /NAN)
      array=data_day_split[*].flag[i]
      ;flagArr=array[UNIQ(array, SORT(array))]
      ;checkWater=where(data_day[*].flag[i] eq 3, countWater) ; water: input 3 --> res 6
      ;aa=where(i eq percs, countPerc)
      ;if countPerc eq 1 then print, 'progress...', string(percs[aa[0]]/(7200d*3600)*100, format='(I02)'), '% ...'
      checkBareSoil=where(data_day_split[*].flag[i] eq 6, countBareSoil) ; bare soil: input 6 --> res 4
      checkClouds=where(data_day_split[*].flag[i] eq 2, countClouds) ; clouds: input 2 --> res 2
      checkVegetation=where(data_day_split[*].flag[i] eq 0, countVegetation) ; vegetation: input 0 --> res 0
      if countVegetation gt nPics/2 then data_tc_split.flag[i]=0
      if countClouds gt nPics/2 then data_tc_split.flag[i]=2
      if countBareSoil gt nPics/2 then data_tc_split.flag[i]=4
    endfor
    print, 'compute slice...', slice+1, '/', nSlice
    data_tc.flag[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.flag[*,*]
    data_tc.fapar[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.fapar[*,*]
    data_tc.nir[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.nir[*,*]
    data_tc.red[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.red[*,*]
  endfor
  endTime=systime(1)-starttime
  print, 'computed in about:', strcompress(endTime), 'seconds'
  window, 1, title='average '+strcompress(daysNumber, /remove)+' days: fapar'
  tv, rebin(bytscl(data_tc.fapar, /NAN), 720, 360) ;*
  window, 2, title='average '+strcompress(daysNumber, /remove)+' days: red'
  tv, rebin(bytscl(data_tc.red, /NAN), 720, 360) ;*
  window, 3, title='average '+strcompress(daysNumber, /remove)+'days: nir'
  tv, rebin(bytscl(data_tc.nir, /NAN), 720, 360) ;*
  notValidIdxs=where(~finite(data_tc.fapar), count)
  data_tc.fapar[notValidIdxs]=2^15
  notValidIdxs=where(~finite(data_tc.nir), count)
  data_tc.nir[notValidIdxs]=2^15
  notValidIdxs=where(~finite(data_tc.red), count)
  data_tc.red[notValidIdxs]=2^15

end
;========================================================================================================
