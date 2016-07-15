PRO compute_mean_memory_split, daysNumber, data_day, data_tc, tempDir
  ;
  ;
  ;
  ; data_day = products for each day and each pixels
  ;
  ; data_tc = time-composite results
  ;
  validIdx=where(data_day.valid eq 1, count)
  data_day=data_day[validIdx]
  daysNumber=count
  print, 'In pure mean section ...'
  ;
  ;tt=size(data_day.day)
  tt=[0, daysNumber]
  ;
  ;
  dims=size(data_day[0])
  dims=[7200,3600]
  dimMod1=dims[0] mod 3 & dim1=dims[0] / 3
  dimMod2=dims[1] mod 3 & dim2=dims[1] / 3


  index_to_subcribe=lonarr(3,3)
  for i=0, 2 do begin
    for j=0, 2 do begin
      index_to_subcribeXFrom=1.*i*dim0
      index_to_subcribeYFrom=1.*j*dim1
      index_to_subcribeXTo=index_to_subcribeX+dim0-1
      index_to_subcribeYTo=index_to_subcribeY+dim1-1
      iSubXFrom=index_to_subcribeXFrom
      iSubXTo=index_to_subcribeYFrom
      iSubYFrom=index_to_subcribeXTo
      iSubYTo=index_to_subcribeYTo
      print, 'x from:', index_to_subcribeX, 'y from:', index_to_subcribeY
      print, 'x to:', , 'y to:',
      ;[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo], 

      data_tc= {day: bytarr(dim0,dim1), $
        nday: bytarr(dim0,dim1), $
        fapar: fltarr(dim0,dim1), $
        red: fltarr(dim0,dim1), $
        nir: fltarr(dim0,dim1), $
        flag: bytarr(dim0,dim1) $
      }
      ; initiate flag to sea mask
      ;
      data_tc.flag(*,*)=6
      ;
      ;==========================================================================================
      for t=0, tt(1)-1 do begin
        idx_mask = where(data_day(t).flag[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo] eq 0.0 and data_day(t).fapar[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo] gt 0.0 and $
          data_day(t).red[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo] gt 0.0 and data_day(t).red[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo] lt 1.0 and $
          data_day(t).nir[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo] gt 0.0 and data_day(t).nir[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo] lt 1.0, ncomplement=ncomplement, complement=complement)
        data_day(t).fapar[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo][complement]=!VALUES.F_NAN
        data_day(t).nir[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo][complement]=!VALUES.F_NAN
        data_day(t).red[iSubXFrom:iSubXTo, iSubYFrom:iSubYTo][complement]=!VALUES.F_NAN
      endfor

      starttime=systime(1)
      n_elems=n_elements(data_day[0].fapar)
      nPics=n_elements(data_day)
      ;percs=(dindgen(10)*7200*3600)/(7200d*3600)*10
      ;percs=(dindgen(10)/10.*7200*3600)
      for i=0l, n_elems-1 do begin
        data_tc.fapar[i]=mean(data_day[*].fapar[i], /NAN)
        data_tc.nir[i]=mean(data_day[*].nir[i], /NAN)
        data_tc.red[i]=mean(data_day[*].red[i], /NAN)
        array=data_day[*].flag[i]
        ;flagArr=array[UNIQ(array, SORT(array))]
        ;checkWater=where(data_day[*].flag[i] eq 3, countWater) ; water: input 3 --> res 6
        aa=where(i eq percs, countPerc)
        if countPerc eq 1 then print, 'progress...', string(percs[aa[0]]/(7200d*3600)*100, format='(I02)'), '% ...'
        checkBareSoil=where(data_day[*].flag[i] eq 6, countBareSoil) ; bare soil: input 6 --> res 4
        checkClouds=where(data_day[*].flag[i] eq 2, countClouds) ; clouds: input 2 --> res 2
        checkVegetation=where(data_day[*].flag[i] eq 0, countVegetation) ; vegetation: input 0 --> res 0
        ; water/Nan by default
        data_tc.flag[i]=6
        if countVegetation gt nPics/2 then data_tc.flag[i]=0
        if countClouds gt nPics/2 then data_tc.flag[i]=2
        if countBareSoil gt nPics/2 then data_tc.flag[i]=4
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
      data_tc.fapar[notValidIdxs]=-9999
      notValidIdxs=where(~finite(data_tc.nir), count)
      data_tc.nir[notValidIdxs]=-9999
      notValidIdxs=where(~finite(data_tc.red), count)
      data_tc.red[notValidIdxs]=-9999
      save, data_tc, fileName=''
    endfor
  endfor


end
;========================================================================================================
