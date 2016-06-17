PRO compute_mean, daysNumber, data_day, data_tc
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
  data_tc= {Composite1, day: bytarr(7200,3600), $
    nday: bytarr(7200,3600), $
    fapar: fltarr(7200,3600), $
    red: fltarr(7200,3600), $
    nir: fltarr(7200,3600), $
    flag: bytarr(7200,3600) $
}
  ;
  ; initiate flag to sea mask
  ;
  data_tc.flag(*,*)=6
  ;
  ;==========================================================================================
  for t=0, tt(1)-1 do begin
    idx_mask = where(data_day(t).flag(*,*) eq 0.0 and data_day(t).fapar(*,*) gt 0.0 and $
      data_day(t).red(*,*) gt 0.0 and data_day(t).red(*,*) lt 1.0 and $
      data_day(t).nir(*,*) gt 0.0 and data_day(t).nir(*,*) lt 1.0, ncomplement=ncomplement, complement=complement)
    data_day(t).fapar[complement]=!VALUES.F_NAN
    data_day(t).nir[complement]=!VALUES.F_NAN
    data_day(t).red[complement]=!VALUES.F_NAN
  endfor

  starttime=systime(1)
  n_elems=n_elements(data_day(0).fapar)
  for i=0, n_elems-1 do begin
    data_tc.fapar[i]=mean(data_day[*].fapar[i], /NAN)
    data_tc.nir[i]=mean(data_day[*].nir[i], /NAN)
    data_tc.red[i]=mean(data_day[*].red[i], /NAN)
  endfor
  endTime=systime(1)-starttime
  print, 'computed in about:', strcompress(endTime), 'seconds'
  window, 1, title='average 16 days: fapar'
  tv, rebin(bytscl(data_tc.fapar, /NAN), 720, 360) ;*
  window, 2, title='average 16 days: red'
  tv, rebin(bytscl(data_tc.red, /NAN), 720, 360) ;*
  window, 3, title='average 16 days: nir'
  tv, rebin(bytscl(data_tc.nir, /NAN), 720, 360) ;*
  notValidIdxs=where(~finite(data_tc.fapar), count)
  data_tc.fapar[notValidIdxs]=-9999
  notValidIdxs=where(~finite(data_tc.nir), count)
  data_tc.nir[notValidIdxs]=-9999
  notValidIdxs=where(~finite(data_tc.red), count)
  data_tc.red[notValidIdxs]=-9999

end
;========================================================================================================
