pro get_current_date, year=year, month=month, day=day, dayoftheyear=dayoftheyear, dayoftheweek=dayoftheweek

  ;DOW MON DD HH:MM:SS YEAR
  systemdate = SYSTIME(0)
  dayoftheyear = STRMID(systemdate, 0, 3)
  dayoftheweek_name = STRMID(systemdate, 0, 3)
  day = STRMID(systemdate, 9, 2)
  month_name = STRMID(systemdate, 4, 3)
  year = STRMID(systemdate, 20, 4)

  dow_name_to_dow, dayoftheweek_name, dayoftheweek 
  month_name_to_month, month_name, month
  ;PRINT, 'year: ', year
  ;PRINT, 'month: ', month
  ;PRINT, 'month_name: ', month_name
  ;PRINT, 'dayoftheweek: ', dayoftheweek
  ;PRINT, 'dayoftheweek_name: ', dayoftheweek_name

end

pro month_length, yyyy, mm, length
  month_length = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  if (yyyy mod 4 eq 0) then month_length(1) = 29
  if (yyyy mod 100 eq 0) then month_length(1) = 28
  if (yyyy mod 400 eq 0) then month_length(1) = 29
  length = month_length(mm - 1)
end

function year_length, yyyy
  month_length = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  if (yyyy mod 4 eq 0) then month_length(1) = 29
  if (yyyy mod 100 eq 0) then month_length(1) = 28
  if (yyyy mod 400 eq 0) then month_length(1) = 29
  return, fix(total(month_length))
end

pro day_of_year_to_day_of_month, yyyy, ddd, mm, dd
  month_names = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  month_length = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  if (yyyy mod 4 eq 0) then month_length(1) = 29
  if (yyyy mod 100 eq 0) then month_length(1) = 28
  if (yyyy mod 400 eq 0) then month_length(1) = 29
  month_sum = intarr(13)
  month_sum(0) = 0
  for kmonth = 1, 12 do begin
    month_sum(kmonth) = month_sum(kmonth-1) + month_length(kmonth-1)
  endfor
  mm = 1
  while (ddd gt month_sum(mm)) do begin
    mm = mm + 1
  endwhile
  dd = ddd - month_sum(mm-1)
  yyyy_str = string(format = '(i4)', yyyy)
  ddd_str = string(format = '(i3.3)', ddd)
  dd_str = string(format = '(i2.2)', dd)
end

pro day_of_month_to_day_of_year, yyyy, mm, dd, ddd, year_frac = year_frac
  month_length = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  if (yyyy mod 4 eq 0) then month_length(1) = 29
  if (yyyy mod 100 eq 0) then month_length(1) = 28
  if (yyyy mod 400 eq 0) then month_length(1) = 29
  ddd = 0
  for kmonth = 1, mm - 1 do begin
    ddd = ddd + month_length(kmonth-1)
  endfor
  ddd = ddd + dd
  ndays = total(month_length)
  year_frac = float(ddd) / ndays
end

pro advance_date, yyyyi, mmi, ddi, yyyyf, mmf, ddf, ndays
  juliani = julday(mmi, ddi, yyyyi)
  julianf = juliani + ndays
  caldat, julianf, mmf, ddf, yyyyf
end

pro extract_date, yyyymmdd, yyyy, mm, dd
  yyyy = yyyymmdd / 10000
  mm = ( yyyymmdd - 10000 * yyyy ) / 100
  dd = yyyymmdd - 10000 * yyyy - 100 * mm
end

function days_since_2000, yyyymmdd
  extract_date, yyyymmdd, yyyy, mm, dd
  return, julday(mm, dd, yyyy) - julday(1, 1, 2000) + 1
end

pro advance_packed_date, yyyymmddi, yyyymmddf, ndays
  extract_date, yyyymmddi, yyyyi, mmi, ddi
  advance_date, yyyyi, mmi, ddi, yyyyf, mmf, ddf, ndays
  yyyymmddf = 10000 * yyyyf + 100 * mmf + ddf
end

function difference_packed_dates, yyyymmdd, yyyymmdd_
  extract_date, yyyymmdd, yyyy, mm, dd
  extract_date, yyyymmdd_, yyyy_, mm_, dd_
  return, julday(mm, dd, yyyy) - julday(mm_, dd_, yyyy_)
end

pro print_date, yyyy, mm, dd
  month_names = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  print, format = '(i4, x, a, x, i2)', yyyy, month_names(mm-1), dd
end

;pro month_name_to_month, mm_str, mm
;  month_names = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
;    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
;  month_names_upper = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', $
;    'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
;  month_names_lower = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', $
;    'jul', 'aug', 'sep', 'oct', 'nov', 'dec']
;  nmonth = n_elements(month_names)
;  for kmonth = 0, nmonth - 1 do begin
;    if (mm_str eq month_names(kmonth) or $
;      mm_str eq month_names_upper(kmonth) or $
;      mm_str eq month_names_lower(kmonth)) then begin
;      mm = kmonth + 1
;    endif
;  endfor
;end

pro month_name_to_month, mm_str, mm
  month_names = strarr(12, 2)
  dims=size(month_names, /dim)
  
  month_names[*,0]=strupcase(['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'])
  month_names[*,1] = strupcase(['Gen', 'Feb', 'Mar', 'Apr', 'Mag', 'Giu', $
    'Lug', 'Ago', 'Set', 'Ott', 'Nov', 'Dic'])
    
  nmonth = n_elements(month_names[*,0])
  
  idx=(where(strupcase(mm_str) eq month_names, count))[0]
  if count gt 0 then mm = (idx mod dims[0])+1 else mm = 0
  ;print, idx, mm
  
end

pro dow_name_to_dow, dow_str, dow
  dow_names = strarr(7, 2)
  dims=size(dow_names, /dim)
  
  dow_names[*,0]=strupcase(['Lun', 'Mar', 'Mer', 'Gio', 'Ven', 'Sab', $
    'Dom'])
  dow_names[*,1] = strupcase(['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', $
    'Sun'])
    
  nmonth = n_elements(dow_names[*,0])
  
  idx=(where(strupcase(dow_str) eq dow_names, count))[0]
  if count gt 0 then dow = (idx mod dims[0])+1 else dow = 0
  ;print, idx, dow
  
end