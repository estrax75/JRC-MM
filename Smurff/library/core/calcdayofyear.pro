; day in the format lonarr[4] where
; [0] year, [1] month, [2] day, [3] hour
function calcDayOfYear, date, outHour=outHour

  jDay = julday(date[1], date[2], date[0], date[3])
  jDayFirst = julday(1, 1, date[0], 0)
  ;jDayFirst = julday(1, 1, date[0], 0)
  dayDiff=fix(jDay-jDayFirst)
  outHour=fix((dayDiff-fix(jDay-jDayFirst))*24)
  return, dayDiff

end