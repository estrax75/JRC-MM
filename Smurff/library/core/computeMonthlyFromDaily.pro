; remove Rrs > 0 check
; remove Rrs > 0 plots
; minor graphics text fixing
; add multiple specific flag check (glint, HiSatZen, ice/cloud)
function computeMonthlyFromDaily, operator, roiNo, singleStatToApply, periodtype, $
  yearList, monthList, $
  checkTimeSeriesData, foundTimeSeriesData, $
  sensor, outputDir, utility, DUPLICATE=DUPLICATE

  histoFolder='histoFinal'

  parNum=size(checkTimeSeriesData, /DIM)
  parNum=parNum[1]
  monthlyBinsNumber=n_elements(yearList)*n_elements(monthList)
  yearNo=n_elements(yearList)
  monthNo=n_elements(monthList)

  foundMonthlyTimeSeriesData=fltarr(roiNo,  monthlyBinsNumber)

  ; This procedure create only 1 (one) specific computation. You can set more than one parameter
  reportMonthlyTimeSeriesDataCheck=fltarr(roiNo, parNum,  monthlyBinsNumber)

  for k=0, roiNo-1 do begin
    dayCount=0
    timeIndex=0
    ;subscribeDayCount=0
    ;for dd=0, keyword_set(DUPLICATE) do begin
    ;subscribeDayCount++
    for m=0, yearNo-1 do begin
      year=yearList[m]
      for n=0, monthNo-1 do begin
        month=monthList[n]
        monthDays=utility->calcDayOfMonth([fix(year),fix(month),1,0])
        if keyword_set(DUPLICATE) then begin
          monthDays=monthDays*2
        endif
        ; only perform a sum...

        foundDays=foundTimeSeriesData[k, dayCount:dayCount+monthDays-1,0]
        res1=total(foundDays, /NAN)
        foundMonthlyTimeSeriesData[k, timeIndex]=res1

        checkComplement=[-1]
        for l=0, parNum-1 do begin
          monthlyData=checkTimeSeriesData[k, l, dayCount:dayCount+monthDays-1]
          res2=total(monthlyData, /NAN)
          idx=where(l eq checkComplement, count)
          if count eq 1 then reportMonthlyTimeSeriesDataCheck[k, l, timeIndex]=res1-res2 else reportMonthlyTimeSeriesDataCheck[k, l, timeIndex]=res2 
        endfor

        if foundMonthlyTimeSeriesData[k, timeIndex] eq 0 then begin
          foundMonthlyTimeSeriesData[k, timeIndex]=!VALUES.F_NAN
        endif
        dayCount=dayCount+monthDays
        timeIndex++
      endfor
    endfor
  endfor

  monthlyData={ reportMonthlyTimeSeriesDataCheck:reportMonthlyTimeSeriesDataCheck, $
    foundMonthlyTimeSeriesData:foundMonthlyTimeSeriesData}

  return, monthlyData

end