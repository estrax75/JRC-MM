;FUNCTION Utility::optimalFormatNumberV1, number, format=format
;
;  decimal=number-long(number)
;  fixPart=(long(alog(number)) > 0) + 2
;  dec=0
;  exitCycle=decimal eq 0 ? 1 : 0
;  while ~(exitCycle) do begin
;    decimal=decimal*10
;    decimal=decimal-ceil(long(decimal))
;    dec++
;    if decimal eq 0 then exitCycle=1
;  ;if decimal lt 1 then exitCycle=1
;  endwhile
;
;  format='(F'+strcompress(fixPart+dec, /REMOVE)+'.'+strcompress(dec, /REMOVE)+')'
;  num=STRING(number, FORMAT=format)
;
;  return, num
;
;END

FUNCTION Utility::calcDays, yearList, monthList

  date=intarr(4)
  totDays=0l
  for i=0, n_elements(yearList)-1 do begin
    thisYear=yearList[i]
    for j=0, n_elements(monthList)-1 do begin
      thisMonth=monthList[j]
      date[1]=fix(thisMonth)
      date[0]=fix(thisYear)
      days=self->calcDayOfMonth(date)
      ;print, days
      totDays=totdays+days
    endfor
  endfor
  return, totDays

END

FUNCTION Utility::optimalLinearFormatNumber, number, format=format

  testFix=alog(number)
  intDigit=(finite(testFix) and abs(number) ge 1) ? ceil(testFix) : 0
  decValue=number-fix(number)
  testDec=alog(decValue)
  decDigit=finite(testDec) ? abs(testDec) : 0
  testVal=decValue
  decTest=strcompress(decValue, /REMOVE)
  decPos=strpos(strmid(decTest, 2, 100), '0')
  decDigit=(decPos > 0) < 3
  num=strnsignif(number, (intDigit+decDigit) > 1)
  return, num

END

FUNCTION Utility::optimalLogFormatNumber, number, format=format

  strNumber=strcompress(float(number), /REMOVE_ALL)
  decimal=(strsplit(strNumber, '.', /EXTRACT))[1]
  fixPart=(long(alog(number)) > 0) + 2
  dec=0
  ;nineClose=0
  exitCycle=decimal eq 0 ? 1 : 0
  ; here manage log scale
  while ~(exitCycle) do begin
    zeroPos=strpos(decimal, '0')
    ninePos=strpos(decimal, '9')
    decimal=strmid(decimal, zeroPos+1, 100)
    dec++
    if zeroPos ne 0 then exitCycle=1
    if ninePos eq 1 then dec++
    ;if nineClose gt 1 then exitCycle=1
    ;if decimal lt 1 then exitCycle=1
  endwhile

  format='(F'+strcompress(fixPart+dec, /REMOVE)+'.'+strcompress(dec, /REMOVE)+')'
  num=STRING(number, FORMAT=format)
  doLog, number, format, '-->', num, LEVEL=4

  return, num

END

FUNCTION Utility::optimalFormatNumber, number, format=format, LOGSCALE=LOGSCLAE


  if keyword_set(LOGSCALE) then return, self->optimalLogFormatNumber(number, format=format) else $
    return, self->optimalLinearFormatNumber(number, format=format)

END

FUNCTION Utility::buildYearMonthTicks, yearList, monthList, MONTH_3_LETTER

  thickList=strarr(n_elements(yearList)*n_elements(monthList))
  k=0
  for i=0, n_elements(yearList)-1 do begin
    for j=0, n_elements(monthList)-1 do begin
      thickList[k]=string(format='(I02, "-", I4)', monthList[j], yearList[i])
      k++
    endfor
  endfor
  return, thickList

END

FUNCTION Utility::sortArray, arrayIn, sortArray=sortArray, DESCEND=DESCEND

  sortArray=sort(arrayIn)
  if keyword_set(DESCEND) then sortArray=reverse(sortArray)
  result=arrayIn[sortArray]
  return, result

END

;**************************************************************
;Geographic section
;**************************************************************


FUNCTION Utility::getRandomNumber1to100, STRING=STRING

  num=fix(randomu(a)*100)
  if keyword_set(STRING) then return, strcompress(num,/remove) else return, num

END

FUNCTION Utility::convertMonthNumberToName, monthNumber, FULLNAME=FULLNAME, NOUPPERCASE=NOUPPERCASE

  months=['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
  if ~keyword_set(NOUPPERCASE) then months=strupcase(months)
  if keyword_set(FULLNAME) then finalIndex=strlen(months[fix(monthNumber)-1]) else finalIndex=3
  return, strmid(months[monthNumber-1], 0, finalIndex)

END

FUNCTION Utility::convertMonthNumber, month, FULLNAME=FULLNAME, NOUPPERCASE=NOUPPERCASE, TONAME=TONAME, TONUMBER=TONUMBER

  monthsName=['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
  monthsNumber=indgen(12)+1
  if ~keyword_set(NOUPPERCASE) then months=strupcase(months)
  if keyword_set(FULLNAME) then finalIndex=strlen(months[fix(monthNumber)-1]) else finalIndex=3
  if keyword_set(TONAME) then idx=where(month eq monthsNumber)
  if keyword_set(TONUMBER) then idx=where(month eq monthsNumber)
  return,  month

END

FUNCTION Utility::doProjection, projInfo, xS, yS

  projInfo.sourceCode=0
  projInfo.destCode=0

  if projInfo.sourceCode eq 0 and projInfo.destCode eq 1 then destXY=coordLambertToLatLon(xy, pars)
  if projInfo.sourceCode eq 1 and projInfo.destCode eq 0 then destXY=coordLatLonToLambert(xy, pars)


  if projInfo.sourceCode ne 0 then begin
    mapInfo=map_proj_init(projInfo.sourceCode)
    latLon=map_proj_inverse( xS, yS, map_structure=mapInfo) ; array (2,n)
  endif else begin
    latLon=fltarr(2, n_elements(xS))
    latLon[0,*]=xS & latLon[1,*]=yS
  endelse

  if projInfo.destCode ne 0 then begin
    mapInfo=map_proj_init(projInfo.destCode)
    destXY=map_proj_forward(latLon, map_structure=mapInfo) ;array (2,n)
  endif else begin
    destXY=latLon
  endelse

  return, destXY

END

FUNCTION Utility::getprojNum, name

  name=strupcase(name)
  projnums=[indgen(19)+1,indgen(29)+101,131,208,220,222]
  projnames=['Stereographic',$;IDL 1
    'Orthographic',$;IDL 2
    'Lambert Conic',$;IDL 3
    'Lambert Azimuthal',$;IDL 4
    'Gnomonic',$;IDL 5
    'Azimuthal Equidistant',$;IDL 6
    'Satellite',$;IDL 7
    'Cylindrical',$;IDL 8
    'Mercator',$;IDL 9
    'Mollweide',$;IDL 10
    'Sinusoidal',$;IDL 11
    'Aitoff',$;IDL 12
    'Hammer Aitoff',$;IDL 13
    'Albers Equal Area Conic',$;IDL 14
    'Transverse Mercator',$;IDL 15
    'Miller Cylindrical',$;IDL 16
    'Robinson',$;IDL 17
    'Lambert Ellipsoid Conic',$;IDL 18
    'Goodes Homolosine',$;IDL 19
    'UTM',$;GCTP 101
    'State Plane',$;GCTP 102
    'Albers Equal Area',$;GCTP 103
    'Lambert Conformal Conic',$;GCTP 104
    'Mercator',$;GCTP 105
    'Polar Stereographic',$;GCTP 106
    'Polyconic',$;GCTP 107
    'Equidistant Conic A',$;GCTP 108
    'Transverse Mercator',$;GCTP 109
    'Stereographic',$;GCTP 10
    'Lambert Azimuthal',$;GCTP 111
    'Azimuthal',$;GCTP 112
    'Gnomonic',$;GCTP 113
    'Orthographic',$;GCTP 114
    'Near Side Perspective',$;GCTP 115
    'Sinusoidal',$;GCTP 116
    'Equirectangular',$;GCTP 117
    'Miller Cylindrical',$;GCTP 118
    'Van der Grinten',$;GCTP 119
    'Hotine Oblique Mercator A',$ ;GCTP 120
    'Robinson',$ ;GCTP 121
    'Space Oblique Mercator A',$ ;GCTP 122
    'Alaska Conformal',$ ;GCTP 123
    'Interrupted Goode',$;GCTP 124
    'Mollweide',$ ;GCTP 125
    'Interrupted Mollweide',$ ;GCTP 126
    'Hammer',$;GCTP 127
    'Wagner IV',$ ;GCTP 128
    'Wagner VII',$ ;GCTP 129
    ;    'Oblated Equal Area',$ ;GCTP 130
    'Integerized Sinusoidal'] ;GCTP 131
  ;    'Equidistant Conic B',$ ;GCTP 208
  ;    'Hotine Oblique Mercator B',$ ;GCTP 220
  ;    'Space Oblique Mercator B'] ;GCTP 222
  projnames=strupcase(projnames)
  idx=(where(projnames eq name))[0]
  if idx ne -1 then projN=projnums[idx] else projN=projnums[0]
  return, projN

END

;**************************************************************
;Graphic section
;**************************************************************

FUNCTION Utility::getGraphicBackgroundColor

  return, getColor(235,235,235)

END

FUNCTION Utility::getGraphicPenColor

  return, getColor(100,100,100)

END

FUNCTION Utility::getButtonLabelFont

  return, "COURIER*BOLD*9"

END

FUNCTION Utility::getButtonMediumFont

  return, "TIMES*BOLD*8"

END

FUNCTION Utility::getCommentLabelFont

  return, "TIMES*20*ITALIC"

END

FUNCTION Utility::getStandardLabelFont

  return, "COURIER*8"

END

FUNCTION Utility::getStandardTextFont

  return, "COURIER*8"

END

FUNCTION Utility::getTitleTextFont

  return, "COURIER*BOLD*ITALIC*12"

END

FUNCTION Utility::colorObjLong64, colorObjs

  nElem=n_elements(colorObjs)
  destColors=lon64arr(nElem)

  for i=0, nElem-1 do begin
    destColors[i]=colorObjs[i]->asLongTrueColor()
  endfor

  return, destColors

END

FUNCTION Utility::colorObjToByte, colorObjs

  nElem=n_elements(colorObjs)
  destColors=bytarr(nElem, 3)

  for i=0, nElem-1 do begin
    colorObjs[i]->getRGB, r, g, b
    destColors[i,*]=[r, g, b]
  endfor

  return, destColors

END

FUNCTION Utility::updateColorTable, limits, leaveColor=leaveColor, colors=colors

  if keyword_set(leaveColor) then begin
    usedColors=colors
  endif else begin
    ;stretch demo color table
    indexes=30
    step=255./(indexes-1)
    demoIdxs=indgen(indexes)*(step)
    ;	demoIdxs=findgen(30)*206/30
    usedColors=objarr(30)
    byteColors=self->getColor(indexes=demoIdxs, /THREE)
    for i=0, 29 do usedColors[i]=obj_new('Color', byteColors[i,0], byteColors[i,1], byteColors[i,2])
  endelse
  if limits[1] gt 1e10 then limits[1]=abs(limits[0])*10

  nClasses=n_elements(usedColors)
  clss = replicate({ScaleClasses, vMin : 0d, vMax : 0d, label : '', gDesc : obj_new()}, nClasses)
  newLimits=findgen(nClasses)*(limits[1]-limits[0])/float(nClasses)+limits[0]
  for i=1, nClasses-1 do begin
    clss[i].vMin = newLimits[i-1]
    clss[i].vMax = newLimits[i]
    clss[i].label = strtrim(newLimits[0], 1)
    clss[i].gDesc = usedColors[i]
  endfor
  clss[0].vMin = newLimits[0]-9999
  clss[0].vMax = newLimits[0]
  clss[0].label = strtrim(newLimits[0], 1)
  clss[0].gDesc = usedColors[0]
  return, clss

END

FUNCTION Utility::buildStandardScaleClasses, limits, colors

  nElem=n_elements(limits)
  clss = replicate({ScaleClasses, vMin : 0d, vMax : 0d, label : '', gDesc : obj_new()}, nElem)
  clss[0].vMin = limits[0]-9999
  clss[0].vMax = limits[0]
  clss[0].label = strtrim(limits[0], 1)
  clss[0].gDesc = obj_new('Color', colors[0,0], colors[0,1], colors[0,2])

  for i=1, n_elements(limits)-1 do begin
    clss[i].vMin = limits[i-1]
    clss[i].vMax = limits[i]
    clss[i].label = strtrim(limits[1], 1)
    clss[i].gDesc = obj_new('Color', colors[i,0], colors[i,1], colors[i,2])
  endfor

  return, clss

END

FUNCTION Utility::getMultiPlotPosition, index, viewNumber, charsize=charsize, labelPos=labelPos

  normpos = fltarr(9,9,4)

  ; one view case
  ;[0]=map index, [1] 0->8=map total, [2:5]=position as xstart, ystart, xend, yend
  normpos[0,0,*] = [0.1, 0.1, .93, .93]

  ; two view case (split horizontal)
  ; first
  normpos[0,1,*] = [0.1, 0.58, .93, .93]
  ; second
  normpos[1,1,*] = [0.1, 0.1, .93, 0.45]

  ; three view case (split horizontal)
  ; first
  normpos[0,2,*] = [0.1, 0.73, .93, .93]
  ; second
  normpos[1,2,*] = [0.1, 0.41, .93, 0.61]
  ; third
  normpos[2,2,*] = [0.1, 0.1, .93, 0.3]

  ; four view case (2X2)
  ; first
  normpos[0,3,*] = [0.1, .58, .45, .93]
  ; second
  normpos[1,3,*] = [0.58, 0.58, .93, .93]
  ; third
  normpos[2,3,*] = [0.1, 0.1, 0.45, 0.45]
  ; fourth
  normpos[3,3,*] = [0.58, 0.1, .93, .45]

  ; five view case (on 3X2 scheme)
  ; first
  normpos[0,4,*] = [0.1, 0.73, .45, .93]
  ; second
  normpos[1,4,*] = [0.58, 0.73, .93, .93]
  ; third
  normpos[2,4,*] = [0.1, 0.41, .45, .61]
  ; fourth
  normpos[3,4,*] = [0.58, 0.41, .93, .61]
  ; fifth
  normpos[4,4,*] = [0.1, 0.1, .45, .3]

  ; six view case (on 3X2 scheme)
  ; first
  normpos[0,5,*] = [0.1, 0.73, .45, .93]
  ; second
  normpos[1,5,*] = [0.58, 0.73, .93, .93]
  ; third
  normpos[2,5,*] = [0.1, 0.41, .45, .61]
  ; fourth
  normpos[3,5,*] = [0.58, 0.41, .93, .61]
  ; fifth
  normpos[4,5,*] = [0.1, 0.1, .45, .3]
  ; sixth
  normpos[5,5,*] = [0.58, 0.1, .93, .3]

  ; seven view case (on 3X3 scheme)
  ; first
  normpos[0,6,*] = [0.1, 0.73, .3, .93]
  ; second
  normpos[1,6,*] = [0.41, 0.73, .61, .93]
  ; third
  normpos[2,6,*] = [0.73, 0.73, .93, .93]
  ; fourth
  normpos[3,6,*] = [0.1, 0.41, .3, .61]
  ; fifth
  normpos[4,6,*] = [0.41, 0.41, .61, .61]
  ; sixth
  normpos[5,6,*] = [0.73, 0.41, .93, .61]
  ; seventh
  normpos[6,6,*] = [0.1, 0.1, .3, .3]

  ; eight view case (on 3X3 scheme)
  ; first
  normpos[0,7,*] = [0.1, 0.73, .3, .93]
  ; second
  normpos[1,7,*] = [0.41, 0.73, .61, .93]
  ; third
  normpos[2,7,*] = [0.73, 0.73, .93, .93]
  ; fourth
  normpos[3,7,*] = [0.73, 0.41, .93, .61]
  ; fifth
  normpos[4,7,*] = [0.41, 0.41, .61, .61]
  ; sixth
  normpos[5,7,*] = [0.1, 0.41, .3, .61]
  ; seventh
  normpos[6,7,*] = [0.1, 0.1, .3, .3]
  ; eigthh
  normpos[7,7,*] = [0.41, 0.1, .61, .3]

  ; nine view case (on 3X3 scheme)
  ; first
  normpos[0,8,*] = [0.1, 0.73, .3, .93]
  ; second
  normpos[1,8,*] = [0.41, 0.73, .61, .93]
  ; third
  normpos[2,8,*] = [0.73, 0.73, .93, .93]
  ; fourth
  normpos[3,8,*] = [0.1, 0.41, .3, .61]
  ; fifth
  normpos[4,8,*] = [0.41, 0.41, .61, .61]
  ; sixth
  normpos[5,8,*] = [0.73, 0.41, .93, .61]
  ; seventh
  normpos[6,8,*] = [0.1, 0.1, .3, .3]
  ; eighth
  normpos[7,8,*] = [0.41, 0.1, .61, .3]
  ; nineth
  normpos[8,8,*] = [0.73, 0.1, .93, .3]
  charSList=[1.5,1.5,1.5,1.5,1.5,1.5,1.,1.,1.]

  charsize=charSList[viewNumber-1]
  windowPos=reform(normPos[index, viewNumber-1, *])
  labelPos=[windowPos[0]+(windowPos[2]-windowPos[0])/10, windowPos[3]-(windowPos[3]-windowPos[1])/7]

  return, windowPos

END

FUNCTION Utility::buildAxisTickMark, xData

  return, strtrim(xData, 2)

END

FUNCTION Utility::buildDateAxisTickMark, startDate, frequency, frameNumber, YEAR=YEAR

  ;start=julday(Month, Day, Year, Hour, Minute, Second)
  if keyword_set(YEAR) and frequency eq '1H' then begin
    frameNumber=8760
    inStartDate=startDate
    inStareDate.year=2001
    inStareDate.month=1
    inStareDate.day=1
    inStareDate.hour=0
    inStareDate.minute=0
    inStareDate.second=0
  endif else begin
    inStartDate=startDate
  endelse

  if inStartDate.year ne 0 then year=inStartDate.year else year=2001
  startCalc=julday(inStartDate.month, inStartDate.day, year, inStartDate.hour, inStartDate.minute, inStartDate.second)
  resDate=strarr(frameNumber)
  dayStep=1.
  hourStep=1./24
  minuteStep=1./(24*60)
  secondStep=1./(24*60*60)
  case strupcase(frequency) of
    'DD-MM':begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*hourStep, month , day, year, hour, minute, second
        ;doLog,month , day, year, hour, minute, second
        ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
        resDate[i]=strtrim(day, 1)+'/'+strtrim(month, 1)
      endfor
    end
    '1H':begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*hourStep, month , day, year, hour, minute, second
        ;doLog,month , day, year, hour, minute, second
        ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
        resDate[i]=strtrim(hour, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
      endfor
    end
    '1D':begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*dayStep, month , day, year, hour, minute, second
        resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
      endfor
    end
    '1M':begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*minuteStep, month , day, year, hour, minute, second
        resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
      endfor
    end
    '1S':begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*secondStep, month , day, year, hour, minute, second
        resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
      endfor
    end
  endcase
  return, resDate

END

FUNCTION Utility::buildBestDateAxisTickMark, startDate, frequency, frameNumber

  ;start=julday(Month, Day, Year, Hour, Minute, Second)
  startCalc=julday(startDate[1], startDate[2], startDate[0], startDate[3], startDate[4], startDate[5])
  resDate=strarr(frameNumber)
  dayStep=1.
  hourStep=1./24
  minuteStep=1./(24*60)
  secondStep=1./(24*60*60)

  if frequency eq '1H' then begin
    stepValue=hourStep
    if frameNumber le 3*24 then template='hh:dd'
    if frameNumber gt 3*24 and frameNumber le 60*24 then template='dd:mm'
    if frameNumber gt 60*24 and frameNumber le 150*24 then template='dd:mm'
    if frameNumber gt 150*24 then template='mm:yyyy'
  endif
  if frequency eq '1D' then begin
    stepValue=dayStep
    if frameNumber le 3 then template='dd'
    if frameNumber gt 3 and frameNumber le 60 then template='dd:mm'
    if frameNumber gt 60 and frameNumber le 150 then template='dd:mm'
    if frameNumber gt 150 then template='mm:yyyy'
  endif

  case template of
    'dd':begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
        ;doLog,month , day, year, hour, minute, second
        ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
        resDate[i]=strtrim(day, 1)
      endfor
    end
    'hh:dd':begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
        ;doLog,month , day, year, hour, minute, second
        ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
        resDate[i]=strtrim(hour, 1)+':'+strtrim(day, 1)
      endfor
    end
    'dd:mm':begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
        ;doLog,month , day, year, hour, minute, second
        ;resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
        resDate[i]=strtrim(day, 1)+':'+strtrim(month, 1)
      endfor
    end
    'mm:yyyy':begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
        resDate[i]=strtrim(month, 1)+':'+strtrim(year, 1)
      endfor
    end
    else:begin
      for i=0, frameNumber-1 do begin
        caldat, startCalc+i*stepValue, month , day, year, hour, minute, second
        resDate[i]=strtrim(day, 1)+':'+strtrim(month, 1)+':'+strtrim(year, 1)
      endfor
    endcase
    ;	'1M':begin
    ;		for i=0, frameNumber-1 do begin
    ;			caldat, startCalc+i*minuteStep, month , day, year, hour, minute, second
    ;			resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
    ;		endfor
    ;	end
    ;	'1S':begin
    ;		for i=0, frameNumber-1 do begin
    ;			caldat, startCalc+i*secondStep, month , day, year, hour, minute, second
    ;			resDate[i]=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(second, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
    ;		endfor
    ;	end
  endcase
  return, resDate

END

FUNCTION Utility::getColorTable, refValue, valueTable, colorTable, TEST=TEST

  ; only for test scale random values to get more colors!!!
  minV=min(valueTable, max=maxV)
  if keyword_set(TEST) then refValue=minV+refValue*(maxv-minV)
  index=max(where(refValue gt valueTable[*]))
  return, colorTable[index>0]

END

FUNCTION Utility::getOverPlotColor, index

  colors=bytarr(6,3)
  colors[0,*]=[200,0,0]
  colors[1,*]=[0,200,0]
  colors[2,*]=[0,0,200]
  colors[3,*]=[200,200,0]
  colors[4,*]=[0,200,200]
  colors[5,*]=[200,0,200]
  return, getColor(colors[index mod 6,0],colors[index mod 6,1],colors[index mod 6,2])

END

FUNCTION Utility::overPlotGraphCheck, title, xTitle, yTitle, dataXLabel, measureUnit, dataNum, controlWindow, colorToUse=colorToUse

  overPlot=0
  if controlWindow.prevGraphXLabel[0] eq dataXLabel[0] and $
    controlWindow.prevGraphXLabel[1] eq dataXLabel[dataNum-1] and $
    controlWindow.prevGraphTitle[0] eq xTitle and $
    controlWindow.prevGraphTitle[1] eq yTitle and $
    controlWindow.prevGraphXMU eq measureUnit and $
    controlWindow.prevGraphElemNum eq dataNum and $
    controlWindow.graphOverplotNum le 4 then begin
    answer=dialog_message('Do you want overplot?', title='Graph overplot', /QUESTION)
    if strupcase(answer) eq 'YES' then begin
      overPlot=1
      ;controlWindow.graphOverplotNum++
      controlWindow.graphOverplotNum=controlWindow.graphOverplotNum+1
    endif else begin
      overPlot=0
      controlWindow.graphOverplotNum=0
      controlWindow.graphOverplotTitles[*]=''
    endelse
  endif else begin
    controlWindow.graphOverplotNum=0
    controlWindow.graphOverplotTitles[*]=''
  endelse
  controlWindow.prevGraphXLabel[0]=dataXLabel[0]
  controlWindow.prevGraphXLabel[1]=dataXLabel[dataNum-1]
  controlWindow.prevGraphXMU=measureUnit
  controlWindow.prevGraphElemNum=dataNum
  controlWindow.prevGraphTitle[0]=xTitle
  controlWindow.prevGraphTitle[1]=yTitle
  controlWindow.graphOverplotTitles[controlWindow.graphOverplotNum]=title
  colorToUse=getOverPlotColor(controlWindow.graphOverplotNum)
  return, overPlot


END

FUNCTION Utility::getColor, red, green, blue, indexes=indexes, THREE=THREE

  if n_elements(indexes) ne 0 then begin

    colorTable=bytarr(256,3)
    ;colStart=[0,0,0] & colEnd= [68, 0, 72]
    colorTable[0:15,0]=bindgen(16)*68./16
    colorTable[0:15,1]=0
    colorTable[0:15,2]=bindgen(16)*72./16
    ;colStart=[69, 0, 77] & colEnd= [87, 0, 150]
    colorTable[16:31,0]=bindgen(16)*(87.-69.)/16+69
    colorTable[16:31,1]=0
    colorTable[16:31,2]=bindgen(16)*(150.-77.)/16+77
    ;colStart=[85, 0, 54] & colEnd= [46, 0, 232]
    colorTable[32:47,0]=bindgen(16)*(46.-85.)/16+85
    colorTable[32:47,1]=0
    colorTable[32:47,2]=bindgen(16)*(232.-54.)/16+54
    ;colStart=[43, 0 ,236] & colEnd= [0, 38, 235]
    colorTable[48:63,0]=bindgen(16)*(0-43.)/16+43.
    colorTable[48:63,1]=bindgen(16)*(38.-0.)/16+0.
    colorTable[48:63,2]=bindgen(16)*(235.-236.)/16+236
    ;colStart=[0, 42, 255] & colEnd= [0, 127, 255]
    colorTable[64:79,0]=0
    colorTable[64:79,1]=bindgen(16)*(127.-42.)/16+42.
    colorTable[64:79,2]=255
    ;colStart=[0, 131, 255] & colEnd= [0, 220, 255]
    colorTable[80:95,0]=0
    colorTable[80:95,1]=bindgen(16)*(220.-131.)/16+131.
    colorTable[80:95,2]=255
    ;colStart=[9, 225, 255] & colEnd= [0, 255, 199]
    colorTable[96:111,0]=bindgen(16)*(0-9.)/16+9.
    colorTable[96:111,1]=bindgen(16)*(255.-225.)/16+225.
    colorTable[96:111,2]=bindgen(16)*(199.-255.)/16+255.
    ;colStart=[0, 255, 195] & colEnd= [0, 255, 110]
    colorTable[112:127,0]=0
    colorTable[112:127,1]=255
    colorTable[112:127,2]=bindgen(16)*(110.-195.)/16+195.
    ;colStart=[0, 255, 106] & colEnd= [0, 255, 16]
    colorTable[128:143,0]=0
    colorTable[128:143,1]=255
    colorTable[128:143,2]=bindgen(16)*(16.-106.)/16+106.
    ;colStart=[0, 255, 12] & colEnd= [72, 255, 0]
    colorTable[144:159,0]=bindgen(16)*(72-0.)/16+0.
    colorTable[144:159,1]=255
    colorTable[144:159,2]=bindgen(16)*(0.-12.)/16+12.
    ;colStart=[76, 255, 0] & colEnd= [161, 255, 0]
    colorTable[160:175,0]=bindgen(16)*(161-76.)/16+76
    colorTable[160:175,1]=255
    colorTable[160:175,2]=0
    ;colStart=[165, 255, 0] & colEnd= [255, 255, 0]
    colorTable[176:191,0]=bindgen(16)*(255-165.)/16+165.
    colorTable[176:191,1]=255
    colorTable[176:191,2]=0
    ;colStart=[255, 250, 0] & colEnd= [255, 165, 0]
    colorTable[192:207,0]=255
    colorTable[192:207,1]=bindgen(16)*(165.-250.)/16+250.
    colorTable[192:207,2]=0
    ;colStart=[255, 161, 0] & colEnd= [255, 76, 0]
    colorTable[208:223,0]=255
    colorTable[208:223,1]=bindgen(16)*(76.-161.)/16+161.
    colorTable[208:223,2]=0
    ;colStart=[76, 255, 0] & colEnd= [161, 255, 0]
    colorTable[224:239,0]=bindgen(16)*(161-76.)/16+76.
    colorTable[224:239,1]=255
    colorTable[224:239,2]=0
    ;colStart=[255, 63, 0] & colEnd= [255, 0, 0]
    colorTable[224:255,0]=255
    colorTable[224:255,1]=bindgen(32)*(0.-63.)/32+63.
    colorTable[224:255,2]=0.
    ;
    ;colorTable[255,*]=255b

    ;stretch demo color table
    ;	step=255./(indexes-1)
    ;	subscribeIndexes=indgen(indexes)*(step)

    retCT=colorTable[indexes,*]
    ;retCT=colorTable[subscribeIndexes,*]

    if keyword_set(THREE) then return, retCT else return, long64(retCT[*,2])*256ll*256ll+retCT[*,1]*256ll+retCT[*,0]

  endif

  return, long64(blue)*256ll*256ll+green*256ll+red

END

;**************************************************************
;System section
;**************************************************************
FUNCTION Utility::isTrue, value

  type=size(value, /TYPE)
  if type eq 7 then return, strupcase(value) eq 'TRUE' or strupcase(value) eq '1'
  return,  value eq 1

END

FUNCTION Utility::isFalse, value

  type=size(value, /TYPE)
  if type eq 7 then return, strupcase(value) eq 'FALSE' or strupcase(value) eq '0'
  return,  value eq 0

END

FUNCTION Utility::stringListToArray, sourceString, separator=separator, FLOAT=FLOAT, INT=INT, BYTE=BYTE, STRING=STRING

  if n_elements(separator) eq 1 then listSeparator=separator else listSeparator=';'
  result=strsplit(sourceString, listSeparator, /EXTRACT, /PRESERVE)
  if keyword_set(FLOAT) then return, float(result)
  if keyword_set(INT) then return, fix(result)
  if keyword_set(BYTE) then return, byte(result)
  return, result

END

FUNCTION Utility::getListDifference, a, b, NULL=NULL

  ; = a and (not b) = elements in A but not in B
  result = cmset_op(A, 'AND', /NOT2, B, NULL=NULL)   ; A but not B
  return, result

END

;Viewing contents of file '../idllib/contrib/groupk/strreplace.pro'

;+
; NAME:
;        STRREPLACE
;
; PURPOSE:
;        The STRREPLACE procedure replaces the contents of one string
;        with another.  The first occurrence of the search substring, Find
;        within the source string, String is replaced by the string,
;        Replacement.
;
; CATEGORY:
;        String Processing.
;
; CALLING SEQUENCE:
;
;        STRREPLACE, String, Find, Replacement
;
; INPUTS:
;        String:   The string to have substring(s) replaced.  If String is
;                  an array, Find is replaced by Replacement in the first
;                  occurrence of Find of every element of the array.
;
;        Find:     The scalar substring to be replaced. If this argument is
;                  not a string, it is converted using IDL's default
;                  formatting rules.
;
;        Replacement:   A scalar string to replace the Find substring. If
;                  this argument is not a string, it is converted using IDL's
;                  default formattting rules.
;
; EXAMPLE:
;
;        If the variable A contains the string "IBM is fun", the
;        substring "IBM" can be replaced with the string "Microsoft"
;        by entering:
;
;        STRREPLACE, A, 'IBM', 'Microsoft'
;
; MODIFICATION HISTORY:
;        Written by:    Han Wen, June 1995.
;-

FUNCTION Utility::STRREPLACE, Strings, Find1, Replacement1

  ;   Check integrity of input parameter

  NP        = N_PARAMS()
  if (NP ne 3) then message,'Must be called with 3 parameters, '+$
    'Strings, Find, Replacement'

  sz        = SIZE(Strings)
  ns        = n_elements(sz)
  if (sz[ns-2] ne 7) then message,'Parameter must be of string type.'

  Find      = STRING(Find1)
  pos       = STRPOS(Strings,Find)
  here      = WHERE(pos ne -1, nreplace)

  if (nreplace eq 0) then return, strings

  Replacement=STRING(Replacement1)
  Flen      = strlen(Find)
  for i=0,nreplace-1 do begin

    j         = here[i]
    prefix    = STRMID(Strings[j],0,pos(j))
    suffix    = STRMID(Strings[j],pos[j]+Flen,$
      strlen(Strings[j])-(pos[j]+Flen))
    strings[j] = prefix + replacement + suffix
  endfor
  return, strings

END



FUNCTION Utility::replaceChar, aString, sourceChar, destChar

  b = byte(aString)          ; convert string to a byte array.
  cb1 = byte(sourceChar)            ; convert char 1 to byte.
  w = where( b EQ cb1[0], Nfound)     ; find occurrences of char 1.
  if Nfound EQ 0 then return, aString     ; if none, return old string.
  if N_params() LT 3 then c2 = ' '    ; default char 2 is space.
  cb2 = byte(destChar)          ; convert char 2 to byte.
  b[w] = cb2[0]             ; replace char 1 by char 2.
  return, string(b)           ; return new string.

END

; using %envVarName% dos style convention
;FUNCTION Utility::findEnvVariables, sourceString, envSign, parStrings=parStrings
;
;  check=strsplit(sourceString, envSign)
;  nElem=n_elements(check)
;  if (nElem mod 2) ne 0 then doLog,'something wrong'
;  check=[check, 999]
;  fixStrings=strarr(nElem/2)
;  parStrings=strarr(nElem/2)
;  for i=0, nElem-1, 2 do begin
;    fixStrings[i/2]=strmid(sourceString, check[i], check[i+1]-check[i]-1)
;    parStrings[i/2]=strmid(sourceString, check[i+1], (check[i+2]<strlen(sourceString)-1)-check[i+1])
;  endfor
;  return, fixStrings
;
;END

FUNCTION Utility::findEnvVariables, sourceString, envSign, parStrings=parStrings

  check=0
  len=strlen(sourceString)
  pos=intarr(20)
  while check ne -1 do begin
    check=strpos(sourceString, envSign, check)
    if check ne -1 then begin
      pos[i]=check
      i++
    endif
  endwhile
  if (nElem mod 2) ne 0 then doLog,'something wrong', level=2

  check=strsplit(sourceString, envSign)
  nElem=n_elements(check)
  if (nElem mod 2) ne 0 then doLog,'something wrong', level=2
  check=[check, 999]
  fixStrings=strarr(nElem/2)
  parStrings=strarr(nElem/2)
  for i=0, nElem-1, 2 do begin
    fixStrings[i/2]=strmid(sourceString, check[i], check[i+1]-check[i]-1)
    parStrings[i/2]=strmid(sourceString, check[i+1], (check[i+2]<strlen(sourceString)-1)-check[i+1])
  endfor
  return, fixStrings

END

FUNCTION Utility::IsNumber, aString

  ;testString=byte(aString)
  wholeAndDecimal=strsplit(aString, '.', /EXTRACT)
  if n_elements(wholeAndDecimal) gt 2 then return, 0
  for i=0, n_elements(wholeAndDecimal)-1 do begin
    test=byte(wholeAndDecimal[i])
    for j=0,n_elements(test)-1 do if test[j] gt byte('9') or test[j] lt byte('0') then return, 0
    ;check=where(test ge byte('0') and test le byte('9'), count)
    ;if count ne strlen(wholeAndDecimal[i]) then return, 0
  endfor
  return, 1

END

FUNCTION Utility::buildWithAsterisk, list, prefix=prefix, numbers=numbers

  separator='*'
  if n_elements(prefix) ne 0 then begin
    record=prefix+'0'+separator
    for i=1, numbers-1 do record=record+prefix+strcompress(i, /REMOVE)+separator
  endif else begin
    record=list[0]+separator
    for i=1, n_elements(list)-1 do record=record+list[i]+separator
  endelse
  return, record

END

FUNCTION Utility::buildFileDataStream, dataName, datalist, sepChar, blockSepChar, POINTER=POINTER

  if n_elements(sepChar) ne 0 then separatorChar=sepChar[0] else separatorChar='*'
  if n_elements(blockSepChar) ne 0 then blockSeparatorChar=blockSepChar[0] else blockSeparatorChar='&'
  if n_elements(dataName) ne 0 then record=dataName+'=' else record=""
  if keyword_set(POINTER) then begin
    nElem=n_elements(datalist)
    for i=0, nElem-1 do begin
      refData=*datalist[i]
      for j=0, n_elements(refData)-1 do record=record+refData[j]+separatorChar
      record=strmid(record, 0, strlen(record)-strlen(separatorChar))
      record=record+blockSeparatorChar
    endfor
    record=strmid(record, 0, strlen(record)-strlen(blockSeparatorChar))

  endif else begin
    record=record+datalist[0]+separatorChar
    for i=1, n_elements(datalist)-1 do record=record+datalist[i]+separatorChar
    record=strmid(record, 0, strlen(record)-strlen(separatorChar))
  endelse
  return, record

END

PRO Utility::convertStreamDataFile, record, dataName, datalist, sepChar, blockSepChar, POINTER=POINTER

  if n_elements(sepChar) ne 0 then separatorChar=sepChar[0] else separatorChar='*'
  if n_elements(blockSepChar) ne 0 then blockSeparatorChar=blockSepChar[0] else blockSeparatorChar='&'

  info=strsplit(record, '=', /EXTRACT)
  data=info[n_elements(info)-1]
  if keyword_set(POINTER) then begin
    datalist=str_sep(data, blockSeparatorChar)
    pointerLen=n_elements(datalist)
    ptrs=ptrarr(pointerLen)
    for i=0, pointerLen-1 do begin
      valuelist=str_sep(datalist[i], separatorChar)
      ptrs[i]=ptr_new(valuelist, /NO_COPY)
    endfor
    datalist=ptrs
  endif else begin
    datalist=str_sep(data, separatorChar)
  endelse
  if n_elements(info) eq 2 then dataName=info[0] else dataName=''

END

;**************************************************************
;DateTime section
;**************************************************************

FUNCTION Utility::getSysTime, FILECOMPATIBILITY=FILECOMPATIBILITY, ONLY_YEAR=ONLY_YEAR, SATFORMAT=SATFORMAT

  date=systime()
  if keyword_set(ONLY_YEAR) then begin
    len=strlen(date)
    year=strmid(date, len-4, 4)
    date=year
  endif
  if keyword_set(FILECOMPATIBILITY) then begin
    ; Remove ":" characters from date
    search=1
    date=strcompress(date, /REMOVE)
    while (search) do begin
      idx=strpos(date, ':')
      if idx ne -1 then date=strmid(date, 0, idx)+strmid(date, idx+1, 100) else search=0
    endwhile
  endif
  if keyword_set(SATFORMAT) then begin
    ; Remove ":" characters from date
    ;09-03-2016 T10:00:00
    search=1
    date=strcompress(date, /REMOVE)
    while (search) do begin
      idx=strpos(date, ':')
      if idx ne -1 then date=strmid(date, 0, idx)+strmid(date, idx+1, 100) else search=0
    endwhile
  endif
  return, date

END

FUNCTION Utility::calcDayOfMonth, date, outHour=outHour

  jDay = julday(date[1], date[2], date[0], date[3])
  jDayFirst = julday(date[1], 1, date[0], 0)
  fyear=date[0]+ (date[1] eq 12 ? 1 : 0)
  fmonth=(date[1] ne 12) ? date[1]+1 : 1
  ;print, fyear, fmonth
  jDayLast = julday(fmonth, 1, fyear, 0)
  dayDiff=fix(jDayLast-jDayFirst)
  outHour=fix((dayDiff-fix(jDay-jDayFirst))*24)
  return, dayDiff

END

FUNCTION Utility::calcDayOfYear, date, outHour=outHour

  jDay = julday(date[1], date[2], date[0], date[3])
  jDayFirst = julday(1, 1, date[0], 0)
  ;jDayFirst = julday(1, 1, date[0], 0)
  dayDiff=fix(jDay-jDayFirst)
  outHour=fix((dayDiff-fix(jDay-jDayFirst))*24)
  return, dayDiff

END

FUNCTION Utility::getDayNumberOfYear, year

  month=1 & emonth=12
  day=1 & eday=31
  hour=0 & ehour=23
  minute=0 & eminute=59
  second=0 & esecond=59

  jDay = julday(month, day, year, hour, minute, second)

  jDaye = julday(emonth, eday, year, ehour, eminute, esecond)
  return, round(jDaye-jDay)

END

FUNCTION Utility::checkDate, year, month=month, day=day, hour=hour, minute=minute, second=second, julNumber=julNumber

  if n_elements(month) eq 0 then month=1
  if n_elements(day) eq 0 then day=1
  if n_elements(hour) eq 0 then hour=0
  if n_elements(minute) eq 0 then minute=0
  if n_elements(second) eq 0 then second=0

  jDay = julday(month, day, year, hour, minute, second)
  julNumber=jDay
  caldat, jDay, resMonth, resDay, resYear, resHour, resMinute, resSecond
  resSecond=round(resSecond)

  if resYear eq year and resMonth eq month and $
    resDay eq day and resHour eq hour and $
    resMinute eq minute and resSecond eq second then return, 1

  return, 0

END

FUNCTION Utility::getDate, startDate, frequency, frameIndex

  ;start=julday(Month, Day, Year, Hour, Minute, Second)
  startCalc=julday(startDate[1], startDate[2], startDate[0], startDate[3], startDate[4], startDate[5])
  dayStep=1.
  hourStep=1./24
  minuteStep=1./(24*60)
  secondStep=1./(24*60*60)

  case frequency of
    '1H':begin
      caldat, startCalc+frameIndex*hourStep, month , day, year, hour, minute, second
      ;doLog,month , day, year, hour, minute, second
      ;resDate=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(floor(second), 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
      resDate=strtrim(year, 1)+':'+strtrim(month, 1)+':'+strtrim(day, 1)+':'+strtrim(hour, 1)
      ;resDate[i]=strtrim(hour, 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
    end
    '1D':begin
      caldat, startCalc+frameIndex*dayStep, month , day, year, hour, minute, second
      resDate=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(floor(second), 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
    end
    '1M':begin
      caldat, startCalc+gframeIndex*minuteStep, month , day, year, hour, minute, second
      resDate=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(floor(second), 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
    end
    '1S':begin
      caldat, startCalc+frameIndex*secondStep, month , day, year, hour, minute, second
      resDate=strtrim(hour, 1)+':'+strtrim(minute, 1)+'.'+strtrim(floor(second), 1)+' '+strtrim(day, 1)+'-'+strtrim(month, 1)+'-'+strtrim(year, 1)
    end
  endcase
  return, resDate

END

FUNCTION Utility::formatDate, date, template=template

  if n_elements(template) eq 0 then template='hh:dd:mm:yyyy'
  ;date var of type intarr(6) [0]: year [5]: seconds

  case template of
    'satellite':begin
      ;2014-02-12T13:39:25Z',
      format='(I04)-(I02))-(I02)TI(02):I(02)Z'
      stringDate=string(format='(I4, A, I02, A, I02, A, I02, A, I02, A, I02, A)', $
        date[0], '-', date[1], '-', date[2], 'T', date[3], ':', date[4], ':', date[5], 'Z')
      ;stringDate=strtrim(date[3], 1)+':'+strtrim(date[2], 1)+':'+strtrim(date[1], 1)+':'+strtrim(date[0], 1)
    end
    'dd':begin
      stringDate=strtrim(date[2], 1)
    end
    'hh:dd':begin
      stringDate=strtrim(date[3], 1)+':'+strtrim(date[2], 1)
    end
    'dd:mm':begin
      stringDate=strtrim(date[2], 1)+':'+strtrim(date[1], 1)
    end
    'mm:yyyy':begin
      stringDate=strtrim(date[1], 1)+':'+strtrim(date[0], 1)
    end
    'hh:dd:mm:yyyy':begin
      stringDate=strtrim(date[3], 1)+':'+strtrim(date[2], 1)+':'+strtrim(date[1], 1)+':'+strtrim(date[0], 1)
    end
    else:begin
      stringDate=strtrim(date[3], 1)+':'+strtrim(date[2], 1)+':'+strtrim(date[1], 1)+':'+strtrim(date[0], 1)
    end
  endcase
  
  return, stringDate

END

;****************************************************************************************

FUNCTION Utility::init

  if not self -> Object :: init() then return , 0
  return , 1

END

PRO Utility::cleanUp

  self -> Object::cleanUp

END

;****************************************************************************************

PRO Utility__Define

  Struct = { Utility , $
    Inherits Object $
  }

END

;****************************************************************************************
