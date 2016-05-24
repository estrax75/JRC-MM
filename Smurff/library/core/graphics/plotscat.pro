pro PlotScat, v0, v1, bin1=bin1, bin2=bin2, max1=max1, max2=max2, xtitle=xtitle, ytitle=ytitle, title=title, filename=filename, $
wix=wix, thresh=thresh, print=print, hbin=hbin, min1=min1, min2=min2, binMax=binMax, stem=stem, rcol=rcol, stat=stat, hist=HIST

loadct,12
if not(keyword_set(wix)) then wix = 0
if not(keyword_set(bin1)) then bin1=0.004
if not(keyword_set(bin2)) then bin2=0.004
if not(keyword_set(max1)) then max1=1.0
if not(keyword_set(max2)) then max2=1.0
if not(keyword_set(min1)) then min1=0.0
if not(keyword_set(min2)) then min2=0.0
if not(keyword_set(thresh)) then thresh=25
if not(keyword_set(hbin)) then hbin=0.01
if not(keyword_set(stem)) then stem=""
;if not(keyword_set(rcol)) then rcol=255

scat = hist_2d(v0, v1, bin1=bin1, min1=min1, max1=max1, bin2=bin2, min2=min2, max2=max2)
scat[0,0]=0

if not(keyword_set(binMax)) then binMax=max(scat)
xs = 75
;ys = 200
ys = 75

;i1 = 1.0/bin1
;i2 = 1.0/bin2
;xB = 25
;yB = 35
xB = 10
yB = 35

xL = 400
yL = 400
xSize = double(xs + xL + xB)
if keyword_set(hist) then xSize = xSize * 2
ySize = double(ys + yL + yB)

help, scat
;print, 'max(scat)', max(scat)
ix = where(scat le thresh, count)

;mm=binMax
ordre = sort(scat)
mm = scat(ordre(N_elements(ordre)-2))



if count gt 0 then scat[ix] = mm	;tous les elements trop petits de scat (ie tous les valeurs qui ont peu d'occurences dans l'image) sont elimines
scat = bytscl(scat > thresh, min=thresh, max=mm) ;scale all elements between min and max to 0 -> 255
scat[ix]=255
;
;
;
if keyword_set(print) then begin
    set_plot,'win'
    ;set_plot,'x'
    resX=!d.x_px_cm
    resY=!d.y_px_cm
    set_plot, 'ps'
    if n_elements(filename) eq 1 then filename=filename+'.eps' else filename=stem+print 
    device, filename=filename, encapsulated=1, /portrait, xsize=18.0, ysize=10.5, /color, bits_per_pixel=8 
    loadct, 2
    colour0 = 100
    colour1 = 200
endif else begin
  ;  loadct, 2
    window, wix, xsize=xsize, ysize=ysize
    scat = congrid(scat, xL, yL)
    colour0 = 255
    colour1 = 255^2
endelse


pos=[xs, ys, xs+xL, ys+yL]/[xSize, ySize, xSize, ySize]

tv, scat, pos[0], pos[1], /normal, xsize=xL/xSize, ysize=yL/ySize
;tvscl, congrid(max(scat)-scat, xL, yL) > thresh, xs, ys, /device

;print, [min2,max2], [min1,max1]
;stop
;affiche les axes
plot, [min1,max1],[min2,max2], pos=pos, /normal, /noerase, title=title, $
xtitle=xtitle, ytitle=ytitle, xstyle=1, ystyle=1, color=0, XCHARSIZE=1.00, YCHARSIZE=1.00
;xyouts, 2/xSize, (ys+yl+yb/2)/ySize, 'Max='+string(binMax, format='(i4)'), /normal
oplot, [min1,max1],[min2,max2], line=0, thick=2.0, colo = 100
if (keyword_set(stat)) THEN BEGIN
	;affiche des informations statistiques
correlateValues=WHERE(v0 NE 0. AND v1 NE 0.)
corr=REGRESS(v0(correlateValues),v1(correlateValues), SIGMA=sigma,CORRELATION=correlation,CONST=const,/DOUBLE)
rmse=rmse(v0, v1)
xyouts, (xs+xb/4)/xsize, max2-5*max2/100.,'N='+strcompress(string(N_elements(v0)),/remove_all),CHARSIZE=0.75
xyouts, (xs+xb/4)/xsize, max2-10*max2/100.,'r='+strcompress(string(correlation, format='(f10.4)'),/remove_all),CHARSIZE=0.75
xyouts, (xs+xb/4)/xsize, max2-15*max2/100.,'rmse='+strcompress(string(rmse, format='(f10.4)'),/remove_all),CHARSIZE=0.75
	;xyouts, (xs+x4/2)/xsize, max2-15*max2/100. , 'sigma='+strcompress(string(sigma, format='(f10.5)'),/remove_all),CHARSIZE=1.00
	;xyouts, (xs+xb/2)/xsize, max2-20*max2/100. ,'a='+strcompress(string(corr, format='(f10.3)'),/remove_all)+', b='+strcompress(string(const, format='(f10.3)'),/remove_all),CHARSIZE=1.00
	;draw the line y=ax+b
	;oplot, [0.,1.],[const,corr+const],linestyle=0
;
; compute rms
;
ENDIF

if keyword_set(hist) then begin
;stop
;h=histogram(v0-v1, binsize=hbin, min=(-1.0), max=1.0, locations=loc)
mini=min(v0-v1)
maxi=max(v0-v1)
;print, mini, maxi
;stop
h=histogram(v0-v1, binsize=hbin, min=mini, max=maxi, locations=loc)
h = h / double(total(h))

;decale les valeurs de pos pour les x pour pouvoir tracer l'histogramme
pos[0] = pos[0] +  (xs+xL+xB)/xSize
pos[2] = pos[0] +  xL/xSize

;prepare l'echelle en abscisse
;x=hbin*findgen(n_elements(h)) / (n_elements(h) /2) - 1.0
;print, loc
x=loc+hbin/2.
plot, x, h*100., psym=10, /noerase, pos=pos, /normal, xstyle=1, title='Histogram of differences', xtitle='!X!7d!X ('+xtitle+'-'+ytitle+')', XRANGE=[mini,maxi], YTITLE='% of values', YCHARSIZE=1.00
oplot, [0,0],[0,max(h*100)], color=colour0, linestyle=2  , thick=2.5	;marque le zero
res = moment(v0 - v1, sdev= rms)
;print, 'res ', res
;print, 'rms' , rms 
res1 = median(v0 - v1)
;print, 'median ', res1
oplot, [res1,res1],[0,max(h*100)], color=10, linestyle=0 , thick=2.5	;marque le zero

rmsd=sqrt((total(v0-v1)^2)/N_elements(v0))
if N_elements(x) ge 3 then begin
    xyouts, x(1), max(h*100)-5.*max(h*100)/100., '<!X!7d!X>= '+strcompress(string(res(0), format='(f10.4)'),/remove_all),CHARSIZE=0.75
    xyouts, x(1), max(h*100)-10.*max(h*100)/100.,   '  !X!7r!X = '+strcompress(string(rms, format='(f10.4)'),/remove_all),CHARSIZE=0.75
    xyouts, x(1), max(h*100)-15.*max(h*100)/100., '!XMed!X = '+strcompress(string(res1, format='(f10.4)'),/remove_all),CHARSIZE=0.75,color=10 
    xyouts, x(1), max(h*100)-20.*max(h*100)/100., '!XN!X = '+strcompress(string(N_elements(v0), format='(f10.0)'),/remove_all),CHARSIZE=0.75 
    xyouts, x(1), max(h*100)-25.*max(h*100)/100., '!XRMSD!X = '+strcompress(string(rmsd, format='(f10.4)'),/remove_all),CHARSIZE=0.75 
;
    endif
endif
;respercent = moment(200.*(v0 - v1)/(v0+v1), sdev= rms4)
;print, 'respercent ', respercent, rms4
;
;respercent = moment(200.*abs(v0 - v1)/(v0+v1), sdev= rms4)
;print, 'respercent ', respercent, rms4
;

;ix=where(h eq max(h))
;b1 = h[ix[0]]/2.0
;ix1=ix[0] 
;while ix1 lt n_elements(h) and b1 lt 0.45 do begin
;    b1 = b1 + h[ix1]
;    ix1 = ix1 + 1
;endwhile
;
;b0 = h[ix[0]]/2.0
;ix0=ix[0] 
;while ix0 ge 0 and b0 lt 0.45 do begin
;    b0 = b0 + h[ix0]
;    ix0 = ix0 - 1
;endwhile
;mh = max(h)
;if ix0 ge 0 then begin
;    m0 = x[ix0]
;    ;oplot, [m0,m0],[0,h[ix0[0]]], color=colour1
;endif
;if ix1 ge 0 then begin
;    m1 = x[ix1]
;    ;oplot, [m1,m1],[0,h[ix1[0]]], color=colour1
;endif

if keyword_set(print) then begin
    device, /close
    set_plot, 'win'
    ;set_plot, 'x'
endif
   

end
