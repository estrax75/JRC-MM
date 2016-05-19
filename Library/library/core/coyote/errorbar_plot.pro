; docformat = 'rst'
;+
; This is an example program to demonstrate how to create a line plot
; with error estimates with Coyote Graphics routines.
;
; :Categories:
;    Graphics
;    
; :Examples:
;    Save the program as "errorbar_plot.pro" and run it like this::
;       IDL> .RUN errorbar_plot
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 7 December 2013 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
PRO Errorbar_Plot

   ; Example data.
   data = Congrid(cgDemoData(1), 15)
   seed = -5L
   time = cgScaleVector(Findgen(N_Elements(data)), 1, 9)
   high_yerror = RandomU(seed, N_Elements(data)) * 5 > 0.5
   low_yerror = RandomU(seed, N_Elements(data)) * 4 > 0.25
   high_xerror = RandomU(seed, N_Elements(data)) * 0.75 > 0.1
   low_xerror = RandomU(seed, N_Elements(data))  * 0.75 > 0.1
   
   ; Set up variables for the plot. Normally, these values would be 
   ; passed into the program as positional and keyword parameters.
   xtitle = 'Time'
   ytitle = 'Signal Strength'
   title = 'Error Bar Plot'
   position = [0.125, 0.125, 0.9, 0.925]
   thick = (!D.Name EQ 'PS') ? 3 : 1
   
   ; Set up a "window" for the plot. The PostScript output will have
   ; the same aspect ratio as the graphics window on the display.
   cgDisplay, 600, 500, Title='Errorbar Plot'
      
   ; Draw the line plot.
   cgPlot, time, data, Color='red5', PSym=-16, SymColor='olive', $
       SymSize=1.0, Thick=thick, Title=title, XTitle=xtitle, YTitle=ytitle, $
       Position=position, YRange=[-5, 35], XRange=[0,10], YStyle=1, $
       ERR_XLow=low_xerror, ERR_XHigh=high_xerror, $
       ERR_YLow=low_yerror, ERR_YHigh=high_yerror, ERR_Color='blu5'
      
END ;*****************************************************************

; This main program shows how to call the program and produce
; various types of output.

  ; Display the line plot in a graphics window.
  cgDisplay, 600, 500
  Errorbar_Plot

  
  ; Display the line plot in a resizeable graphics window.
  cgWindow, 'Errorbar_Plot'
  
  ; Create a PostScript file.
  cgPS_Open, Filename='errorbar_plot.ps'
  Errorbar_Plot
  cgPS_Close
  
  ; Create a PNG file from the PostScript file.
  cgPS2Raster, 'errorbar_plot.ps', /PNG

END