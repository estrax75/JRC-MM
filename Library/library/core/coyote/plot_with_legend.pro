; docformat = 'rst'
;+
; This is an example program to demonstrate how to create a line plot with a legend
; with Coyote Graphics routines.
;
; :Categories:
;    Graphics
;    
; :Examples:
;    Save the program as "plot_with_legend.pro" and run it like this::
;       IDL> .RUN plot_with_legend
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
;        Written, 24 February 2013 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
PRO Plot_With_Legend
 
   ; Set up variables for the plot. Normally, these values would be 
   ; passed into the program as positional and keyword parameters.
   ; Create two random vectors.
   data_1 = cgDemoData(17)
   data_2 = cgDemoData(17)
   
   ; Calculate the data range, so you can create a plot with room at the top
   ; of the plot for the legend.
   maxData = Max(data_1) > Max(data_2)
   minData = Min(data_1) > Min(data_2)
   dataRange = maxData - minData
   yrange = [MinData, maxData + (dataRange*0.25)]
   
   ; Create the plot.
   cgPlot, data_1, PSym=-15, Color='red7', YRange=yrange, YStyle=1, $
      XTitle='Time', YTitle='Signal'
   cgPlot, data_2, PSym=-16, Color='blu7', /Overplot
   
   ; Create the legend with NASA Astronomy routine AL_LEGEND.
    items = ['Experiment 1', 'Experiment 2']
    psyms = [-15, -16]
    colors = ['red7', 'blu7']
    
    ; Location of legend in data coordinates.
    yloc = (!Y.CRange[1] - !Y.CRange[0]) * 0.95 + !Y.CRange[0]
    xloc = (!X.CRange[1] - !X.CRange[0]) * 0.05 + !X.CRange[0]
    
    ; Add the legend.
    AL_Legend, items, PSym=psyms, Lines=lines, Color=colors, Position=[xloc,yloc]
       
END ;*****************************************************************

; This main program shows how to call the program and produce
; various types of output.

  ; Display the plot in a graphics window.
  Plot_With_Legend
  
  ; Display the plot in a resizeable graphics window.
  cgWindow, 'Plot_With_Legend', WTitle='Taylor Diagram in a Resizeable Graphics Window'
  
  ; Create a PostScript file.
  PS_Start, 'plot_with_legend.ps'
  Plot_With_Legend
  PS_End
  
  ; Create a PNG file with a width of 600 pixels.
  cgPS2Raster, 'plot_with_legend.ps', /PNG, Width=600

END