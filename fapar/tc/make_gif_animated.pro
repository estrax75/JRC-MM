PRO Make_GIF_Animated, filter, read_function, startyear=startyear

  ; Check keywords.
  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  if n_elements(startyear) eq 1 then yearlist=startyear else yearlist=2009-indgen(20)
  monthlist=indgen(12)+1
  subdir='/LAN/AVH/L1/PLC'

  for j=0, n_elements(yearlist)-1 do begin
    for k=0, n_elements(monthlist)-1 do begin

      thisMonth=string(monthlist[k], format="(I02)")
      thisYear=string(yearlist[j], format="(I4)")
      folder=getsourcedir_by_year(yearList[j])
      
      thisFolder=folder+subdir+path_sep()+thisYear+path_sep()+thisMonth+path_sep()
      test=file_info(thisFolder)
      if test.directory ne 1 then stop
      ;read_function='read_AVHRR_FAPAR'
      
      read_function='read_BRF'
      ;filter='*FPA*.NC'
      filter='*BRDF*.NC'

      ;folder=dialog_pickfile()
      sourceFileName=ST_fileSystem->getFileNameInfo(folder, filePath=filePath, extension=extension)
      folder=filepath
      list=file_search(thisFolder, filter)
      i=0
      sourceFileName=ST_fileSystem->getFileNameInfo(list[i], filePath=filePath, extension=extension)
      ;resutlfilename = Dialog_Pickfile(/Write, Title='Result Dir...', File='choosedir', path=filepath)
      resultfilename=thisFolder+sourceFileName+'.gif'

      ;data=read_AVHRR_FAPAR(filePath, sourceFileName, FOUND=FOUND)
      data=call_function(read_function, filePath, sourceFileName, 'BRF_BAND_1');, FOUND=FOUND)
      ;readbrf(
      xsize=720 & ysize=360
      ;image24=bytarr(3,720,360)
      ;mpegID = MPEG_Open([xsize, ysize], Filename=filename)

      ;band1=congrid(data.fapar, xsize, ysize)
      band1=congrid(data.data, xsize, ysize)

      sampleImg=bytscl(band1)
      ;LOADCT, 0
      device, decomposed=0
      PPMSA_AlbedoColor,r_curr=r_curr, g_curr=g_curr, b_curr=b_curr
      ;FaparColor, R_CURR, G_CURR, B_CURR
      ;file_delete, monthName
      file_delete, resultfilename, /ALLOW_NONEXISTENT, /QUIET, /VERBOSE
      write_gif, resultfilename, sampleImg, reform(r_curr), reform(g_curr), reform(b_curr), /MULTIPLE;,DELAY_TIME=100

      ;MPEG_Put, mpegID1, Image=image24, Frame=j

      for i=1, n_elements(list)-1 do begin
        sourceFileName=ST_fileSystem->getFileNameInfo(list[i], filePath=filePath, extension=extension)
        print, 'reading...',  sourceFileName
        data=call_function(read_function, filePath, sourceFileName, 'BRF_BAND_1');, FOUND=FOUND)
        print, '...done...'
        ;data=read_BRF(filePath, sourceFileName, FOUND=FOUND)

        band1=congrid(data.data, xsize, ysize)
        ;band1=congrid(data.band1, xsize, ysize)
        ;band2=congrid(data.band2, xsize, ysize)

        sampleImg=bytscl(band1)
        write_gif, resultfilename, sampleImg, /MULTIPLE, DELAY_TIME=100
        ;MPEG_Put, mpegID1, Image=image24, Frame=j

        ;    image24[0,*,*] = band2
        ;    image24[1,*,*] = band2
        ;    image24[2,*,*] = band2
        ;    MPEG_Put, mpegID1, Image=image24, Frame=j

      endfor
      write_gif, resultfilename, /close
    endfor
  endfor

  ; Save the MPEG sequence. Be patient this will take several seconds.

  ;MPEG_Save, mpegID

  ; Close the MPEG sequence and file.

  ;MPEG_Close, mpegID

END


;Making MPEG Movies in Earlier Versions of IDL
;
;This answer comes from some code origially submitted to the IDL newsgroup by Scott Denning, formerly of Colorado State University and now at the University of California at Santa Barabara. It has been modified slightly by Christian Soeller of St. Georges Hopital Medical School in London to better run on most UNIX machines.
;
;
;
;The following IDL procedure will produce an MPEG file from a series of images stored in a 3D array (width x height x number of frames). It requires the mpeg_encode executable to be in the UNIX search path. This can be obtained from ftp://mm-ftp.CS.Berkeley.EDU/pub/mpeg/encode/.
;
;There are a lot of options that can be handled differently to make tradeoffs between image quality, speed, and disk space. See the documentation for mpeg_encode for more details.
;
;If your animation is stored in the array image_array and you want to write it to a file called movie.mpg, you would do so by typing:
;
;WRITE_MPEG, 'movie.mpg', image_array
;Here is a copy of the WRITE_MPEG documentation header. To download a local copy, click here. For the latest updated version, please contact Christian Soeller directly.



;+
; NAME: WRITE_MPEG
;
; PURPOSE: write a sequence of images as an mpeg movie
;
; CATEGORY: utility
;
; CALLING SEQUENCE:
;         WRITE_MPEG,'movie.mpg',ims
;
; INPUTS:
;         ims: sequence of images as a 3D array with dimensions [sx, sy, nims]
;              where sx = xsize of images
;                    sy = ysize of images
;                    nims = number of images
;
; OPTIONAL INPUTS: None
;
; KEYWORD PARAMETERS:
;             delaft:   if set delete temporary array after movie was created
;                       you should actually always do it otherwise you get
;                       problems with permissions on multiuser machines (since
;                       /tmp normally has the sticky bit set)
;             rep:      if given means repeat every image 'rep' times
;                       (as a workaround to modify replay speed)
;
; OUTPUTS: None
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;          creates some files in TMPDIR which are only removed when
;          the DELAFT keyword is used
;
;
; RESTRICTIONS:
;          depends on the program mpeg_encode from University of
;          California, Berkeley, which must be installed in /usr/local/bin
;
;
; PROCEDURE:
;         writes a parameter file based on the dimensions of the image
;         array + the sequence of images in ppm format into a
;         temporary directory; finally spawns mpeg_encode to build the
;         movie
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;       Mon Nov 18 13:13:53 1996, Christian Soeller
;
;
;    grabbed original from the net and made slight modifications
;
;-