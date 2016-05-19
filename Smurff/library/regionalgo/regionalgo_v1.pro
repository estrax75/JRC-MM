
pro read_foq_table_v1, filename, foq_table

  hbad=-9999.
  
  N_W = 7 ; Wavelength
  N_S = 6 ; Solar Zenith Angle
  N_C = 6 ; chla
  N_N =17 ; Sensor Zenith Angle
  N_A =13 ; Azimuth Angle
  
  foqtab = fltarr(N_W,N_C,N_S,N_N,N_A) ;
  
  wavetab = [412.5,442.5,490.0,510.0,560.0,620.0,660.0] ; =fltarr(N_W)
  solztab = [0.,15.,30.,45.,60.,75.]                    ; =fltarr(N_S)
  chltab  = [0.03,0.1,0.3,1.0,3.0,10.0]                 ; =fltarr(N_C)
  senztab = [1.078,3.411,6.289,9.278,12.300,15.330,18.370,21.410,24.450, $
    27.500,30.540,33.590,36.640,39.690,42.730,45.780,48.830] ; =fltarr(N_N)
  phitab  = [0.,15.,30.,45.,60.,75.,90.,105.,120.,135.,150.,165.,180.] ; =fltarr(N_A)
  lchltab = fltarr(N_C)
  
  ; Read data
  close,1
  openr,1,filename
  
  stringline=' '
  
  for iw=0,N_W-1 do begin
    for isu=0,N_S-1 do begin
      for ich=0,N_C-1 do begin
        for ise=0,N_N-1 do begin
        
          stringline=' '
          
          readf,1,stringline
          
          if ( stringline ne '' ) then begin
          
            strarray = str_sep( strcompress(stringline),' ')
            
            stringarray = strarray
            ee=0
            for e=0,n_elements(strarray)-1 do begin
              if (strlen(strcompress(strarray(e),/remove_all)) NE 0) then begin
                stringarray[ee] = strarray[e]
                ee = ee +1
              endif
            endfor
            stringarray = stringarray[0:ee-1]
            
          endif else begin
            print,"ERROR"
            stop
          endelse
          
          for iph=0,N_A-1 DO foqtab[iw,ich,isu,ise,iph] = float(stringarray[iph])
          
        endfor
      endfor
    endfor
  endfor
  
  close,1
  
  ; create table for log of chlorophyll
  for kc=0,N_C-1 do lchltab[kc] = alog(chltab[kc])
  
  a = fltarr(N_W,N_C,N_S,N_N,N_A)
  a[*,*,*,*,*]=hbad
  
  ; Table structure
  foq_table = {   NOWI:N_W, WI:wavetab, $
    NOCH:N_C, CH:chltab, $
    NOSU:N_S, SU:solztab, $
    NOTP:N_N, TP:senztab, $
    NOPH:N_A, PH:phitab, $
    foq:a $
    }
    
  foq_table.foq[*,*,*,*,*] = foqtab[*,*,*,*,*]
  
  foq_table.WI(*) = wavetab[*] & foq_table.NOWI = N_W
  foq_table.SU(*) = solztab[*] & foq_table.NOSU = N_S
  foq_table.CH(*) = chltab[*]  & foq_table.NOCH = N_C
  foq_table.TP(*) = senztab[*] & foq_table.NOTP = N_N
  foq_table.PH(*) = phitab[*]  & foq_table.NOPH = N_A
  
END

;___________________________________________________________________________________________________
;
; get f/Q values
;
; foq_table		I		f/Q table
; chlor_a		I		chlorophyll concentration
; solz			I		solar zenith angle (deg.)
; senz			I		view zenith angle, above water (deg.)
; relaz			I		relative azimuth ( [-180.180] )
; foq			O		f/Q value, for the NOWI bands.

pro get_foq_v1,foq_table,chlor_a,solz,senz,relaz,foq

  ; Dimensions of the table.
  NOWI = foq_table.NOWI
  NOCH = foq_table.NOCH
  NOSU = foq_table.NOSU
  NOTP = foq_table.NOTP
  NOPH = foq_table.NOPH
  
  WI = foq_table.WI
  CH = alog(foq_table.CH)
  SU = foq_table.SU
  TP = foq_table.TP
  PH = foq_table.PH
  
  NBANDS=6 ; Number of bands for SeaWiFS
  
  R2D = 57.2958  ; Radian-to-degree conversion factor
  n = 1.34       ; refractive index of water.
  
  PHI    = abs(relaz)               ; put the relative azimuth in [0,180]
  TETAP = asin(sin(senz/R2D)/n)*R2D ; viewing angle in water
  SUN = solz
  
  LCHL = alog(0.01)
  if ( chlor_a GT 0.01 ) then LCHL = alog(chlor_a)
  
  ; changed to allow program to continue when table limits are reached
  
  if ( SUN LT SU[0]        ) then SUN = SU[0]
  if ( SUN GT SU[NOSU-1]   ) then SUN = SU[NOSU-1]
  
  if ( LCHL LT CH[0]        ) then LCHL = CH[0]
  if ( LCHL GT CH[NOCH-1]   ) then LCHL = CH[NOCH-1]
  
  if ( PHI LT PH[0]        ) then PHI = PH[0]
  if ( PHI GT PH[NOPH-1]   ) then PHI = PH[NOPH-1]
  
  if ( TETAP LT TP[0]      ) then TETAP = TP[0]
  if ( TETAP GT TP[NOTP-1] ) then TETAP = TP[NOTP-1]
  
  for i=0,NOCH-2 do begin
    if ( (LCHL-CH[i]) * (LCHL-CH[i+1]) LE 0. ) then ich=i
  endfor
  for i=0,NOSU-2 do begin
    if ( (SUN-SU[i]) * (SUN-SU[i+1]) LE 0. ) then isu=i
  endfor
  for i=0,NOTP-2 do begin
    if ( (TETAP-TP[i]) * (TETAP-TP[i+1]) LE 0. ) then itp=i
  endfor
  for i=0,NOPH-2 do begin
    if ( (PHI-PH[i]) * (PHI-PH[i+1]) LE 0. ) then iph=i
  endfor
  
  DC = fltarr(2)
  DS = fltarr(2)
  DT = fltarr(2)
  DP = fltarr(2)
  
  DC[0] = ( CH[ich+1] - LCHL ) / ( CH[ich+1] - CH[ich] )
  DC[1] = ( LCHL - CH[ich] ) / ( CH[ich+1] - CH[ich] )
  
  DS[0] = ( SU[isu+1] - SUN ) / ( SU[isu+1] - SU[isu] )
  DS[1] = ( SUN - SU[isu] ) / ( SU[isu+1] - SU[isu] )
  
  DT[0] = ( TP[itp+1] - TETAP ) / ( TP[itp+1] - TP[itp] )
  DT[1] = ( TETAP - TP[itp] ) / ( TP[itp+1] - TP[itp] )
  
  DP[0] = ( PH[iph+1] - PHI ) / ( PH[iph+1] - PH[iph] )
  DP[1] = ( PHI - PH[iph] ) / ( PH[iph+1] - PH[iph] )
  
  FQINTP=fltarr(NOWI)
  FQINTP[*]=0.
  
  for iw=0,NOWI-1 do begin
    i = iw
    ; if ( iw EQ 5 ) then i = i + 1 ; skip 620nm
    for j=0,1 do begin
      for k=0,1 do begin
        for l=0,1 do begin
          for m=0,1 do begin
            FQINTP[iw]=FQINTP[iw]+DC[j]*DS[k]*DT[l]*DP[m]*foq_table.foq(i,ich+j,isu+k,itp+l,iph+m)
          ; doLog, foq_table.foq(i,ich+j,isu+k,itp+l,iph+m)
          endfor
        endfor
      endfor
    endfor
  endfor
  
  foq = FQINTP
  
  foq[5]=foq[6]
  foq = foq[0:5]
  
end
;___________________________________________________________________________________________________

function preproc_v1, x, mu, s, ndata

  ; Data pre-processing
  arr=dblarr(ndata, /NOZERO)
  arr[*]=1
  return, (x-arr##mu)/(arr##s)
;return, (x-replicate(1, ndata)##mu)/(replicate(1, ndata)##s)
  
end
;___________________________________________________________________________________________________

function postproc_v1, x, mu, s, ndata

  ; Data post-processing
  arr=dblarr(ndata, /NOZERO)
  arr[*]=1
  return, x*(arr##s)+arr##mu
;return, x*(replicate(1, ndata)##s)+replicate(1, ndata)##mu
  
end
;___________________________________________________________________________________________________

function dataprod_v1, model, x

  ;doLog, /STACK, callingRoutine=callingRoutine
  ;doLog, 'before:', callingRoutine
  ;help, /memory
  ; Variables definition
  ndata = n_elements(x[0, *])
  ;ones = replicate(1., ndata)
  ones = dblarr(ndata, /NOZERO)
  ones[*]=1.
  
  ; Novelty detection
  scored = (x - ones##model.muIn)##transpose(model.pcvecs)
  scaled = temporary(scored / (ones ## sqrt(model.pcvals)))
  novelty = sqrt(scaled[0, *]^2 + scaled[1, *]^2 + scaled[2, *]^2)
  scaled=0b
  
  ; Data product
  values = 10^temporary(postproc_v1(tanh(preproc_v1(x, model.muIn, model.stdIn, ndata)##transpose(model.par.w1)+$
    ones##model.par.b1)##transpose(model.par.w2)+$
    ones##model.par.b2, model.muOut, model.stdOut, ndata))
  ;  values = 10^postproc(tanh(preproc(x, model.muIn, model.stdIn, ndata)##transpose(model.par.w1)+$
  ;    ones##model.par.b1)##transpose(model.par.w2)+$
  ;    ones##model.par.b2, model.muOut, model.stdOut, ndata)
    
  ; Return results
  ones=0b
  ;wait, 3
  ;doLog, 'after:'
  ;help, /memory
  
  return, [values, novelty]
  
end
;___________________________________________________________________________________________________

function dataread_v1, filename, pixelNumber=pixelNumber

  ; Read data from file
  ;doLog, /STACK, callingRoutine=callingRoutine
  ;doLog, 'before:', callingRoutine
  ;help, /memory
  row = ''
  openr, lun, filename, /get_lun
  readf, lun, row
  row = strcompress(row)
  tmp = str_sep(row, ',')
  nCol = n_elements(tmp)
  iRow = 0l
  if n_elements(pixelNumber) eq 0 then pixs=200000L else pixs=long(pixelNumber)
  readData = dblarr(nCol, pixs, /NOZERO)
  
  while not eof(lun) do begin
    readf, lun, row
    row = strcompress(row)
    tmp = str_sep(row, ',')
    readData[0:nCol - 1, iRow] = tmp[0:nCol - 1]
    iRow = iRow + 1
  endwhile
  free_lun, lun & close, lun
  return, readData[0:nCol-1, 0:iRow - 1]
  ;res=data[*, 0:iRow - 1]
  ;data=0b
  ;wait, 3
  ;doLog, 'after:'
  ;help, /memory
  ;return, res
  
end
;___________________________________________________________________________________________________

pro regionalgo, basin, prod, sensor

  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;NAME
  ; regioanlgo
  ;
  ;OVERVIEW
  ;
  ; This IDL routine is provided for testing regional neural nets trained with data from the
  ; BiOMaP and CoASTS programs. Please refer to the companion pdf document for more details.
  ;
  ;INPUTS
  ;
  ; basin: Northern Adriatic Sea (code name: adrs), western Black Sea (blks), and
  ; Baltic Sea (blts).
  ;
  ; prod (data product name): chla (code name: chlideachla), total suspended matter (tsm) and
  ; yellow substance absorption at 412 nm (ay412).
  ;
  ; sensor: MERIS (code name: mer), MODIS (mod), SeaWiFS (swf) and in-situ (rrs) radiometer.
  ; In fact, different algorithms are provided for specific set of center wavelengths.
  ; In all cases, input to the neural net to compute data products are remote sensing
  ; reflectance RRS values.
  ;
  ;EXECUTION
  ;
  ; 1) Extract the content of the zip file regionalgo.zip in a folder (e.g., 'c:\projects\idl\test').
  ;
  ; 2) Set the IDL working directory to that folder (enter cd, 'c:\projects\idl\test'
  ; in the IDL command line, note the comma after cd and that the path is within quotes).
  ;
  ; 3) Enter the following command in the IDL prompt: regioanlgo, 'blks', 'chlideachla', 'mer'.
  ; Note that the arguments basin, prod, sensor of the procedure regionalgo can be any combination
  ; from the set of options of here above (e.g., regioanlgo, 'blks', 'ay412', 'swf').
  ;
  ;CITATIONS FOR THE PUBLICATION OF RESULTS BASED ON REGIONALGO.PRO
  ;
  ; 1) D'Alimonte, G. Zibordi, J.-F. Berthon, E. Canuti, and T. Kajiyama,
  ; "Performance and applicability of bio-optical algorithms in different european seas,"
  ; Remote Sensing of Environment, vol. 124, no. 0, pp. 402-412, 2012.
  ;
  ; 2) T. Kajiyama, D. D'Alimonte, and G. Zibordi, "Regional algorithms for european seas:
  ; a case study based on meris data," IEEE Geosci. Remote Sens. Lett., 2013.
  ;
  ; 3) G. Zibordi, J.-F. Berthon, F. M�lin, and D. D'Alimonte, "Cross-site consistent in situ
  ; measurements for satellite ocean color applications: the biomap radiometric dataset,"
  ; Remote Sens. Environ., vol. 115, no. 8, pp. 2104-2115.
  ;
  ; 4) D. D'Alimonte, T. Kajiyama, G. Zibordi, and J.-F. Berthon, "Comparison between meris and
  ; regional products maps of dissolved and suspended matter," Remote Sensing of Environment, 2012,
  ; Submitted for publication.
  ;
  ;DISCLAIMER
  ;
  ; The regionalgo.pro routine and the coefficients of BiOMaP regional algorithms are provided
  ; ''AS IS'' and there is no any expressed or implied warranties.
  ;
  ;--------------------------------------------------------------------------------------------------

  ; Input data
  cd, current=current
  cd , 'E:\data\mariomi\develop\code\regionalgo\idl'
  data = dataread_v1('./test.dat')
  nData = n_elements(data[0, *])
  
  ; Input file with the coefficients of regional algorithms
  algoFile = '.\regionalgo.h5'
  strc = h5_parse(algoFile, /read_data)
  
  ; Correction for bi-directional effects (to activate, set the flag to 1)
  foqFlag = 0
  
  ; Rrs for the selected basin and sensor
  case basin of
    'adrs': begin
      case sensor of
        'swf': rrsSet = 'swf443_swf490_swf510_swf555'
        'mod': rrsSet = 'mod443_mod488_mod530_mod555'
        'mer': rrsSet = 'mer443_mer490_mer510_mer560'
        'rrs': rrsSet = 'rrs442_rrs491_rrs511_rrs555'
      end
      lambdaIdx = [0, 1, 1, 1, 1, 0]
    endcase
    'blks': begin
      case sensor of
        'swf': rrsSet = 'swf443_swf490_swf510_swf555'
        'mod': rrsSet = 'mod443_mod488_mod530_mod555'
        'mer': rrsSet = 'mer443_mer490_mer510_mer560'
        'rrs': rrsSet = 'rrs442_rrs491_rrs511_rrs555'
      end
      lambdaIdx = [0, 1, 1, 1, 1, 0]
    endcase
    'blts': begin
      case sensor of
        'swf': rrsSet = 'swf490_swf510_swf555_swf670'
        'mod': rrsSet = 'mod488_mod530_mod555_mod667'
        'mer': rrsSet = 'mer490_mer510_mer560_mer665'
        'rrs': rrsSet = 'rrs491_rrs511_rrs555_rrs665'
      end
      lambdaIdx = [0, 0, 1, 1, 1, 1]
    endcase
  endcase
  
  ; f/Q correction
  if foqFlag eq 1 then begin
  
    ; f/Q correction table
    read_foq_table_v1, '.\fq.dat', foq_table_common
    
    ; Algorithm for chl-a estimates
    algoName = strupcase(basin + '_' + rrsSet + '_to_chlideachla')
    algoArchive=tag_names(strc)
    algoIdx=where(strcmp(algoArchive, algoName) eq 1)
    model = strc.(algoIdx)._data
    
    ; Do three iterations
    for i = 0, 2 do begin
    
      ; Chla
      tmp = dataprod_v1(model, alog10(data[where(lambdaIdx eq 1), *]))
      chla = tmp[0, *]
      
      ; Loop over data records
      for row = 0,  nData - 1 do begin
      
        ; f/Q
        get_foq_v1, foq_table_common, chla(row), data(6, row), data(7, row), data(8, row), foq_a
        
        ; f0/Q0
        get_foq_v1, foq_table_common, chla(row), 0, 0, 0, foq_0
        
        ; Loop over wavelengths
        for chn = 0, 5 do data[chn, row] = data(chn, row) * foq_0(chn) / foq_a(chn)
        
      end
      
    end
    
  endif
  
  ; Regional product
  algoName = strupcase(basin + '_' + rrsSet + '_to_' + prod)
  algoArchive=tag_names(strc)
  algoIdx=where(strcmp(algoArchive, algoName) eq 1)
  model = strc.(algoIdx)._data
  res = dataprod_v1(model, alog10(data[where(lambdaIdx eq 1), *]))
  cd, current
  doLog, res, level=0
  
end

;FUNCTION DOREGIONALaLGO, roiCode, prod, sensorCode, inputFileName=inputFileName, outputFileName=outputFileName, ENVI_INPUT=ENVI_INPUT, algoFile=algoFile, fqfile=fqfile, applyfq=applyfq, ignoreValue=ignoreValue
;;___________________________________________________________________________________________________
;
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;NAME
;; regioanlgo
;;
;;OVERVIEW
;;
;; This IDL routine is provided for testing regional neural nets trained with data from the
;; BiOMaP and CoASTS programs. Please refer to the companion pdf document for more details.
;;
;;INPUTS
;;
;; basin: Northern Adriatic Sea (code name: adrs), western Black Sea (blks), and
;; Baltic Sea (blts).
;;
;; prod (data product name): chla (code name: chlideachla), total suspended matter (tsm) and
;; yellow substance absorption at 412 nm (ay412).
;;
;; sensor: MERIS (code name: mer), MODIS (mod), SeaWiFS (swf) and in-situ (rrs) radiometer.
;; In fact, different algorithms are provided for specific set of center wavelengths.
;; In all cases, input to the neural net to compute data products are remote sensing
;; reflectance RRS values.
;;
;;EXECUTION
;;
;; 1) Extract the content of the zip file regionalgo.zip in a folder (e.g., 'c:\projects\idl\test').
;;
;; 2) Set the IDL working directory to that folder (enter cd, 'c:\projects\idl\test'
;; in the IDL command line, note the comma after cd and that the path is within quotes).
;;
;; 3) Enter the following command in the IDL prompt: regioanlgo, 'blks', 'chlideachla', 'mer'.
;; Note that the arguments basin, prod, sensor of the procedure regionalgo can be any combination
;; from the set of options of here above (e.g., regioanlgo, 'blks', 'ay412', 'swf').
;;
;;CITATIONS FOR THE PUBLICATION OF RESULTS BASED ON REGIONALGO.PRO
;;
;; 1) D'Alimonte, G. Zibordi, J.-F. Berthon, E. Canuti, and T. Kajiyama,
;; "Performance and applicability of bio-optical algorithms in different european seas,"
;; Remote Sensing of Environment, vol. 124, no. 0, pp. 402-412, 2012.
;;
;; 2) T. Kajiyama, D. D'Alimonte, and G. Zibordi, "Regional algorithms for european seas:
;; a case study based on meris data," IEEE Geosci. Remote Sens. Lett., 2013.
;;
;; 3) G. Zibordi, J.-F. Berthon, F. M�lin, and D. D'Alimonte, "Cross-site consistent in situ
;; measurements for satellite ocean color applications: the biomap radiometric dataset,"
;; Remote Sens. Environ., vol. 115, no. 8, pp. 2104-2115.
;;
;; 4) D. D'Alimonte, T. Kajiyama, G. Zibordi, and J.-F. Berthon, "Comparison between meris and
;; regional products maps of dissolved and suspended matter," Remote Sensing of Environment, 2012,
;; Submitted for publication.
;;
;;DISCLAIMER
;;
;; The regionalgo.pro routine and the coefficients of BiOMaP regional algorithms are provided
;; ''AS IS'' and there is no any expressed or implied warranties.
;;
;;--------------------------------------------------------------------------------------------------
;
;  basin=remapBasinCode(roiCode, FOUND=FOUND)
;  sensor=remapSensorCode(sensorCode, FOUND=FOUND)
;  if n_elements(inputFileName) eq 0 then datafile = './test.dat' else dataFile=inputFileName
;  if n_elements(inputFileName) eq 1 and keyword_set(ENVI_INPUT) then dataFile=remapEnviToAscii(inputFileName, sensor, ignoreValue, pixelMask=pixelMask)
;  if keyword_set(test) then return, remapAsciiToEnvi(res, inputFileName, pixelMask=pixelMask)
;   ; Input data
;   data = dataread_v1(dataFile)
;   nData = n_elements(data[0, *])
;
;   ; Input file with the coefficients of regional algorithms
;   if n_elements(algoFile) ne 1 then algoFile = '.\regionalgo.h5'
;   strc = h5_parse(algoFile, /read_data)
;
;   ; Correction for bi-directional effects (to activate, set the flag to 1)
;   foqFlag = 0
;
;   ; Rrs for the selected basin and sensor
;   case basin of
;      'adrs': begin
;         case sensor of
;            'swf': rrsSet = 'swf443_swf490_swf510_swf555'
;            'mod': rrsSet = 'mod443_mod488_mod530_mod555'
;            'mer': rrsSet = 'mer443_mer490_mer510_mer560'
;            'rrs': rrsSet = 'rrs442_rrs491_rrs511_rrs555'
;         end
;         lambdaIdx = [0, 1, 1, 1, 1, 0]
;      endcase
;      'blks': begin
;         case sensor of
;            'swf': rrsSet = 'swf443_swf490_swf510_swf555'
;            'mod': rrsSet = 'mod443_mod488_mod530_mod555'
;            'mer': rrsSet = 'mer443_mer490_mer510_mer560'
;            'rrs': rrsSet = 'rrs442_rrs491_rrs511_rrs555'
;         end
;         lambdaIdx = [0, 1, 1, 1, 1, 0]
;      endcase
;      'blts': begin
;         case sensor of
;            'swf': rrsSet = 'swf490_swf510_swf555_swf670'
;            'mod': rrsSet = 'mod488_mod530_mod555_mod667'
;            'mer': rrsSet = 'mer490_mer510_mer560_mer665'
;            'rrs': rrsSet = 'rrs491_rrs511_rrs555_rrs665'
;         end
;         lambdaIdx = [0, 0, 1, 1, 1, 1]
;      endcase
;   endcase
;
;   ; f/Q correction
;   foqflag=keyword_set(applyfoq)
;   if foqFlag eq 1 then begin
;
;      ; f/Q correction table
;      if n_elements(fqfile) ne 1 then fqfile='.\fq.dat'
;      read_foq_table, fqfile, foq_table_common
;
;      ; Algorithm for chl-a estimates
;      algoName = strupcase(basin + '_' + rrsSet + '_to_chlideachla')
;      algoArchive=tag_names(strc)
;      algoIdx=where(strcmp(algoArchive, algoName) eq 1)
;      model = strc.(algoIdx)._data
;
;      ; Do three iterations
;      for i = 0, 2 do begin
;
;         ; Chla
;         tmp = dataprod(model, alog10(data(where(lambdaIdx eq 1), *)))
;         chla = tmp(0, *)
;
;         ; Loop over data records
;         for row = 0,  nData - 1 do begin
;
;            ; f/Q
;            get_foq, foq_table_common, chla(row), data(6, row), data(7, row), data(8, row), foq_a
;
;            ; f0/Q0
;            get_foq, foq_table_common, chla(row), 0, 0, 0, foq_0
;
;            ; Loop over wavelengths
;            for chn = 0, 5 do data(chn, row) = data(chn, row) * foq_0(chn) / foq_a(chn)
;
;         end
;
;      end
;
;   endif
;
;   ; Regional product
;   algoName = strupcase(basin + '_' + rrsSet + '_to_' + prod)
;   algoArchive=tag_names(strc)
;   algoIdx=where(strcmp(algoArchive, algoName) eq 1)
;   model = strc.(algoIdx)._data
;   res = dataprod(model, alog10(data(where(lambdaIdx eq 1), *)))
;   ;doLog, res
;   return, remapAsciiToEnvi(res, inputFileName, pixelMask=pixelMask)
;
;end
