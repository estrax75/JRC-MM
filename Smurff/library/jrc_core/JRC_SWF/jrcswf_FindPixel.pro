
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       FindPixel
;
; PURPOSE:
;
;       find pixel numbers in a MODIS image for the pixel nearest to a specific location 
;		expressed in latitude/longitude, 
;
; CATEGORY:
; 
;       I/O
;
; CALLING SEQUENCE:
;
;       FindPixel,MODISStr,inlat,inlon,n_side,near_line,near_elem,istatus
;
; INPUTS:
;			MODISStr: 			structure defining the MODIS image
;			inlat:				input latitude (degrees)
;			inlon:				input longitude (degrees).
;			n_side:				size of the square needed around the location.
;
; OUTPUTS:
;			near_line:			line number of the closest pixel.
;			near_elem:			pixel number of the closest pixel.
;			istatus:			=1 if the image contains the specified location, -1 else.
;			
; KEYWORD PARAMETERS:
;					none
;
; COMMENTS:
;		input structure provided by routine ReadMODISGeo
;			
; REFERENCES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Written by: F. MELIN, 08/2002, JRC/IES/GEM.			
;
;------------------------------------------------------------------------------

PRO jrcswf_FindPixel,MODISStr,inlat,inlon,n_side,near_line,near_elem,istatus

; Number of pixels required around the closest one to accept the selection.
; if n_e=1, it means that a 3x3 square around the selected is required
; to be in the image. Otherwise, istatus is switched back to -1.

n_e = FIX ( (n_side-1)/2 + 0.1)

; Thresholds for lat/lon differences.
LATLON_THRESHOLD1 = 1.
LATLON_THRESHOLD2 = 1.

latthreshold = LATLON_THRESHOLD1
lonthreshold = LATLON_THRESHOLD1

r2d = 57.29577951d ; 180./PI

INSIDE = 1
OUTSIDE = -1

; Opens HDF file
catch, error_status

if error_status NE 0 THEN BEGIN
  ERROR=1
  catch, /CANCEL
  msg='Problem with file '+fileName+' check version, contents, existence or read permission.'
  ;errMsg=dialog_message(msg, /ERROR)
  ;message, msg
  return
endif

nline = MODISStr.NumberLine
nelem = MODISStr.NumberPixel

dist_from_point_sq = 0.d

sofar_lowest_dist_from_point_sq = 9999999999.9d

sofar_best_line_subsampled_grid = -1
sofar_best_elem_subsampled_grid = -1

; ---------------------------------
; First search on a subsampled grid
FOR line=0,nline-1,10 DO BEGIN

   FOR elem=0,nelem-1,10 DO BEGIN
   
      this_lat = DOUBLE( MODISStr.Latitude[elem,line] )
      this_lon = DOUBLE( MODISStr.Longitude[elem,line] )
      
      dlat = DOUBLE( ABS(inlat-this_lat) ) & dlon = DOUBLE(ABS(inlon-this_lon) )

; print,line,elem,this_lat,this_lon,dlat,dlon      
;      latthreshold = LATLON_THRESHOLD1 / CosSinScan.cos[elem]
;      lonthreshold = LATLON_THRESHOLD1 / cos(inlat/r2d) / (CosSinScan.cos[elem]*CosSinScan.cos[elem])

;      if ( latthreshold GT 1. ) then latthreshold = 1.
;      if ( lonthreshold GT 1. ) then lonthreshold = 1.
   
      IF ( dlat LT  latthreshold AND dlon LT  lonthreshold ) THEN BEGIN
   
          dist_from_point_sq = dlon*dlon + dlat*dlat

          IF ( dist_from_point_sq LT sofar_lowest_dist_from_point_sq ) THEN BEGIN
					sofar_lowest_dist_from_point_sq = dist_from_point_sq
					sofar_best_line_subsampled_grid = line
					sofar_best_elem_subsampled_grid = elem
          ENDIF
          
	  ENDIF   


  ENDFOR
ENDFOR

; print,'dist=',sofar_lowest_dist_from_point_sq,' line=',sofar_best_line_subsampled_grid,' elem=',sofar_best_elem_subsampled_grid

; ---------------------------------

sofar_lowest_dist_from_point_sq = 9999999999.9d

near_lat = -9999.0
near_lon = -9999.0
sofar_best_line=-1
sofar_best_elem=-1

; -------------------------------------------
; Finer loop if a first guess has been found.
IF ( sofar_best_line_subsampled_grid GE 0 ) THEN BEGIN

; Loop over line number.
FOR line=sofar_best_line_subsampled_grid-20,sofar_best_line_subsampled_grid+20 DO BEGIN

    IF ( line GE 0 AND line LT nline ) THEN BEGIN

; Loop over pixel number
        FOR elem=sofar_best_elem_subsampled_grid-20,sofar_best_elem_subsampled_grid+20 DO BEGIN
           
           IF ( elem GE 0 AND elem LT nelem ) THEN BEGIN
           
                   this_lat = double( MODISStr.Latitude[elem,line] )
                   this_lon = double( MODISStr.Longitude[elem,line] )
                   
                   cmlat = COS( (this_lat+inlat)/2./r2d )
                   
                   dlat = ABS(inlat-this_lat) & dlon = ABS(inlon-this_lon)
 
;                   latthreshold = LATLON_THRESHOLD2 / CosSinScan.cos[elem]
;                   lonthreshold = LATLON_THRESHOLD2 / cos(inlat/r2d) / (CosSinScan.cos[elem]*CosSinScan.cos[elem])

;                   if ( latthreshold GT 1. ) then latthreshold = 1.
;                   if ( lonthreshold GT 1. ) then lonthreshold = 1.

                   IF ( dlat LT latthreshold AND dlon LT lonthreshold ) THEN BEGIN
   
                       dist_from_point_sq = dlon*dlon*cmlat*cmlat + dlat*dlat

                       IF ( dist_from_point_sq LT sofar_lowest_dist_from_point_sq ) THEN BEGIN
                           sofar_lowest_dist_from_point_sq = dist_from_point_sq
                           sofar_best_line = line
                           sofar_best_elem = elem
                           
                           near_lat = this_lat
                           near_lon = this_lon
                           
					   ENDIF
          
				   ENDIF
           
           ENDIF
        
        ENDFOR
        
	ENDIF

ENDFOR

ENDIF
; print,'dist=',sofar_lowest_dist_from_point_sq,' line=',sofar_best_line,' elem=',sofar_best_elem

; Check that the square defined by n_e is located inside the image.

istatus = OUTSIDE

IF ( sofar_best_line GE n_e AND sofar_best_line LT nline-n_e AND $
     sofar_best_elem GE n_e AND sofar_best_elem LT nelem-n_e ) THEN istatus =INSIDE

near_elem = sofar_best_elem
near_line = sofar_best_line

END
