
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       GetMERName
;
; PURPOSE:
;
;       Create name for MERIS L2 files based on information provided in a structure.
;
; CATEGORY:
; 
;       I/O
;
; CALLING SEQUENCE:
;
;       GetMERName,DataStruct,name
;
; INPUTS:
;			DataStruct:	input structure
;
; OUTPUTS:
;			name:		name of the file,
;						string of the type: Myyydddhhmmss.level_type
;						where :	yyyy    : start year,
;								ddd     : start julian day,
;								hh      : start hour
;								mm      : start minute
;								ss      : start second
;								level   : level of processing (L1A, L1B, L2)
;								type    : LAC or GAC.
;			
; KEYWORD PARAMETERS:
;					none
;
; COMMENTS:
;			minimal content of input structure: 
;	DataStruct = {	Name:name, $            Name of the scene (Myyydddhhmmss.level_type).
;			Level:level, $          Data level (Level-1A, 1B, 2).
;			Type:datatype, $        Data type (LAC, GAC).
;			StartYear:syear, $      Start Year.
;			StartDay:sday, $        Start Julian day.
;			StartMillisec:smsec, $  Start millisec (from 00:00).
;                     } 
;
;			
; REFERENCES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Written by: F. MELIN, 09/2010, JRC/IES/GEM.
;			
;
;------------------------------------------------------------------------------

PRO GetMERName,DataStruct,name


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

         syear = DataStruct.StartYear
         sday = DataStruct.StartDay
         smsec = double(DataStruct.StartMillisec)
         
         clevel = DataStruct.Level
         ctype = DataStruct.Type
         
         xhour = smsec / 1000. / 3600. ; Conversion in hour.
         
         hh = fix(xhour)
         mm = fix(60.*(xhour-double(hh)))
         ss = fix(3600.*(xhour-double(hh)-double(mm)/60.))

         cyear = string(syear)
         cday = string(sday)
         if ( sday LT 100 ) then cday = '0'+cday
         if ( sday LT 10 ) then cday = '0'+cday 

         chh = string(hh)
         if ( hh LT 10 ) then chh = '0'+chh
         
         cmm = string (mm)
         if ( mm LT 10 ) then cmm = '0' + cmm
         
         css = string(ss)
         if ( ss LT 10 ) then css = '0' + css
         
         str = STRCOMPRESS('M'+cyear+cday+chh+cmm+css+'.'+clevel+'_'+ctype,/REMOVE_ALL)


         name = str

END


