


;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       l2_flags
;
; PURPOSE:
;
;       split a flag value into base-2 bits.
;
; CATEGORY:
; 
;       statistics
;
; CALLING SEQUENCE:
;
;       value = l2_flags(flag)
;
; INPUTS:
;			flag:			flag value
;			
; OUTPUTS:
;			return value:	flag array in bit
;			
; KEYWORD PARAMETERS:
;					none
;
; COMMENTS:
;	
; REFERENCES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Written by: F. MELIN, JRC/IES/GEM.
;			
;
;------------------------------------------------------------------------------

FUNCTION mer_l2_flags,flag


bin = BYTARR(32)

FOR j=0,31 DO BEGIN
   powerOfTwo = 2L^j
   IF (LONG(flag) AND powerOfTwo) EQ powerOfTwo THEN $
      bin(j) = 1 ELSE bin(j) = 0
ENDFOR

;RETURN, Reverse(bin)
RETURN, bin

END

;  ATMFAIL             1    0
;  LAND                2    1
;  PRODWARN            4    2
;  HIGLINT             8    3
;  HILT               16    4
;  HISATZEN           32    5
;  COASTZ             64    6
;  spare             128    7
;  STRAYLIGHT        256    8
;  CLDICE            512    9
;  COCCOLITH        1024    10
;  TURBIDW          2048    11
;  HISOLZEN         4096    12 
;  spare            8192    13
;  LOWLW           16384    14
;  CHLFAIL         32768    15
;  NAVWARN         65536    16
;  ABSAER         131072    17
;  spare          262144    18
;  MAXAERITER     524288    19
;  MODGLINT      1048576    20
;  CHLWARN       2097152    21
;  ATMWARN       4194304    22
;  spare         8388608    23
;  SEAICE       16777216    24
;  NAVFAIL      33554432	25
;  FILTER       67108864	26
;  SSTWARN     134217728	27
;  SSTFAIL     268435456	28
;  HIPOL       536870912	29
;  PRODFAIL   1073741824	30
;  spare      2147483648 	31


