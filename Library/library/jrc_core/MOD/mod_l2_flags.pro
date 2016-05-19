

; .run /home/melinfr/OC/IDL/MOD/utilities/l2_flags

FUNCTION mod_l2_flags,number


bin = BYTARR(32)

FOR j=0,31 DO BEGIN
   powerOfTwo = 2uL^j
   IF (LONG(number) AND powerOfTwo) EQ powerOfTwo THEN $
      bin(j) = 1 ELSE bin(j) = 0
ENDFOR

;RETURN, Reverse(bin)
RETURN, bin

END

;  ATMFAIL             1    0
;  LAND                2    1
;  SPARE3              4    2
;  HIGLINT             8    3
;  HILT               16    4
;  HISATZEN           32    5
;  COASTZ             64    6
;  NEGLW             128    7
;  STRAYLIGHT        256    8
;  CLDICE            512    9
;  COCCOLITH        1024    10
;  TURBIDW          2048    11
;  HISOLZEN         4096    12 
;  HITAU            8192    13
;  LOWLW           16384    14
;  CHLFAIL         32768    15
;  NAVWARN         65536    16
;  ABSAER         131072    17
;  SPARE2         262144    18
;  MAXAERITER     524288    19
;  MODGLINT      1048576    20
;  CHLWARN       2097152    21
;  ATMWARN       4194304    22
;  DARKPIXEL     8388608    23
;  SEAICE       16777216    24
;  NAVFAIL      33554432	25
;  FILTER       67108864	26
;  SSTWARN     134217728	27
;  SSTFAIL     268435456	28
;  SPARE       536870912	29
;  HIPOL      1073741824	30
;  OCEAN      2147483648 	31


