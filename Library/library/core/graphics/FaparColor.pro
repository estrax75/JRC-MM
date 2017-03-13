PRO BlueRedColor, R_CURR, G_CURR, B_CURR
  pcolor = bytarr( 3, 256)
  i=0
  ;blue=reverse(byte(indgen(255)))*
  red=(byte(indgen(51)))*(255./51)
  blue=(byte(indgen(51)))*(255./51)
  green=reverse(byte(indgen(51)))*(255./51)
  red=reverse(indgen(128)*2)
  white=bytarr(255)
  white[*]=255b
  pcolor(*,*)=255
  for i=128, 128+50 do pcolor(*,i)=[0, green[i-128], blue[i-128]]
  pcolor(*,254)=[255, 255, 128]
  pcolor(*,127)=[255, 255, 255]
  ;for i=0, 127 do pcolor(*,i)=[red[i], 0, 0]
  ;for i=128, 254 do pcolor(*,i)=[0, 0, blue[i-129]]

  ;TVLCT, pcolor(0,*), pcolor(1,*), pcolor(2,*)
  R_CURR = pcolor(0,*)
  G_CURR = pcolor(1,*)
  B_CURR = pcolor(2,*)

END
PRO FaparColor, R_CURR, G_CURR, B_CURR
  pcolor = bytarr( 3, 256)
  i=0
  pcolor(*,i)=[255, 255, 255] & i=i+1
  pcolor(*,i)=[254, 254, 252] & i=i+1
  pcolor(*,i)=[254, 253, 250] & i=i+1
  pcolor(*,i)=[254, 253, 248] & i=i+1
  pcolor(*,i)=[253, 252, 246] & i=i+1
  pcolor(*,i)=[253, 251, 243] & i=i+1
  pcolor(*,i)=[253, 251, 241] & i=i+1
  pcolor(*,i)=[252, 250, 239] & i=i+1
  pcolor(*,i)=[252, 249, 237] & i=i+1
  pcolor(*,i)=[252, 249, 234] & i=i+1
  pcolor(*,i)=[251, 248, 232] & i=i+1
  pcolor(*,i)=[251, 247, 230] & i=i+1
  pcolor(*,i)=[251, 247, 228] & i=i+1
  pcolor(*,i)=[250, 246, 225] & i=i+1
  pcolor(*,i)=[250, 246, 223] & i=i+1
  pcolor(*,i)=[250, 245, 221] & i=i+1
  pcolor(*,i)=[249, 244, 219] & i=i+1
  pcolor(*,i)=[249, 244, 217] & i=i+1
  pcolor(*,i)=[249, 243, 214] & i=i+1
  pcolor(*,i)=[248, 242, 212] & i=i+1
  pcolor(*,i)=[248, 242, 210] & i=i+1
  pcolor(*,i)=[248, 241, 208] & i=i+1
  pcolor(*,i)=[247, 240, 205] & i=i+1
  pcolor(*,i)=[247, 240, 203] & i=i+1
  pcolor(*,i)=[247, 239, 201] & i=i+1
  pcolor(*,i)=[247, 239, 199] & i=i+1
  pcolor(*,i)=[246, 238, 196] & i=i+1
  pcolor(*,i)=[246, 237, 194] & i=i+1
  pcolor(*,i)=[246, 237, 192] & i=i+1
  pcolor(*,i)=[245, 236, 190] & i=i+1
  pcolor(*,i)=[245, 235, 187] & i=i+1
  pcolor(*,i)=[245, 235, 185] & i=i+1
  pcolor(*,i)=[244, 234, 183] & i=i+1
  pcolor(*,i)=[244, 233, 181] & i=i+1
  pcolor(*,i)=[244, 233, 179] & i=i+1
  pcolor(*,i)=[243, 232, 176] & i=i+1
  pcolor(*,i)=[243, 232, 174] & i=i+1
  pcolor(*,i)=[243, 231, 172] & i=i+1
  pcolor(*,i)=[242, 230, 170] & i=i+1
  pcolor(*,i)=[242, 230, 167] & i=i+1
  pcolor(*,i)=[242, 229, 165] & i=i+1
  pcolor(*,i)=[241, 228, 163] & i=i+1
  pcolor(*,i)=[241, 228, 161] & i=i+1
  pcolor(*,i)=[241, 227, 158] & i=i+1
  pcolor(*,i)=[240, 226, 156] & i=i+1
  pcolor(*,i)=[240, 226, 154] & i=i+1
  pcolor(*,i)=[240, 225, 152] & i=i+1
  pcolor(*,i)=[240, 225, 150] & i=i+1

  pcolor(*,i)=[225, 212, 106] & i=i+1
  pcolor(*,i)=[225, 212, 105] & i=i+1
  pcolor(*,i)=[220, 214,  97] & i=i+1
  ;
  ;51-124
  pcolor(*,i)=[217, 213,  95] & i=i+1
  pcolor(*,i)=[215, 213,  94] & i=i+1
  pcolor(*,i)=[212, 213,  93] & i=i+1
  pcolor(*,i)=[210, 213,  91] & i=i+1
  pcolor(*,i)=[207, 212,  90] & i=i+1
  pcolor(*,i)=[205, 212,  89] & i=i+1
  pcolor(*,i)=[202, 212,  87] & i=i+1
  pcolor(*,i)=[200, 212,  86] & i=i+1
  pcolor(*,i)=[197, 211,  85] & i=i+1
  pcolor(*,i)=[195, 211,  83] & i=i+1
  pcolor(*,i)=[192, 211,  82] & i=i+1
  pcolor(*,i)=[190, 211,  81] & i=i+1
  pcolor(*,i)=[187, 211,  79] & i=i+1
  pcolor(*,i)=[185, 210,  78] & i=i+1
  pcolor(*,i)=[182, 210,  77] & i=i+1
  pcolor(*,i)=[180, 210,  76] & i=i+1
  pcolor(*,i)=[177, 210,  74] & i=i+1
  pcolor(*,i)=[175, 209,  73] & i=i+1
  pcolor(*,i)=[172, 209,  72] & i=i+1
  pcolor(*,i)=[170, 209,  70] & i=i+1
  pcolor(*,i)=[167, 209,  69] & i=i+1
  pcolor(*,i)=[165, 208,  68] & i=i+1
  pcolor(*,i)=[162, 208,  66] & i=i+1
  pcolor(*,i)=[160, 208,  65] & i=i+1
  pcolor(*,i)=[157, 208,  64] & i=i+1
  pcolor(*,i)=[155, 208,  62] & i=i+1
  pcolor(*,i)=[152, 207,  61] & i=i+1
  pcolor(*,i)=[150, 207,  60] & i=i+1
  pcolor(*,i)=[147, 207,  58] & i=i+1
  pcolor(*,i)=[145, 207,  57] & i=i+1
  pcolor(*,i)=[142, 206,  56] & i=i+1
  pcolor(*,i)=[140, 206,  55] & i=i+1
  pcolor(*,i)=[137, 206,  53] & i=i+1
  pcolor(*,i)=[135, 206,  52] & i=i+1
  pcolor(*,i)=[132, 205,  51] & i=i+1
  pcolor(*,i)=[130, 205,  49] & i=i+1
  pcolor(*,i)=[128, 205,  48] & i=i+1
  pcolor(*,i)=[125, 205,  47] & i=i+1
  pcolor(*,i)=[123, 205,  45] & i=i+1
  pcolor(*,i)=[120, 204,  44] & i=i+1
  pcolor(*,i)=[118, 204,  43] & i=i+1
  pcolor(*,i)=[115, 204,  41] & i=i+1
  pcolor(*,i)=[113, 204,  40] & i=i+1
  pcolor(*,i)=[110, 203,  39] & i=i+1
  pcolor(*,i)=[108, 203,  38] & i=i+1
  pcolor(*,i)=[105, 203,  36] & i=i+1
  pcolor(*,i)=[103, 203,  35] & i=i+1
  pcolor(*,i)=[100, 202,  34] & i=i+1
  pcolor(*,i)=[ 98, 202,  32] & i=i+1
  pcolor(*,i)=[ 95, 202,  31] & i=i+1
  pcolor(*,i)=[ 93, 202,  30] & i=i+1
  pcolor(*,i)=[ 90, 202,  28] & i=i+1
  pcolor(*,i)=[ 88, 201,  27] & i=i+1
  pcolor(*,i)=[ 85, 201,  26] & i=i+1
  pcolor(*,i)=[ 83, 201,  24] & i=i+1
  pcolor(*,i)=[ 80, 201,  23] & i=i+1
  pcolor(*,i)=[ 78, 200,  22] & i=i+1
  pcolor(*,i)=[ 75, 200,  20] & i=i+1
  pcolor(*,i)=[ 73, 200,  19] & i=i+1
  pcolor(*,i)=[ 70, 200,  18] & i=i+1
  pcolor(*,i)=[ 68, 199,  17] & i=i+1
  pcolor(*,i)=[ 65, 199,  15] & i=i+1
  pcolor(*,i)=[ 63, 199,  14] & i=i+1
  pcolor(*,i)=[ 60, 199,  13] & i=i+1
  pcolor(*,i)=[ 58, 199,  11] & i=i+1
  pcolor(*,i)=[ 55, 198,  10] & i=i+1
  pcolor(*,i)=[ 53, 198,   9] & i=i+1
  pcolor(*,i)=[ 50, 198,   7] & i=i+1
  pcolor(*,i)=[ 48, 198,   6] & i=i+1
  pcolor(*,i)=[ 45, 197,   5] & i=i+1
  pcolor(*,i)=[ 43, 197,   3] & i=i+1
  pcolor(*,i)=[ 40, 197,   2] & i=i+1
  pcolor(*,i)=[ 38, 197,   1] & i=i+1
  pcolor(*,i)=[ 36, 197,   0] & i=i+1
  ;
  ; 125-250
  ;
  pcolor(*,i)=[  2, 180,   0] & i=i+1
  pcolor(*,i)=[  4, 179,   0] & i=i+1
  pcolor(*,i)=[  6, 177,   0] & i=i+1
  pcolor(*,i)=[  8, 176,   0] & i=i+1
  pcolor(*,i)=[ 10, 174,   0] & i=i+1
  pcolor(*,i)=[ 12, 173,   0] & i=i+1
  pcolor(*,i)=[ 14, 171,   0] & i=i+1
  pcolor(*,i)=[ 16, 170,   0] & i=i+1
  pcolor(*,i)=[ 18, 169,   0] & i=i+1
  pcolor(*,i)=[ 20, 167,   0] & i=i+1
  pcolor(*,i)=[ 22, 166,   0] & i=i+1
  pcolor(*,i)=[ 24, 164,   0] & i=i+1
  pcolor(*,i)=[ 26, 163,   0] & i=i+1
  pcolor(*,i)=[ 28, 161,   0] & i=i+1
  pcolor(*,i)=[ 30, 160,   0] & i=i+1
  pcolor(*,i)=[ 32, 158,   0] & i=i+1
  pcolor(*,i)=[ 34, 157,   0] & i=i+1
  pcolor(*,i)=[ 36, 155,   0] & i=i+1
  pcolor(*,i)=[ 38, 154,   0] & i=i+1
  pcolor(*,i)=[ 40, 152,   0] & i=i+1
  pcolor(*,i)=[ 42, 151,   0] & i=i+1
  pcolor(*,i)=[ 44, 149,   0] & i=i+1
  pcolor(*,i)=[ 46, 148,   0] & i=i+1
  pcolor(*,i)=[ 48, 147,   0] & i=i+1
  pcolor(*,i)=[ 50, 145,   0] & i=i+1
  pcolor(*,i)=[ 52, 144,   0] & i=i+1
  pcolor(*,i)=[ 54, 142,   0] & i=i+1
  pcolor(*,i)=[ 56, 141,   0] & i=i+1
  pcolor(*,i)=[ 58, 139,   0] & i=i+1
  pcolor(*,i)=[ 60, 138,   0] & i=i+1
  pcolor(*,i)=[ 62, 136,   0] & i=i+1
  pcolor(*,i)=[ 64, 135,   0] & i=i+1
  pcolor(*,i)=[ 66, 134,   0] & i=i+1
  pcolor(*,i)=[ 68, 132,   0] & i=i+1
  pcolor(*,i)=[ 70, 131,   0] & i=i+1
  pcolor(*,i)=[ 72, 129,   0] & i=i+1
  pcolor(*,i)=[ 74, 128,   0] & i=i+1
  pcolor(*,i)=[ 76, 126,   0] & i=i+1
  pcolor(*,i)=[ 78, 125,   0] & i=i+1
  pcolor(*,i)=[ 80, 123,   0] & i=i+1
  pcolor(*,i)=[ 82, 122,   0] & i=i+1
  pcolor(*,i)=[ 84, 121,   0] & i=i+1
  pcolor(*,i)=[ 86, 119,   0] & i=i+1
  pcolor(*,i)=[ 88, 118,   0] & i=i+1
  pcolor(*,i)=[ 90, 116,   0] & i=i+1
  pcolor(*,i)=[ 92, 115,   0] & i=i+1
  pcolor(*,i)=[ 94, 113,   0] & i=i+1
  pcolor(*,i)=[ 96, 112,   0] & i=i+1
  pcolor(*,i)=[ 98, 110,   0] & i=i+1
  pcolor(*,i)=[100, 109,   0] & i=i+1
  pcolor(*,i)=[102, 108,   0] & i=i+1
  pcolor(*,i)=[105, 106,   0] & i=i+1
  pcolor(*,i)=[107, 105,   0] & i=i+1
  pcolor(*,i)=[109, 103,   0] & i=i+1
  pcolor(*,i)=[111, 102,   0] & i=i+1
  pcolor(*,i)=[113, 100,   0] & i=i+1
  pcolor(*,i)=[115,  99,   0] & i=i+1
  pcolor(*,i)=[117,  98,   0] & i=i+1
  pcolor(*,i)=[119,  96,   0] & i=i+1
  pcolor(*,i)=[121,  95,   0] & i=i+1
  pcolor(*,i)=[123,  93,   0] & i=i+1
  pcolor(*,i)=[125,  92,   0] & i=i+1
  pcolor(*,i)=[127,  90,   0] & i=i+1
  pcolor(*,i)=[129,  89,   0] & i=i+1
  pcolor(*,i)=[131,  87,   0] & i=i+1
  pcolor(*,i)=[133,  86,   0] & i=i+1
  pcolor(*,i)=[135,  85,   0] & i=i+1
  pcolor(*,i)=[137,  83,   0] & i=i+1
  pcolor(*,i)=[139,  82,   0] & i=i+1
  pcolor(*,i)=[141,  80,   0] & i=i+1
  pcolor(*,i)=[143,  79,   0] & i=i+1
  pcolor(*,i)=[145,  77,   0] & i=i+1
  pcolor(*,i)=[147,  76,   0] & i=i+1
  pcolor(*,i)=[149,  74,   0] & i=i+1
  pcolor(*,i)=[151,  73,   0] & i=i+1
  pcolor(*,i)=[153,  72,   0] & i=i+1
  pcolor(*,i)=[155,  70,   0] & i=i+1
  pcolor(*,i)=[157,  69,   0] & i=i+1
  pcolor(*,i)=[159,  67,   0] & i=i+1
  pcolor(*,i)=[161,  66,   0] & i=i+1
  pcolor(*,i)=[163,  64,   0] & i=i+1
  pcolor(*,i)=[165,  63,   0] & i=i+1
  pcolor(*,i)=[167,  61,   0] & i=i+1
  pcolor(*,i)=[169,  60,   0] & i=i+1
  pcolor(*,i)=[171,  59,   0] & i=i+1
  pcolor(*,i)=[173,  57,   0] & i=i+1
  pcolor(*,i)=[175,  56,   0] & i=i+1
  pcolor(*,i)=[177,  54,   0] & i=i+1
  pcolor(*,i)=[180,  53,   0] & i=i+1
  pcolor(*,i)=[182,  51,   0] & i=i+1
  pcolor(*,i)=[184,  50,   0] & i=i+1
  pcolor(*,i)=[186,  49,   0] & i=i+1
  pcolor(*,i)=[188,  47,   0] & i=i+1
  pcolor(*,i)=[190,  46,   0] & i=i+1
  pcolor(*,i)=[192,  44,   0] & i=i+1
  pcolor(*,i)=[194,  43,   0] & i=i+1
  pcolor(*,i)=[196,  41,   0] & i=i+1
  pcolor(*,i)=[198,  40,   0] & i=i+1
  pcolor(*,i)=[200,  38,   0] & i=i+1
  pcolor(*,i)=[202,  37,   0] & i=i+1
  pcolor(*,i)=[204,  36,   0] & i=i+1
  pcolor(*,i)=[206,  34,   0] & i=i+1
  pcolor(*,i)=[208,  33,   0] & i=i+1
  pcolor(*,i)=[210,  31,   0] & i=i+1
  pcolor(*,i)=[212,  30,   0] & i=i+1
  pcolor(*,i)=[214,  28,   0] & i=i+1
  pcolor(*,i)=[216,  27,   0] & i=i+1
  pcolor(*,i)=[218,  25,   0] & i=i+1
  pcolor(*,i)=[220,  24,   0] & i=i+1
  pcolor(*,i)=[222,  23,   0] & i=i+1
  pcolor(*,i)=[224,  21,   0] & i=i+1
  pcolor(*,i)=[226,  20,   0] & i=i+1
  pcolor(*,i)=[228,  18,   0] & i=i+1
  pcolor(*,i)=[230,  17,   0] & i=i+1
  pcolor(*,i)=[232,  15,   0] & i=i+1
  pcolor(*,i)=[234,  14,   0] & i=i+1
  pcolor(*,i)=[236,  12,   0] & i=i+1
  pcolor(*,i)=[238,  11,   0] & i=i+1
  pcolor(*,i)=[240,  10,   0] & i=i+1
  pcolor(*,i)=[242,   8,   0] & i=i+1
  pcolor(*,i)=[244,   7,   0] & i=i+1
  pcolor(*,i)=[246,   5,   0] & i=i+1
  pcolor(*,i)=[248,   4,   0] & i=i+1
  pcolor(*,i)=[250,   2,   0] & i=i+1
  pcolor(*,i)=[252,   1,   0] & i=i+1
  pcolor(*,i)=[255,   0,   0] & i=i+1
  pcolor(*,i)=[128,   128,   128] & i=i+1
  pcolor(*,i)=[255,   255,   255] & i=i+1
  pcolor(*,i)=[0,   0,   255] & i=i+1
  pcolor(*,i)=[255,   0,   200] & i=i+1
  pcolor(*,i)=[0,   0,   0] & i=i+1

  ;TVLCT, pcolor(0,*), pcolor(1,*), pcolor(2,*)
  R_CURR = pcolor(0,*)
  G_CURR = pcolor(1,*)
  B_CURR = pcolor(2,*)

END

