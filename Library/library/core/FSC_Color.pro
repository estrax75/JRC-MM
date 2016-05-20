@PICKCOLORNAME.pro
FUNCTION FSC_Color, theColour, colorIndex, $
    AllColors=allcolors, $
    Brewer=brewer, $
    Check_Connection=check_connection, $ ; This keyword is completely ignored.
    ColorStructure=colorStructure, $
    Cancel=cancelled, $
    Decomposed=decomposedState, $
    _Extra=extra, $
    Filename=filename, $
    Names=names, $
    NColors=ncolors, $
    NODISPLAY=nodisplay, $ ; This keyword is completely ignored.
    Row=row, $
    SelectColor=selectcolor, $
    Triple=triple
    
  ; Return to caller as the default error behavior.
  On_Error, 2
  
  ; Error handling for the rest of the program.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    cancelled = 1
    RETURN, !P.Color
  ENDIF
  
  ; Set up PostScript device for working with colors.
  IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
  
  ; I don't want to change the original variable.
  IF N_Elements(theColour) NE 0 THEN theColor = theColour ELSE $
    theColor = 'OPPOSITE'
    
  ; Make sure the color parameter is a string.
  varInfo = Size(theColor)
  IF varInfo[varInfo[0] + 1] NE 7 THEN $
    Message, 'The color name parameter must be a string.', /NoName
    
  ; We don't want underscores in color names. Turn all underscores
  ; to spaces.
  FOR j=0,N_Elements(theColor)-1 DO BEGIN
    theColor[j] = StrJoin( StrSplit(theColor[j], '_', /Extract, $
      /Preserve_Null), ' ')
  ENDFOR
  
  ; Make sure the color is compressed and uppercase.
  theColor = StrUpCase(StrCompress(StrTrim(theColor,2), /Remove_All))
  
  ; Get the pixel value of the "opposite" color. This is the pixel color
  ; opposite the pixel color in the upper right corner of the display.
  IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    opixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
    IF N_Elements(opixel) NE 3 THEN BEGIN
      IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, /Get
      opixel = [rrr[opixel], ggg[opixel], bbb[opixel]]
    ENDIF
  ENDIF ELSE BEGIN
    IF (!D.Name EQ 'PS') THEN opixel = [255,255,255]
  ENDELSE
  IF N_Elements(opixel) EQ 0 THEN opixel = [0,0,0]
  opixel = 255 - opixel
  
  ; Read the first color as bytes. If none of the bytes are less than 48
  ; or greater than 57, then this is a "number" string and you should
  ; assume the current color table is being used.
  bytecheck = Byte(theColor[0])
  i = Where(bytecheck LT 48, lessthan)
  i = Where(bytecheck GT 57, greaterthan)
  IF (lessthan + greaterthan) EQ 0 THEN useCurrentColors = 1 ELSE useCurrentColors = 0
  
  ; Get the decomposed state of the IDL session right now.
  IF N_Elements(decomposedState) EQ 0 THEN BEGIN
    IF Float(!Version.Release) GE 5.2 THEN BEGIN
      IF (!D.Name EQ 'X' OR !D.Name EQ 'WIN' OR !D.Name EQ 'MAC') THEN BEGIN
        Device, Get_Decomposed=decomposedState
      ENDIF ELSE decomposedState = 0
    ENDIF ELSE decomposedState = 0
    IF (Float(!Version.Release) GE 6.4) AND (!D.NAME EQ 'Z') THEN BEGIN
      Device, Get_Decomposed=decomposedState, Get_Pixel_Depth=theDepth
      IF theDepth LT 24 THEN decomposedState = 0
    ENDIF
  ENDIF ELSE decomposedState = Keyword_Set(decomposedState)
  
  ; Get depth of visual display (and decomposed state for PostScript devices).
  IF (!D.Flags AND 256) NE 0 THEN Device, Get_Visual_Depth=theDepth ELSE theDepth = 8
  IF (Float(!Version.Release) GE 6.4) AND (!D.NAME EQ 'Z') THEN Device, Get_Pixel_Depth=theDepth
  IF (!D.NAME EQ 'PS') AND (Float(!Version.Release) GE 7.1) THEN BEGIN
    decomposedState = DecomposedColor(DEPTH=theDepth)
  ENDIF
  
  ; Need brewer colors?
  brewer = Keyword_Set(brewer)
  
  ; Load the colors.
  IF N_Elements(filename) NE 0 THEN BEGIN
  
    ; Count the number of rows in the file.
    ncolors = FSC_Color_Count_Rows(filename)
    
    ; Read the data.
    OpenR, lun, filename, /Get_Lun
    rvalue = BytArr(NCOLORS)
    gvalue = BytArr(NCOLORS)
    bvalue = BytArr(NCOLORS)
    colors = StrArr(NCOLORS)
    redvalue = 0B
    greenvalue = 0B
    bluevalue = 0B
    colorvalue = ""
    FOR j=0L, NCOLORS-1 DO BEGIN
      ReadF, lun, redvalue, greenvalue, bluevalue, colorvalue
      rvalue[j] = redvalue
      gvalue[j] = greenvalue
      bvalue[j] = bluevalue
      colors[j] = colorvalue
    ENDFOR
    Free_Lun, lun
    
    ; Trim the colors array of blank characters.
    colors = StrTrim(colors, 2)
    
  ENDIF ELSE BEGIN
  
    ; Set up the color vectors.
    IF Keyword_Set(Brewer) THEN BEGIN
    
      ; Set up the color vectors.
      colors = [ 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
      rvalue = [  255,   255,   255,   255,   255,   245,   255,   250 ]
      gvalue = [  255,   250,   255,   255,   248,   245,   245,   240 ]
      bvalue = [  255,   250,   240,   224,   220,   220,   238,   230 ]
      colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
      rvalue = [ rvalue,   250,   255,    255,    255,    255,    245,    222,    210 ]
      gvalue = [ gvalue,   235,   239,    235,    228,    228,    222,    184,    180 ]
      bvalue = [ bvalue,   215,   213,    205,    196,    181,    179,    135,    140 ]
      colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
      rvalue = [ rvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
      gvalue = [ gvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
      bvalue = [ bvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
      colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
      rvalue = [ rvalue,   250,   223,    173,    109,     53,     35,      0,       0 ]
      gvalue = [ gvalue,   253,   242,    221,    193,    156,     132,    97,      69 ]
      bvalue = [ bvalue,   202,   167,    142,    115,     83,      67,    52,      41 ]
      colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
      rvalue = [ rvalue,   232,   202,    158,     99,     53,     33,      8,       8 ]
      gvalue = [ gvalue,   241,   222,    202,    168,    133,    113,     75,      48 ]
      bvalue = [ bvalue,   250,   240,    225,    211,    191,    181,    147,     107 ]
      colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
      rvalue = [ rvalue,   254,    253,    253,    250,    231,    217,    159,    127 ]
      gvalue = [ gvalue,   236,    212,    174,    134,     92,     72,     51,     39 ]
      bvalue = [ bvalue,   217,    171,    107,     52,     12,      1,      3,      4 ]
      colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
      rvalue = [ rvalue,   254,    252,    252,    248,    225,    203,    154,    103 ]
      gvalue = [ gvalue,   232,    194,    146,     97,     45,     24,     12,      0 ]
      bvalue = [ bvalue,   222,    171,    114,     68,     38,     29,     19,     13 ]
      colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
      rvalue = [ rvalue,   244,    222,    188,    152,    119,    106,     80,     63 ]
      gvalue = [ gvalue,   242,    221,    189,    148,    108,     82,     32,      0 ]
      bvalue = [ bvalue,   248,    237,    220,    197,    177,    163,    139,    125 ]
      colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
      rvalue = [ rvalue,   243,    213,    166,     94,     34,      3,      1,      1 ]
      gvalue = [ gvalue,   234,    212,    189,    164,    138,    129,    101,     70 ]
      bvalue = [ bvalue,   244,    232,    219,    204,    171,    139,     82,     54 ]
      colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
      rvalue = [ rvalue,   244,    206,    127,     58,     30,     33,     32,      8 ]
      gvalue = [ gvalue,   250,    236,    205,    175,    125,     95,     48,     29 ]
      bvalue = [ bvalue,   193,    179,    186,    195,    182,    168,    137,     88 ]
      colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
      rvalue = [ rvalue,   201,    245,    253,    251,    228,    193,    114,     59 ]
      gvalue = [ gvalue,    35,    121,    206,    253,    244,    228,    171,     85 ]
      bvalue = [ bvalue,    38,    72,     127,    197,    239,    239,    207,    164 ]
      colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8']
      rvalue = [ rvalue,  84,    163,   197,   220,   105,    51,    13,     0 ]
      gvalue = [ gvalue,  48,    103,   141,   188,   188,   149,   113,    81 ]
      bvalue = [ bvalue,   5,     26,    60,   118,   177,   141,   105,    71 ]
      
    ENDIF ELSE BEGIN
    
      ; Set up the color vectors. Both original and Brewer colors.
      colors= ['White']
      rvalue = [ 255]
      gvalue = [ 255]
      bvalue = [ 255]
      colors = [ colors,   'Snow',     'Ivory','Light Yellow', 'Cornsilk',     'Beige',  'Seashell' ]
      rvalue = [ rvalue,     255,         255,       255,          255,          245,        255 ]
      gvalue = [ gvalue,     250,         255,       255,          248,          245,        245 ]
      bvalue = [ bvalue,     250,         240,       224,          220,          220,        238 ]
      colors = [ colors,   'Linen','Antique White','Papaya',     'Almond',     'Bisque',  'Moccasin' ]
      rvalue = [ rvalue,     250,        250,        255,          255,          255,          255 ]
      gvalue = [ gvalue,     240,        235,        239,          235,          228,          228 ]
      bvalue = [ bvalue,     230,        215,        213,          205,          196,          181 ]
      colors = [ colors,   'Wheat',  'Burlywood',    'Tan', 'Light Gray',   'Lavender','Medium Gray' ]
      rvalue = [ rvalue,     245,        222,          210,      230,          230,         210 ]
      gvalue = [ gvalue,     222,        184,          180,      230,          230,         210 ]
      bvalue = [ bvalue,     179,        135,          140,      230,          250,         210 ]
      colors = [ colors,  'Gray', 'Slate Gray',  'Dark Gray',  'Charcoal',   'Black',  'Honeydew', 'Light Cyan' ]
      rvalue = [ rvalue,      190,      112,          110,          70,         0,         240,          224 ]
      gvalue = [ gvalue,      190,      128,          110,          70,         0,         255,          255 ]
      bvalue = [ bvalue,      190,      144,          110,          70,         0,         255,          240 ]
      colors = [ colors,'Powder Blue',  'Sky Blue', 'Cornflower Blue', 'Cadet Blue', 'Steel Blue','Dodger Blue', 'Royal Blue',  'Blue' ]
      rvalue = [ rvalue,     176,          135,          100,              95,            70,           30,           65,            0 ]
      gvalue = [ gvalue,     224,          206,          149,             158,           130,          144,          105,            0 ]
      bvalue = [ bvalue,     230,          235,          237,             160,           180,          255,          225,          255 ]
      colors = [ colors,  'Navy', 'Pale Green','Aquamarine','Spring Green',  'Cyan' ]
      rvalue = [ rvalue,        0,     152,          127,          0,            0 ]
      gvalue = [ gvalue,        0,     251,          255,        250,          255 ]
      bvalue = [ bvalue,      128,     152,          212,        154,          255 ]
      colors = [ colors, 'Turquoise', 'Light Sea Green', 'Sea Green','Forest Green',  'Teal','Green Yellow','Chartreuse', 'Lawn Green' ]
      rvalue = [ rvalue,      64,          143,               46,          34,             0,      173,           127,         124 ]
      gvalue = [ gvalue,     224,          188,              139,         139,           128,      255,           255,         252 ]
      bvalue = [ bvalue,     208,          143,               87,          34,           128,       47,             0,           0 ]
      colors = [ colors, 'Green', 'Lime Green', 'Olive Drab',  'Olive','Dark Green','Pale Goldenrod']
      rvalue = [ rvalue,      0,        50,          107,        85,            0,          238 ]
      gvalue = [ gvalue,    255,       205,          142,       107,          100,          232 ]
      bvalue = [ bvalue,      0,        50,           35,        47,            0,          170 ]
      colors = [ colors,     'Khaki', 'Dark Khaki', 'Yellow',  'Gold', 'Goldenrod','Dark Goldenrod']
      rvalue = [ rvalue,        240,       189,        255,      255,      218,          184 ]
      gvalue = [ gvalue,        230,       183,        255,      215,      165,          134 ]
      bvalue = [ bvalue,        140,       107,          0,        0,       32,           11 ]
      colors = [ colors,'Saddle Brown',  'Rose',   'Pink', 'Rosy Brown','Sandy Brown', 'Peru']
      rvalue = [ rvalue,     139,          255,      255,        188,        244,        205 ]
      gvalue = [ gvalue,      69,          228,      192,        143,        164,        133 ]
      bvalue = [ bvalue,      19,          225,      203,        143,         96,         63 ]
      colors = [ colors,'Indian Red',  'Chocolate',  'Sienna','Dark Salmon',   'Salmon','Light Salmon' ]
      rvalue = [ rvalue,    205,          210,          160,        233,          250,       255 ]
      gvalue = [ gvalue,     92,          105,           82,        150,          128,       160 ]
      bvalue = [ bvalue,     92,           30,           45,        122,          114,       122 ]
      colors = [ colors,  'Orange',      'Coral', 'Light Coral',  'Firebrick', 'Dark Red', 'Brown',  'Hot Pink' ]
      rvalue = [ rvalue,       255,         255,        240,          178,        139,       165,        255 ]
      gvalue = [ gvalue,       165,         127,        128,           34,          0,        42,        105 ]
      bvalue = [ bvalue,         0,          80,        128,           34,          0,        42,        180 ]
      colors = [ colors, 'Deep Pink',    'Magenta',   'Tomato', 'Orange Red',   'Red', 'Crimson', 'Violet Red' ]
      rvalue = [ rvalue,      255,          255,        255,        255,          255,      220,        208 ]
      gvalue = [ gvalue,       20,            0,         99,         69,            0,       20,         32 ]
      bvalue = [ bvalue,      147,          255,         71,          0,            0,       60,        144 ]
      colors = [ colors,    'Maroon',    'Thistle',       'Plum',     'Violet',    'Orchid','Medium Orchid']
      rvalue = [ rvalue,       176,          216,          221,          238,         218,        186 ]
      gvalue = [ gvalue,        48,          191,          160,          130,         112,         85 ]
      bvalue = [ bvalue,        96,          216,          221,          238,         214,        211 ]
      colors = [ colors,'Dark Orchid','Blue Violet',  'Purple']
      rvalue = [ rvalue,      153,          138,       160]
      gvalue = [ gvalue,       50,           43,        32]
      bvalue = [ bvalue,      204,          226,       240]
      colors = [ colors, 'Slate Blue',  'Dark Slate Blue']
      rvalue = [ rvalue,      106,            72]
      gvalue = [ gvalue,       90,            61]
      bvalue = [ bvalue,      205,           139]
      colors = [ colors, 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
      rvalue = [ rvalue,  255,   255,   255,   255,   255,   245,   255,   250 ]
      gvalue = [ gvalue,  255,   250,   255,   255,   248,   245,   245,   240 ]
      bvalue = [ bvalue,  255,   250,   240,   224,   220,   220,   238,   230 ]
      colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
      rvalue = [ rvalue,   250,   255,    255,    255,    255,    245,    222,    210 ]
      gvalue = [ gvalue,   235,   239,    235,    228,    228,    222,    184,    180 ]
      bvalue = [ bvalue,   215,   213,    205,    196,    181,    179,    135,    140 ]
      colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
      rvalue = [ rvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
      gvalue = [ gvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
      bvalue = [ bvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
      colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
      rvalue = [ rvalue,   250,   223,    173,    109,     53,     35,      0,       0 ]
      gvalue = [ gvalue,   253,   242,    221,    193,    156,     132,    97,      69 ]
      bvalue = [ bvalue,   202,   167,    142,    115,     83,      67,    52,      41 ]
      colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
      rvalue = [ rvalue,   232,   202,    158,     99,     53,     33,      8,       8 ]
      gvalue = [ gvalue,   241,   222,    202,    168,    133,    113,     75,      48 ]
      bvalue = [ bvalue,   250,   240,    225,    211,    191,    181,    147,     107 ]
      colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
      rvalue = [ rvalue,   254,    253,    253,    250,    231,    217,    159,    127 ]
      gvalue = [ gvalue,   236,    212,    174,    134,     92,     72,     51,     39 ]
      bvalue = [ bvalue,   217,    171,    107,     52,     12,      1,      3,      4 ]
      colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
      rvalue = [ rvalue,   254,    252,    252,    248,    225,    203,    154,    103 ]
      gvalue = [ gvalue,   232,    194,    146,     97,     45,     24,     12,      0 ]
      bvalue = [ bvalue,   222,    171,    114,     68,     38,     29,     19,     13 ]
      colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
      rvalue = [ rvalue,   244,    222,    188,    152,    119,    106,     80,     63 ]
      gvalue = [ gvalue,   242,    221,    189,    148,    108,     82,     32,      0 ]
      bvalue = [ bvalue,   248,    237,    220,    197,    177,    163,    139,    125 ]
      colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
      rvalue = [ rvalue,   243,    213,    166,     94,     34,      3,      1,      1 ]
      gvalue = [ gvalue,   234,    212,    189,    164,    138,    129,    101,     70 ]
      bvalue = [ bvalue,   244,    232,    219,    204,    171,    139,     82,     54 ]
      colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
      rvalue = [ rvalue,   244,    206,    127,     58,     30,     33,     32,      8 ]
      gvalue = [ gvalue,   250,    236,    205,    175,    125,     95,     48,     29 ]
      bvalue = [ bvalue,   193,    179,    186,    195,    182,    168,    137,     88 ]
      colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
      rvalue = [ rvalue,   201,    245,    253,    251,    228,    193,    114,     59 ]
      gvalue = [ gvalue,    35,    121,    206,    253,    244,    228,    171,     85 ]
      bvalue = [ bvalue,    38,    72,     127,    197,    239,    239,    207,    164 ]
      colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8', 'OPPOSITE']
      rvalue = [ rvalue,  84,    163,   197,   220,   105,    51,    13,     0,   opixel[0]]
      gvalue = [ gvalue,  48,    103,   141,   188,   188,   149,   113,    81,   opixel[1]]
      bvalue = [ bvalue,   5,     26,    60,   118,   177,   141,   105,    71,   opixel[2]]
    ENDELSE
  ENDELSE
  
  
  ; I have completely removed all access to "system" colors in this code. I'll
  ; leave the code here for awhile to be sure no one is using system colors, but
  ; I seriously doubt it will be coming back.
  
  ; Add system color names for IDL version 5.6 and higher. We don't want to
  ; do this we cannot establish a display connection (e.g., we are running
  ; in a cron job). Check for system variable !FSC_Display_Connection. If not
  ; defined, check the connection.
  ;   DefSysV, '!FSC_Display_Connection', EXISTS=sysvarExists
  ;   IF sysvarExists $
  ;        THEN haveConnection = !FSC_Display_Connection $
  ;        ELSE haveConnection = CanConnect()
  ;
  ; Handle depreciated NODISPLAY keyword.
  IF Keyword_Set(nodisplay) THEN haveConnection = 0
  
  ;   IF (Float(!Version.Release) GE 5.6) && Keyword_Set(haveConnection) THEN BEGIN
  ;
  ;          tlb = Widget_Base()
  ;          sc = Widget_Info(tlb, /System_Colors)
  ;          Widget_Control, tlb, /Destroy
  ;          frame = sc.window_frame
  ;          text = sc.window_text
  ;          active = sc.active_border
  ;          shadow = sc.shadow_3d
  ;          highlight = sc.light_3d
  ;          edge = sc.light_edge_3d
  ;          selected = sc.highlight
  ;          face = sc.face_3d
  ;          colors  = [colors,  'Frame',  'Text',  'Active',  'Shadow']
  ;          rvalue =  [rvalue,   frame[0], text[0], active[0], shadow[0]]
  ;          gvalue =  [gvalue,   frame[1], text[1], active[1], shadow[1]]
  ;          bvalue =  [bvalue,   frame[2], text[2], active[2], shadow[2]]
  ;          colors  = [colors,  'Highlight',  'Edge',  'Selected',  'Face']
  ;          rvalue =  [rvalue,   highlight[0], edge[0], selected[0], face[0]]
  ;          gvalue =  [gvalue,   highlight[1], edge[1], selected[1], face[1]]
  ;          bvalue =  [bvalue,   highlight[2], edge[2], selected[2], face[2]]
  ;
  ;    ENDIF
  
  ; Load the colors from the current color table, if you need them.
  IF useCurrentColors THEN BEGIN
    IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, /GET
    IF decomposedState EQ 0 THEN BEGIN
      colors = SIndgen(256)
      rvalue = rrr
      gvalue = ggg
      bvalue = bbb
    ENDIF ELSE BEGIN
      colors = [colors, SIndgen(256)]
      rvalue = [rvalue, rrr]
      gvalue = [gvalue, ggg]
      bvalue = [bvalue, bbb]
    ENDELSE
  ENDIF
  
  ; Make sure we are looking at compressed, uppercase names.
  colors = StrUpCase(StrCompress(StrTrim(colors,2), /Remove_All))
  
  ; Check synonyms of color names.
  FOR j=0, N_Elements(theColor)-1 DO BEGIN
    IF StrUpCase(theColor[j]) EQ 'GREY' THEN theColor[j] = 'GRAY'
    IF StrUpCase(theColor[j]) EQ 'LIGHTGREY' THEN theColor[j] = 'LIGHTGRAY'
    IF StrUpCase(theColor[j]) EQ 'MEDIUMGREY' THEN theColor[j] = 'MEDIUMGRAY'
    IF StrUpCase(theColor[j]) EQ 'SLATEGREY' THEN theColor[j] = 'SLATEGRAY'
    IF StrUpCase(theColor[j]) EQ 'DARKGREY' THEN theColor[j] = 'DARKGRAY'
    IF StrUpCase(theColor[j]) EQ 'AQUA' THEN theColor[j] = 'AQUAMARINE'
    IF StrUpCase(theColor[j]) EQ 'SKY' THEN theColor[j] = 'SKYBLUE'
    IF StrUpCase(theColor[j]) EQ 'NAVYBLUE' THEN theColor[j] = 'NAVY'
    IF StrUpCase(theColor[j]) EQ 'CORNFLOWER' THEN theColor[j] = 'CORNFLOWERBLUE'
    IF StrUpCase(theColor[j]) EQ 'BROWN' THEN theColor[j] = 'SIENNA'
  ENDFOR
  
  ; How many colors do we have?
  ncolors = N_Elements(colors)
  
  ; Check for offset.
  IF (theDepth EQ 8) OR (decomposedState EQ 0) THEN offset = !D.Table_Size - ncolors - 2 ELSE offset = 0
  IF (useCurrentColors) AND (decomposedState EQ 0) THEN offset = 0
  
  ; Did the user want to select a color name? If so, we set
  ; the color name and color index, unless the user provided
  ; them. In the case of a single positional parameter, we treat
  ; this as the color index number as long as it is not a string.
  cancelled = 0.0
  IF Keyword_Set(selectcolor) THEN BEGIN
  
    CASE N_Params() OF
      0: BEGIN
        theColor = PickColorName(Filename=filename, _Extra=extra, Cancel=cancelled, BREWER=brewer)
        IF cancelled THEN RETURN, !P.Color
        IF theDepth GT 8 AND (decomposedState EQ 1) THEN BEGIN
          colorIndex = Fix(!P.Color < (!D.Table_Size - 1))
        ENDIF ELSE BEGIN
          colorIndex = Where(StrUpCase(colors) EQ StrUpCase(StrCompress(theColor, /Remove_All)), count) + offset
          colorIndex = Fix(colorIndex[0])
          IF count EQ 0 THEN Message, 'Cannot find color: ' + StrUpCase(theColor), /NoName
        ENDELSE
        
      END
      1: BEGIN
        IF Size(theColor, /TName) NE 'STRING' THEN BEGIN
          colorIndex = Fix(theColor)
          theColor = brewer ? 'WT1' : 'White'
        ENDIF ELSE colorIndex = Fix(!P.Color < 255)
        theColor = PickColorName(theColor, Filename=filename, _Extra=extra, Cancel=cancelled, BREWER=brewer)
        IF cancelled THEN RETURN, !P.Color
      END
      2: BEGIN
        theColor = PickColorName(theColor, Filename=filename, _Extra=extra, Cancel=cancelled, BREWER=brewer)
        IF cancelled THEN RETURN, !P.Color
      END
    ENDCASE
  ENDIF
  
  ; Make sure you have a color name and color index.
  CASE N_Elements(theColor) OF
    0: BEGIN
      theColor = brewer ? 'WT1' : 'White'
      IF N_Elements(colorIndex) EQ 0 THEN BEGIN
        IF theDepth GT 8 THEN BEGIN
          colorIndex = Fix(!P.Color < (!D.Table_Size - 1))
        ENDIF ELSE BEGIN
          colorIndex = Where(colors EQ theColor, count) + offset
          colorIndex = Fix(colorIndex[0])
          IF count EQ 0 THEN Message, 'Cannot find color: ' + theColor, /NoName
        ENDELSE
      ENDIF ELSE colorIndex = 0S > colorIndex < Fix((!D.Table_Size - 1))
    ENDCASE
    
    1: BEGIN
      type = Size(theColor, /TNAME)
      IF type NE 'STRING' THEN Message, 'The color must be expressed as a color name.'
      theColor = theColor[0] ; Make it a scalar or you run into a WHERE function "feature". :-(
      IF N_Elements(colorIndex) EQ 0 THEN BEGIN
        IF (theDepth GT 8) AND (decomposedState EQ 1) THEN BEGIN
          colorIndex = Fix(!P.Color < (!D.Table_Size - 1))
        ENDIF ELSE BEGIN
          colorIndex = Where(colors EQ theColor, count) + offset
          colorIndex = Fix(colorIndex[0])
          IF count EQ 0 THEN Message, 'Cannot find color: ' + theColor, /NoName
        ENDELSE
      ENDIF ELSE colorIndex = 0S > colorIndex < Fix(!D.Table_Size - 1)
    ENDCASE
    
    ELSE: BEGIN
      type = Size(theColor, /TNAME)
      IF type NE 'STRING' THEN Message, 'The colors must be expressed as color names.'
      ncolors = N_Elements(theColor)
      CASE N_Elements(colorIndex) OF
        0: colorIndex = Fix(Indgen(ncolors) + (!D.Table_Size - (ncolors + 1)))
        1: colorIndex = Fix(Indgen(ncolors) + colorIndex)
        ELSE: IF N_Elements(colorIndex) NE ncolors THEN $
          Message, 'Index vector must be the same length as color name vector.'
      ENDCASE
      
      ; Did the user want color triples?
      
      IF Keyword_Set(triple) THEN BEGIN
        colors = LonArr(ncolors, 3)
        FOR j=0,ncolors-1 DO colors[j,*] = FSC_Color(theColor[j], colorIndex[j], Filename=filename, $
          Decomposed=decomposedState, /Triple, BREWER=brewer)
        RETURN, colors
      ENDIF ELSE BEGIN
        colors = LonArr(ncolors)
        FOR j=0,ncolors-1 DO colors[j] = FSC_Color(theColor[j], colorIndex[j], Filename=filename, $
          Decomposed=decomposedState, BREWER=brewer)
        RETURN, colors
      ENDELSE
    END
  ENDCASE
  
  ; Did the user ask for the color names? If so, return them now.
  IF Keyword_Set(names) THEN RETURN, Reform(colors, 1, ncolors)
  
  ; Process the color names.
  theNames = StrUpCase( StrCompress(colors, /Remove_All ) )
  
  ; Find the asked-for color in the color names array.
  theIndex = Where(theNames EQ StrUpCase(StrCompress(theColor, /Remove_All)), foundIt)
  theIndex = theIndex[0]
  
  ; If the color can't be found, report it and continue with the color set to "OPPOSITE."
  IF foundIt EQ 0 THEN BEGIN
    Message, "Can't find color " + theColor + ". Substituting 'OPPOSITE'.", /Informational
    theColor = 'OPPOSITE'
    theIndex = Where(StrUpCase(colors) EQ 'OPPOSITE')
  ENDIF
  
  ; Get the color triple for this color.
  r = rvalue[theIndex]
  g = gvalue[theIndex]
  b = bvalue[theIndex]
  
  ; Did the user want a color triple? If so, return it now.
  IF Keyword_Set(triple) THEN BEGIN
    IF Keyword_Set(allcolors) THEN BEGIN
      IF Keyword_Set(row) THEN RETURN, Transpose([[rvalue], [gvalue], [bvalue]]) ELSE RETURN, [[rvalue], [gvalue], [bvalue]]
    ENDIF ELSE BEGIN
      IF Keyword_Set(row) THEN RETURN, [r, g, b] ELSE RETURN, [[r], [g], [b]]
    ENDELSE
  ENDIF
  
  ; Otherwise, we are going to return either an index
  ; number where the color has been loaded, or a 24-bit
  ; value that can be decomposed into the proper color.
  IF decomposedState THEN BEGIN
  
    ; Need a color structure?
    IF Arg_Present(colorStructure) THEN BEGIN
      theColors = FSC_Color_Color24([[rvalue], [gvalue], [bvalue]])
      colorStructure = Create_Struct(theNames[0], theColors[0])
      FOR j=1, ncolors-1 DO colorStructure = Create_Struct(colorStructure, theNames[j], theColors[j])
    ENDIF
    
    IF Keyword_Set(allcolors) THEN BEGIN
      RETURN, FSC_Color_Color24([[rvalue], [gvalue], [bvalue]])
    ENDIF ELSE BEGIN
      RETURN, FSC_Color_Color24([r, g, b])
    ENDELSE
    
  ENDIF ELSE BEGIN
  
    IF Keyword_Set(allcolors) THEN BEGIN
    
      ; Need a color structure?
      IF Arg_Present(colorStructure) THEN BEGIN
        allcolorIndex = !D.Table_Size - ncolors - 2
        IF allcolorIndex LT 0 THEN $
          Message, 'Number of colors exceeds available color table values. Returning.', /NoName
        IF (allcolorIndex + ncolors) GT 255 THEN $
          Message, 'Number of colors exceeds available color table indices. Returning.', /NoName
        theColors = IndGen(ncolors) + allcolorIndex
        colorStructure = Create_Struct(theNames[0],  theColors[0])
        FOR j=1, ncolors-1 DO colorStructure = Create_Struct(colorStructure, theNames[j], theColors[j])
      ENDIF
      
      IF N_Elements(colorIndex) EQ 0 THEN colorIndex = Fix(!D.Table_Size - ncolors - 2)
      IF colorIndex LT 0 THEN $
        Message, 'Number of colors exceeds available color table values. Returning.', /NoName
      IF (colorIndex + ncolors) GT 255 THEN BEGIN
        colorIndex = Fix(!D.Table_Size - ncolors - 2)
      ENDIF
      IF (!D.Name NE 'PRINTER') AND (!D.Name NE 'NULL') THEN TVLCT, rvalue, gvalue, bvalue, colorIndex
      RETURN, IndGen(ncolors) + colorIndex
    ENDIF ELSE BEGIN
    
      ; Need a color structure?
      IF Arg_Present(colorStructure) THEN BEGIN
        colorStructure = Create_Struct(theColor,  colorIndex)
      ENDIF
      
      IF (!D.Name NE 'PRINTER') AND (!D.Name NE 'NULL') THEN $
        TVLCT, rvalue[theIndex], gvalue[theIndex], bvalue[theIndex], colorIndex
      RETURN, Fix(colorIndex)
    ENDELSE
    
    
  ENDELSE
  
END ;-------------------------------------------------------------------------------------------------------