;@structure_definition
;FUNCTION PickColorName, theName, $         ; The name of the starting color.
;    Brewer=brewer, $                        ; Select brewer colors.
;    Bottom=bottom, $                        ; The index number where the colors should be loaded.
;    Cancel=cancelled, $                     ; An output keyword set to 1 if the user cancelled or an error occurred.
;    Columns = ncols, $                      ; The number of columns to display the colors in.
;    Filename=filename, $                    ; The name of the file which contains color names and values.
;    Group_Leader=group_leader, $            ; The group leader of the TLB. Required for modal operation.
;    Index=index, $                          ; The color index number where the final selected color should be loaded.
;    Title=title                             ; The title of the top-level base widget.
;    
;  ; Error handling for this program module.
;    
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /Cancel
;    ok = Error_Message(/Traceback)
;    cancel = 1
;    IF N_Elements(theName) NE 0 THEN RETURN, theName ELSE RETURN, 'White'
;  ENDIF
;  
;  ; Get depth of visual display.
;  
;  IF (!D.Flags AND 256) NE 0 THEN Device, Get_Visual_Depth=theDepth ELSE theDepth = 8
;  
;  ; Is there a filename? If so, get colors from there.
;  
;  IF N_Elements(filename) NE 0 THEN BEGIN
;  
;    ; Count the number of rows in the file.
;  
;    NCOLORS = PickColorName_Count_Rows(filename)
;    
;    ; Read the data.
;    
;    OpenR, lun, filename, /Get_Lun
;    red = BytArr(NCOLORS)
;    green = BytArr(NCOLORS)
;    blue = BytArr(NCOLORS)
;    colors = StrArr(NCOLORS)
;    redvalue = 0B
;    greenvalue = 0B
;    bluevalue = 0B
;    namevalue = ""
;    FOR j=0L, NCOLORS-1 DO BEGIN
;      ReadF, lun, redvalue, greenvalue, bluevalue, namevalue
;      colors[j] = namevalue
;      red[j] = redvalue
;      green[j] = greenvalue
;      blue[j] = bluevalue
;    ENDFOR
;    
;    ; Trim the colors array of blank characters.
;    
;    colors = StrTrim(colors, 2)
;    
;    ; Calculate the number of columns to display colors in.
;    
;    IF N_Elements(ncols) EQ 0 THEN ncols = Fix(Sqrt(ncolors))
;    Free_Lun, lun
;  ENDIF ELSE BEGIN
;  
;    IF theDepth GT 8 THEN BEGIN
;    
;      ; The colors with their names.
;    
;      IF N_Elements(ncols) EQ 0 THEN ncols = 12
;      IF Keyword_Set(brewer) THEN BEGIN
;        colors = [ 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
;        red =   [   255,   255,   255,   255,   255,   245,   255,   250 ]
;        green = [   255,   250,   255,   255,   248,   245,   245,   240 ]
;        blue =  [   255,   250,   240,   224,   220,   220,   238,   230 ]
;        colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
;        red =   [ red,      250,   255,    255,    255,    255,    245,    222,    210 ]
;        green = [ green,    235,   239,    235,    228,    228,    222,    184,    180 ]
;        blue =  [ blue,     215,   213,    205,    196,    181,    179,    135,    140 ]
;        colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
;        red =   [ red,      250,   230,    210,    190,    112,     110,    70,       0 ]
;        green = [ green,    250,   230,    210,    190,    128,     110,    70,       0 ]
;        blue =  [ blue,     250,   230,    210,    190,    128,     110,    70,       0 ]
;        colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
;        red =   [ red,      250,   223,    173,    109,     53,     35,      0,       0 ]
;        green = [ green,    253,   242,    221,    193,    156,     132,    97,      69 ]
;        blue =  [ blue,     202,   167,    142,    115,     83,      67,    52,      41 ]
;        colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
;        red =   [ red,      232,   202,    158,     99,     53,     33,      8,       8 ]
;        green = [ green,    241,   222,    202,    168,    133,    113,     75,      48 ]
;        blue =  [ blue,     250,   240,    225,    211,    191,    181,    147,     107 ]
;        colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
;        red =   [ red,      254,    253,    253,    250,    231,    217,    159,    127 ]
;        green = [ green,    236,    212,    174,    134,     92,     72,     51,     39 ]
;        blue =  [ blue,     217,    171,    107,     52,     12,      1,      3,      4 ]
;        colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
;        red =   [ red,      254,    252,    252,    248,    225,    203,    154,    103 ]
;        green = [ green,    232,    194,    146,     97,     45,     24,     12,      0 ]
;        blue =  [ blue,     222,    171,    114,     68,     38,     29,     19,     13 ]
;        colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
;        red =   [ red,      244,    222,    188,    152,    119,    106,     80,     63 ]
;        green = [ green,    242,    221,    189,    148,    108,     82,     32,      0 ]
;        blue =  [ blue,     248,    237,    220,    197,    177,    163,    139,    125 ]
;        colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
;        red =   [ red,      243,    213,    166,     94,     34,      3,      1,      1 ]
;        green = [ green,    234,    212,    189,    164,    138,    129,    101,     70 ]
;        blue =  [ blue,     244,    232,    219,    204,    171,    139,     82,     54 ]
;        colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
;        red =   [ red,      244,    206,    127,     58,     30,     33,     32,      8 ]
;        green = [ green,    250,    236,    205,    175,    125,     95,     48,     29 ]
;        blue =  [ blue,     193,    179,    186,    195,    182,    168,    137,     88 ]
;        colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
;        red =   [ red,       201,    245,    253,    251,    228,    193,    114,     59 ]
;        green = [ green,      35,    121,    206,    253,    244,    228,    171,     85 ]
;        blue =  [ blue,       38,    72,     127,    197,    239,    239,    207,    164 ]
;        colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8']
;        red =   [ red,      84,    163,   197,   220,   105,    51,    13,     0 ]
;        green = [ green,    48,    103,   141,   188,   188,   149,   113,    81 ]
;        blue =  [ blue,      5,     26,    60,   118,   177,   141,   105,    71 ]
;      ENDIF ELSE BEGIN
;        colors= ['White']
;        red =   [ 255]
;        green = [ 255]
;        blue =  [ 255]
;        colors= [ colors,      'Snow',     'Ivory','Light Yellow',   'Cornsilk',      'Beige',   'Seashell' ]
;        red =   [ red,            255,          255,          255,          255,          245,          255 ]
;        green = [ green,          250,          255,          255,          248,          245,          245 ]
;        blue =  [ blue,           250,          240,          224,          220,          220,          238 ]
;        colors= [ colors,     'Linen','Antique White',    'Papaya',     'Almond',     'Bisque',  'Moccasin' ]
;        red =   [ red,            250,          250,          255,          255,          255,          255 ]
;        green = [ green,          240,          235,          239,          235,          228,          228 ]
;        blue =  [ blue,           230,          215,          213,          205,          196,          181 ]
;        colors= [ colors,     'Wheat',  'Burlywood',        'Tan', 'Light Gray',   'Lavender','Medium Gray' ]
;        red =   [ red,            245,          222,          210,          230,          230,          210 ]
;        green = [ green,          222,          184,          180,          230,          230,          210 ]
;        blue =  [ blue,           179,          135,          140,          230,          250,          210 ]
;        colors= [ colors,      'Gray', 'Slate Gray',  'Dark Gray',   'Charcoal',      'Black',   'Honeydew', 'Light Cyan' ]
;        red =   [ red,            190,          112,          110,           70,            0,          240,          224 ]
;        green = [ green,          190,          128,          110,           70,            0,          255,          255 ]
;        blue =  [ blue,           190,          144,          110,           70,            0,          255,          240 ]
;        colors= [ colors,'Powder Blue',  'Sky Blue', 'Cornflower Blue', 'Cadet Blue', 'Steel Blue','Dodger Blue', 'Royal Blue',  'Blue' ]
;        red =   [ red,            176,          135,         100,           95,            70,           30,           65,            0 ]
;        green = [ green,          224,          206,         149,          158,           130,          144,          105,            0 ]
;        blue =  [ blue,           230,          235,         237,          160,           180,          255,          225,          255 ]
;        colors= [ colors,      'Navy', 'Pale Green','Aquamarine','Spring Green',       'Cyan' ]
;        red =   [ red,              0,          152,          127,            0,            0 ]
;        green = [ green,            0,          251,          255,          250,          255 ]
;        blue =  [ blue,           128,          152,          212,          154,          255 ]
;        colors= [ colors, 'Turquoise', 'Light Sea Green', 'Sea Green','Forest Green',  'Teal','Green Yellow','Chartreuse', 'Lawn Green' ]
;        red =   [ red,             64,       143,             46,           34,             0,      173,           127,         124 ]
;        green = [ green,          224,       188,            139,          139,           128,      255,           255,         252 ]
;        blue =  [ blue,           208,       143,             87,           34,           128,       47,             0,           0 ]
;        colors= [ colors,     'Green', 'Lime Green', 'Olive Drab',     'Olive','Dark Green','Pale Goldenrod']
;        red =   [ red,              0,           50,          107,           85,            0,          238 ]
;        green = [ green,          255,          205,          142,          107,          100,          232 ]
;        blue =  [ blue,             0,           50,           35,           47,            0,          170 ]
;        colors =[ colors,     'Khaki', 'Dark Khaki',     'Yellow',       'Gold','Goldenrod','Dark Goldenrod']
;        red =   [ red,            240,          189,          255,          255,          218,          184 ]
;        green = [ green,          230,          183,          255,          215,          165,          134 ]
;        blue =  [ blue,           140,          107,            0,            0,           32,           11 ]
;        colors= [ colors,'Saddle Brown',       'Rose',       'Pink', 'Rosy Brown','Sandy Brown',      'Peru']
;        red =   [ red,            139,          255,          255,          188,          244,          205 ]
;        green = [ green,           69,          228,          192,          143,          164,          133 ]
;        blue =  [ blue,            19,          225,          203,          143,           96,           63 ]
;        colors= [ colors,'Indian Red',  'Chocolate',     'Sienna','Dark Salmon',    'Salmon','Light Salmon' ]
;        red =   [ red,            205,          210,          160,          233,          250,          255 ]
;        green = [ green,           92,          105,           82,          150,          128,          160 ]
;        blue =  [ blue,            92,           30,           45,          122,          114,          122 ]
;        colors= [ colors,    'Orange',      'Coral', 'Light Coral',  'Firebrick',   'Dark Red',    'Brown',  'Hot Pink' ]
;        red =   [ red,            255,          255,          240,          178,       139,          165,        255 ]
;        green = [ green,          165,          127,          128,           34,         0,           42,        105 ]
;        blue =  [ blue,             0,           80,          128,           34,         0,           42,        180 ]
;        colors= [ colors, 'Deep Pink',    'Magenta',     'Tomato', 'Orange Red',        'Red', 'Crimson', 'Violet Red' ]
;        red =   [ red,            255,          255,          255,          255,          255,      220,        208 ]
;        green = [ green,           20,            0,           99,           69,            0,       20,         32 ]
;        blue =  [ blue,           147,          255,           71,            0,            0,       60,        144 ]
;        colors= [ colors,    'Maroon',    'Thistle',       'Plum',     'Violet',    'Orchid','Medium Orchid']
;        red =   [ red,            176,          216,          221,          238,          218,          186 ]
;        green = [ green,           48,          191,          160,          130,          112,           85 ]
;        blue =  [ blue,            96,          216,          221,          238,          214,          211 ]
;        colors= [ colors,'Dark Orchid','Blue Violet',     'Purple']
;        red =   [ red,            153,          138,          160]
;        green = [ green,           50,           43,           32]
;        blue =  [ blue,           204,          226,          240]
;        colors= [ colors, 'Slate Blue',  'Dark Slate Blue']
;        red =   [ red,           106,            72]
;        green = [ green,            90,            61]
;        blue =  [ blue,           205,           139]
;        colors = [ colors, 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
;        red =   [ red,     255,   255,   255,   255,   255,   245,   255,   250 ]
;        green = [ green,   255,   250,   255,   255,   248,   245,   245,   240 ]
;        blue =  [ blue,    255,   250,   240,   224,   220,   220,   238,   230 ]
;        colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
;        red =   [ red,      250,   255,    255,    255,    255,    245,    222,    210 ]
;        green = [ green,    235,   239,    235,    228,    228,    222,    184,    180 ]
;        blue =  [ blue,     215,   213,    205,    196,    181,    179,    135,    140 ]
;        colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
;        red =   [ red,      250,   230,    210,    190,    112,     110,    70,       0 ]
;        green = [ green,    250,   230,    210,    190,    128,     110,    70,       0 ]
;        blue =  [ blue,     250,   230,    210,    190,    128,     110,    70,       0 ]
;        colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
;        red =   [ red,      250,   223,    173,    109,     53,     35,      0,       0 ]
;        green = [ green,    253,   242,    221,    193,    156,     132,    97,      69 ]
;        blue =  [ blue,     202,   167,    142,    115,     83,      67,    52,      41 ]
;        colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
;        red =   [ red,      232,   202,    158,     99,     53,     33,      8,       8 ]
;        green = [ green,    241,   222,    202,    168,    133,    113,     75,      48 ]
;        blue =  [ blue,     250,   240,    225,    211,    191,    181,    147,     107 ]
;        colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
;        red =   [ red,      254,    253,    253,    250,    231,    217,    159,    127 ]
;        green = [ green,    236,    212,    174,    134,     92,     72,     51,     39 ]
;        blue =  [ blue,     217,    171,    107,     52,     12,      1,      3,      4 ]
;        colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
;        red =   [ red,      254,    252,    252,    248,    225,    203,    154,    103 ]
;        green = [ green,    232,    194,    146,     97,     45,     24,     12,      0 ]
;        blue =  [ blue,     222,    171,    114,     68,     38,     29,     19,     13 ]
;        colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
;        red =   [ red,      244,    222,    188,    152,    119,    106,     80,     63 ]
;        green = [ green,    242,    221,    189,    148,    108,     82,     32,      0 ]
;        blue =  [ blue,     248,    237,    220,    197,    177,    163,    139,    125 ]
;        colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
;        red =   [ red,      243,    213,    166,     94,     34,      3,      1,      1 ]
;        green = [ green,    234,    212,    189,    164,    138,    129,    101,     70 ]
;        blue =  [ blue,     244,    232,    219,    204,    171,    139,     82,     54 ]
;        colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
;        red =   [ red,      244,    206,    127,     58,     30,     33,     32,      8 ]
;        green = [ green,    250,    236,    205,    175,    125,     95,     48,     29 ]
;        blue =  [ blue,     193,    179,    186,    195,    182,    168,    137,     88 ]
;        colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
;        red =   [ red,       201,    245,    253,    251,    228,    193,    114,     59 ]
;        green = [ green,      35,    121,    206,    253,    244,    228,    171,     85 ]
;        blue =  [ blue,       38,    72,     127,    197,    239,    239,    207,    164 ]
;        colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8']
;        red =   [ red,      84,    163,   197,   220,   105,    51,    13,     0 ]
;        green = [ green,    48,    103,   141,   188,   188,   149,   113,    81 ]
;        blue =  [ blue,      5,     26,    60,   118,   177,   141,   105,    71 ]
;      ENDELSE
;    ENDIF ELSE BEGIN
;    
;      IF N_Elements(ncols) EQ 0 THEN ncols = 8
;      colors  = ['Black', 'Magenta', 'Cyan', 'Yellow', 'Green']
;      red =     [  0,        255,       0,      255,       0  ]
;      green =   [  0,          0,     255,      255,     255  ]
;      blue =    [  0,        255,     255,        0,       0  ]
;      colors  = [colors,  'Red', 'Blue', 'Navy', 'Pink', 'Aqua']
;      red =     [red,     255,     0,      0,    255,    112]
;      green =   [green,     0,     0,      0,    127,    219]
;      blue =    [blue,      0,   255,    115,    127,    147]
;      colors  = [colors,  'Orchid', 'Sky', 'Beige', 'Charcoal', 'Gray','White']
;      red =     [red,     219,      0,     245,       80,      135,    255  ]
;      green =   [green,   112,    163,     245,       80,      135,    255  ]
;      blue =    [blue,    219,    255,     220,       80,      135,    255  ]
;      
;    ENDELSE
;  ENDELSE
;  
;  NCOLORS = N_Elements(colors)
;  
;  ;; Add system color names for IDL version 5.6 and higher. We don't want to
;  ;; do this we cannot establish a display connection (e.g., we are running
;  ;; in a cron job). Check for system variable !FSC_Display_Connection. If not
;  ;; defined, check the connection.
;  ;DefSysV, '!FSC_Display_Connection', EXISTS=sysvarExists
;  ;IF sysvarExists $
;  ;     THEN haveConnection = !FSC_Display_Connection $
;  ;     ELSE haveConnection = CanConnect()
;  ;
;  ;IF (Float(!Version.Release) GE 5.6) && Keyword_Set(haveConnection) THEN BEGIN
;  ;
;  ;   tlb = Widget_Base()
;  ;   sc = Widget_Info(tlb, /System_Colors)
;  ;   Widget_Control, tlb, /Destroy
;  ;   frame = sc.window_frame
;  ;   text = sc.window_text
;  ;   active = sc.active_border
;  ;   shadow = sc.shadow_3d
;  ;   highlight = sc.light_3d
;  ;   edge = sc.light_edge_3d
;  ;   selected = sc.highlight
;  ;   face = sc.face_3d
;  ;   colors  = [colors,  'Frame',  'Text',  'Active',  'Shadow']
;  ;   red =     [red,     frame[0], text[0], active[0], shadow[0]]
;  ;   green =   [green,   frame[1], text[1], active[1], shadow[1]]
;  ;   blue =    [blue,    frame[2], text[2], active[2], shadow[2]]
;  ;   colors  = [colors,  'Highlight',  'Edge',  'Selected',  'Face']
;  ;   red =     [red,     highlight[0], edge[0], selected[0], face[0]]
;  ;   green =   [green,   highlight[1], edge[1], selected[1], face[1]]
;  ;   blue =    [blue,    highlight[2], edge[2], selected[2], face[2]]
;  ;
;  ;ENDIF
;  
;  
;  ; Save decomposed state and restore it, if possible.
;  
;  IF Float(!Version.Release) GE 5.2 THEN BEGIN
;    Device, Get_Decomposed=decomposedState
;  ENDIF ELSE decomposedState = 0
;  
;  ; Different color decomposition based on visual depth.
;  
;  IF theDepth GT 8 THEN BEGIN
;    Device, Decomposed=1
;    colors24 = PickColorName_RGB_to_24Bit([[red], [green], [blue]])
;  ENDIF ELSE BEGIN
;    IF NCOLORS GT !D.Table_Size THEN $
;      Message, /NoName, 'Number of colors exceeds color table size. Returning...'
;    Device, Decomposed=0
;    colors24 = -1
;  ENDELSE
;  
;  ; Check argument values. All arguments are optional.
;  
;  IF N_Elements(theName) EQ 0 THEN IF Keyword_Set(brewer) THEN theName = 'WT1' ELSE theName = 'White'
;  IF Size(theName, /TName) NE 'STRING' THEN $
;    Message, 'Color name argument must be STRING type.', /NoName
;  theName = StrCompress(theName, /Remove_All)
;  
;  IF N_Elements(bottom) EQ 0 THEN bottom = 0 > (!D.Table_Size - (NCOLORS + 2))
;  mixcolorIndex = bottom
;  IF N_Elements(title) EQ 0 THEN title='Select a Color'
;  
;  ; We will work with all uppercase names.
;  
;  colorNames = StrUpCase(StrCompress(colors, /Remove_All))
;  
;  ; Get the current color table vectors before we change anything.
;  ; This will allow us to restore the color table when we depart.
;  
;  TVLCT, r_old, g_old, b_old, /Get
;  
;  ; Load the colors if needed. The "bottom" index is reserved as the "mixing color" index.
;  
;  IF theDepth LE 8 THEN TVLCT, red, green, blue, bottom+1
;  
;  ; Can you find the color name in the colors array?
;  
;  nameIndex = WHERE(colorNames EQ StrUpCase(theName), count)
;  IF count EQ 0 THEN BEGIN
;    Message, 'Unable to resolve color name: ' + StrUpCase(theName) + '. Replacing with WHITE.', /Informational
;    theName = 'WT1'
;    nameIndex = WHERE(colorNames EQ StrUpCase(theName), count)
;    IF count EQ 0 THEN Message, /NoName, 'Unable to resolve color name: ' + StrUpCase(theName) + '. Returning...'
;  ENDIF
;  nameIndex = nameIndex[0]
;  
;  ; Who knows how the user spelled the color? Make it look nice.
;  
;  theName = colors[nameIndex]
;  
;  ; Load the mixing color in the mixcolorIndex.
;  
;  IF theDepth LE 8 THEN TVLCT, red[nameIndex], green[nameIndex], blue[nameIndex], mixcolorIndex
;  
;  ; Create the widgets. TLB is MODAL or BLOCKING, depending upon presence of
;  ; Group_Leader variable.
;  
;  IF N_Elements(group_leader) EQ 0 THEN BEGIN
;    tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center)
;  ENDIF ELSE BEGIN
;    tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center, /Modal, $
;      Group_Leader=group_leader)
;  ENDELSE
;  
;  ; Draw widgets for the possible colors. Store the color name in the UVALUE.
;  
;  colorbaseID = Widget_Base(tlb, Column=ncols, Event_Pro='PickColorName_Select_Color')
;  drawID = LonArr(NCOLORS)
;  FOR j=0,NCOLORS-1 DO BEGIN
;    drawID[j] = Widget_Draw(colorbaseID, XSize=20, YSize=15, $
;      UValue=colors[j], Button_Events=1)
;  ENDFOR
;  
;  ; System colors.
;  
;  IF N_Elements(colors) GT NCOLORS THEN BEGIN
;    systemColorbase = Widget_Base(tlb, Column=8, Event_Pro='PickColorName_Select_Color')
;    drawID = [Temporary(drawID), LonArr(8)]
;    FOR j=NCOLORS,N_Elements(colors)-1 DO BEGIN
;      drawID[j] = Widget_Draw(systemColorbase, XSize=20, YSize=15, $
;        UValue=colors[j], Button_Events=1)
;    ENDFOR
;  ENDIF
;  
;  ; Set up the current or mixing color draw widget.
;  
;  currentID = Widget_Base(tlb, Column=1, Base_Align_Center=1)
;  
;  ; Special concerns with LABEL widgets to work around a widget update bug in
;  ; X11 libraries.
;  IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' $
;    THEN labelID = Widget_Label(currentID, Value=theName, /Dynamic_Resize) $
;  ELSE labelID = Widget_Label(currentID, Value=theName, /Dynamic_Resize, SCR_XSIZE=150)
;  mixColorID = Widget_Draw(currentID, XSize=60, YSize=15)
;  
;  ; CANCEL and ACCEPT buttons.
;  
;  buttonbase = Widget_Base(tlb, ROW=1, Align_Center=1, Event_Pro='PickColorName_Buttons')
;  cancelID = Widget_Button(buttonbase, VALUE='Cancel')
;  acceptID = Widget_Button(buttonbase, VALUE='Accept')
;  
;  ; Center the TLB.
;  
;  PickColorName_CenterTLB, tlb
;  Widget_Control, tlb, /Realize
;  
;  ; Load the drawing colors.
;  
;  wids = IntArr(NCOLORS)
;  IF theDepth GT 8 THEN BEGIN
;    FOR j=0,NCOLORS-1 DO BEGIN
;      Widget_Control, drawID[j], Get_Value=thisWID
;      wids[j] = thisWID
;      WSet, thisWID
;      PolyFill, [1,1,19,19,1], [0,13,13,0,0], /Device, Color=colors24[j]
;    ENDFOR
;    IF (N_Elements(colors) GT NCOLORS) THEN BEGIN
;      wids = [Temporary(wids), Intarr(8)]
;      FOR j=NCOLORS, N_Elements(colors)-1 DO BEGIN
;        Widget_Control, drawID[j], Get_Value=thisWID
;        wids[j] = thisWID
;        WSet, thisWID
;        PolyFill, [1,1,19,19,1], [0,13,13,0,0], /Device, Color=colors24[j]
;      ENDFOR
;    ENDIF
;  ENDIF ELSE BEGIN
;    FOR j=1,NCOLORS DO BEGIN
;      Widget_Control, drawID[j-1], Get_Value=thisWID
;      wids[j-1] = thisWID
;      WSet, thisWID
;      PolyFill, [1,1,19,19,1], [0,13,13,0,0], /Device, Color=bottom + black
;    ENDFOR
;    IF (N_Elements(colors) GT NCOLORS) THEN BEGIN
;      wids = [Temporary(wids), Intarr(8)]
;      FOR j=NCOLORS+1, N_Elements(colors) DO BEGIN
;        Widget_Control, drawID[j-1], Get_Value=thisWID
;        wids[j-1] = thisWID
;        WSet, thisWID
;        PolyFill, [1,1,19,19,1], [0,13,13,0,0], /Device, Color=bottom + j
;      ENDFOR
;    ENDIF
;  ENDELSE
;  
;  ; Load the current or mixing color.
;  
;  Widget_Control, mixColorID, Get_Value=mixWID
;  WSet, mixWID
;  IF theDepth GT 8 THEN BEGIN
;    Erase, Color=colors24[nameIndex]
;    eraseColor = Keyword_Set(brewer) ? 'BLK8' : 'BLACK
;    black = Where(colornames EQ eraseColor)
;    black = black[0]
;    PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=colors24[black]
;  ENDIF ELSE BEGIN
;    eraseColor = Keyword_Set(brewer) ? 'BLK8' : 'BLACK
;    black = Where(colornames EQ eraseColor)
;    Erase, Color=mixcolorIndex
;    PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=black
;  ENDELSE
;  
;  ; Pointer to hold the color form information.
;  
;  ptr = Ptr_New({cancel:1.0, r:0B, g:0B, b:0B, name:theName})
;  
;  ; Info structure for program information.
;  
;  info = { ptr:ptr, $                    ; The pointer to the form information.
;    mixColorIndex:mixColorIndex, $
;    colorNames:colorNames, $
;    nameIndex:nameIndex, $
;    red:red, $
;    green:green, $
;    blue:blue, $
;    black:black, $
;    colors24:colors24, $
;    mixWid:mixWid, $
;    theDepth:theDepth, $
;    labelID:labelID, $
;    theName:theName $
;    }
;    
;  ; Store the info structure in the UVALUE of the TLB.
;    
;  Widget_Control, tlb, Set_UValue=info, /No_Copy
;  
;  ; Set up program event loop. This will be blocking widget
;  ; if called from the IDL command line. Program operation
;  ; will stop here until widget interface is destroyed.
;  
;  XManager, 'pickcolor', tlb
;  
;  ; Retrieve the color information from the pointer and free
;  ; the pointer.
;  
;  colorInfo = *ptr
;  Ptr_Free, ptr
;  
;  ; Set the Cancel flag.
;  
;  cancelled = colorInfo.cancel
;  
;  ; Restore color table, taking care to load the color index if required.
;  
;  IF N_Elements(index) NE 0 AND (NOT cancelled) THEN BEGIN
;    r_old[index] = colorInfo.r
;    g_old[index] = colorInfo.g
;    b_old[index] = colorInfo.b
;  ENDIF
;  TVLCT, r_old, g_old, b_old
;  
;  ; Restore decomposed state if possible.
;  
;  IF Float(!Version.Release) GE 5.2 THEN Device, Decomposed=decomposedState
;  
;  ; Return the color name.
;  
;  RETURN, colorInfo.name
;END
;FUNCTION FSC_Color, theColour, colorIndex, $
;    AllColors=allcolors, $
;    Brewer=brewer, $
;    Check_Connection=check_connection, $ ; This keyword is completely ignored.
;    ColorStructure=colorStructure, $
;    Cancel=cancelled, $
;    Decomposed=decomposedState, $
;    _Extra=extra, $
;    Filename=filename, $
;    Names=names, $
;    NColors=ncolors, $
;    NODISPLAY=nodisplay, $ ; This keyword is completely ignored.
;    Row=row, $
;    SelectColor=selectcolor, $
;    Triple=triple
;    
;  ; Return to caller as the default error behavior.
;  On_Error, 2
;  
;  ; Error handling for the rest of the program.
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /Cancel
;    ok = Error_Message(/Traceback)
;    cancelled = 1
;    RETURN, !P.Color
;  ENDIF
;  
;  ; Set up PostScript device for working with colors.
;  IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
;  
;  ; I don't want to change the original variable.
;  IF N_Elements(theColour) NE 0 THEN theColor = theColour ELSE $
;    theColor = 'OPPOSITE'
;    
;  ; Make sure the color parameter is a string.
;  varInfo = Size(theColor)
;  IF varInfo[varInfo[0] + 1] NE 7 THEN $
;    Message, 'The color name parameter must be a string.', /NoName
;    
;  ; We don't want underscores in color names. Turn all underscores
;  ; to spaces.
;  FOR j=0,N_Elements(theColor)-1 DO BEGIN
;    theColor[j] = StrJoin( StrSplit(theColor[j], '_', /Extract, $
;      /Preserve_Null), ' ')
;  ENDFOR
;  
;  ; Make sure the color is compressed and uppercase.
;  theColor = StrUpCase(StrCompress(StrTrim(theColor,2), /Remove_All))
;  
;  ; Get the pixel value of the "opposite" color. This is the pixel color
;  ; opposite the pixel color in the upper right corner of the display.
;  IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
;    opixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
;    IF N_Elements(opixel) NE 3 THEN BEGIN
;      IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, /Get
;      opixel = [rrr[opixel], ggg[opixel], bbb[opixel]]
;    ENDIF
;  ENDIF ELSE BEGIN
;    IF (!D.Name EQ 'PS') THEN opixel = [255,255,255]
;  ENDELSE
;  IF N_Elements(opixel) EQ 0 THEN opixel = [0,0,0]
;  opixel = 255 - opixel
;  
;  ; Read the first color as bytes. If none of the bytes are less than 48
;  ; or greater than 57, then this is a "number" string and you should
;  ; assume the current color table is being used.
;  bytecheck = Byte(theColor[0])
;  i = Where(bytecheck LT 48, lessthan)
;  i = Where(bytecheck GT 57, greaterthan)
;  IF (lessthan + greaterthan) EQ 0 THEN useCurrentColors = 1 ELSE useCurrentColors = 0
;  
;  ; Get the decomposed state of the IDL session right now.
;  IF N_Elements(decomposedState) EQ 0 THEN BEGIN
;    IF Float(!Version.Release) GE 5.2 THEN BEGIN
;      IF (!D.Name EQ 'X' OR !D.Name EQ 'WIN' OR !D.Name EQ 'MAC') THEN BEGIN
;        Device, Get_Decomposed=decomposedState
;      ENDIF ELSE decomposedState = 0
;    ENDIF ELSE decomposedState = 0
;    IF (Float(!Version.Release) GE 6.4) AND (!D.NAME EQ 'Z') THEN BEGIN
;      Device, Get_Decomposed=decomposedState, Get_Pixel_Depth=theDepth
;      IF theDepth LT 24 THEN decomposedState = 0
;    ENDIF
;  ENDIF ELSE decomposedState = Keyword_Set(decomposedState)
;  
;  ; Get depth of visual display (and decomposed state for PostScript devices).
;  IF (!D.Flags AND 256) NE 0 THEN Device, Get_Visual_Depth=theDepth ELSE theDepth = 8
;  IF (Float(!Version.Release) GE 6.4) AND (!D.NAME EQ 'Z') THEN Device, Get_Pixel_Depth=theDepth
;  IF (!D.NAME EQ 'PS') AND (Float(!Version.Release) GE 7.1) THEN BEGIN
;    decomposedState = DecomposedColor(DEPTH=theDepth)
;  ENDIF
;  
;  ; Need brewer colors?
;  brewer = Keyword_Set(brewer)
;  
;  ; Load the colors.
;  IF N_Elements(filename) NE 0 THEN BEGIN
;  
;    ; Count the number of rows in the file.
;    ncolors = FSC_Color_Count_Rows(filename)
;    
;    ; Read the data.
;    OpenR, lun, filename, /Get_Lun
;    rvalue = BytArr(NCOLORS)
;    gvalue = BytArr(NCOLORS)
;    bvalue = BytArr(NCOLORS)
;    colors = StrArr(NCOLORS)
;    redvalue = 0B
;    greenvalue = 0B
;    bluevalue = 0B
;    colorvalue = ""
;    FOR j=0L, NCOLORS-1 DO BEGIN
;      ReadF, lun, redvalue, greenvalue, bluevalue, colorvalue
;      rvalue[j] = redvalue
;      gvalue[j] = greenvalue
;      bvalue[j] = bluevalue
;      colors[j] = colorvalue
;    ENDFOR
;    Free_Lun, lun
;    
;    ; Trim the colors array of blank characters.
;    colors = StrTrim(colors, 2)
;    
;  ENDIF ELSE BEGIN
;  
;    ; Set up the color vectors.
;    IF Keyword_Set(Brewer) THEN BEGIN
;    
;      ; Set up the color vectors.
;      colors = [ 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
;      rvalue = [  255,   255,   255,   255,   255,   245,   255,   250 ]
;      gvalue = [  255,   250,   255,   255,   248,   245,   245,   240 ]
;      bvalue = [  255,   250,   240,   224,   220,   220,   238,   230 ]
;      colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
;      rvalue = [ rvalue,   250,   255,    255,    255,    255,    245,    222,    210 ]
;      gvalue = [ gvalue,   235,   239,    235,    228,    228,    222,    184,    180 ]
;      bvalue = [ bvalue,   215,   213,    205,    196,    181,    179,    135,    140 ]
;      colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
;      rvalue = [ rvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
;      gvalue = [ gvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
;      bvalue = [ bvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
;      colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
;      rvalue = [ rvalue,   250,   223,    173,    109,     53,     35,      0,       0 ]
;      gvalue = [ gvalue,   253,   242,    221,    193,    156,     132,    97,      69 ]
;      bvalue = [ bvalue,   202,   167,    142,    115,     83,      67,    52,      41 ]
;      colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
;      rvalue = [ rvalue,   232,   202,    158,     99,     53,     33,      8,       8 ]
;      gvalue = [ gvalue,   241,   222,    202,    168,    133,    113,     75,      48 ]
;      bvalue = [ bvalue,   250,   240,    225,    211,    191,    181,    147,     107 ]
;      colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
;      rvalue = [ rvalue,   254,    253,    253,    250,    231,    217,    159,    127 ]
;      gvalue = [ gvalue,   236,    212,    174,    134,     92,     72,     51,     39 ]
;      bvalue = [ bvalue,   217,    171,    107,     52,     12,      1,      3,      4 ]
;      colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
;      rvalue = [ rvalue,   254,    252,    252,    248,    225,    203,    154,    103 ]
;      gvalue = [ gvalue,   232,    194,    146,     97,     45,     24,     12,      0 ]
;      bvalue = [ bvalue,   222,    171,    114,     68,     38,     29,     19,     13 ]
;      colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
;      rvalue = [ rvalue,   244,    222,    188,    152,    119,    106,     80,     63 ]
;      gvalue = [ gvalue,   242,    221,    189,    148,    108,     82,     32,      0 ]
;      bvalue = [ bvalue,   248,    237,    220,    197,    177,    163,    139,    125 ]
;      colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
;      rvalue = [ rvalue,   243,    213,    166,     94,     34,      3,      1,      1 ]
;      gvalue = [ gvalue,   234,    212,    189,    164,    138,    129,    101,     70 ]
;      bvalue = [ bvalue,   244,    232,    219,    204,    171,    139,     82,     54 ]
;      colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
;      rvalue = [ rvalue,   244,    206,    127,     58,     30,     33,     32,      8 ]
;      gvalue = [ gvalue,   250,    236,    205,    175,    125,     95,     48,     29 ]
;      bvalue = [ bvalue,   193,    179,    186,    195,    182,    168,    137,     88 ]
;      colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
;      rvalue = [ rvalue,   201,    245,    253,    251,    228,    193,    114,     59 ]
;      gvalue = [ gvalue,    35,    121,    206,    253,    244,    228,    171,     85 ]
;      bvalue = [ bvalue,    38,    72,     127,    197,    239,    239,    207,    164 ]
;      colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8']
;      rvalue = [ rvalue,  84,    163,   197,   220,   105,    51,    13,     0 ]
;      gvalue = [ gvalue,  48,    103,   141,   188,   188,   149,   113,    81 ]
;      bvalue = [ bvalue,   5,     26,    60,   118,   177,   141,   105,    71 ]
;      
;    ENDIF ELSE BEGIN
;    
;      ; Set up the color vectors. Both original and Brewer colors.
;      colors= ['White']
;      rvalue = [ 255]
;      gvalue = [ 255]
;      bvalue = [ 255]
;      colors = [ colors,   'Snow',     'Ivory','Light Yellow', 'Cornsilk',     'Beige',  'Seashell' ]
;      rvalue = [ rvalue,     255,         255,       255,          255,          245,        255 ]
;      gvalue = [ gvalue,     250,         255,       255,          248,          245,        245 ]
;      bvalue = [ bvalue,     250,         240,       224,          220,          220,        238 ]
;      colors = [ colors,   'Linen','Antique White','Papaya',     'Almond',     'Bisque',  'Moccasin' ]
;      rvalue = [ rvalue,     250,        250,        255,          255,          255,          255 ]
;      gvalue = [ gvalue,     240,        235,        239,          235,          228,          228 ]
;      bvalue = [ bvalue,     230,        215,        213,          205,          196,          181 ]
;      colors = [ colors,   'Wheat',  'Burlywood',    'Tan', 'Light Gray',   'Lavender','Medium Gray' ]
;      rvalue = [ rvalue,     245,        222,          210,      230,          230,         210 ]
;      gvalue = [ gvalue,     222,        184,          180,      230,          230,         210 ]
;      bvalue = [ bvalue,     179,        135,          140,      230,          250,         210 ]
;      colors = [ colors,  'Gray', 'Slate Gray',  'Dark Gray',  'Charcoal',   'Black',  'Honeydew', 'Light Cyan' ]
;      rvalue = [ rvalue,      190,      112,          110,          70,         0,         240,          224 ]
;      gvalue = [ gvalue,      190,      128,          110,          70,         0,         255,          255 ]
;      bvalue = [ bvalue,      190,      144,          110,          70,         0,         255,          240 ]
;      colors = [ colors,'Powder Blue',  'Sky Blue', 'Cornflower Blue', 'Cadet Blue', 'Steel Blue','Dodger Blue', 'Royal Blue',  'Blue' ]
;      rvalue = [ rvalue,     176,          135,          100,              95,            70,           30,           65,            0 ]
;      gvalue = [ gvalue,     224,          206,          149,             158,           130,          144,          105,            0 ]
;      bvalue = [ bvalue,     230,          235,          237,             160,           180,          255,          225,          255 ]
;      colors = [ colors,  'Navy', 'Pale Green','Aquamarine','Spring Green',  'Cyan' ]
;      rvalue = [ rvalue,        0,     152,          127,          0,            0 ]
;      gvalue = [ gvalue,        0,     251,          255,        250,          255 ]
;      bvalue = [ bvalue,      128,     152,          212,        154,          255 ]
;      colors = [ colors, 'Turquoise', 'Light Sea Green', 'Sea Green','Forest Green',  'Teal','Green Yellow','Chartreuse', 'Lawn Green' ]
;      rvalue = [ rvalue,      64,          143,               46,          34,             0,      173,           127,         124 ]
;      gvalue = [ gvalue,     224,          188,              139,         139,           128,      255,           255,         252 ]
;      bvalue = [ bvalue,     208,          143,               87,          34,           128,       47,             0,           0 ]
;      colors = [ colors, 'Green', 'Lime Green', 'Olive Drab',  'Olive','Dark Green','Pale Goldenrod']
;      rvalue = [ rvalue,      0,        50,          107,        85,            0,          238 ]
;      gvalue = [ gvalue,    255,       205,          142,       107,          100,          232 ]
;      bvalue = [ bvalue,      0,        50,           35,        47,            0,          170 ]
;      colors = [ colors,     'Khaki', 'Dark Khaki', 'Yellow',  'Gold', 'Goldenrod','Dark Goldenrod']
;      rvalue = [ rvalue,        240,       189,        255,      255,      218,          184 ]
;      gvalue = [ gvalue,        230,       183,        255,      215,      165,          134 ]
;      bvalue = [ bvalue,        140,       107,          0,        0,       32,           11 ]
;      colors = [ colors,'Saddle Brown',  'Rose',   'Pink', 'Rosy Brown','Sandy Brown', 'Peru']
;      rvalue = [ rvalue,     139,          255,      255,        188,        244,        205 ]
;      gvalue = [ gvalue,      69,          228,      192,        143,        164,        133 ]
;      bvalue = [ bvalue,      19,          225,      203,        143,         96,         63 ]
;      colors = [ colors,'Indian Red',  'Chocolate',  'Sienna','Dark Salmon',   'Salmon','Light Salmon' ]
;      rvalue = [ rvalue,    205,          210,          160,        233,          250,       255 ]
;      gvalue = [ gvalue,     92,          105,           82,        150,          128,       160 ]
;      bvalue = [ bvalue,     92,           30,           45,        122,          114,       122 ]
;      colors = [ colors,  'Orange',      'Coral', 'Light Coral',  'Firebrick', 'Dark Red', 'Brown',  'Hot Pink' ]
;      rvalue = [ rvalue,       255,         255,        240,          178,        139,       165,        255 ]
;      gvalue = [ gvalue,       165,         127,        128,           34,          0,        42,        105 ]
;      bvalue = [ bvalue,         0,          80,        128,           34,          0,        42,        180 ]
;      colors = [ colors, 'Deep Pink',    'Magenta',   'Tomato', 'Orange Red',   'Red', 'Crimson', 'Violet Red' ]
;      rvalue = [ rvalue,      255,          255,        255,        255,          255,      220,        208 ]
;      gvalue = [ gvalue,       20,            0,         99,         69,            0,       20,         32 ]
;      bvalue = [ bvalue,      147,          255,         71,          0,            0,       60,        144 ]
;      colors = [ colors,    'Maroon',    'Thistle',       'Plum',     'Violet',    'Orchid','Medium Orchid']
;      rvalue = [ rvalue,       176,          216,          221,          238,         218,        186 ]
;      gvalue = [ gvalue,        48,          191,          160,          130,         112,         85 ]
;      bvalue = [ bvalue,        96,          216,          221,          238,         214,        211 ]
;      colors = [ colors,'Dark Orchid','Blue Violet',  'Purple']
;      rvalue = [ rvalue,      153,          138,       160]
;      gvalue = [ gvalue,       50,           43,        32]
;      bvalue = [ bvalue,      204,          226,       240]
;      colors = [ colors, 'Slate Blue',  'Dark Slate Blue']
;      rvalue = [ rvalue,      106,            72]
;      gvalue = [ gvalue,       90,            61]
;      bvalue = [ bvalue,      205,           139]
;      colors = [ colors, 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
;      rvalue = [ rvalue,  255,   255,   255,   255,   255,   245,   255,   250 ]
;      gvalue = [ gvalue,  255,   250,   255,   255,   248,   245,   245,   240 ]
;      bvalue = [ bvalue,  255,   250,   240,   224,   220,   220,   238,   230 ]
;      colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
;      rvalue = [ rvalue,   250,   255,    255,    255,    255,    245,    222,    210 ]
;      gvalue = [ gvalue,   235,   239,    235,    228,    228,    222,    184,    180 ]
;      bvalue = [ bvalue,   215,   213,    205,    196,    181,    179,    135,    140 ]
;      colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
;      rvalue = [ rvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
;      gvalue = [ gvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
;      bvalue = [ bvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
;      colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
;      rvalue = [ rvalue,   250,   223,    173,    109,     53,     35,      0,       0 ]
;      gvalue = [ gvalue,   253,   242,    221,    193,    156,     132,    97,      69 ]
;      bvalue = [ bvalue,   202,   167,    142,    115,     83,      67,    52,      41 ]
;      colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
;      rvalue = [ rvalue,   232,   202,    158,     99,     53,     33,      8,       8 ]
;      gvalue = [ gvalue,   241,   222,    202,    168,    133,    113,     75,      48 ]
;      bvalue = [ bvalue,   250,   240,    225,    211,    191,    181,    147,     107 ]
;      colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
;      rvalue = [ rvalue,   254,    253,    253,    250,    231,    217,    159,    127 ]
;      gvalue = [ gvalue,   236,    212,    174,    134,     92,     72,     51,     39 ]
;      bvalue = [ bvalue,   217,    171,    107,     52,     12,      1,      3,      4 ]
;      colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
;      rvalue = [ rvalue,   254,    252,    252,    248,    225,    203,    154,    103 ]
;      gvalue = [ gvalue,   232,    194,    146,     97,     45,     24,     12,      0 ]
;      bvalue = [ bvalue,   222,    171,    114,     68,     38,     29,     19,     13 ]
;      colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
;      rvalue = [ rvalue,   244,    222,    188,    152,    119,    106,     80,     63 ]
;      gvalue = [ gvalue,   242,    221,    189,    148,    108,     82,     32,      0 ]
;      bvalue = [ bvalue,   248,    237,    220,    197,    177,    163,    139,    125 ]
;      colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
;      rvalue = [ rvalue,   243,    213,    166,     94,     34,      3,      1,      1 ]
;      gvalue = [ gvalue,   234,    212,    189,    164,    138,    129,    101,     70 ]
;      bvalue = [ bvalue,   244,    232,    219,    204,    171,    139,     82,     54 ]
;      colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
;      rvalue = [ rvalue,   244,    206,    127,     58,     30,     33,     32,      8 ]
;      gvalue = [ gvalue,   250,    236,    205,    175,    125,     95,     48,     29 ]
;      bvalue = [ bvalue,   193,    179,    186,    195,    182,    168,    137,     88 ]
;      colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
;      rvalue = [ rvalue,   201,    245,    253,    251,    228,    193,    114,     59 ]
;      gvalue = [ gvalue,    35,    121,    206,    253,    244,    228,    171,     85 ]
;      bvalue = [ bvalue,    38,    72,     127,    197,    239,    239,    207,    164 ]
;      colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8', 'OPPOSITE']
;      rvalue = [ rvalue,  84,    163,   197,   220,   105,    51,    13,     0,   opixel[0]]
;      gvalue = [ gvalue,  48,    103,   141,   188,   188,   149,   113,    81,   opixel[1]]
;      bvalue = [ bvalue,   5,     26,    60,   118,   177,   141,   105,    71,   opixel[2]]
;    ENDELSE
;  ENDELSE
;  
;  
;  ; I have completely removed all access to "system" colors in this code. I'll
;  ; leave the code here for awhile to be sure no one is using system colors, but
;  ; I seriously doubt it will be coming back.
;  
;  ; Add system color names for IDL version 5.6 and higher. We don't want to
;  ; do this we cannot establish a display connection (e.g., we are running
;  ; in a cron job). Check for system variable !FSC_Display_Connection. If not
;  ; defined, check the connection.
;  ;   DefSysV, '!FSC_Display_Connection', EXISTS=sysvarExists
;  ;   IF sysvarExists $
;  ;        THEN haveConnection = !FSC_Display_Connection $
;  ;        ELSE haveConnection = CanConnect()
;  ;
;  ; Handle depreciated NODISPLAY keyword.
;  IF Keyword_Set(nodisplay) THEN haveConnection = 0
;  
;  ;   IF (Float(!Version.Release) GE 5.6) && Keyword_Set(haveConnection) THEN BEGIN
;  ;
;  ;          tlb = Widget_Base()
;  ;          sc = Widget_Info(tlb, /System_Colors)
;  ;          Widget_Control, tlb, /Destroy
;  ;          frame = sc.window_frame
;  ;          text = sc.window_text
;  ;          active = sc.active_border
;  ;          shadow = sc.shadow_3d
;  ;          highlight = sc.light_3d
;  ;          edge = sc.light_edge_3d
;  ;          selected = sc.highlight
;  ;          face = sc.face_3d
;  ;          colors  = [colors,  'Frame',  'Text',  'Active',  'Shadow']
;  ;          rvalue =  [rvalue,   frame[0], text[0], active[0], shadow[0]]
;  ;          gvalue =  [gvalue,   frame[1], text[1], active[1], shadow[1]]
;  ;          bvalue =  [bvalue,   frame[2], text[2], active[2], shadow[2]]
;  ;          colors  = [colors,  'Highlight',  'Edge',  'Selected',  'Face']
;  ;          rvalue =  [rvalue,   highlight[0], edge[0], selected[0], face[0]]
;  ;          gvalue =  [gvalue,   highlight[1], edge[1], selected[1], face[1]]
;  ;          bvalue =  [bvalue,   highlight[2], edge[2], selected[2], face[2]]
;  ;
;  ;    ENDIF
;  
;  ; Load the colors from the current color table, if you need them.
;  IF useCurrentColors THEN BEGIN
;    IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, /GET
;    IF decomposedState EQ 0 THEN BEGIN
;      colors = SIndgen(256)
;      rvalue = rrr
;      gvalue = ggg
;      bvalue = bbb
;    ENDIF ELSE BEGIN
;      colors = [colors, SIndgen(256)]
;      rvalue = [rvalue, rrr]
;      gvalue = [gvalue, ggg]
;      bvalue = [bvalue, bbb]
;    ENDELSE
;  ENDIF
;  
;  ; Make sure we are looking at compressed, uppercase names.
;  colors = StrUpCase(StrCompress(StrTrim(colors,2), /Remove_All))
;  
;  ; Check synonyms of color names.
;  FOR j=0, N_Elements(theColor)-1 DO BEGIN
;    IF StrUpCase(theColor[j]) EQ 'GREY' THEN theColor[j] = 'GRAY'
;    IF StrUpCase(theColor[j]) EQ 'LIGHTGREY' THEN theColor[j] = 'LIGHTGRAY'
;    IF StrUpCase(theColor[j]) EQ 'MEDIUMGREY' THEN theColor[j] = 'MEDIUMGRAY'
;    IF StrUpCase(theColor[j]) EQ 'SLATEGREY' THEN theColor[j] = 'SLATEGRAY'
;    IF StrUpCase(theColor[j]) EQ 'DARKGREY' THEN theColor[j] = 'DARKGRAY'
;    IF StrUpCase(theColor[j]) EQ 'AQUA' THEN theColor[j] = 'AQUAMARINE'
;    IF StrUpCase(theColor[j]) EQ 'SKY' THEN theColor[j] = 'SKYBLUE'
;    IF StrUpCase(theColor[j]) EQ 'NAVYBLUE' THEN theColor[j] = 'NAVY'
;    IF StrUpCase(theColor[j]) EQ 'CORNFLOWER' THEN theColor[j] = 'CORNFLOWERBLUE'
;    IF StrUpCase(theColor[j]) EQ 'BROWN' THEN theColor[j] = 'SIENNA'
;  ENDFOR
;  
;  ; How many colors do we have?
;  ncolors = N_Elements(colors)
;  
;  ; Check for offset.
;  IF (theDepth EQ 8) OR (decomposedState EQ 0) THEN offset = !D.Table_Size - ncolors - 2 ELSE offset = 0
;  IF (useCurrentColors) AND (decomposedState EQ 0) THEN offset = 0
;  
;  ; Did the user want to select a color name? If so, we set
;  ; the color name and color index, unless the user provided
;  ; them. In the case of a single positional parameter, we treat
;  ; this as the color index number as long as it is not a string.
;  cancelled = 0.0
;  IF Keyword_Set(selectcolor) THEN BEGIN
;  
;    CASE N_Params() OF
;      0: BEGIN
;        theColor = PickColorName(Filename=filename, _Extra=extra, Cancel=cancelled, BREWER=brewer)
;        IF cancelled THEN RETURN, !P.Color
;        IF theDepth GT 8 AND (decomposedState EQ 1) THEN BEGIN
;          colorIndex = Fix(!P.Color < (!D.Table_Size - 1))
;        ENDIF ELSE BEGIN
;          colorIndex = Where(StrUpCase(colors) EQ StrUpCase(StrCompress(theColor, /Remove_All)), count) + offset
;          colorIndex = Fix(colorIndex[0])
;          IF count EQ 0 THEN Message, 'Cannot find color: ' + StrUpCase(theColor), /NoName
;        ENDELSE
;        
;      END
;      1: BEGIN
;        IF Size(theColor, /TName) NE 'STRING' THEN BEGIN
;          colorIndex = Fix(theColor)
;          theColor = brewer ? 'WT1' : 'White'
;        ENDIF ELSE colorIndex = Fix(!P.Color < 255)
;        theColor = PickColorName(theColor, Filename=filename, _Extra=extra, Cancel=cancelled, BREWER=brewer)
;        IF cancelled THEN RETURN, !P.Color
;      END
;      2: BEGIN
;        theColor = PickColorName(theColor, Filename=filename, _Extra=extra, Cancel=cancelled, BREWER=brewer)
;        IF cancelled THEN RETURN, !P.Color
;      END
;    ENDCASE
;  ENDIF
;  
;  ; Make sure you have a color name and color index.
;  CASE N_Elements(theColor) OF
;    0: BEGIN
;      theColor = brewer ? 'WT1' : 'White'
;      IF N_Elements(colorIndex) EQ 0 THEN BEGIN
;        IF theDepth GT 8 THEN BEGIN
;          colorIndex = Fix(!P.Color < (!D.Table_Size - 1))
;        ENDIF ELSE BEGIN
;          colorIndex = Where(colors EQ theColor, count) + offset
;          colorIndex = Fix(colorIndex[0])
;          IF count EQ 0 THEN Message, 'Cannot find color: ' + theColor, /NoName
;        ENDELSE
;      ENDIF ELSE colorIndex = 0S > colorIndex < Fix((!D.Table_Size - 1))
;    ENDCASE
;    
;    1: BEGIN
;      type = Size(theColor, /TNAME)
;      IF type NE 'STRING' THEN Message, 'The color must be expressed as a color name.'
;      theColor = theColor[0] ; Make it a scalar or you run into a WHERE function "feature". :-(
;      IF N_Elements(colorIndex) EQ 0 THEN BEGIN
;        IF (theDepth GT 8) AND (decomposedState EQ 1) THEN BEGIN
;          colorIndex = Fix(!P.Color < (!D.Table_Size - 1))
;        ENDIF ELSE BEGIN
;          colorIndex = Where(colors EQ theColor, count) + offset
;          colorIndex = Fix(colorIndex[0])
;          IF count EQ 0 THEN Message, 'Cannot find color: ' + theColor, /NoName
;        ENDELSE
;      ENDIF ELSE colorIndex = 0S > colorIndex < Fix(!D.Table_Size - 1)
;    ENDCASE
;    
;    ELSE: BEGIN
;      type = Size(theColor, /TNAME)
;      IF type NE 'STRING' THEN Message, 'The colors must be expressed as color names.'
;      ncolors = N_Elements(theColor)
;      CASE N_Elements(colorIndex) OF
;        0: colorIndex = Fix(Indgen(ncolors) + (!D.Table_Size - (ncolors + 1)))
;        1: colorIndex = Fix(Indgen(ncolors) + colorIndex)
;        ELSE: IF N_Elements(colorIndex) NE ncolors THEN $
;          Message, 'Index vector must be the same length as color name vector.'
;      ENDCASE
;      
;      ; Did the user want color triples?
;      
;      IF Keyword_Set(triple) THEN BEGIN
;        colors = LonArr(ncolors, 3)
;        FOR j=0,ncolors-1 DO colors[j,*] = FSC_Color(theColor[j], colorIndex[j], Filename=filename, $
;          Decomposed=decomposedState, /Triple, BREWER=brewer)
;        RETURN, colors
;      ENDIF ELSE BEGIN
;        colors = LonArr(ncolors)
;        FOR j=0,ncolors-1 DO colors[j] = FSC_Color(theColor[j], colorIndex[j], Filename=filename, $
;          Decomposed=decomposedState, BREWER=brewer)
;        RETURN, colors
;      ENDELSE
;    END
;  ENDCASE
;  
;  ; Did the user ask for the color names? If so, return them now.
;  IF Keyword_Set(names) THEN RETURN, Reform(colors, 1, ncolors)
;  
;  ; Process the color names.
;  theNames = StrUpCase( StrCompress(colors, /Remove_All ) )
;  
;  ; Find the asked-for color in the color names array.
;  theIndex = Where(theNames EQ StrUpCase(StrCompress(theColor, /Remove_All)), foundIt)
;  theIndex = theIndex[0]
;  
;  ; If the color can't be found, report it and continue with the color set to "OPPOSITE."
;  IF foundIt EQ 0 THEN BEGIN
;    Message, "Can't find color " + theColor + ". Substituting 'OPPOSITE'.", /Informational
;    theColor = 'OPPOSITE'
;    theIndex = Where(StrUpCase(colors) EQ 'OPPOSITE')
;  ENDIF
;  
;  ; Get the color triple for this color.
;  r = rvalue[theIndex]
;  g = gvalue[theIndex]
;  b = bvalue[theIndex]
;  
;  ; Did the user want a color triple? If so, return it now.
;  IF Keyword_Set(triple) THEN BEGIN
;    IF Keyword_Set(allcolors) THEN BEGIN
;      IF Keyword_Set(row) THEN RETURN, Transpose([[rvalue], [gvalue], [bvalue]]) ELSE RETURN, [[rvalue], [gvalue], [bvalue]]
;    ENDIF ELSE BEGIN
;      IF Keyword_Set(row) THEN RETURN, [r, g, b] ELSE RETURN, [[r], [g], [b]]
;    ENDELSE
;  ENDIF
;  
;  ; Otherwise, we are going to return either an index
;  ; number where the color has been loaded, or a 24-bit
;  ; value that can be decomposed into the proper color.
;  IF decomposedState THEN BEGIN
;  
;    ; Need a color structure?
;    IF Arg_Present(colorStructure) THEN BEGIN
;      theColors = FSC_Color_Color24([[rvalue], [gvalue], [bvalue]])
;      colorStructure = Create_Struct(theNames[0], theColors[0])
;      FOR j=1, ncolors-1 DO colorStructure = Create_Struct(colorStructure, theNames[j], theColors[j])
;    ENDIF
;    
;    IF Keyword_Set(allcolors) THEN BEGIN
;      RETURN, FSC_Color_Color24([[rvalue], [gvalue], [bvalue]])
;    ENDIF ELSE BEGIN
;      RETURN, FSC_Color_Color24([r, g, b])
;    ENDELSE
;    
;  ENDIF ELSE BEGIN
;  
;    IF Keyword_Set(allcolors) THEN BEGIN
;    
;      ; Need a color structure?
;      IF Arg_Present(colorStructure) THEN BEGIN
;        allcolorIndex = !D.Table_Size - ncolors - 2
;        IF allcolorIndex LT 0 THEN $
;          Message, 'Number of colors exceeds available color table values. Returning.', /NoName
;        IF (allcolorIndex + ncolors) GT 255 THEN $
;          Message, 'Number of colors exceeds available color table indices. Returning.', /NoName
;        theColors = IndGen(ncolors) + allcolorIndex
;        colorStructure = Create_Struct(theNames[0],  theColors[0])
;        FOR j=1, ncolors-1 DO colorStructure = Create_Struct(colorStructure, theNames[j], theColors[j])
;      ENDIF
;      
;      IF N_Elements(colorIndex) EQ 0 THEN colorIndex = Fix(!D.Table_Size - ncolors - 2)
;      IF colorIndex LT 0 THEN $
;        Message, 'Number of colors exceeds available color table values. Returning.', /NoName
;      IF (colorIndex + ncolors) GT 255 THEN BEGIN
;        colorIndex = Fix(!D.Table_Size - ncolors - 2)
;      ENDIF
;      IF (!D.Name NE 'PRINTER') AND (!D.Name NE 'NULL') THEN TVLCT, rvalue, gvalue, bvalue, colorIndex
;      RETURN, IndGen(ncolors) + colorIndex
;    ENDIF ELSE BEGIN
;    
;      ; Need a color structure?
;      IF Arg_Present(colorStructure) THEN BEGIN
;        colorStructure = Create_Struct(theColor,  colorIndex)
;      ENDIF
;      
;      IF (!D.Name NE 'PRINTER') AND (!D.Name NE 'NULL') THEN $
;        TVLCT, rvalue[theIndex], gvalue[theIndex], bvalue[theIndex], colorIndex
;      RETURN, Fix(colorIndex)
;    ENDELSE
;    
;    
;  ENDELSE
;  
;END ;-------------------------------------------------------------------------------------------------------

;****************************************************************************************

;****************************

;*****************************************************************************************************
;+
; NAME:
;       PROGRESSBARBTT__DEFINE
;
; PURPOSE:
;
;       Creates a simple progress bar for indicating the progess of a looping
;       operation in IDL.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       Utilities
;
; CALLING SEQUENCE:
;
;       PROGRESSBARBTT = Obj_New("PROGRESSBARBTT")
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR:         The name of the color for the progress bar. By default: "red".
;
;       Possible color names are those defined by FSC_COLOR:
;
;                Almond   Antique White      Aquamarine           Beige          Bisque           Black
;                  Blue     Blue Violet           Brown       Burlywood        Charcoal      Chartreuse
;             Chocolate           Coral        Cornsilk            Cyan  Dark Goldenrod       Dark Gray
;            Dark Green      Dark Khaki     Dark Orchid     Dark Salmon       Deep Pink     Dodger Blue
;             Firebrick    Forest Green            Gold       Goldenrod            Gray           Green
;          Green Yellow        Honeydew        Hot Pink      Indian Red           Ivory           Khaki
;              Lavender      Lawn Green     Light Coral      Light Cyan      Light Gray    Light Salmon
;          Light Yellow      Lime Green           Linen         Magenta          Maroon     Medium Gray
;         Medium Orchid        Moccasin            Navy           Olive      Olive Drab          Orange
;            Orange Red          Orchid  Pale Goldenrod      Pale Green          Papaya            Peru
;                  Pink            Plum     Powder Blue          Purple             Red            Rose
;            Rosy Brown      Royal Blue    Saddle Brown          Salmon     Sandy Brown       Sea Green
;              Seashell          Sienna        Sky Blue      Slate Gray            Snow    Spring Green
;            Steel Blue             Tan         Thistle          Tomato       Turquoise          Violet
;            Violet Red           Wheat           White          Yellow
;
;       FAST_LOOP:     Set this keyword if what you are doing in the loop doesn't involve
;                      any color operations and you want the progress bar to update as fast
;                      as possible. With this keyword set, the program will eliminate extra
;                      calls to FSC_COLOR, which can be slow if you are calling it, say,
;                      10,000 times!
;
;       GROUP_LEADER:  The group leader for the progress bar.
;
;       NOCANCEL:      Set this keyword to eliminate the CANCEL button from the progres bar.
;
;       PERCENT:       The initial percent on the progress bar. Used only if the START keyword is
;                      also set.
;
;       START:         Set this keyword if you wish to call the START method immediately upon initialization.
;
;       TEXT:          The textual message that goes above the progress bar. By default:
;                      "Operation in progress..."
;
;       TITLE:         The title of the progress bar window. By default: "Progress Bar".
;
;       XSIZE:         The X size of the progress bar itself. By default, 150 pixels.
;
;       YSIZE:         The Y size of the progress bar itself. By default, 10 pixels.

;
; PROCEDURE:
;
;       The user is responsible for starting, updating, checking for CANCEL events, and
;       destroying the progress indicator. The sequence of commands might look
;       like this:
;
;          PROGRESSBARBTT = Obj_New("PROGRESSBARBTT")
;          PROGRESSBARBTT -> Start
;          FOR j=0,9 DO BEGIN
;             IF PROGRESSBARBTT -> CheckCancel() THEN BEGIN
;                ok = Dialog_Message('The user cancelled operation.')
;                RETURN
;             ENDIF
;             Wait, 0.5  ; Would probably be doing something ELSE here!
;             PROGRESSBARBTT -> Update, (j+1)*10
;          ENDFOR
;          PROGRESSBARBTT -> Destroy
;
;       See the example program at the end of this file for a working example of code.
;
; METHODS:
;
;       CHECKCANCEL: This function method returns a 1 if the user has clicked
;           the CANCEL button. Otherwise, it returns a 0.
;
;          cancelled = PROGRESSBARBTT -> CheckCancel()
;          IF cancelled THEN PROGRESSBARBTT->Destroy
;
;       DESTROY: Destroys the progress bar widgets as well as the object.
;
;          PROGRESSBARBTT -> Destroy
;
;       GETPROPERTY: Gets certain properties of the object.
;
;          PROGRESSBARBTT -> GetProperty, Color=currentColor
;
;       SETPROPERTY: Allows the user to set certain properties of the object.
;
;          PROGRESSBARBTT -> SetProperty, Color='green'
;
;       START: Puts the progress bar on the display and enables it to receive events.
;
;          PROGRESSBARBTT -> Start
;
;       UPDATE: Updates the progress bar. Requires on argument, a number between 0
;          and 100 that indicates the percent of progress bar that should be filled
;          with a color. Can optional specify TEXT that is displayed above the progress
;          bar.
;
;          PROGRESSBARBTT -> Update, 50
;          PROGRESSBARBTT -> Update, 50, Text1='Operation 50% completed...'
;
; EXAMPLE:
;
;       See the example program at the bottom of this file.
;
; RESTRICTIONS:
;
;       Note that the progress bar cannot be run as a MODAL widget program and
;       still capture CANCEL button events. Thus, the user *may* be able to generate events
;       in the calling program while this progress bar is in operation.
;
; DEPENDENCIES:
;
;       This program requires FSC_COLOR from the Coyote Library:
;
;          http://www.dfanning.com/programs/fsc_color.pro
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 19 September 2002.
;       Added TEXT keyword to Update method. 12 Nov 2002. DWF.
;       Added FAST_LOOP keyword. 19 Dec 2002. DWF.
;       Fixed a problem in where I was checking for CANCEL button event. 2 April 2003. DWF.
;       Removed the XMANAGER call in the START method, since it wasn't needed. 7 October 2003. DWF.
;       General maintenance updates. Added START keyword to INIT method to allow immediate
;          starting upon initialization. Added better error handling and checking. 10 October 2003. DWF.
;       Added ACCEPT button and CheckButton method. Modified Example program to demonstrate new
;          functionality. 1 December 2003. DWF.
;       Added XOFFSET and YOFFSET keywords. 4 March 2004. DWF.
;       Removed obsolete STR_SEP and replaced with STRSPLIT. 27 Oct 2004. DWF.
;-
;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT_Cleanup
;
; PURPOSE:
;
;       This procedure makes sure the object is destroyed when the progress bar is destroyed.
;
;*****************************************************************************************************
PRO PROGRESSBARBTT_Cleanup, tlb

  Widget_Control, tlb, Get_UValue=self
  Obj_Destroy, self
  
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT_Error_Message
;
; PURPOSE:
;
;       This function is the standard ERROR_MESSAGE error handling functionality.
;
;*****************************************************************************************************
FUNCTION PROGRESSBARBTT_Error_Message, theMessage, Error=error, Informational=information, $
    Traceback=traceback, NoName=noname, Title=title, _Extra=extra
    
  On_Error, 2
  
  ; Check for presence and type of message.
  
  IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
  s = Size(theMessage)
  messageType = s[s[0]+1]
  IF messageType NE 7 THEN BEGIN
    Message, "The message parameter must be a string.", _Extra=extra
  ENDIF
  
  ; Get the call stack and the calling routine's name.
  
  Help, Calls=callStack
  callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
  
  ; Are widgets supported?
  
  widgetsSupported = ((!D.Flags AND 65536L) NE 0)
  IF widgetsSupported THEN BEGIN
  
    ; If this is an error produced with the MESSAGE command, it is a trapped
    ; error and will have the name "IDL_M_USER_ERR".
  
    IF !ERROR_STATE.NAME EQ "IDL_M_USER_ERR" THEN BEGIN
    
      IF N_Elements(title) EQ 0 THEN title = 'Trapped Error'
      
      ; If the message has the name of the calling routine in it,
      ; it should be stripped out. Can you find a colon in the string?
      
      ; Is the calling routine an object method? If so, special processing
      ; is required. Object methods will have two colons together.
      
      doublecolon = StrPos(theMessage, "::")
      IF doublecolon NE -1 THEN BEGIN
      
        prefix = StrMid(theMessage, 0, doublecolon+2)
        submessage = StrMid(theMessage, doublecolon+2)
        colon = StrPos(submessage, ":")
        IF colon NE -1 THEN BEGIN
        
          ; Extract the text up to the colon. Is this the same as
          ; the callingRoutine? If so, strip it.
        
          IF StrMid(theMessage, 0, colon+StrLen(prefix)) EQ callingRoutine THEN $
            theMessage = StrMid(theMessage, colon+1+StrLen(prefix))
        ENDIF
      ENDIF ELSE BEGIN
      
        colon = StrPos(theMessage, ":")
        IF colon NE -1 THEN BEGIN
        
          ; Extract the text up to the colon. Is this the same as
          ; the callingRoutine? If so, strip it.
        
          IF StrMid(theMessage, 0, colon) EQ callingRoutine THEN $
            theMessage = StrMid(theMessage, colon+1)
        ENDIF
        
      ENDELSE
      
      
      ; Add the calling routine's name, unless NONAME is set.
      
      IF Keyword_Set(noname) THEN BEGIN
        answer = Dialog_Message(theMessage, Title=title, _Extra=extra, $
          Error=error, Information=information)
      ENDIF ELSE BEGIN
        answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + $
          theMessage, Title=title, _Extra=extra, $
          Error=error, Information=information)
      ENDELSE
      
    ENDIF ELSE BEGIN
    
      ; Otherwise, this is an IDL system error.
    
      IF N_Elements(title) EQ 0 THEN title = 'System Error'
      
      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN $
        answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
        Error=error, Information=information) ELSE $
        IF Keyword_Set(noname) THEN BEGIN
        answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
          Error=error, Information=information)
      ENDIF ELSE BEGIN
        answer = Dialog_Message(StrUpCase(callingRoutine) + "--> " + $
          theMessage, _Extra=extra, Title=title, $
          Error=error, Information=information)
      ENDELSE
    ENDELSE
  ENDIF ELSE BEGIN
    Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix, _Extra=extra
    Print, '%' + callingRoutine + ': ' + theMessage
    answer = 'OK'
  ENDELSE
  
  ; Provide traceback information if requested.
  
  IF Keyword_Set(traceback) THEN BEGIN
    Help, /Last_Message, Output=traceback
    Print,''
    Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
    Print, ''
    FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
  ENDIF
  
  RETURN, answer
END


PRO PROGRESSBARBTT_Event, event

  ; This is the event handler for the program. It simply sets the CANCEL
  ; flag if this is a button event.

  Widget_Control, event.top, Get_UValue=self
  thisEvent = Tag_Names(event, /Structure_Name)
  IF thisEvent EQ 'WIDGET_BUTTON' THEN self -> SetProperty, Cancel=1
END

;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::CreateButtonValue, xSize, ySize, FILE=FILE|TRUECOLOR=TRUECOLOR|GRAYSCALE=GRAYSCALE, TYPE=[1|2|3]
;
; PURPOSE:
;
;       Returns:
;         a valid value for VALUE keyword of bitmap button.
;
; SYNTAX:
;
;       imageValue = PROGRESSBARBTT -> CreateButtonValue(xSize, ySize, /TRUECOLOR) OR imageValue = PROGRESSBARBTT -> CreateButtonValue(xSize, ySize, TYPE=1)
;       imageValue = PROGRESSBARBTT -> CreateButtonValue(xSize, ySize, /GRAYSCALE) OR imageValue = PROGRESSBARBTT -> CreateButtonValue(xSize, ySize, TYPE=2)
;       imageValue = PROGRESSBARBTT -> CreateButtonValue(xSize, ySize, /FILE) OR imageValue = PROGRESSBARBTT -> CreateButtonValue(xSize, ySize, TYPE=3)
;
; ARGUMENTS:
;
;       xSize: output x dimension of "image" (ignored if FILE)
;       ySize: output y dimension of "image" (ignored if FILE)
;
; KEYWORDS (are exclusive):
;
;       TRUECOLOR:   Set 1 if argument is a bytarr(3).
;       GRAYSCALE:   Set 1 if argument is a bytarr(nXm).
;       FILE:   Set 1 if argument is a file name.
;       (output) OUTPUTBITMAP:   Set BITMAP keyword of widget_button according to this.
;
;*****************************************************************************************************
FUNCTION PROGRESSBARBTT::CreateButtonValue, xSize, ySize, TRUECOLOR=truecolor, GRAYSCALE=grayscale, FILE=file, OUTPUTBITMAP=OUTPUTBITMAP, TYPE=TYPE

  ; This method create a valid VALUE for widget_button VALUE keyword
  ; using BITMAP=OUTBITMAP when you call widget_button.

  OUTPUTBITMAP=0
  if n_elements(TYPE) ne 0 then begin
    truecolor=0
    grayscale=0
    file=0
    if TYPE eq 1 then file=1
    if TYPE eq 2 then truecolor=1
    if TYPE eq 3 then grayscale=1
  endif
  sample=self->getSample()
  if keyword_set(file) then begin
    ;congrid??
    OUTPUTBITMAP=1
    workedSampled=sample
  endif
  if keyword_set(truecolor) then begin
    workedSampled=bytarr(xSize, ySize, 3)
    workedSampled[*,*, 0]=sample[0]
    workedSampled[*,*, 1]=sample[1]
    workedSampled[*,*, 2]=sample[2]
  endif
  if keyword_set(grayscale) then begin
    workedSampled=congrid(sample, xSize, ySize)
  endif
  return, workedSampled
  
END
;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::turnOffButton, index
;
; PURPOSE:
;
;       Returns:
;         none.
;
; SYNTAX:
;
;       PROGRESSBARBTT -> turnOffButton, index
;
; ARGUMENTS:
;
;       index: position of the button to turn off
;
; KEYWORDS:
;
;       none
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::turnOffButton, index

  ; This method turn off index specific button
  ; using show keyword

  ;buttons=self->getButtonList()
  ;widget_control, buttons[index], SHOW=0
  if widget_info((*self.buttonList)[index], /valid_id) then widget_control, (*self.buttonList)[index], /DESTROY
  
END
;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::turnOnButton, index
;
; PURPOSE:
;
;       Returns:
;         none.
;
; SYNTAX:
;
;       PROGRESSBARBTT -> turnOnButton, index
;
; ARGUMENTS:
;
;       index: position of the button to turn on
;
; KEYWORDS:
;
;       none
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::turnOnButton, index

  ; This method turn on index specific button
  ; using show keyword

  buttons=self->getButtonList()
  buttonXSize=self.xsize/self.buttonNumber
  ;self.buttonBase=widget_base(self.tlb, xpad=0, ypad=0, space=0, /ROW)
  bttValue=self->createButtonValue(buttonXSize, self.ysize, TYPE=self.type, OUTPUTBITMAP=OUTPUTBITMAP)
  ;buttonList=lonarr(self.buttonNumber)
  ;for i=0, self.buttonNumber-1 do begin
  (*self.buttonList)[index]=widget_button(self.buttonBase, scr_xsize=buttonXSize, scr_ysize=self.ysize, VALUE=bttValue, BITMAP=BITMAP, FRAME=0)
  ;endfor
  widget_control, (*self.buttonList)[index], SHOW=1
  
END
;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::updateButtons, percent
;
; PURPOSE:
;
;       Returns:
;         none.
;
; SYNTAX:
;
;       PROGRESSBARBTT -> updateButtons, percent
;
; ARGUMENTS:
;
;       percent: progress bar value
;
; KEYWORDS:
;
;       none
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::updateButtons, percent

  ; This method turn on & of right number of buttons
  ; to emulate widget_draw beahviour

  lastOnBtt=(float(self.buttonNumber)*percent/100.0) > 0
  for i=0, self.buttonNumber-1 do self->turnOffButton, i
  for i=0, lastOnBtt-1 do self->turnOnButton, i
  
END

;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::buildButtonSection
;
; PURPOSE:
;
;       Returns:
;         none.
;
; SYNTAX:
;
;       PROGRESSBARBTT -> buildButtonSection
;
; ARGUMENTS:
;
;       none
;
; KEYWORDS:
;
;       none
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::buildButtonSection

  ; This method build a sequence of buttons
  ; to emulate widget_draw beahviour

  buttonXSize=self.xsize/self.buttonNumber
  self.buttonBase=widget_base(self.tlb, xpad=0, ypad=0, space=0, scr_xsize=self.xsize, scr_ysize=self.ysize, xsize=self.xsize, ysize=self.ysize, /ROW, /ALIGN_LEFT)
  bttValue=self->createButtonValue(buttonXSize, self.ysize, TYPE=self.type, OUTPUTBITMAP=OUTPUTBITMAP)
  buttonList=lonarr(self.buttonNumber)
  for i=0, self.buttonNumber-1 do begin
    ;buttonList[i]=widget_button(self.buttonBase, scr_xsize=buttonXSize, scr_ysize=self.ysize, VALUE=bttValue, BITMAP=BITMAP, FRAME=0)
    ;widget_control, buttonList[i], SHOW=0
  endfor
  self->setButtonList, buttonList
  
END

;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::setButtonList, buttonList
;
; PURPOSE:
;
;       Returns:
;         none.
;
; SYNTAX:
;
;       PROGRESSBARBTT -> setButtonList, buttonList
;
; ARGUMENTS:
;
;       buttonList: lonarr(n)
;
; KEYWORDS:
;
;       none
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::setButtonList, buttonList

  ; This method set internal attribute called buttonList
  ; using a pointer

  ptr_free, self.buttonList
  self.buttonList=ptr_new(buttonList, /NO_COPY)
END
;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::getButtonList
;
; PURPOSE:
;
;       Returns:
;         The list of all buttons widget id.
;
; SYNTAX:
;
;       buttonList = PROGRESSBARBTT -> getButtonList()
;
; ARGUMENTS:
;
;       none.
;
; KEYWORDS:
;
;       none
;
;*****************************************************************************************************
FUNCTION PROGRESSBARBTT::getButtonList

  ; This method get internal attribute called buttonList
  ; dereferencing a pointer

  if ptr_valid(self.buttonList) then return, *self.buttonList
  return, [-1]
  
END
;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::setSample, sample
;
; PURPOSE:
;
;       Returns:
;         none.
;
; SYNTAX:
;
;       PROGRESSBARBTT -> setSample, sample
;
; ARGUMENTS:
;
;       sample: bytarr(3) OR bytarr(nXm) OR fullFileName
;
; KEYWORDS:
;
;       none
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::setSample, sample

  ; This method set internal attribute called sample
  ; using a pointer

  ptr_free, self.sample
  self.sample=ptr_new(sample, /NO_COPY)
END

;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::getSample
;
; PURPOSE:
;
;       Returns:
;         the value of stored sample (see setSample method).
;
; SYNTAX:
;
;       sample = PROGRESSBARBTT -> getSample()
;
; ARGUMENTS:
;
;       none.
;
; KEYWORDS:
;
;       none
;
;*****************************************************************************************************
FUNCTION PROGRESSBARBTT::getSample

  ; This method get internal attribute called sample
  ; dereferencing a pointer

  if ptr_valid(self.sample) then return, *self.sample
  return, [-1]
  
END

;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::CheckButton
;
; PURPOSE:
;
;       Returns a 1 if a button was selected. Returns 0 othewise.
;
; SYNTAX:
;
;       check = PROGRESSBARBTT -> CheckButton()
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ACCEPT:   If the Accept button was selected, this keyword is set to one.
;
;       CANCEL:   If the Cancel button was selected, this keyword is set to one.
;
;*****************************************************************************************************
FUNCTION PROGRESSBARBTT::CheckButton, ACCEPT=accept, CANCEL=cancel

  ; This method checks for a button event. It returns 1
  ; if an event has occurred and 0 otherwise.


  accept = 0
  cancel = 0
  retValue = 0
  
  ; Check for a Cancel button event.
  IF Widget_Info(self.cancelID, /Valid_ID) THEN BEGIN
    event = Widget_Event(self.cancelID, /NoWait)
    name = Tag_Names(event, /Structure_Name)
    IF name EQ 'WIDGET_BUTTON' THEN BEGIN
      Widget_Control, event.id, Get_Value=buttonValue
      IF buttonValue EQ 'Cancel' THEN BEGIN
        self.cancelFlag = 1
        cancel = 1
        retValue = 1
      ENDIF
    ENDIF
  ENDIF
  
  IF Widget_Info(self.acceptID, /Valid_ID) THEN BEGIN
    event = Widget_Event(self.acceptID, /NoWait)
    name = Tag_Names(event, /Structure_Name)
    IF name EQ 'WIDGET_BUTTON' THEN BEGIN
      Widget_Control, event.id, Get_Value=buttonValue
      IF buttonValue EQ 'Accept' THEN BEGIN
        accept = 1
        retValue = 1
      ENDIF
    ENDIF
  ENDIF
  
  
  RETURN, retValue
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::CheckCancel
;
; PURPOSE:
;
;       Returns a 1 if the user selected the CANCEL button. Returns 0 othewise.
;
; SYNTAX:
;
;       check = PROGRESSBARBTT -> CheckCancel()
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;*****************************************************************************************************
FUNCTION PROGRESSBARBTT::CheckCancel

  ; This method checks for a CANCEL Button event. It returns 1
  ; if an event has occurred and 0 otherwise.

  ; Check for a CANCEL button event.

  IF Widget_Info(self.cancelID, /Valid_ID) THEN BEGIN
    event = Widget_Event(self.cancelID, /NoWait)
    name = Tag_Names(event, /Structure_Name)
    IF name EQ 'WIDGET_BUTTON' THEN self.cancelFlag = 1
  ENDIF
  
  RETURN, self.cancelFlag
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::Destroy
;
; PURPOSE:
;
;       Destroys both the widget hierarchy and the object.
;
; SYNTAX:
;
;      PROGRESSBARBTT -> Destroy
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::Destroy

  ; This method takes the widget off the display and destroys the self object.

  ; Restore the old !P.Color.

  TVLCT, self.r, self.g, self.b, (255 < self.oldcolor)
  
  ; Destroy the object.
  
  Widget_Control, self.tlb, Destroy=1
  Obj_Destroy, self
  
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::GETPROPERTY
;
; PURPOSE:
;
;       Allows user to get various progress bar properties.
;
; SYNTAX:
;
;       PROGRESSBARBTT -> GetProperty, Color=currentColor
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR: The name of the color for the progress bar.
;
;       FAST_LOOP: The value of the current "fast loop" flag.
;
;       TEXT:  The textual message that goes above the progress bar.
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::GetProperty, COLOR=color, FAST_LOOP=fast_loop, text2=text, text1=text1

  ; Error handling.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = PROGRESSBARBTT_Error_Message(Traceback=1)
    RETURN
  ENDIF
  
  IF Arg_Present(color) THEN color = self.color
  IF Arg_Present(fast_loop) THEN fast_loop = self.fast
  IF Arg_Present(text2) THEN BEGIN
    IF Widget_Info(self.labelID, /Valid_ID) THEN BEGIN
      Widget_Control, self.labelID, Get_Value=text
      text = text[0]
    ENDIF ELSE text = ""
  ENDIF
  IF Arg_Present(text1) THEN BEGIN
    IF Widget_Info(self.labelID1, /Valid_ID) THEN BEGIN
      Widget_Control, self.labelID1, Get_Value=text1
      text1 = text1[0]
    ENDIF ELSE text1 = ""
  ENDIF
  
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::SETPROPERTY
;
; PURPOSE:
;
;       Allows user to set various progress bar properties.
;
; SYNTAX:
;
;       PROGRESSBARBTT -> SetProperty, Color='yellow'
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CANCEL:        Set this keyword to set the cancelFlag field in the self object.
;
;       COLOR:         The name of the color for the progress bar. By default: "red".
;
;       Possible color names are those defined by FSC_COLOR:
;
;                Almond   Antique White      Aquamarine           Beige          Bisque           Black
;                  Blue     Blue Violet           Brown       Burlywood        Charcoal      Chartreuse
;             Chocolate           Coral        Cornsilk            Cyan  Dark Goldenrod       Dark Gray
;            Dark Green      Dark Khaki     Dark Orchid     Dark Salmon       Deep Pink     Dodger Blue
;             Firebrick    Forest Green            Gold       Goldenrod            Gray           Green
;          Green Yellow        Honeydew        Hot Pink      Indian Red           Ivory           Khaki
;              Lavender      Lawn Green     Light Coral      Light Cyan      Light Gray    Light Salmon
;          Light Yellow      Lime Green           Linen         Magenta          Maroon     Medium Gray
;         Medium Orchid        Moccasin            Navy           Olive      Olive Drab          Orange
;            Orange Red          Orchid  Pale Goldenrod      Pale Green          Papaya            Peru
;                  Pink            Plum     Powder Blue          Purple             Red            Rose
;            Rosy Brown      Royal Blue    Saddle Brown          Salmon     Sandy Brown       Sea Green
;              Seashell          Sienna        Sky Blue      Slate Gray            Snow    Spring Green
;            Steel Blue             Tan         Thistle          Tomato       Turquoise          Violet
;            Violet Red           Wheat           White          Yellow
;
;       FAST_LOOP:     Set this keyword to one to allow "fast looping". Set to 0 to turn it off.
;
;       TEXT:          The textual message that goes above the progress bar.
;
;       TITLE:         The title of the progress bar window. By default: "Progress Bar".
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::SetProperty, CANCEL=cancel, COLOR=color, FAST_LOOP=fast_loop, text2=text, TITLE=title, text1=text1

  ; Error handling.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = PROGRESSBARBTT_Error_Message(Traceback=1)
    RETURN
  ENDIF
  
  IF N_Elements(cancel) NE 0 THEN self.cancelFlag = Keyword_Set(cancel)
  IF N_Elements(fast_loop) NE 0 THEN self.fast = fast_loop
  IF N_Elements(color) NE 0 THEN BEGIN
    self.color = color
    IF self.fast THEN TVLCT, FSC_Color(self.color, /Triple), self.colorindex
  ENDIF
  IF N_Elements(text) NE 0 THEN BEGIN
    self.text = text
    IF Widget_Info(self.labelID, /Valid_ID) THEN Widget_Control, self.labelID, Set_Value=text
  ENDIF
  IF N_Elements(text1) NE 0 THEN BEGIN
    self.text1 = text1
    IF Widget_Info(self.labelID1, /Valid_ID) THEN Widget_Control, self.labelID1, Set_Value=text1
  ENDIF
  IF N_Elements(title) NE 0 THEN BEGIN
    self.title = title
    IF Widget_Info(self.tlb, /Valid_ID) THEN Widget_Control, self.tlb, TLB_Set_Title=title
  ENDIF
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::START
;
; PURPOSE:
;
;       Puts the progress bar on the display.
;
; SYNTAX:
;
;       PROGRESSBARBTT -> Start
;
; ARGUMENTS:
;
;       initialPercent -- The initial percentage that should be on the progress bar. By default, 0.
;
; KEYWORDS:
;
;       None.
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::Start, initialPercent

  ; Error handling.

  Widget_Control, self.tlb, /show
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = PROGRESSBARBTT_Error_Message(Traceback=1)
    RETURN
  ENDIF
  
  ; Save the old !P.Color.
  
  self.oldcolor = !P.Color
  TVLCT, r, g, b, /Get
  self.r = r[self.oldcolor < 255]
  self.g = g[self.oldcolor < 255]
  self.b = b[self.oldcolor < 255]
  
  ; Do we want fast looping?
  
  IF self.fast THEN BEGIN
    self.colorindex = (!D.Table_Size-1) < !P.Color
    TVLCT, FSC_Color(self.color, /Triple), self.colorindex
  ENDIF
  
  ; Find the window index number of any open display window.
  
  thisWindow = !D.Window
  
  ; Realize the widget.
  
  Widget_Control, self.tlb, /Realize, Map=1
  
  ; Get the window index number of the draw widget.
  
  if ~self.noDraw then begin
    Widget_Control, self.drawID, Get_Value=wid
    self.wid = wid
    
    ; Back to the open display window.
    
    IF thisWindow GE 0 THEN WSet, thisWindow
  endif
  
  ; Do you need a starting update?
  
  IF N_Elements(initialPercent) NE 0 THEN self -> Update, initialPercent
  
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::Update
;
; PURPOSE:
;
;       Updates the progress bar
;
; SYNTAX:
;
;       PROGRESSBARBTT -> Update, percent
;
; ARGUMENTS:
;
;       percent -- A value between 0 and 100 that represents the percentage of the progress
;                  bar that should be colored.
;
; KEYWORDS:
;
;       None.
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::Update, percent, text2=theText, text1=text1
  ; This method updates the display. PERCENT should be a value between 0 and 100.
  ; The text will be substituted for the message text.
  Widget_Control, self.tlb, /SHOW
  
  percent = 0 > percent < 100
  ;if ~self.noDraw then begin
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    
    ; Catch a WSET error silently.
    
    IF !Error_State.Code EQ -386 THEN RETURN
    ok = PROGRESSBARBTT_Error_Message(Traceback=1)
    RETURN
  ENDIF
  
  
  ; Update the progress box.   ; number of buttons
  
  if ~self.noDraw then begin
    thisWindow = !D.Window
    WSet, self.wid
    x1 = 0
    y1 = 0
    x2 = Fix(self.xsize  * (percent/100.0))
    y2 = self.ysize
    IF N_Elements(theText) NE 0 THEN Widget_Control, self.labelID, Set_Value=theText
    IF N_Elements(text1) NE 0 THEN Widget_Control, self.labelID1, Set_Value=text1
    IF self.fast THEN BEGIN
      Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=self.colorindex
    ENDIF ELSE BEGIN
      Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=FSC_Color(self.color, !P.Color)
    ENDELSE
    IF thisWindow GE 0 AND thisWindow NE self.wid THEN WSet, thisWindow
  ;endif
  endif else begin
    self->updateButtons, percent
  endelse
END

;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::CLEANUP
;
; PURPOSE:
;
;       Nothing to do in this cleanup routine.
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::setTotalCount, value

 self.totalCount = value

END

PRO PROGRESSBARBTT::updateCount, value

 self.count=value

END

;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::CLEANUP
;
; PURPOSE:
;
;       Nothing to do in this cleanup routine.
;
;*****************************************************************************************************
PRO PROGRESSBARBTT::CLEANUP
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT::INIT
;
; PURPOSE:
;
;       Implements a progress bar widget functionality.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ACCEPT:        If this keyword is set, and "Accept" button is created. When "ACCEPT"
;                      is discovered, a BREAK command is issued to get out of the loop.
;
;       COLOR:         The name of the color for the progress bar. By default: "red".
;
;       Possible color names are those defined by FSC_COLOR:
;
;                Almond   Antique White      Aquamarine           Beige          Bisque           Black
;                  Blue     Blue Violet           Brown       Burlywood        Charcoal      Chartreuse
;             Chocolate           Coral        Cornsilk            Cyan  Dark Goldenrod       Dark Gray
;            Dark Green      Dark Khaki     Dark Orchid     Dark Salmon       Deep Pink     Dodger Blue
;             Firebrick    Forest Green            Gold       Goldenrod            Gray           Green
;          Green Yellow        Honeydew        Hot Pink      Indian Red           Ivory           Khaki
;              Lavender      Lawn Green     Light Coral      Light Cyan      Light Gray    Light Salmon
;          Light Yellow      Lime Green           Linen         Magenta          Maroon     Medium Gray
;         Medium Orchid        Moccasin            Navy           Olive      Olive Drab          Orange
;            Orange Red          Orchid  Pale Goldenrod      Pale Green          Papaya            Peru
;                  Pink            Plum     Powder Blue          Purple             Red            Rose
;            Rosy Brown      Royal Blue    Saddle Brown          Salmon     Sandy Brown       Sea Green
;              Seashell          Sienna        Sky Blue      Slate Gray            Snow    Spring Green
;            Steel Blue             Tan         Thistle          Tomato       Turquoise          Violet
;            Violet Red           Wheat           White          Yellow
;
;       GROUP_LEADER:  The group leader for the progress bar.
;
;       NOCANCEL:      Set this keyword to eliminate the CANCEL button from the progres bar.
;
;       PERCENT:       The initial percent on the progress bar. Used only if the START keyword is
;                      also set.
;
;       START:         Set this keyword if you wish to call the START method immediately upon initialization.
;
;       TEXT:          The textual message that goes above the progress bar. By default:
;                      "Operation in progress..."
;
;       TITLE:         The title of the progress bar window. By default: "Progress Bar".
;
;       XOFFSET:       The X offset for the progress bar when it appears.
;
;       XSIZE:         The X size of the progress bar itself. By default, 150 pixels.
;
;       YOFFSET:       The Y offset of the progress bar when it appears.
;
;       YSIZE:         The Y size of the progress bar itself. By default, 10 pixels.
;
;*****************************************************************************************************
FUNCTION PROGRESSBARBTT::INIT, $
    ACCEPT=accept, $             ; Set this keyword to get an Accept button.
    COLOR=color, $               ; The name of the color of the progress bar.
    FAST_LOOP=fast_loop, $       ; The user plans to use the progress bar in a fast loop.
    GROUP_LEADER=group_leader, $ ; The identifier of the group leader widget.
    NOCANCEL=noCancel, $         ; Set this keyword to leave off the CANCEL button.
    PERCENT=percent, $           ; Initial percent of the progress bar. (Only recognized if START used.)
    START=start, $               ; Set this keyword if you wish to call the START method from INIT.
    text2=text, $                 ; The message text to be written over the progress bar.
    TEXT1=text1, $                 ; The message text to be written under the progress bar.
    TITLE=title, $               ; The title of the top-level base widget.
    XOFFSET=xoffset, $           ; The X offset of the progress bar.
    XSIZE=xsize, $               ; The X size of the progress bar.
    YOFFSET=yoffset, $           ; The Y offset of the progress bar.
    YSIZE=ysize, $                  ; The Y size of the progress bar.
    NODRAW=NODRAW, $            ; Don't use widget_draw but widget_button to emulate progress bar.
    BUTTONSAMPLE=buttonSample, $            ; The argument for VALUE filling of widget_button.
    BUTTONNUMBER=buttonnumber, $            ; The number of buttons you want to emulate draw.
    TRUECOLOR=truecolor, $             ; The sample keyword is a bytarr(3) rgb
    GRAYSCALE=grayscale, $             ; The sample keyword is a bytarr(nxm)
    FILE=file, $             ; The sample keyword is a full file name
    screensize=screensize             ; The sample keyword is a full file name
    
  ; Error handling.
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = PROGRESSBARBTT_Error_Message(Traceback=1)
    RETURN, 0
  ENDIF
  
  ; Check keywords.
  self.noDraw=0
  IF N_Elements(color) EQ 0 THEN self.color = "red" ELSE self.color = color
  if keyword_set(NODRAW) then begin
    self.noDraw=1
    tooManyArgs=keyword_set(truecolor)+keyword_set(grayscale)+keyword_set(file)
    if tooManyArgs gt 1 then message, 'conflicting args TRUECOLOR/GRAYSCALE/FILE: set only one (FILE for default)'
    if tooManyArgs eq 0 then file=1
    if keyword_set(file) then begin
      self.type=1
      check=file_test(buttonSample)
      if ~check then message, 'samplebuttons: filename '+buttonSample+'doesn''t exist'
    endif
    if keyword_set(truecolor) then begin
      self.type=2
      checkStr=size(buttonSample, /STR)
      if checkStr.dimensions[0] ne 3 and $
        checkStr.n_elements ne 3 then message, 'samplebuttons: need bytarr(3) '
    endif
    if keyword_set(grayscale) then begin
      self.type=3
      checkStr=size(buttonSample, /STR)
      if checkStr.dimensions[0] le 1 and $
        checkStr.dimensions[1] le 1 and $
        checkStr.n_elements ne checkStr.dimensions[0]*checkStr.dimensions[1] $
        then message, 'samplebuttons: need bytarr(n,m) '
    endif
    self->setSample, buttonSample
    if keyword_set(buttonNumber) then self.buttonNumber=buttonNumber else self.buttonNumber=10
    if self.buttonNumber lt 10 then begin
      message, 'buttonNumber too low: set 10 as default value)
      self.buttonNumber=10
    endif
  endif
  self.fast = Keyword_Set(fast_loop)
  IF N_Elements(text) EQ 0 THEN text = "Operation in progress..."
  IF N_Elements(text1) EQ 0 THEN text1 = " "
  IF N_Elements(title) EQ 0 THEN title = "Progress Bar"
  IF N_Elements(xsize) EQ 0 THEN self.xsize = 150 ELSE self.xsize = xsize
  IF N_Elements(ysize) EQ 0 THEN self.ysize = 10 ELSE self.ysize = ysize
  
  ; Create the widgets for the progress bar.
  
  self.tlb = Widget_Base(Title=title, Column=1, Base_Align_Center=1, $
    Map=0, Group_Leader=group_leader, TLB_FRAME_ATTR=11)
  self.labelID = Widget_Label(self.tlb, Value=text, /Dynamic_Resize)
  if keyword_set(NODRAW) then self->buildButtonSection else self.drawID = Widget_Draw(self.tlb, XSize=self.xsize, YSize=self.ysize)
  self.labelID1 = Widget_Label(self.tlb, Value=text1, /Dynamic_Resize)
  Widget_Control, self.tlb, Set_UValue=self
  
  IF Keyword_Set(accept) THEN BEGIN
    buttonBase = Widget_Base(self.tlb,Row=1)
    self.acceptID = Widget_Button(buttonBase, Value='Accept')
  ENDIF ELSE self.acceptID = -1L
  IF NOT Keyword_Set(nocancel) THEN BEGIN
    IF N_Elements(buttonBase) EQ 0 THEN buttonBase = Widget_Base(self.tlb,Row=1)
    self.cancelID = Widget_Button(buttonBase, Value='Cancel')
  ENDIF ELSE self.cancelID = -1L
  
  ; Center the top-level base if offsets are not used. Othersize, use offsets.
  IF (N_Elements(xoffset) NE 0) OR (N_Elements(yoffset) NE 0) THEN BEGIN
    IF N_Elements(xoffset) EQ 0 THEN xoffset=0
    IF N_Elements(yoffset) EQ 0 THEN yoffset=0
    Widget_Control, self.tlb, XOFFSET=xoffset, YOFFSET=yoffset
    
  ENDIF ELSE BEGIN
    ;Device, Get_Screen_Size=screenSize
    IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
    xCenter = screenSize(0) / 2
    yCenter = screenSize(1) / 2
    
    geom = Widget_Info(self.tlb, /Geometry)
    xHalfSize = geom.Scr_XSize / 2
    yHalfSize = geom.Scr_YSize / 2
    
    Widget_Control, self.tlb, XOffset = xCenter-xHalfSize, $
      YOffset = yCenter-yHalfSize
  ENDELSE
  
  ; Start it up?
  IF Keyword_Set(start) THEN self -> Start, percent
  
  RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT CLASS DEFINITION
;
; PURPOSE:
;
;       This is the PROGRESSBARBTT object's structure definition code.
;
;*****************************************************************************************************
PRO PROGRESSBARBTT__DEFINE

  struct = { PROGRESSBARBTT, $      ; The object class name.
    cancelFlag: 0L, $   ; A flag to indicate that the CANCEL button was clicked.
    cancelID: 0L, $     ; The identifier of the CANCEL button.
    acceptID: 0L, $     ; The identifier of the ACCEPT button.
    color: "", $        ; The name of the color of the progress bar.
    colorindex: 0L, $   ; The color index number (set by a call to FSC_COLOR).
    drawID: 0L, $       ; The identifier of the draw widget.
    fast: 0L, $         ; A "fast loop" flag.
    labelID: 0L, $      ; The identifier of the label widget.
    labelID1: 0L, $      ; The identifier of the label widget.
    oldcolor: 0L, $     ; The color index of !P.Color.
    r: 0B, $            ; The r value of !P.Color.
    g: 0B, $            ; The g value of !P.Color.
    b: 0B, $            ; The b value of !P.Color.
    text: "", $         ; The text message to be written over the progress bar.
    text1: "", $         ; The text message to be written over the progress bar.
    title: "", $        ; The title of the top-level base widget.
    totalCount: 0l, $ ; The total of objects to process
    count: 0l, $ ; The actual object position
    tlb: 0L, $          ; The identifier of the top-level base.
    wid: 0L, $          ; The window index number of the draw widget.
    xsize: 0L, $        ; The XSize of the progress bar.
    ysize: 0L, $         ; The YSize of the progress bar.
    buttonList: ptr_new(), $ ; The widget_button ids that emulate draw.
    sample: ptr_new(), $ ; The argument for VALUE filling of widget_button.
    type: 0b, $     ; The type for VALUE filling of widget_button.
    buttonNumber: 0, $; The number of buttons that emulate draw.
    buttonBase: 0l, $; The base tha t contains draw buttons
    noDraw: 0b $; The mode of the progress bar (1 buttons, 0 draw).
    }
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBARBTT EXAMPLE PROGRAM
;
; PURPOSE:
;
;       This is the PROGRESSBARBTT example program that demonstrates how to use the progress bar.
;
;*****************************************************************************************************
PRO PROGRESSBARBTT_Example_Event, event

  ;Respond to program button events.

  Widget_Control, event.id, Get_Value=buttonValue
  
  CASE buttonValue OF
  
    'Start Loop (Normal)': BEGIN
    
      ;   Create the progress bar.
    
      ;PROGRESSBARBTT = Obj_New('PROGRESSBARBTT', Color='red', Text1='Loop Iteration 0')
      PROGRESSBARBTT = Obj_New('PROGRESSBARBTT', Color='red', Text1='Loop Iteration 0', /NODRAW, buttonSample=[0b,200b,0b], /TRUE)
      
      ;   Place the progress bar on the display.
      
      PROGRESSBARBTT -> Start
      
      ;   Start the loop.
      
      count = 0
      FOR j=0, 1000 DO BEGIN
      
        IF j MOD 100 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.
        
          ;       Did the user try to cancel the progress bar?
        
          IF PROGRESSBARBTT->CheckCancel() THEN BEGIN
            ok = Dialog_Message('User cancelled operation.') ; Other cleanup, etc. here.
            PROGRESSBARBTT -> Destroy ; Destroy the progress bar.
            RETURN
          ENDIF
          
          ;       If user didn't cancel, update the progress bar. Update value
          ;       must be between 0 and 100.
          
          PROGRESSBARBTT -> Update, (count * 10.0), Text2='Loop Iteration ' + StrTrim(j,2)
          count = count + 1
        ENDIF
        
        Wait, 0.01 ; This is where you would do something useful.
      ENDFOR
      
      ;   Destroy the progress bar when you are finished with it.
      
      PROGRESSBARBTT -> Destroy
    ENDCASE
    
    'Start Loop (Accept)': BEGIN
    
      ;   Create the progress bar.
    
      ;PROGRESSBARBTT = Obj_New('PROGRESSBARBTT', Color='red', Text1='Loop Iteration 0', /Accept)
      PROGRESSBARBTT = Obj_New('PROGRESSBARBTT', Color='red', Text1='Loop Iteration 0', /Accept, /NODRAW, buttonSample=[0b,200b,0b], /TRUE)
      
      ;   Place the progress bar on the display.
      
      PROGRESSBARBTT -> Start
      
      ;   Start the loop.
      
      count = 0
      FOR j=0, 1000 DO BEGIN
      
        IF j MOD 100 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.
        
          ;       Did the user try to cancel the progress bar or did the user Accept?
        
          IF PROGRESSBARBTT -> CheckButton(Accept=acceptButton) THEN BEGIN
          
            IF acceptButton THEN BEGIN
            
              PROGRESSBARBTT -> Update, (count * 10.0), Text2='Loop Iteration ' + StrTrim(j,2)
              ok = Dialog_Message('Final loop count is: '+ StrTrim(j,2))
              BREAK
              
            ENDIF ELSE BEGIN
            
              ok = Dialog_Message('User cancelled operation.') ; Other cleanup, etc. here.
              PROGRESSBARBTT -> Destroy ; Destroy the progress bar.
              RETURN
            ENDELSE
            
          ENDIF
          
          ;       If user didn't cancel, update the progress bar. Update value
          ;       must be between 0 and 100.
          
          PROGRESSBARBTT -> Update, (count * 10.0), Text2='Loop Iteration ' + StrTrim(j,2)
          count = count + 1
        ENDIF
        
        Wait, 0.01 ; This is where you would do something useful.
      ENDFOR
      
      ;Destroy the progress bar when you are finished with it.
      
      PROGRESSBARBTT -> Destroy
    ENDCASE
    
    'Quit': Widget_Control, event.top, /Destroy
    
  ENDCASE
  
END


PRO PROGRESSBARBTT_Example
  DEVICE,DECOMPOSE=0
  LOADCT,39
  tlb = Widget_Base(Column=1, Xoffset=200, Yoffset=200)
  button = Widget_Button(tlb, Value='Start Loop (Normal)')
  button = Widget_Button(tlb, Value='Start Loop (Accept)')
  quiter = Widget_Button(tlb, Value='Quit')
  Widget_Control, tlb, /Realize
  XManager, 'PROGRESSBARBTT_example', tlb, /No_Block
END


;+
; NAME:
;       FSC_COLOR
;
; PURPOSE:
;
;       The purpose of this function is to obtain drawing colors
;       by name and in a device/decomposition independent way.
;       The color names and values may be read in as a file, or 192 color
;       names and values are supplied with the program. These colors were
;       obtained from the file rgb.txt, found on most X-Window distributions,
;       and from colors in the Brewer color tables (http://colorbrewer2.org/).
;       Representative colors were chosen from across the color spectrum. To
;       see a list of colors available, type:
;
;          Print, FSC_Color(/Names), Format='(6A18)'
;
;        If the color names '0', '1', '2', ..., '255' are used, they will
;        correspond to the colors in the current color table in effect at
;        the time the FSC_Color program is called.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING:
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438 begin_of_the_skype_highlighting              970-221-0438      end_of_the_skype_highlighting
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics, Color Specification.
;
; CALLING SEQUENCE:
;
;       color = FSC_Color(theColor, theColorIndex)
;
; NORMAL CALLING SEQUENCE FOR DEVICE-INDEPENDENT COLOR:
;
;       If you write your graphics code *exactly* as it is written below, then
;       the same code will work in all graphics devices I have tested.
;       These include the PRINTER, PS, and Z devices, as well as X, WIN, and MAC.
;
;       In practice, graphics code is seldom written like this. (For a variety of
;       reasons, but laziness is high on the list.) So I have made the
;       program reasonably tolerant of poor programming practices. I just
;       point this out as a place you might return to before you write me
;       a nice note saying my program "doesn't work". :-)
;
;       axisColor = FSC_Color("Green", !D.Table_Size-2)
;       backColor = FSC_Color("Charcoal", !D.Table_Size-3)
;       dataColor = FSC_Color("Yellow", !D.Table_Size-4)
;       thisDevice = !D.Name
;       Set_Plot, 'toWhateverYourDeviceIsGoingToBe', /Copy
;       Device, .... ; Whatever you need here to set things up properly.
;       IF (!D.Flags AND 256) EQ 0 THEN $
;         POLYFILL, [0,1,1,0,0], [0,0,1,1,0], /Normal, Color=backColor
;       Plot, Findgen(11), Color=axisColor, Background=backColor, /NoData, $
;          NoErase= ((!D.Flags AND 256) EQ 0)
;       OPlot, Findgen(11), Color=dataColor
;       Device, .... ; Whatever you need here to wrap things up properly.
;       Set_Plot, thisDevice
;
; OPTIONAL INPUT PARAMETERS:
;
;       theColor: A string with the "name" of the color. To see a list
;           of the color names available set the NAMES keyword. This may
;           also be a vector of color names. Colors available are these:
;
;           Active            Almond     Antique White        Aquamarine             Beige            Bisque
;             Black              Blue       Blue Violet             Brown         Burlywood        Cadet Blue
;          Charcoal        Chartreuse         Chocolate             Coral   Cornflower Blue          Cornsilk
;           Crimson              Cyan    Dark Goldenrod         Dark Gray        Dark Green        Dark Khaki
;       Dark Orchid          Dark Red       Dark Salmon   Dark Slate Blue         Deep Pink       Dodger Blue
;              Edge              Face         Firebrick      Forest Green             Frame              Gold
;         Goldenrod              Gray             Green      Green Yellow         Highlight          Honeydew
;          Hot Pink        Indian Red             Ivory             Khaki          Lavender        Lawn Green
;       Light Coral        Light Cyan        Light Gray      Light Salmon   Light Sea Green      Light Yellow
;        Lime Green             Linen           Magenta            Maroon       Medium Gray     Medium Orchid
;          Moccasin              Navy             Olive        Olive Drab            Orange        Orange Red
;            Orchid    Pale Goldenrod        Pale Green            Papaya              Peru              Pink
;              Plum       Powder Blue            Purple               Red              Rose        Rosy Brown
;        Royal Blue      Saddle Brown            Salmon       Sandy Brown         Sea Green          Seashell
;          Selected            Shadow            Sienna          Sky Blue        Slate Blue        Slate Gray
;              Snow      Spring Green        Steel Blue               Tan              Teal              Text
;           Thistle            Tomato         Turquoise            Violet        Violet Red             Wheat
;             White            Yellow
;
;           The color OPPOSITE is used if this parameter is absent or a color name is mis-spelled. To see a list
;           of the color names available in the program, type this:
;
;              IDL> Print, FSC_Color(/Names), Format='(6A18)'
;
;       theColorIndex: The color table index (or vector of indices the same length
;           as the color name vector) where the specified color is loaded. The color table
;           index parameter should always be used if you wish to obtain a color value in a
;           color-decomposition-independent way in your code. See the NORMAL CALLING
;           SEQUENCE for details. If theColor is a vector, and theColorIndex is a scalar,
;           then the colors will be loaded starting at theColorIndex.
;
;        When the BREWER keyword is set, you must use more arbitrary and less descriptive color
;        names. To see a list of those names, use the command above with the BREWER keyword set,
;        or call PICKCOLORNAME with the BREWER keyword set:
;
;               IDL> Print, FSC_Color(/Names, /BREWER), Format='(8A10)'
;               IDL> color = PickColorName(/BREWER)
;
;         Here are the Brewer names:
;
;       WT1       WT2       WT3       WT4       WT5       WT6       WT7       WT8
;      TAN1      TAN2      TAN3      TAN4      TAN5      TAN6      TAN7      TAN8
;      BLK1      BLK2      BLK3      BLK4      BLK5      BLK6      BLK7      BLK8
;      GRN1      GRN2      GRN3      GRN4      GRN5      GRN6      GRN7      GRN8
;      BLU1      BLU2      BLU3      BLU4      BLU5      BLU6      BLU7      BLU8
;      ORG1      ORG2      ORG3      ORG4      ORG5      ORG6      ORG7      ORG8
;      RED1      RED2      RED3      RED4      RED5      RED6      RED7      RED8
;      PUR1      PUR2      PUR3      PUR4      PUR5      PUR6      PUR7      PUR8
;      PBG1      PBG2      PBG3      PBG4      PBG5      PBG6      PBG7      PBG8
;      YGB1      YGB2      YGB3      YGB4      YGB5      YGB6      YGB7      YGB8
;      RYB1      RYB2      RYB3      RYB4      RYB5      RYB6      RYB7      RYB8
;       TG1       TG2       TG3       TG4       TG5       TG6       TG7       TG8
;
;       As of 3 July 2008, the Brewer names are also now available to the user without using
;       the BREWER keyword. If the BREWER keyword is used, *only* Brewer names are available.
;
;       The color name "OPPOSITE" is also available. It chooses a color "opposite" to the
;       color of the pixel in the upper-right corner of the display, if a window is open.
;       Otherwise, this color is "black" in PostScript and "white" everywhere else.
;
; RETURN VALUE:
;
;       The value that is returned by FSC_Color depends upon the keywords
;       used to call it, on the version of IDL you are using,and on the depth
;       of the display device when the program is invoked. In general,
;       the return value will be either a color index number where the specified
;       color is loaded by the program, or a 24-bit color value that can be
;       decomposed into the specified color on true-color systems. (Or a vector
;       of such numbers.)
;
;       If you are running IDL 5.2 or higher, the program will determine which
;       return value to use, based on the color decomposition state at the time
;       the program is called. If you are running a version of IDL before IDL 5.2,
;       then the program will return the color index number. This behavior can
;       be overruled in all versions of IDL by setting the DECOMPOSED keyword.
;       If this keyword is 0, the program always returns a color index number. If
;       the keyword is 1, the program always returns a 24-bit color value.
;
;       If the TRIPLE keyword is set, the program always returns the color triple,
;       no matter what the current decomposition state or the value of the DECOMPOSED
;       keyword. Normally, the color triple is returned as a 1 by 3 column vector.
;       This is appropriate for loading into a color index with TVLCT:
;
;          IDL> TVLCT, FSC_Color('Yellow', /Triple), !P.Color
;
;       But sometimes (e.g, in object graphics applications) you want the color
;       returned as a row vector. In this case, you should set the ROW keyword
;       as well as the TRIPLE keyword:
;
;          viewobj= Obj_New('IDLgrView', Color=FSC_Color('charcoal', /Triple, /Row))
;
;       If the ALLCOLORS keyword is used, then instead of a single value, modified
;       as described above, then all the color values are returned in an array. In
;       other words, the return value will be either an NCOLORS-element vector of color
;       table index numbers, an NCOLORS-element vector of 24-bit color values, or
;       an NCOLORS-by-3 array of color triples.
;
;       If the NAMES keyword is set, the program returns a vector of
;       color names known to the program.
;
;       If the color index parameter is not used, and a 24-bit value is not being
;       returned, then colorIndex is typically set to !D.Table_Size-1. However,
;       this behavior is changed on 8-bit devices (e.g., the PostScript device,
;       or the Z-graphics buffer) and on 24-bit devices that are *not* using
;       decomposed color. On these devices, the colors are loaded at an
;       offset of !D.Table_Size - ncolors - 2, and the color index parameter reflects
;       the actual index of the color where it will be loaded. This makes it possible
;       to use a formulation as below:
;
;          Plot, data, Color=FSC_Color('Dodger Blue')
;
;       on 24-bit displays *and* in PostScript output! Note that if you specify a color
;       index (the safest thing to do), then it will always be honored.
;
; INPUT KEYWORD PARAMETERS:
;
;       ALLCOLORS: Set this keyword to return indices, or 24-bit values, or color
;              triples, for all the known colors, instead of for a single color.
;
;       BREWER: Set this keyword if you wish to use the Brewer Colors, as defined
;              in this reference:
;
;              http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_intro.html
;
;              As of 3 July 2008, the BREWER names are always available to the user, with or
;              without this keyword. If the keyword is used, only BREWER names are available.
;
;       DECOMPOSED: Set this keyword to 0 or 1 to force the return value to be
;              a color table index or a 24-bit color value, respectively.
;
;       CHECK_CONNECTION: If this keyword is set, the program will check to see if it can obtain
;              a window connection before it tries to load system colors (which require one). If you
;              think you might be using FSC_COLOR in a cron job, for example, you would want to set this
;              keyword. If there is no window connection, the system colors are not available from the program.
;              As of 18 October 2010, this keyword is no longer used, as system colors have been
;              removed from the program.
;
;       FILENAME: The string name of an ASCII file that can be opened to read in
;              color values and color names. There should be one color per row
;              in the file. Please be sure there are no blank lines in the file.
;              The format of each row should be:
;
;                  redValue  greenValue  blueValue  colorName
;
;              Color values should be between 0 and 255. Any kind of white-space
;              separation (blank characters, commas, or tabs) are allowed. The color
;              name should be a string, but it should NOT be in quotes. A typical
;              entry into the file would look like this:
;
;                  255   255   0   Yellow
;
;       NAMES: If this keyword is set, the return value of the function is
;              a ncolors-element string array containing the names of the colors.
;              These names would be appropriate, for example, in building
;              a list widget with the names of the colors. If the NAMES
;              keyword is set, the COLOR and INDEX parameters are ignored.
;
;                 listID = Widget_List(baseID, Value=GetColor(/Names), YSize=16)
;
;
;       NODISPLAY: Normally, FSC_COLOR loads "system" colors as part of its palette of colors.
;              In order to do so, it has to create an IDL widget, which in turn has to make
;              a connection to the windowing system. If your program is being run without a
;              window connection, then this program will fail. If you can live without the system
;              colors (and most people don't even know they are there, to tell you the truth),
;              then setting this keyword will keep them from being loaded, and you can run
;              FSC_COLOR without a display. THIS KEYWORD NOW DEPRECIATED IN FAVOR OF CHECK_CONNECTION.
;              As of 18 October 2010, this keyword is no longer used, as system colors have been
;              removed from the program.
;
;       ROW:   If this keyword is set, the return value of the function when the TRIPLE
;              keyword is set is returned as a row vector, rather than as the default
;              column vector. This is required, for example, when you are trying to
;              use the return value to set the color for object graphics objects. This
;              keyword is completely ignored, except when used in combination with the
;              TRIPLE keyword.
;
;       SELECTCOLOR: Set this keyword if you would like to select the color name with
;              the PICKCOLORNAME program. Selecting this keyword automaticallys sets
;              the INDEX positional parameter. If this keyword is used, any keywords
;              appropriate for PICKCOLORNAME can also be used. If this keyword is used,
;              the first positional parameter can be a color name that will appear in
;              the SelectColor box.
;
;       TRIPLE: Setting this keyword will force the return value of the function to
;              *always* be a color triple, regardless of color decomposition state or
;              visual depth of the machine. The value will be a three-element column
;              vector unless the ROW keyword is also set.
;
;       In addition, any keyword parameter appropriate for PICKCOLORNAME can be used.
;       These include BOTTOM, COLUMNS, GROUP_LEADER, INDEX, and TITLE.
;
; OUTPUT KEYWORD PARAMETERS:
;
;       CANCEL: This keyword is always set to 0, unless that SELECTCOLOR keyword is used.
;              Then it will correspond to the value of the CANCEL output keyword in PICKCOLORNAME.
;
;       COLORSTRUCTURE: This output keyword (if set to a named variable) will return a
;              structure in which the fields will be the known color names (without spaces)
;              and the values of the fields will be either color table index numbers or
;              24-bit color values. If you have specified a vector of color names, then
;              this will be a structure containing just those color names as fields.
;
;       NCOLORS: The number of colors recognized by the program. It will be 104 by default.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;
;   Required programs from the Coyote Library:
;
;      http://www.dfanning.com/programs/error_message.pro
;      http://www.dfanning.com/programs/pickcolorname.pro
;      http://www.dfanning.com/programs/decomposedcolor.pro
;
; EXAMPLE:
;
;       To get drawing colors in a device-decomposed independent way:
;
;           axisColor = FSC_Color("Green", !D.Table_Size-2)
;           backColor = FSC_Color("Charcoal", !D.Table_Size-3)
;           dataColor = FSC_Color("Yellow", !D.Table_Size-4)
;           Plot, Findgen(11), Color=axisColor, Background=backColor, /NoData
;           OPlot, Findgen(11), Color=dataColor
;
;       To set the viewport color in object graphics:
;
;           theView = Obj_New('IDLgrView', Color=FSC_Color('Charcoal', /Triple))
;
;       To change the viewport color later:
;
;           theView->SetProperty, Color=FSC_Color('Antique White', /Triple)
;
;       To load the drawing colors "red", "green", and "yellow" at indices 100-102, type this:
;
;           IDL> TVLCT, FSC_Color(["red", "green", and "yellow"], /Triple), 100
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 19 October 2000. Based on previous
;          GetColor program.
;       Fixed a problem with loading colors with TVLCT on a PRINTER device. 13 Mar 2001. DWF.
;       Added the ROW keyword. 30 March 2001. DWF.
;       Added the PICKCOLORNAME code to the file, since I keep forgetting to
;          give it to people. 15 August 2001. DWF.
;       Added ability to specify color names and indices as vectors. 5 Nov 2002. DWF.
;       Fixed a problem with the TRIPLE keyword when specifying a vector of color names. 14 Feb 2003. DWF.
;       Fixed a small problem with the starting index when specifying ALLCOLORS. 24 March 2003. DWF.
;       Added system color names. 23 Jan 2004. DWF
;       Added work-around for WHERE function "feature" when theColor is a one-element array. 22 July 2004. DWF.
;       Added support for 8-bit graphics devices when color index is not specified. 25 August 2004. DWF.
;       Fixed a small problem with creating color structure when ALLCOLORS keyword is set. 26 August 2004. DWF.
;       Extended the color index fix for 8-bit graphics devices on 25 August 2004 to
;         24-bit devices running with color decomposition OFF. I've concluded most of
;         the people using IDL don't have any idea how color works, so I am trying to
;         make it VERY simple, and yet still maintain the power of this program. So now,
;         in general, for most simple plots, you don't have to use the colorindex parameter
;         and you still have a very good chance of getting what you expect in a device-independent
;         manner. Of course, it would be *nice* if you could use that 24-bit display you paid
;         all that money for, but I understand your reluctance. :-)   11 October 2004. DWF.
;       Have renamed the first positional parameter so that this variable doesn't change
;         while the program is running. 7 December 2004. DWF.
;       Fixed an error I introduced on 7 December 2004. Sigh... 7 January 2005. DWF.
;       Added eight new colors. Total now of 104 colors. 11 August 2005. DWF.
;       Modified GUI to display system colors and removed PickColorName code. 13 Dec 2005. DWF.
;       Fixed a problem with colorIndex when SELECTCOLOR keyword was used. 13 Dec 2005. DWF.
;       Fixed a problem with color name synonyms. 19 May 2006. DWF.
;       The previous fix broke the ability to specify several colors at once. Fixed. 24 July 2006. DWF.
;       Updated program to work with 24-bit Z-buffer in IDL 6.4. 11 June 2007. DWF
;       Added the CRONJOB keyword. 07 Feb 2008. DWF.
;       Changed the CRONJOB keyword to NODISPLAY to better reflect its purpose. 7 FEB 2008. DWF.
;       Added the BREWER keyword to allow selection of Brewer Colors. 15 MAY 2008. DWF.
;       Added the CHECK_CONNECTION keyword and depreciated the NODISPLAY keyword for cron jobs. 15 MAY 2008. DWF.
;       Added the BREWER names to the program with or without the BREWER keyword set. 3 JULY 2008. DWF.
;       If color names '0', '1', '2', ..., '255', are used, the colors are taken from the current
;          color table in effect when the program is called. 23 March 2009. DWF.
;       Added the ability to use 24-bit PostScript color, if available. 24 May 2009. DWF.
;       Program relies on DecomposedColor() to determine decomposed state of PostScript device. 24 May 2009. DWF.
;       Mis-spelled variable name prevented color structure from being returned by COLORSTRUCTURE keyword. 14 Oct 2009. DWF.
;       The Z-buffer, after IDL 6.4, appears to be brain dead. It is *always* in DECOMPOSED=1
;           mode, even when using an 8-bit graphics buffer. (Unless set manaually.) Now I set
;           the decomposition mode in the program by looking at buffer depth, rather than
;           the decomposition state, for the Z-buffer. 23 July 2010. DWF.
;       Adding "system" colors to FSC_Color may have been the worst design decision I ever made.
;            Since these require a window connection to determine, this decision has haunted
;            me for years in the form of people wanting to use the software in places where
;            there is no window connection. For example, in cron jobs. To accommodate legacy
;            code, I need to *always* check for a connection. I am loath to do this because it
;            means opening and closing a window, which is, relatively speaking, a slow process.
;            I've made another attempt to solve this problem with this update and the
;            corresponding update to the Coyote Library program CanConnect. CanConnect how
;            creates the system variable !FSC_Display_Connection. If this system variable
;            exists, FSC_Color consults it to determine if there is a display connection. It
;            it doesn't exist, it calls CanConnect. This way, a window has to be open only
;            once in an IDL session to determine if a display connection can be made. 7 Oct 2010. DWF.
;       I am tired of dealing with "system" colors. I've never used them, no one I know
;            has ever used them, and they have given me nothing but headaches. I'm done
;            with them. I'll leave the code here for awhile in case you use them, but
;            I'm pretty sure I'm not putting them back in. 17 October 2010. DWF.
;       Added a new color name "OPPOSITE". This is a color that is "opposite" of the pixel color
;            in the upper right hand corner of the display, if a window is open and the pixel
;            color can be determined. Otherwise, it is black in PostScript and white everywhere
;            else. 19 Nov 2010. DWF.
;       Made sure the ColorIndex that is returned is always an INTEGER. 24 Dec 2010. DWF.
;       Changed the default "unknown" color from WHITE to OPPOSITE. 30 Dec 2010. DWF.
;       TVLCT commands protected from NULL device. 4 Jan 2011. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2010, by Fanning Software Consulting, Inc.                           ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
;FUNCTION FSC_Color_Count_Rows, filename, MaxRows = maxrows
;
;  ; This utility routine is used to count the number of
;  ; rows in an ASCII data file.
;
;  IF N_Elements(maxrows) EQ 0 THEN maxrows = 500L
;  IF N_Elements(filename) EQ 0 THEN BEGIN
;    filename = Dialog_Pickfile()
;    IF filename EQ "" THEN RETURN, -1
;  ENDIF
;  
;  OpenR, lun, filename, /Get_Lun
;  
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /Cancel
;    count = count-1
;    Free_Lun, lun
;    RETURN, count
;  ENDIF
;  
;  RESTART:
;  
;  count = 1L
;  line = ''
;  FOR j=count, maxrows DO BEGIN
;    ReadF, lun, line
;    count = count + 1
;    
;    ; Try again if you hit MAXROWS without encountering the
;    ; end of the file. Double the MAXROWS parameter.
;    
;    IF j EQ maxrows THEN BEGIN
;      maxrows = maxrows * 2
;      Point_Lun, lun, 0
;      GOTO, RESTART
;    ENDIF
;    
;  ENDFOR
;  
;  RETURN, -1
;  
;END ;-------------------------------------------------------------------------------
;
;
;
;FUNCTION FSC_Color_Color24, color
;
;  ; This FUNCTION accepts a [red, green, blue] triple that
;  ; describes a particular color and returns a 24-bit long
;  ; integer that is equivalent to (can be decomposed into)
;  ; that color. The triple can be either a row or column
;  ; vector of 3 elements or it can be an N-by-3 array of
;  ; color triples.
;
;  ON_ERROR, 2
;  
;  s = Size(color)
;  
;  IF s[0] EQ 1 THEN BEGIN
;    IF s[1] NE 3 THEN Message, 'Input color parameter must be a 3-element vector.'
;    RETURN, color[0] + (color[1] * 2L^8) + (color[2] * 2L^16)
;  ENDIF ELSE BEGIN
;    IF s[2] GT 3 THEN Message, 'Input color parameter must be an N-by-3 array.'
;    RETURN, color[*,0] + (color[*,1] * 2L^8) + (color[*,2] * 2L^16)
;  ENDELSE
;  
;END ;--------------------------------------------------------------------------------------------



;+
; NAME:
;       PICKCOLORNAME
;
; PURPOSE:
;
;       The purpose of this program is to provide a blocking
;       or modal widget interface for selecting a color "name".
;       The program uses colors familiar to the cgColor program,
;       and is often used to select a color name for passing to cgColor.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING:
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438 begin_of_the_skype_highlighting              970-221-0438      end_of_the_skype_highlighting
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics, Color Specification.
;
; CALLING SEQUENCE:
;
;       colorName = PickColorName(startColorName)
;
; OPTIONAL INPUT PARAMETERS:
;
;       startColorName: A string with the "name" of the color. Colors available are these:
;
;            Almond     Antique White        Aquamarine             Beige            Bisque             Black
;              Blue       Blue Violet             Brown         Burlywood        Cadet Blue          Charcoal
;        Chartreuse         Chocolate             Coral   Cornflower Blue          Cornsilk           Crimson
;              Cyan    Dark Goldenrod         Dark Gray        Dark Green        Dark Khaki       Dark Orchid
;          Dark Red       Dark Salmon   Dark Slate Blue         Deep Pink       Dodger Blue         Firebrick
;      Forest Green              Gold         Goldenrod              Gray             Green      Green Yellow
;          Honeydew          Hot Pink        Indian Red             Ivory             Khaki          Lavender
;        Lawn Green       Light Coral        Light Cyan        Light Gray      Light Salmon   Light Sea Green
;      Light Yellow        Lime Green             Linen           Magenta            Maroon       Medium Gray
;     Medium Orchid          Moccasin              Navy             Olive        Olive Drab            Orange
;        Orange Red            Orchid    Pale Goldenrod        Pale Green            Papaya              Peru
;              Pink              Plum       Powder Blue            Purple               Red              Rose
;        Rosy Brown        Royal Blue      Saddle Brown            Salmon       Sandy Brown         Sea Green
;          Seashell            Sienna          Sky Blue        Slate Blue        Slate Gray              Snow
;      Spring Green        Steel Blue               Tan              Teal           Thistle            Tomato
;         Turquoise            Violet        Violet Red             Wheat             White            Yellow
;
;
;       The color WHITE is used if this parameter is absent.
;
;   If the BREWER keyword is set, you can use the Brewer Color names:
;
;       WT1       WT2       WT3       WT4       WT5       WT6       WT7       WT8
;      TAN1      TAN2      TAN3      TAN4      TAN5      TAN6      TAN7      TAN8
;      BLK1      BLK2      BLK3      BLK4      BLK5      BLK6      BLK7      BLK8
;      GRN1      GRN2      GRN3      GRN4      GRN5      GRN6      GRN7      GRN8
;      BLU1      BLU2      BLU3      BLU4      BLU5      BLU6      BLU7      BLU8
;      ORG1      ORG2      ORG3      ORG4      ORG5      ORG6      ORG7      ORG8
;      RED1      RED2      RED3      RED4      RED5      RED6      RED7      RED8
;      PUR1      PUR2      PUR3      PUR4      PUR5      PUR6      PUR7      PUR8
;      PBG1      PBG2      PBG3      PBG4      PBG5      PBG6      PBG7      PBG8
;      YGB1      YGB2      YGB3      YGB4      YGB5      YGB6      YGB7      YGB8
;      RYB1      RYB2      RYB3      RYB4      RYB5      RYB6      RYB7      RYB8
;       TG1       TG2       TG3       TG4       TG5       TG6       TG7       TG8
;
;   As of 3 JULY 2008, the BREWER names are always available. If the BREWER keyword is set, only
;   the BREWER names are available.
;
; INPUT KEYWORD PARAMETERS:
;
;       BOTTOM: The colors used in the program must be loaded somewhere
;           in the color table. This keyword indicates where the colors
;           start loading. By default BOTTOM is set equal to !D.Table_Size-NCOLORS-1.
;
;       BREWER: Set this keyword if you wish to use the Brewer Colors, as defined
;              in this reference:
;
;              http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_intro.html
;
;       COLUMNS: Set this keyword to the number of columns the colors should
;           be arranged in.
;
;       FILENAME: The string name of an ASCII file that can be opened to read in
;           color values and color names. There should be one color per row
;           in the file. Please be sure there are no blank lines in the file.
;           The format of each row should be:
;
;              redValue  greenValue  blueValue  colorName
;
;           Color values should be between 0 and 255. Any kind of white-space
;           separation (blank characters, commas, or tabs) are allowed. The color
;           name should be a string, but it should NOT be in quotes. A typical
;           entry into the file would look like this:
;
;               255   255   0   Yellow
;
;       GROUP_LEADER: This identifies a group leader if the program is called
;           from within a widget program. Note that this keyword MUST be provided
;           if you want to guarantee modal widget functionality. (If you don't know
;           what this means, believe me, you WANT to use this keyword, always.)
;
;       INDEX: This keyword identifies a color table index where the selected color
;           is to be loaded when the program exits. The default behavior is to restore
;           the input color table and NOT load a color.
;
;       TITLE: This keyword accepts a string value for the window title. The default
;           is "Select a Color".
;
; OUTPUT KEYWORD PARAMETERS:
;
;       CANCEL: On exit, this keyword value is set to 0 if the user selected
;           the ACCEPT button. IF the user selected the CANCEL button, or
;           closed the window in any other way, this keyword value is set to 1.

; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       Colors are loaded in the current color table. The input color table
;       is restored when the program exits. This will only be noticable on
;       8-bit displays. The startColorName is returned if the user cancels
;       or destroys the widget before a selection is made. Users should
;       check the CANCEL flag before using the returned color.
;
; EXAMPLE:
;
;       To call the program from the IDL comamnd line:
;
;         IDL> color = PickColorName("red") & Print, color
;
;       To call the program from within a widget program:
;
;         color = PickColorName("red", Group_Leader=event.top) & Print, color
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 31 August 2000.
;       Modified program to read colors from a file and to use more
;         colors on 24-bit platforms. 16 October 2000. DWF.
;       Added the COLUMNS keyword. 16 October 2000. DWF.
;       Fixed a small problem with mapping a modal widget. 2 Jan 2001. DWF
;       Now drawing small box around each color. 13 March 2003. DWF.
;       Added eight new colors. Total now of 104 colors. 11 August 2005. DWF.
;       Modified GUI to display system colors. 13 Dec 2005. DWF.
;       Added BREWER keyword to allow Brewer Colors. 15 May 2008. DWF.
;       Added all BREWER names to the default naming scheme. 3 July 2008. DWF.
;       Set a size for the color name label widget. Otherwise, the widget always
;          jumps back to the center of the display when I select a color on UNIX
;          machines. Also had to remove TLB updating with UPDATE keyword to avoid
;          tickling the same IDL bug. Sigh... 13 March (Friday) 2009.
;       Removed system color names, since these are no longer available in cgColor.
;          27 Nov 2010. DWF
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;


;PRO PickColorName_CenterTLB, tlb
;
;  ; Center the top-level base widget.
;
;  screenSize = Get_Screen_Size()
;  Device, Get_Screen_Size=screenSize
;  IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2
;  xCenter = screenSize(0) / 2
;  yCenter = screenSize(1) / 2
;  
;  geom = Widget_Info(tlb, /Geometry)
;  xHalfSize = geom.Scr_XSize / 2
;  yHalfSize = geom.Scr_YSize / 2
;  
;  Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
;    YOffset = yCenter-yHalfSize
;    
;END ;-------------------------------------------------------------------------------
;
;
;
;FUNCTION PickColorName_RGB_to_24Bit, number
;
;  ; Convert RGB values to 24-bit number values.
;
;  IF Size(number, /N_Dimensions) EQ 1 THEN BEGIN
;    RETURN, number[0] + (number[1] * 2L^8) + (number[2] * 2L^16)
;  ENDIF ELSE BEGIN
;    RETURN, number[*,0] + (number[*,1] * 2L^8) + (number[*,2] * 2L^16)
;  ENDELSE
;END ;-------------------------------------------------------------------------------
;
;
;
;FUNCTION PickColorName_Count_Rows, filename, MaxRows = maxrows
;
;  ; This utility routine is used to count the number of
;  ; rows in an ASCII data file.
;
;  IF N_Elements(maxrows) EQ 0 THEN maxrows = 500L
;  IF N_Elements(filename) EQ 0 THEN BEGIN
;    filename = Dialog_Pickfile()
;    IF filename EQ "" THEN RETURN, -1
;  ENDIF
;  
;  OpenR, lun, filename, /Get_Lun
;  
;  Catch, error
;  IF error NE 0 THEN BEGIN
;    count = count-1
;    Free_Lun, lun
;    RETURN, count
;  ENDIF
;  
;  RESTART:
;  
;  count = 1L
;  line = ''
;  FOR j=count, maxrows DO BEGIN
;    ReadF, lun, line
;    count = count + 1
;    
;    ; Try again if you hit MAXROWS without encountering the
;    ; end of the file. Double the MAXROWS parameter.
;    
;    IF j EQ maxrows THEN BEGIN
;      maxrows = maxrows * 2
;      Point_Lun, lun, 0
;      GOTO, RESTART
;    ENDIF
;    
;  ENDFOR
;  
;  RETURN, -1
;END ;-------------------------------------------------------------------------------
;
;
;
;PRO PickColorName_Select_Color, event
;
;  ; This event handler permits color selection by clicking on a color window.
;
;  IF event.type NE 0 THEN RETURN
;  
;  Widget_Control, event.top, Get_UValue=info, /No_Copy
;  
;  ; Get the color names from the window you clicked on and set it.
;  
;  Widget_Control, event.id, Get_UValue=thisColorName
;  ;Widget_Control, event.top, Update=0
;  Widget_Control, info.labelID, Set_Value=thisColorName
;  ;Widget_Control, event.top, Update=1
;  
;  ; Get the color value and load it as the current color.
;  
;  WSet, info.mixWID
;  info.theName = thisColorName
;  theIndex = Where(info.colorNames EQ StrUpCase(StrCompress(thisColorName, /Remove_All)))
;  theIndex = theIndex[0]
;  info.nameIndex = theIndex
;  
;  IF info.theDepth GT 8 THEN BEGIN
;    Erase, Color=info.colors24[theIndex]
;    PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=info.black
;  ENDIF ELSE BEGIN
;    TVLCT, info.red[theIndex], info.green[theIndex], info.blue[theIndex], info.mixcolorIndex
;    Erase, Color=info.mixcolorIndex
;    PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=info.black
;  ENDELSE
;  
;  Widget_Control, event.top, Set_UValue=info, /No_Copy
;END ;---------------------------------------------------------------------------
;
;
;
;PRO PickColorName_Buttons, event
;
;  ; This event handler responds to CANCEL and ACCEPT buttons.
;
;  Widget_Control, event.top, Get_UValue=info, /No_Copy
;  
;  ; Which button is this?
;  
;  Widget_Control, event.id, Get_Value=buttonValue
;  
;  ; Branch on button value.
;  
;  CASE buttonValue OF
;  
;    'Cancel': BEGIN
;    
;      ; Simply destroy the widget. The pointer info is already
;      ; set up for a CANCEL event.
;    
;      Widget_Control, event.top, /Destroy
;    ENDCASE
;    
;    'Accept': BEGIN
;    
;      ; Get the new color table after the color has been selected.
;    
;      TVLCT, r, g, b, /Get
;      
;      ; Save the new color and name in the pointer.
;      
;      *(info.ptr) = {cancel:0.0, r:info.red[info.nameIndex], g:info.green[info.nameIndex], $
;        b:info.blue[info.nameIndex], name:info.theName}
;        
;      ; Destoy the widget.
;        
;      Widget_Control, event.top, /Destroy
;      
;    ENDCASE
;  ENDCASE
;END ;---------------------------------------------------------------------------
;
;
;
;
;;+
;; NAME:
;;       TVREAD
;;
;; PURPOSE:
;;
;;       To get accurate screen dumps with the IDL command TVRD on 24-bit
;;       PC and Macintosh computers, you have to be sure to set color
;;       decomposition on. This program adds that capability automatically.
;;       In addition, the program will optionally write BMP, GIF, JPEG,
;;       PICT, PNG, and TIFF color image files of the screen dump.
;;
;; AUTHOR:
;;
;;       FANNING SOFTWARE CONSULTING
;;       David Fanning, Ph.D.
;;       1645 Sheely Drive
;;       Fort Collins, CO 80526 USA
;;       Phone: 970-221-0438 begin_of_the_skype_highlighting              970-221-0438      end_of_the_skype_highlighting
;;       E-mail: davidf@dfanning.com
;;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;;
;; CATEGORY:
;;
;;       Graphics
;;
;; CALLING SEQUENCE:
;;
;;       image = TVREAD(xstart, ystart, ncols, nrows)
;;
;;       The returned image will be a 2D image on 8-bit systems and
;;       a 24-bit pixel interleaved true-color image on 24-bit systems.
;;       A -1 will be returned if a file output keyword is used (e.g., JPEG, TIFF, etc.).
;;
;; OPTIONAL INPUTS:
;;
;;       XSTART -- The starting column index.  By default, 0.
;;
;;       YSTART -- The starting row index. By default, 0.
;;
;;       NCOLS -- The number of columns to read. By default, !D.X_Size - XSTART
;;
;;       NROWS -- The number of rows to read. By default, !D.Y_Size - YSTART.
;;
;; KEYWORD PARAMETERS:
;;
;;       BMP -- Set this keyword to write the screen dump as a color BMP file.
;;
;;       CANCEL -- An output keyword set to 1 if the user cancels out of a
;;          filename dialog. Set to 0 otherwise.
;;
;;       COLORS -- If a 24-bit image has to be quantized, this will set the number
;;          of colors in the output image. Set to 256 by default. Applies to BMP,
;;          GIF, PICT, and PNG formats written from 24-bit displays.(See the
;;          COLOR_QUAN documentation for details.)
;;
;;       CUBE -- If this keyword is set to a value between 2 and 6 the color
;;          quantization will use a cubic method of quantization. Applies to BMP,
;;          GIF, PICT, and PNG formats written from 24-bit displays.(See the
;;          COLOR_QUAN documentation for details.)
;;
;;       DITHER -- If this keyword is set the quantized image will be dithered.
;;          Applies to BMP, GIF, PICT, and PNG formats written from 24-bit displays.
;;          (See the COLOR_QUAN documentation for details.)
;;
;;       FILENAME -- The base name of the output file. (No file extensions;
;;           they will be added automatically.) This name may be changed by the user.
;;
;;              image = TVREAD(Filename='myfile', /JPEG)
;;
;;           No file will be written unless a file output keyword is used
;;           (e.g., JPEG, TIFF, etc.) in the call. By default the FILENAME is
;;           set to "idl". The file extension will be set automatically to match
;;           the type of file created.
;;
;;       GIF -- Set this keyword to write the screen dump as a color GIF file.
;;
;;       JPEG -- Set this keyword to write the screen dump as a color JPEG file.
;;
;;       NODIALOG -- Set this keyword if you wish to avoid the DIALOG_PICKFILE
;;           dialog that asks you to name the output file. This keyword should be
;;           set, for example, if you are processing screens in batch mode.
;;
;;       ORDER -- Set this keyword to determine the image order for reading the
;;           display. Corresponds to !Order and set to such as the default.
;;
;;       OVERWRITE_PROMPT -- Set this keyword if you would like to get a prompt
;;           if you are overwriting a file. This applies only to operations with
;;           DIALOG_PICKFILE.
;;
;;       PICT -- Set this keyword to write the screen dump as a color PICT file.
;;
;;       PNG -- Set this keyword to write the screen dump as a color PNG file.
;;
;;       TIFF -- Set this keyword to write the screen dump as a color TIFF file.
;;
;;       TRUE -- Set this keyword to the type of interleaving you want. 1 = Pixel
;;           interleaved, 2 = row interleaved, 3 = band interleaved.
;;
;;       TYPE -- Can be set to the type of file to write. Use this instead of
;;           setting BMP, GIF, JPEG, PICT, PNG, or TIFF keywords: TYPE='JPEG'. The
;;           primary purpose of this is to make event handlers easier to write.
;;
;;       QUALITY -- This keyword sets the amount of compression for JPEG images.
;;           It should be set to a value between 0 and 100. It is set to 75 by default.
;;           (See the WRITE_JPEG documentation for details.)
;;
;;       WID -- The index number of the window to read from. The current graphics window
;;           (!D.Window) is selected by default. An error is issued if no windows are
;;           currently open on a device that supports windows.
;;
;;       _EXTRA -- Any keywords that are appropriate for the WRITE_*** routines are
;;           also accepted via keyword inheritance.
;;
;; COMMON BLOCKS:
;;
;;       None
;;
;; RESTRICTIONS:   Requires IDL 5.2 and higher.
;;
;; MODIFICATION HISTORY:
;;
;;       Written by David W. Fanning, 9 AUG 2000.
;;       Added changes to make the program more device independent. 16 SEP 2000. DWF.
;;       Removed GIF file support for IDL 5.4 and above. 18 JAN 2001. DWF.
;;       Added NODIALOG keyword. 28 MAR 2001. DWF.
;;       Added an output CANCEL keyword. 29 AUG 2001. DWF.
;;       Added ERROR_MESSAGE code to file. 17 DEC 2001. DWF.
;;       Added ORDER keyword. 25 March 2002. DWF.
;;       Now create 24-bit PNG files if reading from a 24-bit display. 11 May 2002. DWF.
;;       Now create 24-bit BMP files if reading from a 24-bit display. 23 May 2002. DWF.
;;       Removed obsolete STR_SEP and replaced with STRSPLIT. 27 Oct 2004. DWF.
;;       Unleashed the GIF code for IDL 6.2 and above. 10 Nov 2005. DWF.
;;       Rolled back previous change to IDL 6.1. 24 Jan 2006. DWF.
;;       Fixed a problem in which 16-bit displays were treated as 24-bit displays,
;;         and as a result could not produce WHITE colors. Now all 16-bit displays
;;         are treated as 8-bit displays. It is an ugly trade-off. 24 Jan 2006. DWF.
;;       Added TYPE keyword 27 Sep 2006. DWF.
;;       Updated program to work with 24-bit Z-buffer in IDL 6.4. 11 June 2007. DWF.
;;       Added OVERWRITE_PROMPT keyword. 2 Oct 2008. DWF.
;;       Found an extra line of code that wrote PNG files twice! Removed. 10 Nov 2010. DWF.
;;       Replaced TVREAD_ERROR_MESSAGE with ERROR_MESSAGE. 25 Nov 2010. DWF.
;;-
;;
;;******************************************************************************************;
;;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;;  All rights reserved.                                                                    ;
;;                                                                                          ;
;;  Redistribution and use in source and binary forms, with or without                      ;
;;  modification, are permitted provided that the following conditions are met:             ;
;;                                                                                          ;
;;      * Redistributions of source code must retain the above copyright                    ;
;;        notice, this list of conditions and the following disclaimer.                     ;
;;      * Redistributions in binary form must reproduce the above copyright                 ;
;;        notice, this list of conditions and the following disclaimer in the               ;
;;        documentation and/or other materials provided with the distribution.              ;
;;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;;        contributors may be used to endorse or promote products derived from this         ;
;;        software without specific prior written permission.                               ;
;;                                                                                          ;
;;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;;******************************************************************************************;
;;FUNCTION TVREAD, xstart, ystart, ncols, nrows, $
;;    BMP=bmp, $
;;    Cancel=cancel, $
;;    Colors=colors, $
;;    Cube=cube, $
;;    Dither=dither, $
;;    _Extra=extra, $
;;    Filename=filename, $
;;    GIF=gif, $
;;    JPEG=jpeg, $
;;    NoDialog=nodialog, $
;;    Order=order, $
;;    Overwrite_Prompt=overwrite_prompt, $
;;    PICT=pict, $
;;    PNG=png, $
;;    TIFF=tiff, $
;;    True=true, $
;;    TYPE=type, $
;;    Quality=quality, $
;;    WID=wid
;;    
;;  ; Error handling.
;;    
;;  Catch, theError
;;  IF theError NE 0 THEN BEGIN
;;    Catch, /Cancel
;;    ok = Error_Message()
;;    IF N_Elements(thisWindow) EQ 0 THEN RETURN, -1
;;    IF thisWindow GE 0 THEN WSet, thisWindow
;;    RETURN, -1
;;  ENDIF
;;  
;;  cancel = 0
;;  
;;  ; Check for availability of GIF files.
;;  
;;  thisVersion = Float(!Version.Release)
;;  IF (thisVersion LT 5.3) OR (thisVersion GE 6.1) THEN haveGif = 1 ELSE haveGIF = 0
;;  
;;  ; Go to correct window.
;;  
;;  IF N_Elements(wid) EQ 0 THEN wid =!D.Window
;;  thisWindow = !D.Window
;;  IF (!D.Flags AND 256) NE 0 THEN WSet, wid
;;  
;;  ; Check keywords and parameters. Define values if necessary.
;;  
;;  IF N_Elements(xstart) EQ 0 THEN xstart = 0
;;  IF N_Elements(ystart) EQ 0 THEN ystart = 0
;;  IF N_Elements(ncols) EQ 0 THEN ncols = !D.X_Size - xstart
;;  IF N_Elements(nrows) EQ 0 THEN nrows = !D.Y_Size - ystart
;;  IF N_Elements(order) EQ 0 THEN order = !Order
;;  IF N_Elements(true) EQ 0 THEN true = 1
;;  dialog = 1 - Keyword_Set(nodialog)
;;  
;;  ; Do you want to write an image file instead of
;;  ; capturing an image?
;;  IF N_Elements(type) NE 0 THEN BEGIN
;;    CASE StrUpCase(type) OF
;;      'BMP': bmp = 1
;;      'GIF': gif = 1
;;      'JPEG': jpeg = 1
;;      'JPG': jpeg = 1
;;      'PICT': pict = 1
;;      'PNG': png = 1
;;      'TIFF': tiff = 1
;;      'TIF': tif = 1
;;      ELSE: Message, 'Cannot write a file of type: ' + StrUpCase(type) + '.'
;;    ENDCASE
;;  ENDIF
;;  writeImage = 0
;;  fileType = ""
;;  extention = ""
;;  IF Keyword_Set(bmp)THEN BEGIN
;;    writeImage = 1
;;    fileType = 'BMP'
;;    extension = 'bmp'
;;  ENDIF
;;  IF Keyword_Set(gif) THEN BEGIN
;;    IF havegif THEN BEGIN
;;      writeImage = 1
;;      fileType = 'GIF'
;;      extension = 'gif'
;;    ENDIF ELSE BEGIN
;;      ok = Dialog_Message('GIF files not supported in this IDL version. Replacing with JPEG.')
;;      writeImage = 1
;;      fileType = 'JPEG'
;;      extension = 'jpg'
;;    ENDELSE
;;  ENDIF
;;  IF Keyword_Set(jpeg) THEN BEGIN
;;    writeImage = 1
;;    fileType = 'JPEG'
;;    extension = 'jpg'
;;  ENDIF
;;  IF Keyword_Set(PICT) THEN BEGIN
;;    writeImage = 1
;;    fileType = 'PICT'
;;    extension = 'pict'
;;  ENDIF
;;  IF Keyword_Set(png) THEN BEGIN
;;    writeImage = 1
;;    fileType = 'PNG'
;;    extension = 'png'
;;  ENDIF
;;  IF Keyword_Set(tiff) THEN BEGIN
;;    writeImage = 1
;;    fileType = 'TIFF'
;;    extension = 'tif'
;;  ENDIF
;;  
;;  IF N_Elements(colors) EQ 0 THEN colors = 256
;;  IF N_Elements(quality) EQ 0 THEN quality = 75
;;  dither = Keyword_Set(dither)
;;  
;;  ; On 24-bit displays, make sure color decomposition is ON.
;;  
;;  IF (!D.Flags AND 256) NE 0 THEN BEGIN
;;    Device, Get_Decomposed=theDecomposedState, Get_Visual_Depth=theDepth
;;    IF theDepth GT 8 THEN BEGIN
;;      Device, Decomposed=1
;;      IF theDepth EQ 24 THEN truecolor = true ELSE truecolor = 0
;;    ENDIF ELSE truecolor = 0
;;    IF thisWindow LT 0 THEN $
;;      Message, 'No currently open windows. Returning.', /NoName
;;  ENDIF ELSE BEGIN
;;    truecolor = 0
;;    theDepth = 8
;;  ENDELSE
;;  
;;  ; Fix for 24-bit Z-buffer.
;;  IF (Float(!Version.Release) GE 6.4) AND (!D.NAME EQ 'Z') THEN BEGIN
;;    Device, Get_Decomposed=theDecomposedState, Get_Pixel_Depth=theDepth
;;    IF theDepth EQ 24 THEN truecolor = true ELSE truecolor = 0
;;  ENDIF
;;  
;;  
;;  ; Get the screen dump. 2D image on 8-bit displays. 3D image on 24-bit displays.
;;  
;;  image = TVRD(xstart, ystart, ncols, nrows, True=truecolor, Order=order)
;;  
;;  ; Need to set color decomposition back?
;;  
;;  IF theDepth GT 8 THEN Device, Decomposed=theDecomposedState
;;  
;;  ; If we need to write an image, do it here.
;;  
;;  IF writeImage THEN BEGIN
;;  
;;    ; Get the name of the output file.
;;  
;;    IF N_Elements(filename) EQ 0 THEN BEGIN
;;      filename = 'idl.' + StrLowCase(extension)
;;    ENDIF ELSE BEGIN
;;      filename = filename + "." + StrLowCase(extension)
;;    ENDELSE
;;    IF dialog THEN filename = Dialog_Pickfile(/Write, File=filename, OVERWRITE_PROMPT=Keyword_Set(overwrite_prompt))
;;    
;;    IF filename EQ "" THEN BEGIN
;;      cancel = 1
;;      RETURN, image
;;    ENDIF
;;    
;;    ; Write the file.
;;    
;;    CASE fileType OF
;;    
;;      'BMP': BEGIN
;;        IF truecolor THEN BEGIN
;;          ; BMP files assume blue, green, red planes.
;;          temp = image[0,*,*]
;;          image[0,*,*] = image[2,*,*]
;;          image[2,*,*] = temp
;;          Write_BMP, filename, image, _Extra=extra
;;        ENDIF ELSE BEGIN
;;          TVLCT, r, g, b, /Get
;;          Write_BMP, filename, image, r, g, b, _Extra=extra
;;        ENDELSE
;;      END
;;      
;;      'GIF': BEGIN
;;        IF truecolor THEN BEGIN
;;          CASE Keyword_Set(cube) OF
;;            0: image2D = Color_Quan(image, 1, r, g, b, Colors=colors, Dither=dither)
;;            1: image2D = Color_Quan(image, 1, r, g, b, Cube=2 > cube < 6)
;;          ENDCASE
;;        ENDIF ELSE BEGIN
;;          TVLCT, r, g, b, /Get
;;          image2D = image
;;        ENDELSE
;;        Write_GIF, filename, image2D, r, g, b, _Extra=extra
;;      END
;;      
;;      'JPEG': BEGIN
;;        IF truecolor THEN BEGIN
;;          image3D = image
;;        ENDIF ELSE BEGIN
;;          s = Size(image, /Dimensions)
;;          image3D = BytArr(3, s[0], s[1])
;;          TVLCT, r, g, b, /Get
;;          image3D[0,*,*] = r[image]
;;          image3D[1,*,*] = g[image]
;;          image3D[2,*,*] = b[image]
;;        ENDELSE
;;        Write_JPEG, filename, image3D, True=1, Quality=quality, _Extra=extra
;;      END
;;      
;;      'PICT': BEGIN
;;        IF truecolor THEN BEGIN
;;          CASE Keyword_Set(cube) OF
;;            0: image2D = Color_Quan(image, 1, r, g, b, Colors=colors, Dither=dither)
;;            1: image2D = Color_Quan(image, 1, r, g, b, Cube=2 > cube < 6)
;;          ENDCASE
;;        ENDIF ELSE BEGIN
;;          TVLCT, r, g, b, /Get
;;          image2D = image
;;        ENDELSE
;;        Write_PICT, filename, image2D, r, g, b
;;      END
;;      
;;      'PNG': BEGIN
;;        IF truecolor THEN BEGIN
;;          Write_PNG, filename, image, _Extra=extra
;;        ENDIF ELSE BEGIN
;;          TVLCT, r, g, b, /Get
;;          image2D = image
;;          Write_PNG, filename, image2D, r, g, b, _Extra=extra
;;        ENDELSE
;;      END
;;      
;;      'TIFF': BEGIN
;;        IF truecolor THEN BEGIN
;;          image3D = Reverse(image,3)
;;        ENDIF ELSE BEGIN
;;          s = Size(image, /Dimensions)
;;          image3D = BytArr(3, s[0], s[1])
;;          TVLCT, r, g, b, /Get
;;          image3D[0,*,*] = r[image]
;;          image3D[1,*,*] = g[image]
;;          image3D[2,*,*] = b[image]
;;          image3D = Reverse(Temporary(image3D), 3)
;;        ENDELSE
;;        Write_TIFF, filename, image3D, 1, _Extra=extra
;;      END
;;    ENDCASE
;;    RETURN, -1
;;  ENDIF
;;  
;;  ; Return the screen dump image.
;;  
;;  RETURN, image
;;END ;-------------------------------------------------------------------------------
