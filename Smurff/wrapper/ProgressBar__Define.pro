;********************
@structure_definition
;********************
;*****************************************************************************************************
PRO PROGRESSBAR_Cleanup, tlb

  Widget_Control, tlb, Get_UValue=self
  Obj_Destroy, self
  
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR_Error_Message
;
; PURPOSE:
;
;       This function is the standard ERROR_MESSAGE error handling functionality.
;
;*****************************************************************************************************
FUNCTION PROGRESSBAR_Error_Message, theMessage, Error=error, Informational=information, $
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


PRO PROGRESSBAR_Event, event

  ; This is the event handler for the program. It simply sets the CANCEL
  ; flag if this is a button event.

  Widget_Control, event.top, Get_UValue=self
  thisEvent = Tag_Names(event, /Structure_Name)
  IF thisEvent EQ 'WIDGET_BUTTON' THEN self -> SetProperty, Cancel=1
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::CheckButton
;
; PURPOSE:
;
;       Returns a 1 if a button was selected. Returns 0 othewise.
;
; SYNTAX:
;
;       check = progressbar -> CheckButton()
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
FUNCTION PROGRESSBAR::CheckButton, ACCEPT=accept, CANCEL=cancel

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
;       PROGRESSBAR::CheckCancel
;
; PURPOSE:
;
;       Returns a 1 if the user selected the CANCEL button. Returns 0 othewise.
;
; SYNTAX:
;
;       check = progressbar -> CheckCancel()
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
FUNCTION PROGRESSBAR::CheckCancel

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
;       PROGRESSBAR::Destroy
;
; PURPOSE:
;
;       Destroys both the widget hierarchy and the object.
;
; SYNTAX:
;
;      progressbar -> Destroy
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
PRO PROGRESSBAR::Destroy

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
;       PROGRESSBAR::GETPROPERTY
;
; PURPOSE:
;
;       Allows user to get various progress bar properties.
;
; SYNTAX:
;
;       progressbar -> GetProperty, Color=currentColor
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
PRO PROGRESSBAR::GetProperty, COLOR=color, FAST_LOOP=fast_loop, text2=text, text1=text1

  ; Error handling.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = ProgressBar_Error_Message(Traceback=1)
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
;       PROGRESSBAR::SETPROPERTY
;
; PURPOSE:
;
;       Allows user to set various progress bar properties.
;
; SYNTAX:
;
;       progressbar -> SetProperty, Color='yellow'
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
PRO PROGRESSBAR::SetProperty, CANCEL=cancel, COLOR=color, FAST_LOOP=fast_loop, text2=text, TITLE=title, text1=text1

  ; Error handling.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = ProgressBar_Error_Message(Traceback=1)
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
;       PROGRESSBAR::START
;
; PURPOSE:
;
;       Puts the progress bar on the display.
;
; SYNTAX:
;
;       progressbar -> Start
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
PRO PROGRESSBAR::Start, initialPercent

  ; Error handling.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = ProgressBar_Error_Message(Traceback=1)
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
  
  Widget_Control, self.drawID, Get_Value=wid
  self.wid = wid
  
  ; Back to the open display window.
  
  IF thisWindow GE 0 THEN WSet, thisWindow
  
  ; Do you need a starting update?
  
  IF N_Elements(initialPercent) NE 0 THEN self -> Update, initialPercent
  
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::Update
;
; PURPOSE:
;
;       Updates the progress bar
;
; SYNTAX:
;
;       progressbar -> Update, percent
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
PRO PROGRESSBAR::Update, percent, text2=theText, text1=text1
  ; This method updates the display. PERCENT should be a value between 0 and 100.
  ; The text will be substituted for the message text.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    
    ; Catch a WSET error silently.
    
    IF !Error_State.Code EQ -386 THEN RETURN
    ok = Progressbar_Error_Message(Traceback=1)
    RETURN
  ENDIF
  
  percent = 0 > percent < 100
  
  ; Update the progress box.
  
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
  
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::CLEANUP
;
; PURPOSE:
;
;       Nothing to do in this cleanup routine.
;
;*****************************************************************************************************
PRO PROGRESSBAR::CLEANUP
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::INIT
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
FUNCTION PROGRESSBAR::INIT, $
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
    YSIZE=ysize                  ; The Y size of the progress bar.
    
  ; Error handling.
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = ProgressBar_Error_Message(Traceback=1)
    RETURN, 0
  ENDIF
  
  ; Check keywords.
  
  IF N_Elements(color) EQ 0 THEN self.color = "red" ELSE self.color = color
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
  self.drawID = Widget_Draw(self.tlb, XSize=self.xsize, YSize=self.ysize)
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
    Device, Get_Screen_Size=screenSize
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
;       PROGRESSBAR CLASS DEFINITION
;
; PURPOSE:
;
;       This is the PROGRESSBAR object's structure definition code.
;
;*****************************************************************************************************
PRO PROGRESSBAR__DEFINE

  struct = { PROGRESSBAR, $      ; The object class name.
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
    tlb: 0L, $          ; The identifier of the top-level base.
    wid: 0L, $          ; The window index number of the draw widget.
    xsize: 0L, $        ; The XSize of the progress bar.
    ysize: 0L $         ; The YSize of the progress bar.
    }
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR EXAMPLE PROGRAM
;
; PURPOSE:
;
;       This is the PROGRESSBAR example program that demonstrates how to use the progress bar.
;
;*****************************************************************************************************
;PRO Progressbar_Example_Event, event
;
; Respond to program button events.
;
;Widget_Control, event.id, Get_Value=buttonValue
;
;CASE buttonValue OF
;
;   'Start Loop (Normal)': BEGIN
;
;          Create the progress bar.
;
;      progressbar = Obj_New('progressbar', Color='red', Text='Loop Iteration 0')
;
;          Place the progress bar on the display.
;
;      progressbar -> Start
;
;          Start the loop.
;
;      count = 0
;      FOR j=0, 1000 DO BEGIN
;
;          IF j MOD 100 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.
;
;                Did the user try to cancel the progress bar?
;
;            IF progressbar->CheckCancel() THEN BEGIN
;               ok = Dialog_Message('User cancelled operation.') ; Other cleanup, etc. here.
;               progressbar -> Destroy ; Destroy the progress bar.
;               RETURN
;            ENDIF
;
;                If user didn't cancel, update the progress bar. Update value
;                must be between 0 and 100.
;
;            progressbar -> Update, (count * 10.0), Text='Loop Iteration ' + StrTrim(j,2)
;            count = count + 1
;          ENDIF
;
;          Wait, 0.01 ; This is where you would do something useful.
;      ENDFOR
;
;          Destroy the progress bar when you are finished with it.
;
;      progressbar -> Destroy
;      ENDCASE
;
;   'Start Loop (Accept)': BEGIN
;
;          Create the progress bar.
;
;      progressbar = Obj_New('progressbar', Color='red', Text='Loop Iteration 0', /Accept)
;
;          Place the progress bar on the display.
;
;      progressbar -> Start
;
;          Start the loop.
;
;      count = 0
;      FOR j=0, 1000 DO BEGIN
;
;          IF j MOD 100 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.
;
;                Did the user try to cancel the progress bar or did the user Accept?
;
;            IF progressBar -> CheckButton(Accept=acceptButton) THEN BEGIN
;
;               IF acceptButton THEN BEGIN
;
;                  progressbar -> Update, (count * 10.0), Text='Loop Iteration ' + StrTrim(j,2)
;                  ok = Dialog_Message('Final loop count is: '+ StrTrim(j,2))
;                  BREAK
;
;               ENDIF ELSE BEGIN
;
;               ok = Dialog_Message('User cancelled operation.') ; Other cleanup, etc. here.
;               progressbar -> Destroy ; Destroy the progress bar.
;               RETURN
;               ENDELSE
;
;            ENDIF
;
;                If user didn't cancel, update the progress bar. Update value
;                must be between 0 and 100.
;
;            progressbar -> Update, (count * 10.0), Text='Loop Iteration ' + StrTrim(j,2)
;            count = count + 1
;          ENDIF
;
;          Wait, 0.01 ; This is where you would do something useful.
;      ENDFOR
;
;          Destroy the progress bar when you are finished with it.
;
;      progressbar -> Destroy
;      ENDCASE
;
;   'Quit': Widget_Control, event.top, /Destroy
;
;ENDCASE
;
;END


;PRO Progressbar_Example
;DEVICE,DECOMPOSE=0
;LOADCT,39
;tlb = Widget_Base(Column=1, Xoffset=200, Yoffset=200)
;button = Widget_Button(tlb, Value='Start Loop (Normal)')
;button = Widget_Button(tlb, Value='Start Loop (Accept)')
;quiter = Widget_Button(tlb, Value='Quit')
;Widget_Control, tlb, /Realize
;XManager, 'progressbar_example', tlb, /No_Block
;END


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