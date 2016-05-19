FUNCTION GUI::SplitStringInArray, originalString

  rows=strsplit(originalString, ' ', /EXTRACT, /PRESERVE)
  if n_elements(rows) gt 1 then begin
    firstLen=0 & secondLen=0
    firstRow='' & secondRow='' 
    for i=0, (n_elements(rows)/2)-1 do begin
      firstLen=firstLen+strlen(rows[i])
      firstRow=firstRow+rows[i]
    endfor
    for i=(n_elements(rows)/2), n_elements(rows)-1 do begin
      secondLen=secondLen+strlen(rows[i])
      secondRow=secondRow+rows[i]
    endfor
    ;return, [firstRow, secondRow]
    ;return, firstRow+self.mgr->getReturnCharacter()+secondRow
    return, originalString
  endif
  return, originalString
  
  
END

FUNCTION GUI::getButtonLabelFont

  return, self.mgr->getButtonLabelFont()
  
END

FUNCTION GUI::GetStandardTextFont

  return, self.mgr->GetStandardTextFont()
  
END

FUNCTION GUI::getButtonMediumFont

  return, self.mgr->getButtonMediumFont()
  
END

FUNCTION GUI::getTitle

  message, 'You must implement this one on your child class!!!'
  return, ''
  
END

FUNCTION GUI::getMainButtonDimension, RESOLUTION=RESOLUTION

  maxDim=70
  return, [maxDim, maxDim/1.618]
  
END

FUNCTION GUI::getMainBigButtonDimension, RESOLUTION=RESOLUTION

  maxDim=125
  return, [maxDim, maxDim/1.618]
  
END

FUNCTION GUI::getBestDimensions, RESOLUTION=RESOLUTION

  return, get_screen_size(RESOLUTION=resolution)*0.85
  
END

FUNCTION GUI::dialogMessage, textMessage, title=title, INFORMATION=INFORMATION, error=error, question=question, WARNING=WARNING

  return, dialog_message(textMessage, dialog_parent=self.topBase, title=title, INFORMATION=INFORMATION, ERROR=ERROR, QUESTION=QUESTION)
  
END

PRO GUI::moveToCenterScreen

  screendimension = get_screen_size()
  geom = widget_info(self.topBase, /GEOMETRY)
  xoffset = (screendimension[0] - geom.scr_xsize)/2
  yoffset = (screendimension[1] - geom.scr_ysize)/2
  widget_control, self.topBase, XOFFSET=xoffset ,YOFFSET=yoffset
  
END

PRO GUI::exitRequest

  self.mgr->exitRequest
  
END

PRO GUI::destroy

  widget_control, self.topBase, /DESTROY
  
END

PRO GUI::hide

  widget_control, self.topBase, MAP=0
  
END

PRO GUI::show

  widget_control, self.topBase, /MAP
  
END

PRO GUI::disable

  widget_control, self.topBase, SENSITIVE=0
  
END

PRO GUI::enable

  widget_control, self.topBase, /SENSITIVE
  
END

PRO GUI::realize

  self->moveToCenterScreen
  if widget_info(self.topBase, /VALID_ID) then widget_control, self.topBase, /REALIZE
;xmanager
  
END

PRO GUI::setTopBase, id

  self.topBase=id
  
END

FUNCTION GUI::getTopBase

  return, self.topBase
  
END


FUNCTION GUI::init, mgr, fonts=fonts

  if not self -> Object :: init() then return , 0
  
  if obj_valid(mgr) then self.mgr=mgr
  if n_elements(fonts) ne 3 then begin
    self.titleFont=self.mgr->getTitleTextFont()
    self.labelFont=self.mgr->getStandardLabelFont()
    self.textFont=self.mgr->getStandardTextFont()
  endif else begin
    self.titleFont=fonts[0]
    self.labelFont=fonts[1]
    self.textFont=fonts[2]
  endelse
  self.eventPrefix='oxyRisk_'
  
  return , 1
  
END

PRO GUI::cleanUp

  self.mgr=obj_new()
  widget_control, self.topBase, /DESTROY
  self -> Object::cleanUp
  
END

;****************************************************************************************

PRO GUI__Define

  Struct = { GUI , $
    mgr: obj_new(), $
    topBase: 0l, $
    titleFont: '', $
    labelFont: '', $
    textFont: '', $
    eventPrefix: '', $
    Inherits Object $
    }
    
END

;****************************************************************************************

