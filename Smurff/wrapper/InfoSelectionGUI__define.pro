FUNCTION InfoSelectionGUI::getInfo

 return, self.info

END

PRO InfoSelectionGUI::setInfo, info

 self.info=info

END

FUNCTION InfoSelectionGUI::getVisibleXSize

 return, self.dimensions[0]-self.xPad*2

END

FUNCTION  InfoSelectionGUI::getVisibleYSize

 return, self.dimensions[1]-self.yPad*2

END

PRO InfoSelectionGUI::configure

 message, 'You must implement this one on your child class!!!'

END

FUNCTION InfoSelectionGUI::getTitle

 ;return, 'Generic selection'
 return, self.mgr->getLangCatLabelFromKey('InfoGUI:MainTitle')

END

PRO InfoSelectionGUI::build

 message, 'You must implement this one on your child class!!!'

END

PRO InfoSelectionGUI::realize

 self->build
 self->GUI::realize
 self->configure

END

PRO InfoSelectionGUI::exitRequest

 ;doLog,'Destroy Info gui'
 obj_destroy, self.info
 obj_destroy, self

END

PRO InfoSelectionGUI::buildOKButton, base

 subBase=base
 okBtt=widget_button(subBase, value=self.mgr->getLangCatLabelFromKey('InfoGUI:OKButton'), UNAME='DISPLAYOK', $
	event_pro=self.eventprefix+'OKRequest', SCR_XSIZE=70, SCR_YSIZE=35, /ALIGN_CENTER)
 ;okButton = widget_button(mainButtonBase, Value='OK', UNAME='APPLY', $
 ; XSIZE=70, YSIZE=35, event_pro=self.eventPrefix+'okModeBtt', /ALIGN_CENTER)

END

PRO InfoSelectionGUI::OKRequest

 if self->checkIntegrity() then begin
	self.mgr->setBlockWindowControl, /OFF
	self->updateToCaller
	obj_destroy, self
 endif else begin
  doLog,'Bad ', obj_class(self), level=2
	;exitMessage=['Something wrong in your selection', 'Check data']
	;dlgResult=dialog_message(exitMessage, dialog_parent=self->getTopBase(), Title='Warning')
 endelse

END

FUNCTION InfoSelectionGUI::checkIntegrity

 message, 'You must implement this one on your child class!!!'
 return, 0

END
FUNCTION InfoSelectionGUI::getSmallLabelXDim

 return, self.dimensions[0]-20

END

FUNCTION InfoSelectionGUI::getSmallLabelYDim

 return, 20

END

FUNCTION InfoSelectionGUI::getSubTitleYDim

 return, 20

END

FUNCTION InfoSelectionGUI::getValueXDim

 return, (self->xSummarySize()-12)*.68

END

FUNCTION InfoSelectionGUI::getValueYDim

 return, 20

END

FUNCTION InfoSelectionGUI::getSubTitleXDim

 return, (self->xSummarySize()-12)-self->getValueXDim()

END

FUNCTION InfoSelectionGUI::getTitleYDim

 return, 20

END

FUNCTION InfoSelectionGUI::getTitleXDim

 return, self.dimensions[0]/3

END

FUNCTION InfoSelectionGUI::xSummarySize


 return, self.dimensions[0]-self->xGraphSize()

END

FUNCTION InfoSelectionGUI::ySummarySize

 return, self.dimensions[1]*1.

END

FUNCTION InfoSelectionGUI::xGraphSize

 return, self.dimensions[0]*.7

END

FUNCTION InfoSelectionGUI::yGraphSize

 return, self.dimensions[1]*.7

END

FUNCTION InfoSelectionGUI::xInfoGraphSize

 return, self->xGraphSize()

END

FUNCTION InfoSelectionGUI::yInfoGraphSize

 return, self.dimensions[1]-self->yGraphSize()

END

FUNCTION InfoSelectionGUI::getBestDimensions

 return, get_screen_size(RESOLUTION=resolution)

END

PRO InfoSelectionGUI::updateToCaller, info

 message, 'You must implement this one on your child class!!!'
 ;self.info->Clone(/DEEP)
 ; this must be implemented one level up
 ;self.mgr->UpdateXXX, self.info

END

FUNCTION InfoSelectionGUI::init, info, mgr, fonts=fonts

 if not self -> GUI :: init(mgr, fonts=fonts) then return , 0
 ;self.mgr->disable
 dims=self->getBestDimensions()
 dims[0]=dims[0]*.65
 dims[1]=dims[1]*.50
 self.dimensions=dims
 self.yPad=2
 self.xPad=2
 self.info=info
 return , 1

END

PRO InfoSelectionGUI::cleanUp

 self.mgr->enable
 obj_destroy, self.info
 self -> GUI::cleanUp

END

;****************************************************************************************

PRO InfoSelectionGUI__Define

Struct = { InfoSelectionGUI , $
		info: obj_new(), $
		dimensions: lonarr(2), $
		yPad: 0, $
		xPad: 0, $
		Inherits GUI $
		 }

END

;****************************************************************************************

