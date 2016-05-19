FUNCTION Qaa::getVersion

  return, '1.0'
  
END

PRO Qaa::setCode, value

  self.code=value
  
END

FUNCTION Qaa::getCode

  return, self.code
  
END

PRO Qaa::setDisplayname, value

  self.displayname=value
  
END

FUNCTION Qaa::getDescription

  return, self.description
  
END

PRO Qaa::setDescription, value

  self.description=value
  
END

FUNCTION Qaa::getS

  return, self.s
  
END

PRO Qaa::setS, value

  self.s=value
  
END

PRO Qaa::setZetaList, list

  ptr_free, self.setZetaList
  self.setZetaList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Qaa::getZetaList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.zetaList) then   return, *self.zetaList
  NOTFOUND=1
  return, [-1]
  
END

PRO Qaa::setYList, list

  ptr_free, self.yList
  self.yList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Qaa::getYList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.yList) then   return, *self.yList
  NOTFOUND=1
  return, [-1]
  
END

PRO Qaa::setA555List, list

  ptr_free, self.a555List
  self.a555List=ptr_new(list, /NO_COPY)
  
END

FUNCTION Qaa::getA555List, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.a555List) then   return, *self.a555List
  NOTFOUND=1
  return, [-1]
  
END

PRO Qaa::setA440List, list

  ptr_free, self.a440List
  self.a440List=ptr_new(list, /NO_COPY)
  
END

FUNCTION Qaa::getA440List, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.a440List) then   return, *self.a440List
  NOTFOUND=1
  return, [-1]
  
END

PRO Qaa::setGList, list

  ptr_free, self.gList
  self.gList=ptr_new(list, /NO_COPY)
  
END

FUNCTION Qaa::getGList, NOTFOUND=NOTFOUND

  NOTFOUND=0
  if ptr_valid(self.gList) then   return, *self.gList
  NOTFOUND=1
  return, [-1]
  
END

PRO Qaa::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  print, '**** code:', self->getcode()
  print, '**** displayname:', self->getdisplayname()
  print, '**** description:', self->getdescription()
  print, '**** s:', self->getdescription()
  print, '**** GList:', self->getGList()
  print, '**** A440List:', self->getA440List()
  print, '**** A555List:', self->getA555List()
  print, '**** YList:', self->getYList()
  print, '**** ZetaList:', self->getZetaList()
  
END

;*****************************
; constructor/destructor
;*****************************

PRO Qaa::cleanUp

  ptr_free, self.parametersList
  self->SimpleXML::cleanup
  
END

FUNCTION Qaa::init

  if not (self -> SimpleXML :: init(obj_class(self), [''])) then return, 0
  return, 1
  
END

PRO Qaa__Define

  Struct = { Qaa , $
    code: '', $
    displayName: '', $
    description: '', $
    gList: ptr_new(), $
    a440List: ptr_new(), $
    a555List: ptr_new(), $
    yList: ptr_new(), $
    zetaList: ptr_new(), $
    s: 0., $
    Inherits SimpleXML $
    }
    
END