FUNCTION createShift, seriesNo, xDataNumber, minX, maxX, percentage

  extension=maxX-minX
  extension=extension/xDataNumber
  extension=extension*float(percentage)/100
  extensionsStart=-(extension/2)
  extensions=extensionsStart+findgen(seriesNo)*extension/(seriesNo-1)
  return, extensions
  
END
