pro writeColorTable, file, image, r,g,b, backGroundColorIndex, labelColorIndex, backGroundColorRGB=backGroundColorRGB, labelColorRGB=labelColorRGB

  oldBck=[r[backGroundColorIndex], g[backGroundColorIndex],b[backGroundColorIndex]]
  r[backGroundColorIndex]=backGroundColorRGB[0]
  g[backGroundColorIndex]=backGroundColorRGB[1]
  b[backGroundColorIndex]=backGroundColorRGB[2]
  
  oldLab=[r[labelColorIndex], g[labelColorIndex],b[labelColorIndex]]
  r[labelColorIndex]=labelColorRGB[0]
  g[labelColorIndex]=labelColorRGB[1]
  b[labelColorIndex]=labelColorRGB[2]
  
  write_png,file,image,r,g,b
  doLog, 'write image: '+file, LEVEL=4
  r[backGroundColorIndex]=oldBck[0]
  g[backGroundColorIndex]=oldBck[1]
  b[backGroundColorIndex]=oldBck[2]
  
  r[labelColorIndex]=oldBck[0]
  g[labelColorIndex]=oldBck[1]
  b[labelColorIndex]=oldBck[2]
  
END