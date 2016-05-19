FUNCTION doScatter

 serie1=obj_new('BaseStatisticsOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList) 
 serie2=obj_new('BaseStatisticsOperator', self, self.fileSystem->getTempDir(/WITH), fileName=fileName, OPEN=OPEN, COPY=COPY, bandToExportList=bandToExportList) 

END
