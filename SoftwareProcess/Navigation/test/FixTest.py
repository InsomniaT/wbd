'''
Created on Oct 11, 2016

@author: Insomnia_T
'''\

import unittest
import Navigation.prod.Fix as Fix
from cookielib import MISSING_FILENAME_TEXT

class FixTest(unittest.TestCase):
    def setUp(self):
        self.className = "Fix."



    # Acceptance Test: 100
    #     Analysis - Constructor
    #         inputs: logFile
    #             
    #         outputs: instance of Fix
    #             
    #         state change: Start of log
    #             
    #         Happy path: create log.txt
    #             
    #         Sad path:
    #             invalid file name
    #             cannot create new file or be appended
 


    def test100_010_ShouldCreateInstanceofFix(self):
        self.assertIsInstance(Fix.Fix(), Fix.Fix)
    def test100_020_ShouldCreateInstanceofFixWithAFile(self):
        self.assertIsInstance(Fix.Fix("log.txt"),Fix.Fix)
    def test100_030_ShouldWriteMessageToLogFile(self):
        aF = Fix.Fix("log.txt")
        with open(aF.logFile, "r") as f:
            lines = f.readlines()
            last_line = lines[-1]
            message = last_line.split(" ",3)[3]
        f.close()
        expectedLine = "Start of log\n"
        self.assertEquals(message, expectedLine)
# Sad path
    def test100_910_ShouldRaiseExceptionWithillegalFilename(self):
        expectedDiag = self.className + "__init__:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix(123)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test100_920_ShouldRaiseExceptionWithShortFileNameSize(self):
        expectedDiag = self.className + "__init__:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])


#     Acceptance Test: 200
#         Analysis - setSightingFile(sightingFile)
#            inputs:
#                     sightingFile:
#                         sightingFile contains ".xml" as the file extension  
#                         length of the file name should be .GE.1
#             outputs:
#                     A string having the value passed as the "sightingFile"
#             state change:
#                     Writes the following entry to the log file:
#                     Start of sighting file f.xml
#                     where f.xml is the actual name of the file
# 
#             Happy path:
#                     write "start of sighting file" in log.txt
#             Sad path:
#                     missing para:    setSighting()
#                     non-string filename:    setSightingFile(123.xml)
#                     small filename:    setSightingFile(".xml")
#                     missing file extension:    setSightingFile(sightingFile)
#                     non-integer x:  setDegreesAndMinutes("1.1d0.0")
#                     file failed:    
#                     can't write; append;  

    def test200_010_ShouldReturnString(self):
        aF = Fix.Fix()
        sightingFile = "sightingFile.xml"
        self.assertEquals(sightingFile, aF.setSightingFile(sightingFile))
    def test200_020_ShouldWriteMessageToLogFile(self):
        aF = Fix.Fix("log.txt")
        fileName = "sighting.xml"
        aF.setSightingFile(fileName)
        with open(aF.logFile, "r") as f:
            lines = f.readlines()
            last_line = lines[-1]
            message = last_line.split(" ",3)[3]
        f.close()
        expectedLine = "Start of sighting file " + fileName
        self.assertEquals(message, expectedLine) 
    def test200_910_ShouldRaisedExceptionWithNoParameter(self):
        expectedDiag = self.className + "setSightingFile:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test200_920_ShouldRaisedExceptionWithNoneStringParameter(self):
        expectedDiag = self.className + "setSightingFile:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile(123)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test200_930_ShouldRaisedExceptionWithParameterLengthLessThan1(self):
        expectedDiag = self.className + "setSightingFile:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test200_940_ShouldRaisedExceptionWithWrongFileExtension(self):
        expectedDiag = self.className + "setSightingFile:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("test.txt")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test200_950_ShouldRaisedExceptionWithoutFileExtension(self):
        expectedDiag = self.className + "setSightingFile:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("test")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    
  
    # AcceptTest 300
    #     Analysis - getSightings()
    #     input: 
    #             None
    #     output: 
    #             A tuple that contains latitude and longitude of the approximate location
    #     state change:
    #             Navigational calculations are written into the log file
    #     Happy Path:
    #             return correct value
    #             sighting_disordered.xml
    #             Missing sightings:    sighting_MissSightings.xml
    #             Missing Tag(default value):
    #                 Tag-Height:
    #                     default Value 0:    sighting_Hight_Miss.xml
    #                                         sighting_Hight_MissValue.xml
    #                 Tag-Temperature:
    #                     default Value 72    sighting_Temp_Missing.xml
    #                                         sighting_Temp_MissingValue.xml  
    #                 Tag-Pressure:
    #                     default value 1010  sighting_Pressure_Missing.xml
    #                                         sighting_Pressure_MissingValue.xml
    #                 Tag-Horizon:
    #                     default value natural  sighting_Horizon_Missing.xml
    #                                            sighting_Horizon_MissingValue.xml
    #             Ignore Not Listed Tag:
    #                     sighting_IgnoreTag.xml  
    #     Sad Path:
    #             sightingFile:   
    #                     Mandatory tag is missing   
    #                         body:   sighting_MissBody.xml
    #                         date:   sighting_MissDate.xml
    #                         time:   sighting_MissTime.xml
    #                         observation:    sighting_MissObservation.xml
    #                     Root is incorrect
    #                         sighting_RootsIsNotFix.xml
    #             Tag-Date:
    #                 sighting_Date_FalseFormat.xml
    #             Tag-Time:
    #                 sighting_Time_FalseFormat.xml
    #             Tag-Observation:
    #                 No degree:  sighting_Observation_NoDegree.xml
    #                 No minute:  sighting_Observation_NoMinute.xml
    #                 No separator:   sighting_Observation_NoD.xml
    #                 Multiple separator: sighting_Observation_MultipleD.xml
    #                 degree boundary .GE.0 and .LT. 90
    #                     degree boundary left:   sighting_Observation_DegreeBoundaryLeft.xml
    #                     degree boundary right:  sighting_Observation_DegreeBoundaryRight.xml
    #                 degree is not int:  sighting_Observation_DegreeFloat.xml
    #                 minute boundary .GE.0 and .LT. 60
    #                 minute boundary left:   sighting_Observation_MinuteBoundaryLeft.xml
    #                 minute boundary right:  sighting_Observation_MinuteBoundaryRight.xml
    #             Tag-Height: .GE. 0
    #                 height boundary left:   sighit_Height_BoundaryLeft.xml
    #                 not numeric:    sighting_Height_NotNumric.xml
    #             Tag-Temperature:    .GE.-20 and .LE. 120
    #                 temp boundary left:     sighting_Temp_BoundaryLeft.xml
    #                 temp boundary right:    sighting_Temp_BoundaryRight.xml
    #                 temp is not int:    sighting_Temp_NotInt.xml
    #             Tag-Pressure:   .GE. 100 and .LE. 1100
    #                 pressure boundary left: sighting_Pressure_BoudnaryLeft.xml
    #                 pressure boundary right:    sighting_Pressure_BoundaryRight.xml
    #                 pressure is not int:    sighting_Pressure_NotInt.xml
    #             Tag-Horizon:    Artificial or Nature    
    #                 horizon not (artificial and nature): sighting_Horizon_NotArtificial.xml



    def test300_030_ShouldReturnMissHeightTag(self):
        aF = Fix.Fix()
        aF.setSightingFile("sighting_Hight_Miss.xml")
        aF.getSightings()
        self.assertEquals(("0d0.0","0d0.0"), aF.getSightings())
    def test300_040_ShouldNotRaiseForMissHeightValue(self):
        aF = Fix.Fix()
        aF.setSightingFile("sighting_Hight_MissValue.xml")
        aF.getSightings()
        self.assertEquals(("0d0.0","0d0.0"), aF.getSightings())
    def test300_050_ShouldNotRaiseValueErrorForMissTemperatureTag(self):
        aF = Fix.Fix()
        aF.setSightingFile("sighting_Temp_Missing.xml")
        aF.getSightings()
        self.assertEquals(("0d0.0","0d0.0"), aF.getSightings())
    def test300_060_ShouldNotRaiseValueErrorForMissTemperatureValue(self):
        aF = Fix.Fix()
        aF.setSightingFile("sighting_Temp_MissingValue.xml")
        aF.getSightings()
        self.assertEquals(("0d0.0","0d0.0"), aF.getSightings())
    def test300_070_ShouldNotRaisedValueErrorForMissPressureTag(self):
        aF = Fix.Fix()
        aF.setSightingFile("sighting_Pressure_Missing.xml")
        aF.getSightings()
        self.assertEquals(("0d0.0","0d0.0"), aF.getSightings())
    def test300_080_ShouldNotRaisedValueErrorForMissPressureValue(self):
        aF = Fix.Fix()
        aF.setSightingFile("sighting_Pressure_MissingValue.xml")
        aF.getSightings()
        self.assertEquals(("0d0.0","0d0.0"), aF.getSightings())
    def test300_090_ShouldNotRaisedValueErrorForMissHorizonTag(self):
        aF = Fix.Fix()
        aF.setSightingFile("sighting_Horizon_Missing.xml")
        aF.getSightings()
        self.assertEquals(("0d0.0","0d0.0"), aF.getSightings())
    def test300_1000_ShouldNotRaisedValueErrorForMissHorizonValue(self):
        aF = Fix.Fix()
        aF.setSightingFile("sighting_Horizon_MissingValue.xml")
        aF.getSightings()
        self.assertEquals(("0d0.0","0d0.0"), aF.getSightings())
    def test300_910_ShouldRaiseValueErrorForWrongRootValue(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_RootIsNotFix.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_920_ShouldRaiseValueErrorForMissBody(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_MissBody.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_940_ShouldRaiseValueErrorForMissDate(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_MissDate.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_950_ShouldRaiseValueErrorForWrongFormatDate(self):    
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Date_FalseFormat.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_960_ShouldRaiseValueErrorForMissTime(self): 
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_MissTime.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_970_ShouldRaiseValueErrorForWrongFormatTime(self):    
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Time_FalseFormat.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_980_ShouldRaiseValueErrorMissingObservation(self): 
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_MissObservation.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_990_ShouldRaiseValueErrorMissingSeperator(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Observation_NoD.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_991_ShouldRaiseValueErrorForMultipleSeperator(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Observation_MultipleD.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_91000_ShouldRaiseValueErrorMissingDegree(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Observation_NoDegree.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_91100_ShouldRaiseValueErrorDegreeBoundaryLeft(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Observation_DegreeBoundaryLeft.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_91200_ShouldRaiseValueErrorDegreeBoundaryRight(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Observation_DegreeBoundaryRight.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_91210_ShouldRasieValueErrorFloatDegree(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Observation_DegreeFloat.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_91300_ShouldRaiseValueErrorMissMinute(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Observation_NoMinute.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_91400_ShouldRaiseValueErrorMinuteBoundaryLeft(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Observation_MinuteBoundaryLeft.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_91500_ShouldRaiseValueErrorMinuteBoundaryRight(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Observation_MinuteBoundaryRight.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    
    def test300_91800_ShouldRaiseValueErrorNotNumericHieght(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Hight_NotNumric.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])

    def test300_92100_ShouldRaiseValueErrorNotIntTemp(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Temp_NotInt.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
    def test300_92200_ShouldRaiseValueErrorNotIntPressure(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Pressure_NotInt.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_92300_ShouldRaiseValueErrorPressureBoundaryLeft(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Pressure_BoundaryLeft.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_92400_ShouldRaiseValueErrorPressureBoundaryRight(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Pressure_BoundaryRight.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test300_92500_ShouldRaiseValueErrorNotArtificialHorizon(self):
        expectedDiag = self.className + "getSightings:"
        with self.assertRaises(ValueError) as context:
            aF = Fix.Fix()
            aF.setSightingFile("sighting_Horizon_NotArtificial.xml")
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
