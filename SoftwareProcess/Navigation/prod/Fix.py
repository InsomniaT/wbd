from genericpath import isfile
from datetime import datetime
import xml.etree.ElementTree as ET
import re
import math
from math import sqrt
from operator import itemgetter
import os as os

class Fix():
    def __init__(self,logFile = 'log.txt'):
        self.logFile = logFile
        self.sightingfile = None
        
        if not isinstance(logFile, str):
            raise ValueError('Fix.__init__: "Logfile" is not a string')
        if (len(logFile) < 1):
            raise ValueError('Fix.__init__: "Logfile" should have a length >= 1')
        if isfile(logFile):
            try:
                with open(self.logFile,'a') as f:
                    time = self.date() + 'Start of log' +"\n"
                    f.write(time)
            except:
                raise ValueError('Fix.__init__: logFile cannot be opened for appending')
        else:
            try:
                with open(self.logFile,'w') as f:
                    time = self.date() + 'Start of log' + "\n"
                    f.write(time)
            except:
                raise ValueError('Fix.__init__: logFile cannot be created')
    
    def date(self):
        now = str(datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
        utc = '-06:00 '
        time = 'LOG:' + now + utc + " "
        return time
        
    def setSightingFile(self,sightingFile = None):
        self.sightingFile = sightingFile
        if sightingFile == None:
            raise ValueError('Fix.setSightingFile: File not existed')
        if not isinstance(sightingFile, str):
            raise ValueError('Fix.setSightingFile: "sightingfile" is not a string')
        if (len(sightingFile) < 1):
            raise ValueError('Fix.setSightingFile: the file name should have a length >= 1')
        if not ((os.path.splitext(self.sightingFile)[1]) == ".xml"):
            raise ValueError("Fix.setSightingFile: the extension of the file is illegal") 
        if isfile(sightingFile):
            try:
                with open(self.logFile,'a') as f:
                    time = self.date() + 'Start of sighting file ' + str(sightingFile)
                    f.write(time)
            except:
                raise ValueError('Fix.setSightingFile: logFile cannot be opened for appending')
        else:
            try:
                with open(self.logFile,'w') as f:
                    time = self.date() + 'Start of log'
                    f.write(time)
            except:
                raise ValueError('Fix.setSightingFile: logFile cannot be created')
            
        self.sightingFile = sightingFile
        return sightingFile
    
    
    
    
    def getSightings(self):
    ###____init_____    
        fix = 'fix'
        sightingTag = 'sighting'
        bodyTag = 'body'
        dateTag = 'date'
        timeTag = 'time'
        observationTag = 'observation'
        heightTag = 'height'
        tempTag = 'temperature'
        pressureTag = 'pressure'
        horizonTag = 'horizon'
        
        tree = ET.parse(self.sightingFile)
        root = tree.getroot()
        if not (root.tag == fix):
            raise ValueError("Fix.getSightings: root should be 'fix'")
        else:
            sightingTuples = []
            for sighting in (root.findall(sightingTag)):
                if (sighting is None):
                    print ("Fix.getSightings:  No more sightings")
                else:
#body               
                    body = sighting.find(bodyTag)
                    if (body == None):
                        raise ValueError('Fix.getSightings: missing body')
                    bodyValue = body.text
                    if not isinstance(bodyValue, str):
                        raise ValueError("Fix.getSightings: 'body' should be a string")
#date               
                    date = sighting.find(dateTag)
                    if (date == None):
                        raise ValueError('Fix.getSightings: missing date')
                    dateValue = date.text
#                 if re.match(r'^\d{4}\-\d{2}\-\d{2}$', dateValue) == None:
#                     raise ValueError('date format incorrect')
                    try:
                        datetime.strptime(dateValue, "%Y-%m-%d")
                    except:
                        raise ValueError('Fix.getSightings: date format incorrect')
#time
                    time = sighting.find(timeTag)
                    if (time == None):
                        raise ValueError('Fix.getSightings: missing date')
                    timeValue = time.text
#                     if re.match(r'^\d{4}\-\d{2}\-\d{2}$', dateValue) == None:
#                     raise ValueError('date format incorrect')
                    try:
                        datetime.strptime(timeValue, "%H:%M:%S")
                    except:
                        raise ValueError('Fix.getSightings: Time format incorrect')
# #                 if re.match(r'^\d{2}\-\d{2}\-\d{2}$', timeValue) == None:
# #                     raise ValueError('Time format incorrect')
                    observation = sighting.find(observationTag)
                    if (observation == None):
                        raise ValueError('Fix.getSightings: missing observation')
                    observationValue = observation.text
                    if observationValue.find("d") == -1:     
                        raise ValueError('Fix.getSightings: seperator is missing')
                    observationValueList = observationValue.split("d",1)
                    degree = observationValueList[0]
                    minute = observationValueList[1]
                    if (degree == ""):
                        raise ValueError('Fix.getSightings: degree is empty')
                    try:
                        degree = int(degree)
                    except:
                        raise ValueError('Fix.getSightings: degree is not an integer')
                    if (degree < 0 ) or (degree >= 90):
                        raise ValueError('Fix.getSightings: degree should be GE.0 and LT.90')
                    
                    if (minute == None):
                        raise ValueError('Fix.getSightings: minute is missing')
                    observationValue = observation.text
                    
                    minuteStd = re.match("\d+\.?\d?\Z",minute)
                    if minuteStd:
                        try:
                            minute = float(minute)
                        except:
                            raise ValueError('Fix.getSightings: minute is not a floating number')
                        if (minute < 0.0) or (minute >= 60.0):
                            raise ValueError('Fix.getSightings: minute should be GE.0.0 and LT.60.0')
                    else:
                        raise ValueError('Fix.getSightings: minute should be a floating number with only one digit to the right of the decimal point')
                    
#height            
                    height = sighting.find(heightTag)
                    if (height is None):
                        heightValue = 0
                    else:
                        heightValue = height.text
                        if(heightValue is None):
                            heightValue = 0
                        try:
                            heightValue = float(heightValue)
                        except:
                            raise ValueError('Fix.getSightings: height should be a numeric number')
                        if not heightValue >= 0:
                            raise ValueError('Fix.getSightings: height should be greater than 0')
                        
                    
#temp     
                    temperature = sighting.find(tempTag)
                    if (temperature is None):
                        temperatureValue = 72
                    else:
                        temperatureValue = temperature.text
                        if (temperatureValue is None):
                            temperatureValue = 72
                        try:
                            temperatureValue = int(temperatureValue)
                        except:
                            raise ValueError('Fix.getSightings: temperature should be an integer')
                        if (temperatureValue < -20 ) or (temperatureValue > 120):
                            raise ValueError('Fix.getSightings: temperature should be GE -20 and LE 120')
                    
#pressure    
                    pressure = sighting.find(pressureTag)
                    if (pressure is None):
                        pressureValue = 1010
                    else:
                        pressureValue = pressure.text
                        if (pressureValue is None):
                            pressureValue = 1010
                        try:
                            pressureValue = int(pressureValue)
                        except:
                            raise ValueError('Fix.getSightings: pressure should be an integer')
                        if (pressureValue < 100) or (pressureValue > 1100):
                            raise ValueError('Fix.getSightings: pressure should be GE 100 and LE 1100')
                    
#horizon     
                    horizon = sighting.find(horizonTag)
                    Natural = "Natural"
                    Artificial = "Artificial"
                    if (horizon is None):
                        horizonValue = "Natural"
                    else:
                        horizonValue = horizon.text
    
                        if (horizonValue is None):
                            horizonValue = "Natural"
                        NaturalFlag = bool(re.search(Natural,horizonValue,re.I))
                        ArtificialFlag = bool(re.search(Artificial,horizonValue,re.I))
                        if (ArtificialFlag == False) and (NaturalFlag == False): 
                            raise ValueError ('Fix.getSightings: horizon should be either Artificial or Natural')
                    
                    if (horizonValue == Natural):
                        dip = (-0.97 * sqrt(heightValue)) / 60
                    else:
                        dip = 0
                    celsuiusTemp = (temperatureValue - 32) / 1.8
                    observedAltitude = degree + (minute / 60)
                    refraction = (-0.00452 * pressureValue) / (273 + celsuiusTemp) / math.tan(observedAltitude * math.pi / 180)
                    adjustedAltitude = observedAltitude + dip + refraction
                    degrees = int(adjustedAltitude)
                    minutes = round((adjustedAltitude - degree)*60, 1)
                    altitude = str(degrees) + "d" + str(minutes)
                    sightingStd = (bodyValue, dateValue, timeValue, altitude)
                    sightingTuples.append(sightingStd)
                sightingTuples = sorted(sightingTuples, key = itemgetter(1,2,0))
                getSightingsMessage = "End of sighting " + self.sightingFile 
                for s in sightingTuples:
                    message = self.read(s)
                if os.path.getsize(self.logFile) == 0:
                    f = open(self.logFile,"w")
                    try:
                        f.write(self.date()+ message)
                    except:
                        raise ValueError("Fix.getSightings: can't write to file")
                    finally:
                        f.close()
#append
                else:
                    f = open(self.logFile,"a+")
                    try:
                        f.write("\n")
                        f.write(self.date() + message)
                    except:
                        raise ValueError("Fix.setSightingFile:    can't append to file")
                    finally:
                        f.close()
            
            f = open(self.logFile,"a+")
            try:
                f.write("\n")
                f.write(self.date() + getSightingsMessage)
            except:
                raise ValueError ("Fix.setSightingFile:    can't append to file")
            finally:
                f.close()
                            
            approximateLatitude = "0d0.0"             
            approximateLongitude = "0d0.0"
        return(approximateLatitude,approximateLongitude)
    
    def read(self,tuples):
        message = ""
        for item in tuples:
            message = message + str(item)+"\t"
        return message.rstrip()
        