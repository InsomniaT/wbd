import os as os
import time
import datetime
import xml.etree.ElementTree as ET
import re
import math
from math import sqrt
from operator import itemgetter
from genericpath import isfile

class Fix():
    def __init__(self,logFile = 'log.txt'):
        self.logFile = logFile
        self.sightingfile = None
        self.ariesFile = None
        self.starFile = None
        
        if not isinstance(logFile, str):
            raise ValueError('Fix.__init__: "Logfile" is not a string')
        if (len(logFile) < 1):
            raise ValueError('Fix.__init__: "Logfile" should have a length >= 1')
        if isfile(logFile):
            try:
                with open(self.logFile,'a') as f:
                    time = self.date() + 'Log file:' + "\t" + os.path.abspath(self.logFile) + "\n"
                    f.write(time)
            except:
                raise ValueError('Fix.__init__: logFile cannot be opened for appending')
        else:
            try:
                with open(self.logFile,'w') as f:
                    time = self.date() + 'Log file:' + "\t" + os.path.abspath(self.logFile) + "\n"
                    f.write(time)
            except:
                raise ValueError('Fix.__init__: logFile cannot be created')
        f.close()
        self.sightingError = 0
        
    def date(self):
        now = str(datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
        utc = '-06:00 '
        time = 'LOG:\t' + now + utc + " "
        return time
        
    def setSightingFile(self,sightingFile = None):
        if (sightingFile is None):
            raise ValueError("Fix.setSightingFile:    missing parameter")
        self.sightingFile = sightingFile
        if not isinstance(self.sightingFile, str):
            raise ValueError("Fix.setSightingFile:    filename is not a string")
        if len(self.sightingFile) < 5:
            raise ValueError("Fix.setSightingFile:    length of the filename should be greater than 1")
        if ((self.sightingFile.split(".",1)[-1] != "xml")):
            raise ValueError("Fix.setSightingFile:    the extension of the file is illegal")
        try:
            f = open(self.sightingFile,"r")
            f.close()
        except:
            raise ValueError("Fix.setSightingFile:    can't open file")
        time = self.date() + 'Sighting file:' + '\t' + os.path.abspath(self.sightingFile) + "\n"
        try:
            f = open(self.logFile,"a+")
            f.write(time)
            f.close()
        except:
            raise ValueError("Fix.setSightingFile:    can't append to file")
        return os.path.abspath(self.sightingFile)   
    
    def read(self,tuples):
        message = ""
        for item in tuples:
            message = message + str(item)+"\t"
        return message.rstrip()    
    
    def setAriesFile(self,ariesFile = None):
        if (ariesFile is None):
            raise ValueError("Fix.setAriesFile:    missing parameter")
        self.ariesFile = ariesFile
        if not isinstance(self.ariesFile, str):
            raise ValueError("Fix.setAriesFile:    filename is not a string")
        if len(self.ariesFile) < 5:
            raise ValueError("Fix.setAriesFile:    length of the filename should be greater than 1")
        if ((self.ariesFile.split(".",1)[-1] != "txt")):
            raise ValueError("Fix.setAriesFile:    the extension of the file is illegal")
        try:
            f = open(self.ariesFile,"r")
            f.close()
        except:
            raise ValueError("Fix.setAriesFile:    can't open file")
        time = self.date() + 'Aries file:' + '\t' + os.path.abspath(self.ariesFile) + "\n"
        try:
            f = open(self.logFile,"a+")
            f.write(time)
            f.close()
        except:
            raise ValueError("Fix.setAriesFile:    can't append to file")
        return os.path.abspath(self.ariesFile) 
    
    
    def getAries(self, ariesFile, dateValue, hourValue):
        f = open(ariesFile, "r")
        ariesEntry = sorted(f,key = itemgetter(0,1))
        list1 = []
        n = -1
        for index,line in enumerate(ariesEntry):
            lineList = line.split("\t",2)
            date = lineList[0]
            hour = lineList[1]
            if (date == dateValue) and (int(hour) == hourValue):
                try:
                    datetime.datetime.strptime(date, "%m/%d/%y")
                except:    
                    self.sightingError = self.sightingError + 1
                    continue
                try:
                    datetime.datetime.strptime(hour, "%H")
                except:
                    self.sightingError = self.sightingError + 1
                    continue            
                list1.append(line)
                n = index
        f.close()
        if (not len(list1) == 1):
            return []
        try:
            GHA_Next = ariesEntry[n + 1]
        except:
            return []
        return [list1[0], GHA_Next]  
        
    def setStarFile(self,starFile = None):
        if (starFile is None):
            raise ValueError("Fix.setStarFile:    missing parameter")
        self.starFile = starFile
        if not isinstance(self.starFile, str):
            raise ValueError("Fix.setStarFile:    filename is not a string")
        if len(self.starFile) < 5:
            raise ValueError("Fix.setStarFile:    length of the filename should be greater than 1")
        if ((self.starFile.split(".",1)[-1] != "txt")):
            raise ValueError("Fix.setStarFile:    the extension of the file is illegal")
        try:
            f = open(self.starFile,"r")
            f.close()
        except:
            raise ValueError("Fix.setStarFile:    can't open file")
        time = self.date() + 'Star file:' + '\t' + os.path.abspath(self.starFile) + "\n"
        try:
            f = open(self.logFile,"a+")
            f.write(time)
            f.close()
        except:
            raise ValueError("Fix.setAriesFile:    can't append to file")
        return os.path.abspath(self.starFile)       

    def getStar(self, starFile, bodyValue, dateTag):
        f = open (starFile, "r")
        lines = f.readlines()
        starEntry = []
        for line in lines:
            lineList = line.split("\t", 3)
            body = lineList[0]
            date = lineList[1]
            latitude = lineList[3]
            if (body == bodyValue) and (date <= dateTag):
                try:
                    body = str(body)
                except:
#                     raise ValueError("Fix.setStarFile:   invalid body")
                    self.sightingError = self.sightingError + 1
                    continue
                try:
                    datetime.datetime.strptime(date, "%m/%d/%y")
                except:    
                    #raise ValueError("Fix.setStars:    invalid date time format")
                    self.sightingError = self.sightingError + 1
                    continue
                try:
                    degreeList = latitude.split("d",1)
                    degree = degreeList[0]
                    minute = degreeList[1]
                except:
                    #raise ValueError("Fix.setStars:    invalid degree format")
                    self.sightingError = self.sightingError + 1
                    continue
                try:
                    degree = int(degree)
                except:
                    self.sightingError = self.sightingError + 1
                if (degree <= -90) or (degree >= 90):
                    #raise ValueError("Fix.setStars:    invalid degree value")
                    self.sightingError = self.sightingError + 1
                    continue
                try:
                    minute = float(minute)
                except:
                    self.sightingError = self.sightingError + 1
                if (minute < 0 ) or (minute >= 60):
#                    raise ValueError("Fix.setStars:    invalid minute value")
                    self.sightingError = self.sightingError + 1
                    continue
                starEntry.append(line)

        f.close()
        if (not starEntry ==[]):
            sortedEntry = sorted(starEntry, key = itemgetter(1))[-1].split("\t")
            SHA_star = sortedEntry[-2]
            try:
                SHA_degreeList = SHA_star.split("d",1)
            except:
#                 raise ValueError("Fix.setStars:    invalid degree format")
                self.sightingError = self.sightingError + 1
            try:
                SHA_degree = int(SHA_degreeList[0])
            except:
                self.sightingError = self.sightingError + 1
            if (SHA_degree < 0) or (SHA_degree >= 360):
#                 raise ValueError("Fix.setStars:    invalid degree value")
                self.sightingError = self.sightingError + 1
            try:
                SHA_minute = float(SHA_degreeList[1])
            except:
                self.sightingError = self.sightingError + 1
            if (SHA_minute < 0 ) or (SHA_minute >= 60):
#                 raise ValueError("Fix.setStars:    invalid minute value")
                self.sightingError = self.sightingError + 1
            SHA_minute = float(SHA_minute / 60)
            SHA_star = (SHA_degree + SHA_minute)%360
            latitude = sortedEntry[-1]
            return [SHA_star,latitude]
        else:
            return []

#################CA05     
    def getSightings(self, assumedLatitude='0d0.0', assumedLongitude='0d0.0'):
        if not isinstance(assumedLatitude,str):
            raise ValueError("Fix.getSightings:    parameter is not a string")
        self.assumedLatitude = assumedLatitude.rstrip().strip()
        self.assumedLongitude = assumedLongitude.rstrip().strip()
        hValue = assumedLatitude[0]
        if (hValue == "N") or (hValue == "S"):
            assumedLatitudeList = assumedLatitude[1:]
            assumedLatitudeList = assumedLatitudeList.split("d",1)
            assumedLatitudeDegree = assumedLatitudeList[0]
            assumedLatitudeMinute = assumedLatitudeList[1]
            if not assumedLatitudeDegree.strip():
                raise ValueError("Fix.getSightings:    latitudeDegree is empty")
            try:
                assumedLatitudeDegree = int(assumedLatitudeDegree)
            except:
                raise ValueError("Fix.getSightings:    latitudeDegree is not an integer")
            if (assumedLatitudeDegree < 0 ) or (assumedLatitudeDegree >= 90):
                raise ValueError("Fix.getSightings:    latitudeDegree should be GE.0 and LT.90")
            if (hValue == "S"):
                assumedLatitudeDegree = 0 - assumedLatitudeDegree

        elif(hValue.isdigit()) and (assumedLatitude == "0d0.0"):
            assumedLatitudeList = assumedLatitude
            assumedLatitudeList = assumedLatitudeList.split("d",1)
            assumedLatitudeDegree = assumedLatitudeList[0]
            assumedLatitudeMinute = assumedLatitudeList[1]
            if not assumedLatitudeDegree.strip():
                raise ValueError("Fix.getSightings:    latitudeDegree is empty")
            try:
                assumedLatitudeDegree = int(assumedLatitudeDegree)
            except:
                raise ValueError("Fix.getSightings:    latitudeDegree is not an integer")
            if (assumedLatitudeDegree < 0 ) or (assumedLatitudeDegree >= 90):
                raise ValueError("Fix.getSightings:    latitudeDegree should be GE.0 and LT.90")
        else:
            raise ValueError("Fix.getSightings:    invalid h value")
        if not assumedLatitudeMinute.strip():
            raise ValueError("Fix.getSightings:    latitudeMinute is missing")
        latitudeMinuteStd = re.match("\d+\.?\d?\Z",assumedLatitudeMinute)
        if latitudeMinuteStd:
            try:
                assumedLatitudeMinute = float(assumedLatitudeMinute)
            except:
                raise ValueError("Fix.getSightings:    latitudeMinute is not a floating number")
            if (assumedLatitudeMinute < 0.0) or (assumedLatitudeMinute >= 60.0):
                raise ValueError("Fix.getSightings:    latitudeMinute should be GE.0.0 and LT.60.0")
        else:
            raise ValueError("Fix.getSightings:    latitudeMinute should be a floating number with only one digit to the right of the decimal point")

        if assumedLatitudeDegree < 0:
            assumedLatitude = assumedLatitudeDegree - (assumedLatitudeMinute / 60)
        else:
            assumedLatitude = assumedLatitudeDegree + (assumedLatitudeMinute / 60)
        
        assumedLongitudeList = assumedLongitude.split("d",1)
        assumed_LongitudeDegree = assumedLongitudeList[0]
        assumed_longitudeMinute = assumedLongitudeList[1]

        if not assumed_LongitudeDegree.strip():
            raise ValueError("Fix.getSightings:    GP_longitudeDegree is empty")
        try:
            assumed_LongitudeDegree = int(assumed_LongitudeDegree)
        except:
            raise ValueError("Fix.getSightings:    GP_longitudeDegree is not an integer")
        if (assumed_LongitudeDegree < 0 ) or (assumed_LongitudeDegree >= 360):
            raise ValueError("Fix.getSightings:    GP_longitudeDegree should be GE.0 and LT.90")
        if not assumed_longitudeMinute.strip():
            raise ValueError("Fix.getSightings:    GP_longitudeMinute is missing")
        assumed_longitudeMinuteStd = re.match("\d+\.?\d?\Z",assumed_longitudeMinute)
        if assumed_longitudeMinuteStd:
            try:
                assumed_longitudeMinute = float(assumed_longitudeMinute)
            except:
                raise ValueError("Fix.getSightings:    GP_longitudeMinute is not a floating number")
            if (assumed_longitudeMinute < 0.0) or (assumed_longitudeMinute >= 60.0):
                raise ValueError("Fix.getSightings:    GP_longitudeMinute should be GE.0.0 and LT.60.0")
        else:
            raise ValueError("Fix.getSightings:    GP_longitudeMinute should be a floating number with only one digit to the right of the decimal point")
        
        if assumed_LongitudeDegree < 0:
            assumedLongitude = assumed_LongitudeDegree - (assumed_longitudeMinute / 60)
        else:
            assumedLongitude = assumed_LongitudeDegree + (assumed_longitudeMinute / 60)
       
        if (self.sightingFile == None):
            raise ValueError('Fix.getSightings:  "SightingFile" should not be empty')
        if (self.ariesFile == None):
            raise ValueError('Fix.getSightings:  "AriesFile" should not be empty')
        if (self.starFile == None):
            raise ValueError('Fix.getSightings:  "StarFile" should not be empty')

##########
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
        
        if self.starFile == None:
            raise ValueError ("Fix.getSightings: no starFile")    
        if self.ariesFile == None:
            raise ValueError("Fix.getSightings: no ariesFile")
        try:
            tree = ET.parse(self.sightingFile)
        except:
            raise ValueError("Fix.getSightings: no sightingFile")
        root = tree.getroot()
        if not (root.tag == fix):
            raise ValueError("Fix.getSightings: root should be 'fix'")
        else:
            sightingTuples = []
            sumLatitude = 0.0
            sumLongitude = 0.0
            for sighting in (root.findall(sightingTag)):
                if (sighting is None):
                    print ("Fix.getSightings:  No more sightings")
                else:
#body               
                    body = sighting.find(bodyTag)
                    if (body == None):
#                        raise ValueError('Fix.getSightings: missing body')
                        self.sightingError = self.sightingError + 1
                        continue
                    bodyValue = body.text
                    if not isinstance(bodyValue, str):
#                        raise ValueError("Fix.getSightings: 'body' should be a string")
                        self.sightingError = self.sightingError + 1
                        continue
#date               
                    date = sighting.find(dateTag)
                    if (date == None):
#                        raise ValueError('Fix.getSightings: missing date')
                        self.sightingError = self.sightingError + 1
                        continue
                    else:
                        dateValue = date.text
                    if (dateValue is None):
                        self.sightingError = self.sightingError + 1
                        continue
                    try:
                        datetime.datetime.strptime(dateValue, "%Y-%m-%d")
                    except:    
#                         raise ValueError("Fix.getSightings:    date format should be yyyy-mm-dd")
                        self.sightingError = self.sightingError + 1
                        continue
#time
                    time = sighting.find(timeTag)
                    if (time == None):
#                        raise ValueError('Fix.getSightings: missing date')
                        self.sightingError = self.sightingError + 1
                        continue
                    timeValue = time.text
#                     if re.match(r'^\d{4}\-\d{2}\-\d{2}$', dateValue) == None:
#                     raise ValueError('date format incorrect')
                    try:
                        datetime.datetime.strptime(timeValue, "%H:%M:%S")
                    except:
#                        raise ValueError('Fix.getSightings: Time format incorrect')
                        self.sightingError = self.sightingError + 1
                        continue
# #                 if re.match(r'^\d{2}\-\d{2}\-\d{2}$', timeValue) == None:
# #                     raise ValueError('Time format incorrect')
                    observation = sighting.find(observationTag)
                    if (observation == None):
#                        raise ValueError('Fix.getSightings: missing observation')
                        self.sightingError = self.sightingError + 1
                        continue
                    observationValue = observation.text
                    if observationValue.find("d") == -1:     
#                        raise ValueError('Fix.getSightings: seperator is missing')
                        self.sightingError = self.sightingError + 1
                        continue
                    observationValueList = observationValue.split("d",1)
                    degree = observationValueList[0]
                    minute = observationValueList[1]
                    if (degree == ""):
#                        raise ValueError('Fix.getSightings: degree is empty')
                        self.sightingError = self.sightingError + 1
                        continue
                    try:
                        degree = int(degree)
                    except:
#                        raise ValueError('Fix.getSightings: degree is not an integer')
                        self.sightingError = self.sightingError + 1
                        continue
                    if (degree < 0 ) or (degree >= 90):
#                        raise ValueError('Fix.getSightings: degree should be GE.0 and LT.90')
                        self.sightingError = self.sightingError + 1
                        continue
                    
                    if (minute == None):
#                        raise ValueError('Fix.getSightings: minute is missing')
                        self.sightingError = self.sightingError + 1
                        continue
                    observationValue = observation.text
                    
                    minuteStd = re.match("\d+\.?\d?\Z",minute)
                    if minuteStd:
                        try:
                            minute = float(minute)
                        except:
#                            raise ValueError('Fix.getSightings: minute is not a floating number')
                            self.sightingError = self.sightingError + 1
                            continue
                        if (minute < 0.0) or (minute >= 60.0):
#                            raise ValueError('Fix.getSightings: minute should be GE.0.0 and LT.60.0')
                            self.sightingError = self.sightingError + 1
                            continue
                    else:
#                        raise ValueError('Fix.getSightings: minute should be a floating number with only one digit to the right of the decimal point')
                        self.sightingError = self.sightingError + 1
                        continue
                    
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
#                            raise ValueError('Fix.getSightings: height should be a numeric number')
                            self.sightingError = self.sightingError + 1
                            continue
                        if not heightValue >= 0:
#                            raise ValueError('Fix.getSightings: height should be greater than 0')
                            self.sightingError = self.sightingError + 1
                            continue
                        
                    
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
#                            raise ValueError('Fix.getSightings: temperature should be an integer')
                            self.sightingError = self.sightingError + 1
                            continue
                        if (temperatureValue < -20 ) or (temperatureValue > 120):
#                            raise ValueError('Fix.getSightings: temperature should be GE -20 and LE 120')
                            self.sightingError = self.sightingError + 1
                            continue
                    
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
#                            raise ValueError('Fix.getSightings: pressure should be an integer')
                            self.sightingError = self.sightingError + 1
                            continue
                        if (pressureValue < 100) or (pressureValue > 1100):
#                           raise ValueError('Fix.getSightings: pressure should be GE 100 and LE 1100')
                            self.sightingError = self.sightingError + 1
                            continue
                    
#horizon     
                    horizon = sighting.find(horizonTag)
                    Natural = "natural"
                    Artificial = "Artificial"
                    if (horizon is None):
                        horizonValue = "natural"
                    else:
                        horizonValue = horizon.text.lower()
    
                        if (horizonValue is None):
                            horizonValue = "natural"
                        NaturalFlag = bool(re.search(Natural,horizonValue,re.I))
                        ArtificialFlag = bool(re.search(Artificial,horizonValue,re.I))
                        if (ArtificialFlag == False) and (NaturalFlag == False): 
#                            raise ValueError ('Fix.getSightings: horizon should be either Artificial or Natural')
                            self.sightingError = self.sightingError + 1
                            continue
                    
                    if (horizonValue == Natural):
                        dip = (-0.97 * sqrt(heightValue)) / 60
                    else:
                        dip = 0
                    celsuiusTemp = (temperatureValue - 32) / 1.8
                    observedAltitude = degree + (minute / 60)
                    refraction = (-0.00452 * pressureValue) / (273 + celsuiusTemp) / math.tan(observedAltitude * math.pi / 180.0)
                    adjustedAltitude = observedAltitude + dip + refraction
                    degrees = int(adjustedAltitude)
                    minutes = round((adjustedAltitude - degrees)*60, 1)
                    altitude = str(degrees) + "d" + str(minutes)

                    date1 = datetime.datetime.strptime(dateValue, "%Y-%m-%d").strftime("%m/%d/%y")
                    SHA_lati = self.getStar(self.starFile, bodyValue, date1)
                    if SHA_lati == []:
                        self.sightingError = self.sightingError + 1
                        continue
                    else:
                        SHA_star = SHA_lati[0]
            
##############################
                        GP_LatitudeList = SHA_lati[1].strip().split("d",1)
                        GP_latitudeDegree = GP_LatitudeList[0]
                        GP_latitudeMinute = GP_LatitudeList[1]
                        try:
                            GP_latitudeDegree = int(GP_latitudeDegree)
                        except:
    #                        raise ValueError ("Error")
                            self.sightingError = self.sightingError + 1
                            continue
                        try:
                            GP_latitudeMinute = float(GP_latitudeMinute)
                        except:
    #                       raise ValueError("Error")
                            self.sightingError = self.sightingError + 1
                            continue
                        if GP_latitudeDegree < 0:
                            self.GP_Latitude = GP_latitudeDegree - (GP_latitudeMinute / 60.0)
                        else:
                            self.GP_Latitude = GP_latitudeDegree + (GP_latitudeMinute / 60.0)
####################################
                    dateAries = datetime.datetime.strptime(dateValue, "%Y-%m-%d").strftime("%m/%d/%y")
                    HOUR, MINUTE, SECOND = timeValue.split(":")
                    GHA = self.getAries(self.ariesFile, dateAries, int(HOUR))
                    if not (len(GHA) == 2):
#                     raise ValueError("Fix.getStightings:    invalid aries file")
                        self.sightingError = self.sightingError + 1
                        continue
                    try:
                        GHA_A1 = GHA[0].split("\t")[-1].strip()
                        GHA_A2 = GHA[1].split("\t")[-1].strip()
                    except:
#                     raise ValueError("Fix.getSightings:    invalid aries value")
                        self.sightingError = self.sightingError + 1
                        continue
                    GHA_NextDate = GHA[1].split("\t")[0]
                    try:
                        datetime.datetime.strptime(GHA_NextDate, "%m/%d/%y")
                    except:    
#                     raise ValueError("Fix.setAries:    invalid date time format")
                        self.sightingError = self.sightingError + 1
                        continue                               
                    GHA_A1 = self.setDegreeAndMinutes(GHA_A1)
                    if(GHA_A1 == []):   
#                     raise ValueError("Fix.getSighting:    invalid values")
                        self.sightingError = self.sightingError + 1
                        continue                
                    GHA_A2 = self.setDegreeAndMinutes(GHA_A2)
                    if(GHA_A1 == []):   
#                     raise ValueError("Fix.getSighting:    invalid values")
                        self.sightingError = self.sightingError + 1
                        continue
                                    
                    GHA_NextHour = GHA[1].split("\t")[1]
                    try:
                        datetime.datetime.strptime(GHA_NextHour, "%H")
                    except:
#                     raise ValueError("Fix.setAries:    invalid hour format")
                        self.sightingError = self.sightingError + 1
                        continue                
                    AriesDate = datetime.datetime.strptime(dateAries,"%m/%d/%y")
                    dateDelta = datetime.timedelta(days = 1)
                    nextDay = (AriesDate + dateDelta).strftime("%m/%d/%y")  
    
                    
                    if (HOUR != "23") and (GHA_NextHour != str(int(HOUR)+1)) and (GHA_NextDate != dateAries):
#                     raise ValueError("Fix.getSightings:    invalid aries value")
                        self.sightingError = self.sightingError + 1
                        continue
                    if (HOUR == "23") and (GHA_NextHour != "0") and (GHA_NextDate != nextDay):
#                     raise ValueError("Fix.getSighting:    invalid aries value")
                        self.sightingError = self.sightingError + 1
                        continue                
                   
                   
                   
                    s = int(MINUTE) * 60 + int(SECOND)
                    GHA_Aries = GHA_A1 + abs(GHA_A2 - GHA_A1) * (s / 3600.0)
                    
                    self.GP_Longitude = (GHA_Aries + SHA_star) % 360
    
                    GP_longitudeDegree = int(self.GP_Longitude) % 360
                    GP_longitudeMinute = round((self.GP_Longitude - GP_longitudeDegree) * 60, 1)
                    
    
    
                    LHA = (self.GP_Longitude + assumedLongitude)% 360
    
                    sinGP_Latitude = math.sin(math.radians(self.GP_Latitude))
                    sinAssumedLatitude = math.sin(math.radians(assumedLatitude))
                    sinLat = sinGP_Latitude * sinAssumedLatitude
    
                    cosGP_Latitude = math.cos(math.radians(self.GP_Latitude))
                    cosAssumedLatitude = math.cos(math.radians(assumedLatitude))
                    cosLHA = math.cos(math.radians(LHA))
                    cosLat =  cosGP_Latitude * cosAssumedLatitude * cosLHA
    
                    intermediateDistance = sinLat + cosLat
    
                    correctedAltitude = math.asin(intermediateDistance)
    
                    self.distanceAdjustment = int(round(((correctedAltitude * 180 / math.pi) - adjustedAltitude) * 60,0))
                 
                    numerator = sinGP_Latitude - sinAssumedLatitude * intermediateDistance
                    denominator = cosAssumedLatitude * math.cos(correctedAltitude)               
                    intermeidaAzimuth = numerator / denominator
    
                    azimuthAdjustment = math.acos(intermeidaAzimuth)
    
                    azimuthAdjustment = azimuthAdjustment * 180 / math.pi
                    azimuthAdjustmentDegree = int(azimuthAdjustment)
                    azimuthAdjustmentMinute = round((azimuthAdjustment - azimuthAdjustmentDegree) * 60 , 1)
                    
                    self.azimuthAdjustment = str(azimuthAdjustmentDegree) + "d" + str(azimuthAdjustmentMinute)
    
                    self.GP_Longitude = str(GP_longitudeDegree) + "d" + str(GP_longitudeMinute)
    
                    self.GP_Latitude = str(GP_latitudeDegree) + "d" + str(GP_latitudeMinute)
                    
                    cosAzi = math.cos(math.radians(azimuthAdjustment))
                    sinAzi = math.sin(math.radians(azimuthAdjustment))
                    sinAzi
                    latitudechange = cosAzi * self.distanceAdjustment
                    sumLatitude = sumLatitude + latitudechange
                    longitudechange = sinAzi * self.distanceAdjustment
                    sumLongitude = sumLongitude + longitudechange 
#############################                  
                    sightingStd = (bodyValue, dateValue, timeValue, altitude, self.GP_Latitude, self.GP_Longitude, self.assumedLatitude, self.assumedLongitude, self.azimuthAdjustment,self.distanceAdjustment)
                    sightingTuples.append(sightingStd)
                
            sightingTuples = sorted(sightingTuples, key = itemgetter(1,2,0))
            for s in sightingTuples:
                message = self.read(s)
                try:
                    f = open(self.logFile,"a+")
                    f.write(self.date() + message + "\n")
                    f.close()
                except:
                    raise ValueError("Fix.getSightings:    can't append to file")
                 
        try:
            errorMessage = "Sighting errors:"
            f = open(self.logFile, "a+")
            f.write(self.date() + errorMessage + "\t" + str(self.sightingError) + "\n")
            f.close()
        except:
            raise ValueError ("Fix.getSightings:    can't append to file")
        approximateLatitude = assumedLatitude + (sumLatitude / 60)
        approximateLatitudeDegree = int(approximateLatitude)
        approximateLatitudeMinute = round(abs(approximateLatitude - approximateLatitudeDegree) * 60 , 1) 
        if approximateLatitudeDegree < 0:
            self.approximateLatitude = "S" + str(abs(approximateLatitudeDegree)) + "d" + str(approximateLatitudeMinute)
        else:
            self.approximateLatitude = "N" + str(approximateLatitudeDegree) + "d" + str(approximateLatitudeMinute)
        approximateLongitude = (assumedLongitude + (sumLongitude / 60) ) % 360
        approximateLongitudeDegree = int(approximateLongitude)
        approximateLongitudeMinute = round((approximateLongitude - approximateLongitudeDegree) * 60 , 1)
        self.approximateLongitude = str(approximateLongitudeDegree) + "d" + str(approximateLongitudeMinute)
        try:
            LatMessage = "Approximate latitude:"
            LongMessage = "Approximate longitude:"
            f = open(self.logFile,"a+")
            f.write(self.date() + LatMessage + "\t" + self.approximateLatitude + "\t" + LongMessage + "\t" + self.approximateLongitude + "\n")
            f.close()
        except:
            raise ValueError ("Fix.getSightings:    can't append to file")
        
        return(self.approximateLatitude,self.approximateLongitude)
        
    def setDegreeAndMinutes(self, angle):
        if (angle == None):
            return []
        if angle.find("d") == -1:
            return []   
        List = angle.split("d",1)
        if (len(List) != 2):
            return []
        else:
            try:
                degree = int(List[0])
            except:
                return []
            if (degree < 0) or (degree >= 360):
                return []
            try:
                minute = float(List[1])
            except:
                return []
            if (minute * 10 % 1 != 0):
                return []
            if (minute < 0 ) or (minute >= 60):
                return []
            minute = float(minute / 60)
            angle = round((degree + minute) % 360 ,4)
        return angle


       
        
        
        
        
        
        
        