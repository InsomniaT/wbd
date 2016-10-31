from math import degrees
from _socket import setdefaulttimeout
import re

class Angle():
    def __init__(self):
        self.angle = 0.0
        
    def setDegrees(self, degrees = 0.0):
        if(not isinstance(degrees, int) and not isinstance(degrees, float)):
            raise ValueError('Angle.setDegrees: degrees violates the parameter specifications')
        else:
              
            self.angle = float(degrees) % 360
            degree = int(self.angle)
            minute = round((self.angle - degree) * 60, 1)
            self.angle = degree + minute / 60.0
        return self.angle
    
    def setDegreesAndMinutes(self, angleString):
        if not angleString.strip():     #check if the parameter is an empty string
            raise ValueError ("Angle.setDegreesAndMinutes:    the parameter is an empty string")
        if angleString.find("d") == -1:     #check if the string contains the separator
            raise ValueError("Angle.setDegreesAndMinutes:    the parameter doesn't contain the required separator")
        angList = angleString.split('d',1) #separate the string by "d"
        degree = angList[0]
        minute = angList[1]
        if not degree.strip():      #check if the first part is empty      
            raise ValueError ("Angle.setDegreesAndMinutes:    should contain degree")
        degreeStd = re.match("[+-]?\d+\Z",degree)     #regular expression for degree.
        if degreeStd:
            degree = int(degreeStd.group(0)) 
        else:
            raise ValueError("Angle.setDegreesAndMinutes:    the format of degree is invalid")
        minuteStd = re.match("\d+\.?\d?\Z",minute)  #regular expression for minute.
        if minuteStd:
            minute = minuteStd.group(0)
            if minute.find(".") > 0:
                minute = float(minute)
        else:
            raise ValueError("Angle.setDegreesAndMinutes:    the format of minute is invalid")
        minute = float(minute) /  60
        if degree >= 0:
            degree = (degree + minute) % 360
        elif degree < 0:
            degree = (degree - minute) % 360        
        self.angle = degree
        return self.angle
    def add(self, angle = None):
        if angle == None:
            raise ValueError('Angle.add: missing angle value')
        if isinstance(angle, Angle) == False:
            raise ValueError('Angle.add: "angle" is not a valid instance')
        else:
            self.angle = (self.angle + angle.angle) % 360
            return self.angle
        
    def subtract(self, angle = None):
        if angle == None:
            raise ValueError('Angle.subtract: missing angle value')
        if isinstance(angle, Angle) == False:
            raise ValueError('Angle.subtract: "angle" is not a valid instance')
        else:
            self.angle = (self.angle - angle.angle) % 360
            return self.angle
    
    def compare(self, angle = None):
        if angle == None:
            raise ValueError("Angle.compare:  angle should not be empty")
        if not isinstance(angle, Angle):
            raise ValueError("Angle.compare:  the parameter is not an instance of Angle")
        if not (isinstance(angle.angle,int)) and not (isinstance(angle.angle, float)) and not (isinstance(angle.angle, str)):
            raise ValueError("Angle.compare:    the parameter should be a numeric value")
        ComparatorAngle = Angle()
        if (isinstance(angle.angle,int)) or (isinstance(angle.angle, float)):
            ComparatorAngle.setDegrees(angle.angle)
        else:
            ComparatorAngle.setDegreesAndMinutes(angle)
        ComparatorAngleDegrees = ComparatorAngle.getDegrees()
        if (self.angle == ComparatorAngleDegrees):
            return 0
        elif(self.angle > ComparatorAngleDegrees):
            return 1
        else:
            return -1
    
    def getString(self):
        degrees = int(self.angle) % 360
        minutes = round(((self.angle - int(self.angle)) * 60), 1)
        string = str(degrees) + 'd' + str(minutes)
        return string
        
    def getDegrees(self):
        return self.angle % 360
