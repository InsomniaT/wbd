from math import degrees
from _socket import setdefaulttimeout
class Angle():
    def __init__(self):
#         self.degrees = 0            #set to 0 degrees 0 minutes
#         self.minutes = 0
        self.angle = 0.0
        
    def setDegrees(self, degrees):
        if (isinstance(degrees, float) or isinstance(degrees, int)) == False:
            raise ValueError ('"degrees" violates the parameter specifications')
        else:
            degrees = degrees % 360
            degrees = round(degrees, 1)
            self.angle = degrees                 #####
        return degrees
    
    def setDegreesAndMinutes(self, angleString):
        if "" == angleString:
            raise ValueError('null string')
        result = angleString.split("d")
        # if isdigit(result[0] or result[1]) == False:
        #     raise ValueError('degrees and minutes must be integer')
        if not "d" in angleString:
            raise ValueError ('missing separator')
        if (result[0] == "" or result[1] == ""):
            raise ValueError ('missing value')
        if "." in result[0]:
            raise ValueError ('degrees must be an integer')
        try:
            result = map(float, result)
            
        except:
            raise ValueError ('violates the parameter specifications')

        else:
            if result[1] < 0:
                raise ValueError ('minutes must be positive')
            if (len(str(result[1] - int(result[1]))) > 3):
                raise ValueError('minutes must have only one decimal place')
#             self.degrees = int(result[0])
#             self.minutes = result[1]
            if result[0] < 0:
                result[1] = - result[1]
            else:
                a = (result[0] + result[1] / 60) % 360
                self.angle = a
            return a
        
    def add(self, angle):
        if isinstance(angle, Angle) == False:
            raise ValueError('"angle" is not a valid instance')
        else:
#             self.degrees = angle.degrees + self.degrees
#             self.minutes = angle.minutes + self.minutes
            self.angle = (self.angle + angle) % 360
            return self.angle
        
    def subtract(self, angle):
        if isinstance(angle, Angle) == False:
            raise ValueError('"angle" is not a valid instance')
        else:
#             self.degrees = self.degrees - angle.degrees
#             self.minutes = self.minutes - angle.minutes
#             return (self.degrees + self.minutes / 60) % 360
            self.angle = (self.angle - angle) % 360
            return self.angle
    
    def compare(self, angle):
        if isinstance(angle, Angle) == False:
            raise ValueError('"angle" is not a valid instance')
        else:
#             self.degrees = angle.degrees + self.degrees
#             self.minutes = angle.minutes + self.minutes
#             a = (self.degrees + self.minutes / 60) % 360
#             b = (angle.degrees + angle.minutes / 60) % 360
            if angle > self.angle:
                return 1
            if angle == self.angle:
                return 0
            if angle < self.angle:
                return -1
    
    def getString(self):
        degrees = int(self.angle) % 360
        minutes = round(((self.angle - int(self.angle)) * 60), 1)
        string = str(degrees) + 'd' + str(minutes)
        return string
        
    def getDegrees(self):
        return self.angle % 360

if __name__ == '__main__':
    angle1 = Angle()
    angle2 = Angle()
    angle1.setDegrees(45.0) 
    a = angle2.setDegrees(45.1)
    print a
    angle1s = angle1.getDegrees()
    angle2s = angle2.getDegrees()
    print(angle1s, angle2s)