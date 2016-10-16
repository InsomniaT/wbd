from math import degrees
from _socket import setdefaulttimeout


class Angle():
    def __init__(self):
        self.angle = 0.0
        
    def setDegrees(self, degrees = 0.0):
        if (isinstance(degrees, float) or isinstance(degrees, int)) == False:
            raise ValueError ('"degrees" violates the parameter specifications')
        else:
            degrees = degrees % 360
            degrees = round(degrees, 1)
            self.angle = degrees                 
        return degrees
    
    def setDegreesAndMinutes(self, angleString):
        if "" == angleString:
            raise ValueError('null string')
        result = angleString.split("d")
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
            if result[0] < 0:
                result[1] = - result[1]
            else:
                self.angle = (result[0] + result[1] / 60) % 360
            return self.angle
        
    def add(self, angle = None):
        if isinstance(angle, Angle) == False:
            raise ValueError('"angle" is not a valid instance')
        else:
            self.angle = (self.angle + angle.angle) % 360
            return self.angle
        
    def subtract(self, angle = None):
        if isinstance(angle, Angle) == False:
            raise ValueError('"angle" is not a valid instance')
        else:
            self.angle = (self.angle - angle.angle) % 360
            return self.angle
    
    def compare(self, angle = None):
        if isinstance(angle, Angle) == False:
            raise ValueError('"angle" is not a valid instance')
        else:
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

# if __name__ == '__main__':
#     isdigit(asd)
#     angle1 = Angle()
#     angle2 = Angle()
#     angle1.setDegrees(45.0) 
#     a = angle2.setDegrees(45.1)
#     print a
#     angle1s = angle1.getDegrees()
#     angle2s = angle2.getDegrees()
#     print(angle1s, angle2s)

