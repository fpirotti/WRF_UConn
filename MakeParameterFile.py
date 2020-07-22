import arcpy;import numpy as np;import hashlib;import os;import zipfile;
import random

currentDirectory = os.getcwd()
arcpy.env.workspace = currentDirectory;
wkt = "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],\
               PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];\
               -400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119522E-09;\
               0.001;0.001;IsHighPrecision"
sr = arcpy.SpatialReference()
sr.loadFromString(wkt)
print("Set up Workspace")

WRFFILE = currentDirectory + "\\WRFFiles\\Nam12_WRF_Winter_param_v2016_4km_ALL.shp"
staticVariablesFile = currentDirectory + "\\out\\fromGEEreducedVars\\nodesWithGEEvars_andRvar_crs4326.shp"
treeDataFolder = currentDirectory + "\\out\\fromGEEreducedVars\\copernicus.zip"
treeDataName = "copernicus.csv"
output = currentDirectory + "\\out\\combinedFile.shp"





#Imports and combines the WRF DATA and static variables

#arcpy.management.XYTableToPoint(WRFFILE, "temp1", "lon", "lat")
arcpy.management.Copy(WRFFILE, "temp1.shp")
arcpy.management.Project(staticVariablesFile, "temp2", sr)

try:
    arcpy.GenerateNearTable_analysis(WRFFILE, "temp2.shp", "temp4", None, "NO_LOCATION", "NO_ANGLE", "CLOSEST", 0, "PLANAR")
except:
    print("skipping, check existing file is correct")
arcpy.management.JoinField("temp1.shp", "FID", "temp4", "IN_FID")
arcpy.management.JoinField("temp1.shp", "NEAR_FID", "temp2.shp", "FID")

print("Done joining WRF DATA and static variables")

#now we must add the copernicus data
#This imports the copernicus data and transforms it into a proper csv file
#Unzipping Copirnicus and reading it to a csv that may be imported into arcgis
print("Importing and reading Copernicus")
output = np.empty((97524,44))
possibleKeys = ['lon', 'lat',
     '111', '112', '121', '122', '123', '124', '131', '132', '133', '141',
     '142', '211', '212', '213', '221', '222', '223', '231', '241', '242',
     '243', '244', '311', '312', '313', '321', '322', '323', '324', '331',
     '332', '333', '334', '335', '411', '412', '421', '422', '511', '512',
     '521', '523']
header = ""
for i in possibleKeys:
    header += ", " + "tree_" + str(i)
header = header[1:]

zf = zipfile.ZipFile(treeDataFolder)
zf.extract(treeDataName)
with open(currentDirectory +"\\"+ treeDataName) as file:
    file.readline()
    linenumber= 0
    for line in file:
        
        outputLine = [0 for i in range(44)]
        line = line.strip()
        line = line.split("}")
        for item in line[0].strip('"').strip("{").split(","):
            if "=" in item:
                advbjk = item.strip()
                key = advbjk[0:3]
                value = advbjk[4:]
                outputLine[possibleKeys.index(key)] = str(value)
            elif item.strip() == "":pass
            else:
                print(item)
                raise ValueError
        i = 0
        for item in line[1].strip('"').strip("{").split(","):
            if item.strip() == "":
                pass
            elif i == 0:
                lon = item.strip()
                outputLine[possibleKeys.index("lon")] = lon.strip('"')
                i += 1
            elif i == 1:
                lat = item.strip()
                outputLine[possibleKeys.index("lat")] = lat.strip('"')
                i += 1
            
        output[linenumber] = np.array(outputLine,dtype=np.dtype(str))
        linenumber+=1
np.savetxt("temp5.csv",output,delimiter=",",header=header,comments="")
print("Imported Copernicus")



otherfile = currentDirectory + "\\out\\copernicus.shp"

arcpy.GenerateNearTable_analysis("temp1.shp", otherfile, "temp7", None, "NO_LOCATION", "NO_ANGLE", "CLOSEST", 0, "PLANAR")


arcpy.management.JoinField("temp1.shp", "FID", "temp7", "IN_FID")

arcpy.management.JoinField("temp1.shp", "NEAR_FID", otherfile, "FID")





arcpy.management.Copy("temp1.shp", output)

#remove temporary file
print("Removing Temporary Files")
os.remove(treeDataName)
currentDirectory = os.getcwd();filenames = [file for file in os.listdir(currentDirectory) if os.path.isfile(currentDirectory + "\\" + file)]
for i in filenames:
    if i[0:4] == "temp":
        os.remove(i)
print("Done")

