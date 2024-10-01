###script to change exif date to windows modified timestamp date. The script assumes the pictures are placed into separate folders (one per camera). If you have one single folder with all pictures in it, just create an empty one and place your full folder inside that one, and then set the directory to the previously empty folder.

###import modules
import piexif
import os
import os.path
from datetime import datetime as dt

###set working directory (this is where you should keep all the pictures you want to update the exif date for)
os.chdir("C:/Holmon photos")

###get paths to photos
folders = os.listdir()
###get paths to files
#create empty list to hold picture paths
picturepaths = list()
#iterate over folders
for x in range(len(folders)):
    #get names of all pictures in folder
    pictures = os.listdir(folders[x])
    #iterate over every picture in folder
    for y in range(len(pictures)):
        #append folder name + / + picture name to list of picture paths
        picturepaths.append(folders[x] + "/" + pictures[y])

###get timestamps in format needed for piexif
#create list to hold timestamps
timestamps = list()
#iterate over picture paths
for x in range(len(picturepaths)):
    #get timestamps in posix format
    timestamp = os.path.getmtime(picturepaths[x])
    #convert to format needed for piexif
    timestamps.append(dt.fromtimestamp(timestamp).strftime("%Y:%m:%d %H:%M:%S"))

###Modify files
#iterate over all pictures
for x in range(len(picturepaths)):
    #save windows modified time
    timestamp = os.path.getmtime(picturepaths[x])
    #get existing exifdata for picture as dictionary
    ExifData = piexif.load(picturepaths[x])
    #insert timestamp and camera make into exifdata dictionary
    ExifData['Exif'][piexif.ExifIFD.DateTimeOriginal] = timestamps[x]
    ExifData['0th'][piexif.ImageIFD.Make] = 'Burell'
    #dump library as insertable data
    DumpedExifData = piexif.dump(ExifData)
    #insert data
    piexif.insert(DumpedExifData, picturepaths[x])
    #reset windows modified time again
    os.utime(picturepaths[x], (timestamp,timestamp))