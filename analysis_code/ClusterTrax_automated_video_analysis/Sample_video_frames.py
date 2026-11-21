import cv2
import numpy as np
import os

#path to video folder
full_exp_path = '/path_to_video_folder/*.mp4'


currdir = os.getcwd()
video = cv2.VideoCapture(full_exp_path)
os.chdir(os.path.dirname(full_exp_path))
print(os.getcwd())

outputdir_name = os.path.basename(full_exp_path).split('.')[0]
outputdir = os.path.join(os.getcwd(), fr'{outputdir_name}')

if not os.path.exists(outputdir):
    os.makedirs(outputdir)

os.chdir(outputdir)

# Create a VideoCapture object and read from input file
# If the input is the camera, pass 0 instead of the video file name

def ResizeWithAspectRatio(image, width=None, height=None, inter=cv2.INTER_AREA):
    dim = None
    (h, w) = image.shape[:2]

    if width is None and height is None:
        return image
    if width is None:
        r = height / float(h)
        dim = (int(w * r), height)
    else:
        r = width / float(w)
        dim = (width, int(h * r))

    return cv2.resize(image, dim, interpolation=inter)

# cv2.namedWindow("output", cv2.WINDOW_NORMAL) 
length = int(video.get(cv2.CAP_PROP_FRAME_COUNT))
fps = int(video.get(cv2.CAP_PROP_FPS))

# How do you want to step through the video??
minutes = 0
seconds = 30

step = int(fps*(minutes*60 + seconds))

for i in range(step-1,length, step):
    video.set(cv2.CAP_PROP_POS_FRAMES, i)
    ret, frame = video.read()
    # frame = cv2.rotate(frame, cv2.ROTATE_90_CLOCKWISE)
    # Name my image frames like so...
    cv2.imwrite(f'Timelapse_frame{i}.png', frame)

os.chdir(currdir)

video.set(cv2.CAP_PROP_POS_FRAMES, 0)
# Check if camera opened successfully
if (video.isOpened()== False): 
  print("Error opening video stream or file")
 
# Read until video is completed
while(video.isOpened()):
  # Capture frame-by-frame
  ret, frame = video.read()
  if ret == True:
    
    Fresize = ResizeWithAspectRatio(frame, width=1280) # Resize by width OR 
    # Display the resulting frame
    cv2.imshow('output',Fresize)
 
    # Press Q on keyboard to  exit
    if cv2.waitKey(25) & 0xFF == ord('q'):
      break
 
  # Break the loop
  else: 
    break
 
# When everything done, release the video capture object
video.release()
 
# Closes all the frames
cv2.destroyAllWindows()
