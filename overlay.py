import sys
import cv2
from os import listdir
from os.path import isfile, join

# Get three image paths from the cli
path1 = sys.argv[1]
background_files = set([f for f in listdir(path1) if isfile(join(path1, f))])

path2 = sys.argv[2]
overlay_files = set([f for f in listdir(path1) if isfile(join(path2, f))]).intersection(background_files)

out_path = sys.argv[3]

for f in overlay_files:
    background = cv2.imread(path1 + f)
    overlay = cv2.imread(path2 + f)

    added_image = cv2.addWeighted(background,0.9,overlay,0.3,0)

    cv2.imwrite(out_path + f, added_image)

