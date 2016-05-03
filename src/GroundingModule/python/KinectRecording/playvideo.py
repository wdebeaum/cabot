import cv2
import os
import cPickle
import glob
import sys
import numpy
from ni import color_depthmap, color_usermap, draw_skel2d
from ni import show_frame

g_next_frame_time = 1./60

g_type = 0

def write_video_frame(rgb, depth, user, frame):
    if g_type == 1:
        return write_video_frame1(rgb, depth, user, frame)
    return write_video_frame2(rgb, depth, user, frame)

def write_video_frame1(rgbvid, dvid, uvid, frame):
    global g_next_frame_time

    images, depths, user = frame
    image = cv2.cvtColor(images[0], cv2.cv.CV_BGR2RGB)
    rgbvid.write(image)

    d = depths[0]
    dimg = numpy.zeros((480, 640, 3), dtype=numpy.uint8)
    dimg[:,:,0], dimg[:,:,1], dimg[:,:,2] = d, d, d
    dvid.write(dimg)

    #u = numpy.array(user*32, dtype=numpy.uint8)
    uimg = color_usermap(15, user)
    #uimg = numpy.zeros((480, 640, 3), dtype=numpy.uint8)
    #uimg[:,:,0], uimg[:,:,1], uimg[:,:,2] = u, u, u
    uvid.write(uimg)

    g_next_frame_time += 1./60

def write_video_frame2(rgbvid, dvid, uvid, frame):
    global g_next_frame_time

    rgbvid.write(frame['images'][0])

    #d = numpy.array((frame['depths'][0]/5000.)*255, dtype=numpy.uint8)
    d = color_depthmap(frame['depths'][0])
    dimg = numpy.zeros((480, 640, 3), dtype=numpy.uint8)
    dimg[:,:,0], dimg[:,:,1], dimg[:,:,2] = d, d, d
    dvid.write(dimg)

    num_users = frame['num_users']
    userimg = color_usermap(num_users, frame['user'])
    for i in range(num_users):
        draw_skel2d(userimg, frame['skel'][i])
    blendimg = cv2.addWeighted(frame['images'][0], .5, userimg, .5, 0.)
    uvid.write(blendimg)

    g_next_frame_time += 1./60

def get_time(filename):
    if g_type == 1:
        return get_time1(filename)
    return get_time2(filename)

def get_time1(filename):
    return float(filename.split(".")[0].split("-")[1].replace("_", "."))

def get_time2(filename):
    return float(filename.split(".")[0].split("-")[2])/100.

def read_pkl_frame(datadir, filename):
    f = open(datadir + "/" + filename)
    frame = cPickle.load(f)
    f.close()
    return frame

def main(prefix):
    datadir = prefix
    filelist = []
    for name in glob.glob(datadir + "/*.pkl"):
        filelist.append(name.split("/")[-1])
    filelist.sort()

    for filename in filelist:
        print filename
        frame = read_pkl_frame(datadir, filename)
        show_frame(frame)

if __name__ == '__main__':
    try:
        prefix = sys.argv[1]
    except IndexError:
        print "Please specify a data directory."
        exit(0)
    main(prefix)
