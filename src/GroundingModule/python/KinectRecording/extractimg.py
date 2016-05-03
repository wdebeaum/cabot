import cv2
import os
import cPickle
import glob
import sys
import numpy
from ni2 import color_depthmap, color_usermap, draw_skel2d

g_next_frame_time = 1./10

g_type = 0

g_prefix = "./"
g_idx = 0

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

    g_next_frame_time += 1./10

def write_video_frame2(rgbvid, dvid, uvid, frame):
    global g_next_frame_time, g_prefix, g_idx

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

    cv2.imwrite("img-%05d-user.jpg" % g_idx, blendimg)
    cv2.imwrite("img-%05d-depth.jpg" % g_idx, dimg)

    g_idx += 1

    g_next_frame_time += 1./10

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
    global g_type, g_prefix
    datadir = prefix
    filelist = []
    for name in glob.glob(datadir + "/*.pkl"):
        filelist.append(name.split("/")[-1])
    filelist.sort()
    filelist_idx = 0

    if filelist[0].split("-")[0] == "data":
        g_type = 1
    print "DATATYPE:", g_type

    g_prefix = datadir

    data_time_base = get_time(filelist[0])
    rgb = cv2.VideoWriter(datadir + '/ni2vid-rgb.avi', cv2.cv.FOURCC(*"mjpg"), 60, (640, 480))
    depth = cv2.VideoWriter(datadir + '/ni2vid-depth.avi', cv2.cv.FOURCC(*"mjpg"), 60, (640, 480))
    user = cv2.VideoWriter(datadir + '/ni2vid-user.avi', cv2.cv.FOURCC(*"mjpg"), 60, (640, 480))

    filename = filelist[filelist_idx]
    filelist_idx += 1
    curr_frame = read_pkl_frame(datadir, filelist[0])
    data_time = get_time(filelist[0]) - data_time_base

    try:
        while True:
            if data_time < g_next_frame_time:
                # read a frame
                filename = filelist[filelist_idx]
                print "PROCESSING .PKL %06d" % filelist_idx
                filelist_idx += 1
                curr_frame = read_pkl_frame(datadir, filename)
                data_time = get_time(filename) - data_time_base
            else:
                # write a frame
                write_video_frame(rgb, depth, user, curr_frame)
    except IndexError:
        pass

if __name__ == '__main__':
    try:
        prefix = sys.argv[1]
    except IndexError:
        print "Please specify a data directory."
        exit(0)
    main(prefix)
