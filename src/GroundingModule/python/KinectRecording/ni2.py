from laceutils import mp2np
import multiprocessing as mp
import numpy
import nimod
import cv2
import cPickle
import time

MAX_KINECTS = 2

def _debug_print(s):
    print s

def process_raw_data(raw_data):
    npy_data = {}
    for key, raw_items in raw_data.iteritems():
        npy_items = []
        for raw_item in raw_items:
            npy_item = mp2np(raw_item['data']).reshape(raw_item['size'])
            npy_items.append(npy_item)
        npy_data[key] = npy_items
    return npy_data

def _create_raw_item(str_type, dim):
    raw_item = {}
    totpx = 1
    for d in dim:
        totpx *= d
    raw_item['data'] = mp.RawArray(str_type, totpx)
    raw_item['size'] = dim 
    return raw_item

def _create_raw_items(str_type, dim, num):
    raw_items = []
    raw_item_dim = dim
    for i in range(num):
        raw_item = _create_raw_item(str_type, dim)
        raw_items.append(raw_item)
    return raw_items

def _create_shmem(raw_data, key, str_type, dim=None, num=None):
    if dim == None and num == None:
        ret = _create_raw_items(str_type, [1], 1)
    else:
        ret = _create_raw_items(str_type, list(dim), num)
    raw_data[key] = ret
    return raw_data

def _set_dataval(data, key, value):
    data[key][0][0] = value

def _get_dataval(data, key):
    return data[key][0][0]

def _init_raw_data():
    raw_data = {}
    _create_shmem(raw_data, 'depth', 'H', (480, 640), MAX_KINECTS)
    _create_shmem(raw_data, 'image', 'B', (480, 640, 3), MAX_KINECTS)
    _create_shmem(raw_data, 'user', 'H', (480, 640), 1)
    _create_shmem(raw_data, 'skeleton', 'f', (15, 64, 8), 1)
    _create_shmem(raw_data, 'num_kinects', 'i')
    _create_shmem(raw_data, 'num_users', 'i')
    return raw_data

def _capture(c_exit, c_lock, raw_data):
    _debug_print("[ni:child  ] capture started")
    data = process_raw_data(raw_data)
    num_kinects = nimod.initialize()
    _set_dataval(data, 'num_kinects', num_kinects)
    nimod.add_user(data['user'][0].ravel())
    nimod.add_skeleton(data['skeleton'][0].ravel())
    for i in range(num_kinects):
        nimod.add_depth(data['depth'][i].ravel())
        nimod.add_image(data['image'][i].ravel())
    try:
        while not c_exit.acquire(False):
            nimod.step()
            c_lock.acquire()
            nimod.update_depth()
            nimod.update_image()
            num_users = nimod.update_user()
            _set_dataval(data, 'num_users', num_users)
            nimod.update_skeleton()
            c_lock.release()
    except KeyboardInterrupt:
        pass
    nimod.shutdown()
    _debug_print("[ni:child  ] capture completed")

def start_capture():
    c_lock = mp.Lock()
    c_exit = mp.Lock()
    c_exit.acquire()

    raw_data = _init_raw_data()

    cap = mp.Process(target=_capture, args=(c_exit, c_lock, raw_data))
    cap.start()
    _debug_print("[ni:parent ] capture initiated")

    return cap, c_exit, c_lock, raw_data

def stop_capture(cap, c_exit):
    c_exit.release()
    cap.join()
    _debug_print("[ni:parent ] capture completed")

# user 0-15 colors
USER_COLORS = [(0, 255, 0), (0, 0, 255), (255, 0, 0),
               (128, 128, 0), (128, 0, 128), (0, 128, 128),
               (64, 128, 0), (0, 128, 64), (64, 64, 64),
               (64, 128, 64), (0, 0, 64), (0, 64, 0),
               (64, 0, 0), (64, 0, 64), (128, 64, 128)]
def color_usermap(num_users, user_data):
    userimg = numpy.zeros((480, 640, 3), dtype=numpy.uint8)
    user = numpy.array(user_data, dtype=numpy.uint8)
    for i in range(num_users):
        comp = numpy.ones((480, 640), dtype=numpy.uint8) * (i+1)
        mask = numpy.equal(user, comp)
        numpy.putmask(userimg[:,:,0], mask, USER_COLORS[i][0])
        numpy.putmask(userimg[:,:,1], mask, USER_COLORS[i][1])
        numpy.putmask(userimg[:,:,2], mask, USER_COLORS[i][2])
    return userimg

def color_depthmap(depth_data):
    depth = numpy.array((depth_data/5000.)*255, dtype=numpy.uint8)
    return depth

BODY_PARTS = ['COM', 'HEAD', 'NECK', 'LEFT_SHOULDER', 'RIGHT_SHOULDER',\
              'LEFT_ELBOW', 'RIGHT_ELBOW', 'LEFT_HAND', 'RIGHT_HAND',\
              'TORSO', 'LEFT_HIP', 'RIGHT_HIP', 'LEFT_KNEE', 'RIGHT_KNEE',\
              'LEFT_FOOT', 'RIGHT_FOOT']
def _process_skel_data(num_users, skel_data):
    skels = []
    for k in range(num_users):
        skel = {}
        for partIdx in range(len(BODY_PARTS)):
            part = BODY_PARTS[partIdx]
            i, j = skel_data[k][partIdx][0:2]
            if i != i or j != j:
                i, j = -1, -1
            conf = skel_data[k][partIdx][5]
            if i <= 0 or j <= 0:
                conf = 0.
            skel[part] = {'2d':(int(i), int(j)),\
                          '3d':skel_data[k][partIdx][2:5],\
                          'conf':conf}
        skels.append(skel)
    return skels

def read_frame(c_lock, data):
    c_lock.acquire()
    num_kinects = _get_dataval(data, 'num_kinects')
    num_users = _get_dataval(data, 'num_users')
    user = numpy.zeros((480, 640), dtype=numpy.uint8)
    skel_data = numpy.zeros((15, 64, 8), dtype=numpy.float)
    depths = []
    images = []
    if num_users > 0:
        user = numpy.array(data['user'][0], dtype=numpy.uint8)
        skel_data = numpy.array(data['skeleton'][0])
    for i in range(num_kinects):
        depth = numpy.array(data['depth'][i])
        depths.append(depth)
        image = numpy.array(data['image'][i])
        image = cv2.cvtColor(image, cv2.cv.CV_BGR2RGB)
        images.append(image)
    c_lock.release()
    skels = _process_skel_data(num_users, skel_data)
    frame = {'num_kinects':num_kinects, 'num_users':num_users,
             'user':user, 'depths':depths, 'images':images, 'skel':skels}
    return frame

def _draw_skel2d_conn(img, skel, part1, part2):
    if skel[part1]['conf'] > .5 and skel[part2]['conf'] > .5:
        cv2.line(img, skel[part1]['2d'], skel[part2]['2d'], (0, 0, 128), 5)

def draw_skel2d(img, skel):
    _draw_skel2d_conn(img, skel, 'HEAD', 'NECK')
    _draw_skel2d_conn(img, skel, 'LEFT_HAND', 'LEFT_ELBOW')
    _draw_skel2d_conn(img, skel, 'RIGHT_HAND', 'RIGHT_ELBOW')
    _draw_skel2d_conn(img, skel, 'LEFT_SHOULDER', 'LEFT_ELBOW')
    _draw_skel2d_conn(img, skel, 'RIGHT_SHOULDER', 'RIGHT_ELBOW')
    _draw_skel2d_conn(img, skel, 'LEFT_SHOULDER', 'NECK')
    _draw_skel2d_conn(img, skel, 'RIGHT_SHOULDER', 'NECK')
    _draw_skel2d_conn(img, skel, 'LEFT_SHOULDER', 'TORSO')
    _draw_skel2d_conn(img, skel, 'RIGHT_SHOULDER', 'TORSO')
    _draw_skel2d_conn(img, skel, 'LEFT_HIP', 'TORSO')
    _draw_skel2d_conn(img, skel, 'RIGHT_HIP', 'TORSO')
    _draw_skel2d_conn(img, skel, 'LEFT_HIP', 'LEFT_KNEE')
    _draw_skel2d_conn(img, skel, 'RIGHT_HIP', 'RIGHT_KNEE')
    _draw_skel2d_conn(img, skel, 'LEFT_FOOT', 'LEFT_KNEE')
    _draw_skel2d_conn(img, skel, 'RIGHT_FOOT', 'RIGHT_KNEE')
    for part in BODY_PARTS:
        i, j = skel[part]['2d']
        if skel[part]['conf'] > .5:
            cv2.circle(img, skel[part]['2d'], 10, (0, 0, 128), -1)
        else:
            cv2.circle(img, skel[part]['2d'], 10, (128, 0, 0), -1)

def show_frame(frame):
    num_kinects = frame['num_kinects']
    num_users = frame['num_users']
    for i in range(num_kinects):
        depthimg = color_depthmap(frame['depths'][i])
        cv2.imshow('Depth' + str(i), depthimg)
        cv2.waitKey(10)
        cv2.imshow('Image' + str(i), frame['images'][i])
        cv2.waitKey(10)
    if num_kinects > 0:
        userimg = color_usermap(num_users, frame['user'])
        for i in range(num_users):
            draw_skel2d(userimg, frame['skel'][i])
        blendimg = cv2.addWeighted(frame['images'][0], .5, userimg, .5, 0.)
        cv2.imshow('User', blendimg)
        cv2.waitKey(10)


def _example(c_lock, raw_data):
    data = process_raw_data(raw_data)
    while True:
        frame = read_frame(c_lock, data)
        show_frame(frame)

if __name__ == '__main__':
    cap, c_exit, c_lock, raw_data = start_capture()
    try:
        _example(c_lock, raw_data)
    except KeyboardInterrupt:
        pass
    stop_capture(cap, c_exit)
