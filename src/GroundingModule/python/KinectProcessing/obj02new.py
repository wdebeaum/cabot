import ni
import cvmod
import cv2
import cv2.cv as cv
import maskblob
from glwin import GLWin
from laceutils import *
from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
import random
import laceutils
from numpy import ma
from numpy import linalg as LA
import xml.etree.ElementTree as xml
import sys
import xmlhelper
import os
import traceback
import mahotas
from mahotas.features import zernike_moments
from mahotas.features import haralick



#gx1, gy1, gz1 = -.3, -.3, .5
#gx2, gy2, gz2 = .3, .3, 1.
#gd1, gd2 = 0., 0.

# RikRun calibrations
#gx1, gx2 = -.18, .48
#gy1, gy2 = -.74, -.15
#gz1, gz2 = .96, 1.45
#gd1, gd2 = 14.5, -2.


# gx1 = right plane
# gx2 = left plane

# gy1 = top plane
# gy2 = bottom plane

#gx1, gx2 = -.15, .48

#gy1, gy2 = -.757, -.15

#gz1, gz2 = .96, 1.5
#gd1, gd2 = 14.5, -2.

#IHMC 
gx1, gx2 = -.2, .3
gy1, gy2 = -.5,-.20
gz1, gz2 = .5,1.2
gd1, gd2 = 18, -2.5

# g_mode (keycode 49-56 -> 0-7) gx1, gx2, gy1, gy2, gz1, gz2, gd1, gd2
g_mode = 0
g_step = .02
g_showall = True
g_adjacency = None

g_myrect = None
g_myhist = None

BATCH_MODE = False
BATCH_BLOB_MODE = False
SKELETON_REQUIRED = False
PERFORM_SEGMENTATION = False
SHOW_RGB = False
WRITE_IMAGES = False
MIN_SEGMENT_PIXELS = 500
time_color_dict = {}
current_frame_time = 0
g_last_depth = None
g_last_depth_change = None

SHAPE_FEATURE_SIZE = 64
Z_ORDER = 8
Z_FEATURES = 25
NUM_HARALICK_FEATURES = 13
NUM_NORMAL_HISTOGRAM_FEATURES = 18

storage = cv.CreateMemStorage()

# Gets the name of the directory above "kinect"
print (os.path.dirname(sys.argv[1])).split(os.sep)[-2]
data_directory_name = (os.path.dirname(sys.argv[1])).split(os.sep)[-2]
print "Dirname: " + data_directory_name

xml_root = xml.Element('session')
xml_root.attrib['name'] = data_directory_name
spaces_element = xml.SubElement(xml_root,'spaces')
size_space_element = xml.SubElement(spaces_element,'size')
xml.SubElement(size_space_element, 'dim').text = '1'
xml.SubElement(size_space_element, 'type').text = 'euclidean'
hu_element = xml.SubElement(spaces_element,'hu')
xml.SubElement(hu_element, 'dim').text = '7'
xml.SubElement(hu_element, 'type').text = 'euclidean'
zernike_element = xml.SubElement(spaces_element,'zernike')
xml.SubElement(zernike_element, 'dim').text = str(Z_FEATURES)
xml.SubElement(zernike_element, 'type').text = 'euclidean'
rgb_element = xml.SubElement(spaces_element,'rgb')
xml.SubElement(rgb_element, 'dim').text = '3'
xml.SubElement(rgb_element, 'type').text = 'euclidean'
rgb_var_element = xml.SubElement(spaces_element,'rgb_var')
xml.SubElement(rgb_var_element, 'dim').text = '3'
xml.SubElement(rgb_var_element, 'type').text = 'euclidean'
haralick_element = xml.SubElement(spaces_element,'haralick')
xml.SubElement(haralick_element, 'dim').text = str(NUM_HARALICK_FEATURES)
xml.SubElement(haralick_element, 'type').text = 'euclidean'
normal_histogram_element = xml.SubElement(spaces_element,'normalhistogram')
xml.SubElement(normal_histogram_element, 'dim').text = str(NUM_NORMAL_HISTOGRAM_FEATURES)
xml.SubElement(normal_histogram_element, 'type').text = 'euclidean'


def indent(elem, level=0):
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i

def change_calibration_values(calibration_parameters):
    global gx1, gx2, gy1, gy2, gz1, gz2, gd1, gd2
    gx1 = calibration_parameters['gx1']
    gx2 = calibration_parameters['gx2']
    gy1 = calibration_parameters['gy1']
    gy2 = calibration_parameters['gy2']
    gz1 = calibration_parameters['gz1']
    gz2 = calibration_parameters['gz2']
    gd1 = calibration_parameters['gd1']
    gd2 = calibration_parameters['gd2']

def load_calibration_file():
    """Load from a file named calibration located above the 'kinect' folder for
        determining the extents of the volume where objects are detected."""
    calibration_path = os.path.join(os.sep.join(sys.argv[1].split(os.sep)[0:-2]),"calibration")
    print calibration_path
    calibration_parameters = {}
    try:
        with open(calibration_path, "r") as f:
            for line in f:
                split_line = line.split("=")
                calibration_parameters[split_line[0].strip()] = split_line[1].strip()
            change_calibration_values(calibration_parameters)
            print "Loaded calibration file"
    except IOError:
        print 'No calibration file'
        return

def show_features(img, max_corners):
    feat = cv2.goodFeaturesToTrack(img, max_corners, .01, 5.)
    if feat == None:
        return None
    for f in feat:
        x, y = f[0]
        cv2.circle(img, (x, y), 4, 255, -1)
    return feat

def distance(a,b):
    return math.sqrt((a[0] - b[0]) * (a[0] - b[0]) + (a[1] - b[1]) * (a[1] - b[1]))

def get_rcol():
    r = random.randint(0, 255)
    g = random.randint(0, 255)
    b = random.randint(0, 255)
    return (r, g, b)

def get_unique_colors(image, spacing = 1):
    if image is None:
        return None
    #retval, bestLabels, centers = cv2.kmeans(image, 3, (cv2.TERM_CRITERIA_EPS, 0.1), 2,
    colors = set()

    for row in image:
        for col in xrange(0,len(row),spacing):
            #print row[col][0],row[col][1],row[col][2]
            colors.add((row[col][0],row[col][1],row[col][2]))

    return colors

def split_components(image, components):
    masks = []
    i = 0
    for component in components:
        x = component[2][0]
        y = component[2][1]
        width = component[2][2]
        height = component[2][3]

        mask = cv2.inRange(image[y:y+height, x:x+width, :],
                                   numpy.array(component[1][:3]),
                                   numpy.array(component[1][:3]))
                                   
        i += 1
        if numpy.sum(mask) > 100:
            masks.append(mask)
        else:
            masks.append(None)

    return masks

def segment_image(bgr_image, mask, illuminance_scale = 1):
    global storage
    bgr_image[:,:,0] = bgr_image[:,:,0] & mask
    bgr_image[:,:,1] = bgr_image[:,:,1] & mask
    bgr_image[:,:,2] = bgr_image[:,:,2] & mask
    blob_color_rect_hsv = cv2.cvtColor(bgr_image, cv2.COLOR_BGR2HSV)
   
    # Set the Value of the color to a constant to avoid segmenting
    # based on lighting
    blob_color_rect_hsv[:,:,2] = 100

    blob_color_rect_hsv = cv2.pyrMeanShiftFiltering(blob_color_rect_hsv,60,57,
                                                    maxLevel = 0)
    
    
    
    bgr_result= cv2.cvtColor(blob_color_rect_hsv, cv2.COLOR_HSV2BGR)
    # PyrSegmentation uses a BGR-specific algorithm, so we convert back
    blob_color_rect_hsv = cv2.cvtColor(blob_color_rect_hsv, cv2.COLOR_HSV2BGR)
    
    # Workaround for image having to be a power of 2 square, crops to 256x256
    newarray = numpy.zeros((256,256,3), dtype=numpy.uint8)
    newarray[0:min(255,blob_color_rect_hsv.shape[0]), \
             0:min(255,blob_color_rect_hsv.shape[1]), :] = \
        blob_color_rect_hsv[0:min(255,blob_color_rect_hsv.shape[0]), \
                            0:min(255,blob_color_rect_hsv.shape[1]), :]
             
    
    # This is an old algorithm so it needs a storage space
    bitmap = cv.CreateImageHeader((newarray.shape[1], newarray.shape[0]),
                                  cv.IPL_DEPTH_8U, 3)
    cv.SetData(bitmap, newarray.tostring(),
               newarray.dtype.itemsize * 3 * newarray.shape[1])

    components = cv.PyrSegmentation(bitmap, bitmap, storage,1,50,20)

    return (bgr_result, blob_color_rect_hsv, components)
    
def get_blob_element(mask, rect, skel, num_blob_pixels, color, color_variance, grey, depth, label, is_subblob = False):
    # Scale image for scale-invariant shape features
    # Make it so the longest edge is equal to SHAPE_FEATURE_SIZE
    resized_image = mahotas.imresize(mask,
                                     float(SHAPE_FEATURE_SIZE) / max(mask.shape))

    z_moments = zernike_moments(resized_image, SHAPE_FEATURE_SIZE, degree = Z_ORDER)

    blob_element = xml.Element(label)
    
    x_1 = rect[0]
    y_1 = rect[1]
    x_2 = x_1 + rect[2]
    y_2 = y_1 + rect[3]

    blob_element.attrib['x'] = str(rect[0])
    blob_element.attrib['y'] = str(rect[1])
    blob_element.attrib['width'] = str(rect[2])
    blob_element.attrib['height'] = str(rect[3])
    rect_center = (rect[0] + .5 * rect[2],
                   rect[1] + .5 * rect[3])
    if (skel is not None and len(skel) > 0 ):
        blob_element.attrib['head_dist'] = str(distance(rect_center,
                                                skel['HEAD']['2d']))
        blob_element.attrib['right_hand_dist'] = str(distance(rect_center,
                                                skel['RIGHT_HAND']['2d']))
        blob_element.attrib['left_hand_dist'] = str(distance(rect_center,
                                                skel['LEFT_HAND']['2d']))
    
    

    
    features_element = xml.Element('features')
    blob_element.append(features_element)

    size_element = xml.SubElement(features_element, 'size')
    xml.SubElement(size_element, 'pixels').text = str(num_blob_pixels)

    hu_element = get_hu_moments_element(mask)

    features_element.append(hu_element)

    grey_masked_image = grey & mask

    blob_depth = depth & mask

    if is_subblob:
        blob_depth_rect = blob_depth
        blob_mask_rect = mask
    else:
        blob_depth_rect = blob_depth[y_1:y_2, x_1:x_2]
        blob_mask_rect = mask[y_1:y_2, x_1:x_2]


    normal_vector_histogram = get_normal_vector_histogram(blob_depth_rect, blob_mask_rect.astype(numpy.uint8))
    
    #print normal_vector_histogram

    if normal_vector_histogram is None:
        print 'Error calculating normal_vector_histogram'
        return None

    normal_histogram_element = xml.Element('normalhistogram')

    for i in xrange(normal_vector_histogram.size):
        xml.SubElement(normal_histogram_element, 'a_' + str(i)).text = str(normal_vector_histogram[i])

    features_element.append(normal_histogram_element)

    haralick_element = xml.Element('haralick')

    haralick_features = haralick(grey_masked_image)
    # Average the rows (the different directions of the features)
    haralick_features_averaged = numpy.mean(haralick_features,axis=0)
    #print len(haralick_features_averaged)

    for i in xrange(NUM_HARALICK_FEATURES):
        xml.SubElement(haralick_element, 'a_' + str(i)).text = str(haralick_features_averaged[i])

    features_element.append(haralick_element)

    zernike_element = xml.Element('zernike')
        
    for i in xrange(Z_FEATURES):
        xml.SubElement(zernike_element, 'a_' + str(i)).text = str(z_moments[i])

    features_element.append(zernike_element)
    
    rgb_element = xml.Element('rgb')
    xml.SubElement(rgb_element, 'r').text = str(color[0])
    xml.SubElement(rgb_element, 'g').text = str(color[1])
    xml.SubElement(rgb_element, 'b').text = str(color[2])

    features_element.append(rgb_element)
    
    rgb_variance_element = xml.Element('rgb_var')
    xml.SubElement(rgb_variance_element, 'r').text = str(color_variance[0])
    xml.SubElement(rgb_variance_element, 'g').text = str(color_variance[1])
    xml.SubElement(rgb_variance_element, 'b').text = str(color_variance[2])
    
    features_element.append(rgb_variance_element)

    return blob_element

def get_hu_moments_element(mask):
    moments = cv2.moments(mask)
    hu_moments = cv2.HuMoments(moments)
                
    hu_element = xml.Element('hu')
    xml.SubElement(hu_element, 'i_1').text = str(hu_moments[0][0])
    xml.SubElement(hu_element, 'i_2').text = str(hu_moments[1][0])
    xml.SubElement(hu_element, 'i_3').text = str(hu_moments[2][0])
    xml.SubElement(hu_element, 'i_4').text = str(hu_moments[3][0])
    xml.SubElement(hu_element, 'i_5').text = str(hu_moments[4][0])
    xml.SubElement(hu_element, 'i_6').text = str(hu_moments[5][0])
    xml.SubElement(hu_element, 'i_7').text = str(hu_moments[6][0])

    return hu_element
            

g_max, g_prev = None, None
g_gray = numpy.zeros((480, 640), dtype=numpy.uint8)
g_colors = [None]
g_feat = None
g_rgb = None
g_rgbfeat = None

g_blobidx = 0
g_blobidxset = []

#g_frameidx = 0

def show_histogram(hist):
    bin_count = hist.shape[0]
    bin_w = 24
    img = numpy.zeros((256, bin_count*bin_w, 3), numpy.uint8)
    for i in xrange(bin_count):
        h = int(hist[i])
        cv2.rectangle(img, (i*bin_w+2, 255), ((i+1)*bin_w-2, 255-h), (int(180.*i/bin_count), 255, 255), -1)
    img = cv2.cvtColor(img, cv2.COLOR_HSV2BGR)
    cv2.imshow('hist', img)
    cv2.waitKey()

def get_histogram(img, x0, y0, x1, y1):
    hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
    mask = cv2.inRange(hsv, numpy.array((0., 60., 32.)), numpy.array((180., 255., 255.)))
    win = (x0, y0, x1-x0, y1-y0)
    hsv_roi = hsv[y0:y1, x0:x1]
    mask_roi = mask[y0:y1, x0:x1]
    hist = cv2.calcHist([hsv_roi], [0], mask_roi, [16], [0, 180])
    cv2.normalize(hist, hist, 0, 255, cv2.NORM_MINMAX)
    hist = hist.reshape(-1)
    return hist


def step(frame, file_time, output_file = None):
    global g_adjacency, g_max, g_prev, g_colors, g_feat, g_gray, g_rgb, g_rgbfeat, g_blobidx, g_blobidxset, g_myrect, g_myhist
    global current_frame_time, xml_root
    global g_last_depth, g_last_depth_change
    #global g_frameidx
    #ni.show_frame(frame)
    num_kinects = frame['num_kinects']
    if num_kinects == 0:
        return
    depth = numpy.array(frame['depths'][0])
    
    rgb = numpy.array(frame['images'][0])
    cvmod.set_depth(depth.ravel())
    g_sa = 0.
    if g_showall:
        g_sa = 1.
    var = numpy.array([gx1, gx2, gy1, gy2, gz1, gz2, gd1, gd2, g_sa], dtype=numpy.float)
    cvmod.set_vars(var)
    if not BATCH_MODE and not BATCH_BLOB_MODE:
        cvmod.output3d()
    cvmod.process_blob()
    
    adj = numpy.array(g_adjacency, dtype=numpy.uint8) * 255
    adj = cv2.erode(adj, None)
    adj = cv2.erode(adj, None)
    
    if not BATCH_MODE and not BATCH_BLOB_MODE:
        #cv2.imshow('ADJ', adj)
        cv2.waitKey(10)
    
    max_idx, img, rects = maskblob.createblobmask(adj)

    #print 'Rects',rects
    
    if g_prev == None:
        g_max, g_prev = max_idx, img
        for i in range(max_idx):
            g_colors.append(get_rcol())
        return
##    print "print", g_max, max_idx
    new_g_colors = [None]



    if not BATCH_MODE:
        if g_feat != None:
            nextPts, status, err = cv2.calcOpticalFlowPyrLK(g_gray, gorig, g_feat, None, winSize=(15, 15), maxLevel=5)
            for i in range(len(nextPts)):
                if status[i][0] == 1:
                    x1, y1 = g_feat[i][0]
                    #cv2.circle(cmask, (x1, y1), 4., (0, 0, 255), -1)
                    x2, y2 = nextPts[i][0]
                    dx = abs(x1-x2)
                    dy = abs(y1-y2)
                    #if dx*dx+dy*dy < 70*70:
                    #    cv2.line(cmask, (x1, y1), (x2, y2), (0, 255, 0), 3)


            ass_mat = numpy.zeros((max_idx, g_max), dtype=numpy.uint8)
            for idx1 in range(1, g_max+1):
                bmask = maskblob.getblob(g_prev, idx1)
                bmask = cv2.dilate(bmask, None)

                for i in range(len(nextPts)):
                    if status[i][0] == 1:
                        x1, y1 = g_feat[i][0]
                        if bmask[y1][x1] > 0:

                            for idx2 in range(1, max_idx+1):
                                dmask = maskblob.getblob(img, idx2)
                                dmask = cv2.dilate(dmask, None)
                                x2, y2 = nextPts[i][0]
                                dx = abs(x1-x2)
                                dy = abs(y1-y2)
                                if dx*dx+dy*dy < 70*70:
                                    if dmask[y2][x2] > 0:
                                        ass_mat[idx2-1][idx1-1] += 1
                                        cv2.line(cmask, (x1, y1), (x2, y2), (0, 255, 0), 1)
            if not BATCH_MODE and not BATCH_BLOB_MODE:
                print ass_mat

            for i in range(max_idx):
                k = ass_mat[i]
                if numpy.max(k) > 0:
                    p = numpy.argmax(k)
                    blobidxset[i] = g_blobidxset[p]

            if not BATCH_MODE and not BATCH_BLOB_MODE:
                print blobidxset


        # End Batch

    if len(rects) > 0:
        a0, b0, w0, h0 =  rects[0]
        a1 = a0+w0
        b1 = b0+h0
##        cv2.rectangle(cmask, (a0, b0), (a1, b1), (255, 255, 255), 5)
    else:
        a0 = 0
        b0 = 0
        a1 = 630
        b1 = 470
        
        
    if g_myrect == None:
        
        g_myrect = a0, b0, a1, b1
        g_myhist = get_histogram(rgb, a0, b0, a1, b1)
        #show_histogram(g_myhist)
    else:

        frame_element = xml.SubElement(xml_root,'frame')
        frame_element.attrib['timestamp'] = str(current_frame_time)
##        frame_element.attrib['confidence'] = str(numpy.std(prob_vector * hsv))

        if (len(frame['skel']) > 0):
            skeleton_element = xml.SubElement(frame_element,'skeleton')
            if not (frame['skel'][0]['HEAD']['2d'][0] == 0 and
                    frame['skel'][0]['HEAD']['2d'][1] == 0):
                xmlhelper.generate_skeleton_xml(frame, skeleton_element)
        
        blobs_element = xml.SubElement(frame_element,'blobs')        
        grayrgb = cv2.cvtColor(rgb, cv2.COLOR_BGR2GRAY)
        #print 'Max prob: ', numpy.max(prob)

        if output_file is not None:
            blue_channel = rgb[:,:,0]
            green_channel = rgb[:,:,1]
            red_channel = rgb[:,:,2]
            
            for blob_num in range(0, max_idx):
                
                blob_mask = maskblob.getblob(img,blob_num + 1)
                num_blob_pixels = numpy.sum(blob_mask, dtype=numpy.int32) / 255
                
                if num_blob_pixels <= 8:
                    continue
                
                x_1 = rects[blob_num][0]
                y_1 = rects[blob_num][1]
                x_2 = x_1 + rects[blob_num][2]
                y_2 = y_1 + rects[blob_num][3]
                
                #print 'rect shape: (', rects[blob_num][2],',',rects[blob_num][3],')'

                blob_red = red_channel & blob_mask
                red_value = numpy.sum(blob_red, dtype=numpy.int32) / num_blob_pixels
                red_variance = numpy.std(blob_red[blob_red.nonzero()])
                blob_red_rect = blob_red[y_1:y_2, x_1:x_2]

                blob_green = green_channel & blob_mask
                green_value = numpy.sum(blob_green, dtype=numpy.int32) / num_blob_pixels
                green_variance = numpy.std(blob_green[blob_green.nonzero()])
                blob_green_rect = blob_green[y_1:y_2, x_1:x_2]

                blob_blue = blue_channel & blob_mask
                blue_value = numpy.sum(blob_blue, dtype=numpy.int32) / num_blob_pixels
                blue_variance = numpy.std(blob_blue[blob_blue.nonzero()])
                blob_blue_rect = blob_blue[y_1:y_2, x_1:x_2]

                max_depth = numpy.max(depth)
                min_depth = numpy.min(depth)


                #blob_depth = depth & blob_mask
                #blob_depth_rect = blob_depth[y_1:y_2, x_1:x_2]
                #blob_mask_rect = blob_mask[y_1:y_2, x_1:x_2]

                
                #normal_vector_histogram = get_normal_vector_histogram(blob_depth_rect, blob_mask_rect.astype(numpy.uint8))
                
                #print normal_vector_histogram

                blob_color_rect = numpy.zeros((rects[blob_num][3], rects[blob_num][2], 3), numpy.uint8)
                
                #if (len(frame['skel']) == 0):
                #    frame['skel'].append(None)
                skeleton = None

                for skel in frame['skel']:
                    if skel is not None:
                        skeleton = skel
                        break

                # Blob processing
                blob_element = get_blob_element(blob_mask, rects[blob_num], skeleton,
                                 num_blob_pixels, (red_value, green_value, blue_value),
                                (red_variance, green_variance, blue_variance), grayrgb,
                                                depth, 'blob')

                blobs_element.append(blob_element)
                
                rect = rects[blob_num]
                
                # Color Segmentation for subblobs
                if PERFORM_SEGMENTATION:
                    blob_color_rect = numpy.zeros((rect[3], rect[2], 3), numpy.uint8)

                    # Needed for segmentation
                    blob_color_rect[:,:,0] = blob_blue_rect
                    blob_color_rect[:,:,1] = blob_green_rect
                    blob_color_rect[:,:,2] = blob_red_rect
                    add_sublobs_to_element(blob_element, blob_mask[y_1:y_2, x_1:x_2],
                                           rects[blob_num],
                                           frame, skeleton,
                                           depth, blob_color_rect)
                        


# Indented once, check that it's right
        if not BATCH_MODE:
            
            grayrgborig = cv2.cvtColor(rgb, cv2.COLOR_BGR2GRAY)
            
            grayrgbfeat = show_features(grayrgb, 500)

            
            if g_rgbfeat != None:
                nextPts, status, err = cv2.calcOpticalFlowPyrLK(g_rgb, grayrgborig, g_rgbfeat, None, winSize=(15, 15), maxLevel=5)
                for i in range(len(nextPts)):
                    if status[i][0] == 1:
                        x1, y1 = g_rgbfeat[i][0]
                        #cv2.circle(rgb, (x1, y1), 4., (0, 0, 255), -1)
                        x2, y2 = nextPts[i][0]
                        dx = abs(x1-x2)
                        dy = abs(y1-y2)
                        if dx*dx+dy*dy < 30*30:
                            cv2.line(rgb, (x1, y1), (x2, y2), (0, 255, 0), 1)
                #f = cv2.calcOpticalFlowFarneback(g_rgb, grayrgborig, None, .5, 3, 15, 3, 5, 1.2, 0)
                f = cv2.calcOpticalFlowFarneback(g_rgb, grayrgborig,0.5,1,3,15,3,5,1)            
                print f.shape

            g_rgb = grayrgborig
            g_rgbfeat = grayrgbfeat

##if not BATCH_BLOB_MODE:
##            cv2.imshow('MAS', cmask)
##            cv2.waitKey(10)
##            cv2.imshow('GAS', gmask)
##            cv2.waitKey(10)
##            cv2.imshow('Gray',grayrgborig)
##            cv2.waitKey(10)

        if SHOW_RGB:
            cv2.imshow('IMG', rgb)
            cv2.waitKey(10)

    if (BATCH_MODE or BATCH_BLOB_MODE) and WRITE_IMAGES:
        #print numpy.max(depth)
        print cv2.imwrite('./ImageOutput/' + file_time + 'rgb.jpg',rgb)
        print cv2.imwrite('./ImageOutput/' + file_time + 'd.jpg',numpy.divide(depth,12))
##    g_max, g_prev = max_idx, img
##    g_gray = gorig
##    g_colors = new_g_colors
##    g_feat = feat
    
    
##    g_blobidxset = blobidxset

def add_sublobs_to_element(blob_element, blob_mask, rect, frame, skeleton, depth, blob_color_rect):

    global MIN_SEGMENT_PIXELS
    #print 'Blob num ', blob_num, ' shape: ', blob_color_rect.shape
    
    (bgr_result, lab_result, components) = segment_image(blob_color_rect, blob_mask)
    
    component_masks = split_components(lab_result, components)
    
    subblobs_element = xml.SubElement(blob_element,'subblobs')
    
    # Split the same-color but separate components for when same-color
    # blocks are used but not touching
    new_components = []
    new_masks = []
    for component, c_mask in zip(components,component_masks):
        # Make sure not to get negative masks
        if (c_mask is not None and
            component[0] > 100 and (component[2][2] < rect[2] or
                                    component[2][3] < rect[3])):
            (same_color_components, same_color_masks) = \
                        split_same_color_components(component, c_mask)
            new_components.extend(same_color_components)
            new_masks.extend(same_color_masks)
    
    # TODO: Figure out color modes - are they consistent and correct?
    component_count = 0
    
    # Component definition
    # x = component[2][0]
    # y = component[2][1]
    # width = component[2][2]
    # height = component[2][3]
    i = 0
    cv2.destroyAllWindows()
    for component, c_mask in zip(new_components,new_masks):
                        # Remove trivial components
                        
        if (c_mask is not None and
                component[0] > 100 and (component[2][2] < rect[2] or
                component[2][3] < rect[3])):
            c_mask = cv2.erode(c_mask,None,iterations=2)
            c_mask = cv2.dilate(c_mask,None, iterations=4)
            
            if numpy.sum(c_mask) > 255 * MIN_SEGMENT_PIXELS and not BATCH_MODE:
                print numpy.sum(c_mask)
                print "Good comp:" + str(component)
                #cv2.imshow('mask',image[y:y+height, x:x+width, :])
                cv2.imshow('mask' + str(i),c_mask)
                cv2.moveWindow('mask' + str(i),component[2][0] * 3,
                                                component[2][1] * 3)
            
            i += 1
            rgb_color = cv2.cvtColor(numpy.array([[component[1][:3]]],
                                                    dtype=numpy.uint8),
                                                    cv2.COLOR_LAB2RGB)
            
            grey_color = cv2.cvtColor(rgb_color, cv2.COLOR_RGB2GRAY)
            
            # Variance doesn't really make sense right now
            #red_channel = rgb_color[:,:,0]
            #green_channel = rgb_color[:,:,1]
            #blue_channel = rgb_color[:,:,2]
            
            #red_variance = numpy.std(red_channel[red_channel.nonzero()])
            #green_variance = numpy.std(green_channel[green_channel.nonzero()])
            #blue_variance = numpy.std(blue_channel[blue_channel.nonzero()])
            
            # This will be handled by the get_blob_element code
            x_1 = rect[0] + component[2][0]
            x_2 = x_1 + component[2][2]
            y_1 = rect[1] + component[2][1]
            y_2 = y_1 + component[2][3]
            
            depth_rect = depth[y_1:y_2, x_1:x_2]
            
            if (len(frame['skel']) == 0):
                frame['skel'].append(None)
            
            # These next two lines were indented, don't know why but might be important
            subblob_element = get_blob_element(c_mask, component[2],
                                                        skeleton, int(component[0]),
                                                        rgb_color[0,0,:],
                                                        (0,0,0),
                                                        grey_color,
                                                   depth_rect, 'subblob', is_subblob = True)
            if not (subblob_element is None):
                subblobs_element.append(subblob_element)
        component_count += 1

def split_same_color_components(component, mask):
    masks = []
    components = []
    if mask is None:
        return ([],[])
    contours, hierarchy = cv2.findContours(numpy.copy(mask), cv2.RETR_EXTERNAL,
                                            cv2.CHAIN_APPROX_SIMPLE)
    for contour in contours:
        
        x,y,w,h = cv2.boundingRect(contour)
        new_mask = mask[y:y+h, x:x+w]
        new_component = (numpy.sum(new_mask) / 255, component[1],
                         (component[2][0] + x,component[2][1] + y,w,h))
        
        masks.append(new_mask)
        components.append(new_component)

    return (components, masks)


def get_normal_vector_histogram(depth_rect, depth_mask):

    if depth_rect is None:
        print 'Depth rect is null'
    depth_image = cv2.GaussianBlur(depth_rect, (9,9), 3)
    if depth_image is None:
        print 'Depth image is null'
        return None
    diff_x_image = get_difference_x_image(depth_image)
    diff_y_image = get_difference_y_image(depth_image)
                
    azimuth_image = numpy.arctan2(diff_y_image,diff_x_image)
    zenith_image = numpy.arctan(numpy.sqrt(numpy.square(diff_x_image) +
                                            numpy.square(diff_y_image)))
    
#if (not BATCH_MODE):
        #cv2.imshow('azimuth', (azimuth_image * 162).astype(numpy.uint8))
        #cv2.waitKey(10)
        #cv2.imshow('zenith', (zenith_image * 162).astype(numpy.uint8))
        #cv2.waitKey(10)
    
    combined = cv2.merge([(azimuth_image * 162).astype(numpy.uint8, copy=False),
                          (zenith_image * 162).astype(numpy.uint8, copy=False)])

    masked_histogram = cv2.calcHist([combined], [0,1], depth_mask, [7, 4],
                        [0, 255, 0, 255])

    normalized_masked_histogram = masked_histogram / LA.norm(masked_histogram)
    
    normalized_masked_histogram = normalized_masked_histogram.flatten()
    
    important_histogram_values = [0,1,2,3,6,7,10,11,14,15,18,19,22,23,24,25,26,27]
    
    return normalized_masked_histogram[important_histogram_values]

def get_difference_x_image(image):
    kernel = numpy.array([.5,0,-.5])
    result = image.copy()
    cv2.filter2D(image, -1, kernel, result)

    return result

def get_difference_y_image(image):
    kernel = numpy.array([[.5],[0],[-.5]])
    
    result = cv2.transpose(image.copy())
    cv2.filter2D(cv2.transpose(image.copy()), -1, kernel, result)
    
    return cv2.transpose(result)

#def get_spherical_images(image):


def process_keys(ret):
    global g_mode, gx1, gx2, gy1, gy2, gz1, gz2, gd1, gd2, g_showall
    for elem in ret:
        print elem
        if elem == 48:
            g_showall = not g_showall
            print "SHOWALL:", g_showall
        if 48 < elem and elem < 57:
            g_mode = elem - 49
            print "mode:", g_mode
        if elem == 276 or elem == 274:
            if g_mode == 0:
                gx1 -= g_step
            elif g_mode == 1:
                gx2 -= g_step
            elif g_mode == 2:
                gy1 -= g_step
            elif g_mode == 3:
                gy2 -= g_step
            elif g_mode == 4:
                gz1 -= g_step
            elif g_mode == 5:
                gz2 -= g_step
            elif g_mode == 6:
                gd1 -= 1
            elif g_mode == 7:
                gd2 -= 1
        if elem == 273 or elem == 275:
            if g_mode == 0:
                gx1 += g_step
            elif g_mode == 1:
                gx2 += g_step
            elif g_mode == 2:
                gy1 += g_step
            elif g_mode == 3:
                gy2 += g_step
            elif g_mode == 4:
                gz1 += g_step
            elif g_mode == 5:
                gz2 += g_step
            elif g_mode == 6:
                gd1 += 1
            elif g_mode == 7:
                gd2 += 1
    if len(ret) > 0:
        print gx1, gx2, gy1, gy2, gz1, gz2, gd1, gd2

def main():
    global g_adjacency, current_frame_time, xml_root, BATCH_MODE, BATCH_BLOB_MODE
    global SKELETON_REQUIRED, PERFORM_SEGMENTATION, WRITE_IMAGES, SHOW_RGB
    #cap, c_exit, c_lock, raw_data = ni.start_capture()
    load_calibration_file()
    filelist = laceutils.read_filelist(sys.argv[1])
    directory = os.path.basename(sys.argv[1])
    start_frame = 0
    flags = []
    if len(sys.argv) > 2:
        for flag in sys.argv[2:]:
            if flag.isdigit():
                start_frame = int(flag)
            if flag == "-b":
                BATCH_MODE = True
                BATCH_BLOB_MODE = True
            if flag == "-k":
                SKELETON_REQUIRED = True
            if flag == "-s":
                PERFORM_SEGMENTATION = True
            if flag == "-r":
                SHOW_RGB = True
            if flag == "-w":
                WRITE_IMAGES = True

    #filelist = laceutils.read_filelist("..\Take02")
    #g_adjacency = numpy.zeros((480-1, 640-1), dtype=numpy.uint8)
    g_adjacency = numpy.zeros((480, 640), dtype=numpy.uint8)
    cvmod.set_adjacency(g_adjacency.ravel())
    filelist_idx = start_frame
    with open('color_output','w') as output_file:
        try:
            win = GLWin()
            #data = ni.process_raw_data(raw_data)

            while True:
                #frame = ni.read_frame(c_lock, data)
                #print filelist_idx
                print filelist[filelist_idx]
                frame = laceutils.read_pkl_frame(filelist[filelist_idx])
                current_frame_time = float(filelist[filelist_idx].split('-')[-1].split('.')[0]) / 100.0
                filelist_idx += 1
                ret = win.step()
                process_keys(ret)

                if (len(frame['skel']) > 0 or SKELETON_REQUIRED is False):
                    step(frame, str(int(current_frame_time * 100.0)), output_file)
                #win.update()
        except KeyboardInterrupt:
            indent(xml_root)
            tree = xml.ElementTree(xml_root)
            tree.write(data_directory_name + '_features.xml')
            pass
        except IndexError:
            print 'IndexError'
            traceback.print_exc()
            indent(xml_root)
            tree = xml.ElementTree(xml_root)
            tree.write(data_directory_name + '_features.xml')
    #ni.stop_capture(cap, c_exit)

if __name__ == '__main__':
    main()
