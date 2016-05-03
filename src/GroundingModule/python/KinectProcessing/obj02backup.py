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
import xml.etree.ElementTree as xml
import sys
import xmlhelper
import os
from mahotas.features import zernike

#gx1, gy1, gz1 = -.3, -.3, .5
#gx2, gy2, gz2 = .3, .3, 1.
#gd1, gd2 = 0., 0.
gx1, gx2 = -.18, .48

gy1, gy2 = -.74, -.15

gz1, gz2 = .96, 1.45
gd1, gd2 = 14.5, -2.
# g_mode (keycode 49-56 -> 0-7) gx1, gx2, gy1, gy2, gz1, gz2, gd1, gd2
g_mode = 0
g_step = .02
g_showall = True
g_adjacency = None

g_myrect = None
g_myhist = None

BATCH_MODE = True
BATCH_BLOB_MODE = True
time_color_dict = {}
current_frame_time = 0
g_last_depth = None
g_last_depth_change = None

xml_root = xml.Element('session')
xml_root.attrib['name'] = 'Take02'
spaces_element = xml.SubElement(xml_root,'spaces')
size_space_element = xml.SubElement(spaces_element,'size')
xml.SubElement(size_space_element, 'dim').text = '1'
xml.SubElement(size_space_element, 'type').text = 'euclidean'
hu_element = xml.SubElement(spaces_element,'hu')
xml.SubElement(hu_element, 'dim').text = '7'
xml.SubElement(hu_element, 'type').text = 'euclidean'
zernike_element = xml.SubElement(spaces_element,'zernike')
xml.SubElement(zernike_element, 'dim').text = '9'
xml.SubElement(zernike_element, 'type').text = 'euclidean'
rgb_element = xml.SubElement(spaces_element,'rgb')
xml.SubElement(rgb_element, 'dim').text = '3'
xml.SubElement(rgb_element, 'type').text = 'euclidean'
rgb_var_element = xml.SubElement(spaces_element,'rgb_var')
xml.SubElement(rgb_var_element, 'dim').text = '3'
xml.SubElement(rgb_var_element, 'type').text = 'euclidean'


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

def show_features(img, max_corners):
    feat = cv2.goodFeaturesToTrack(img, max_corners, .01, 5.)
    if feat == None:
        return None
    for f in feat:
        x, y = f[0]
        cv2.circle(img, (x, y), 4., 255, -1)
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
    #img = cv2.cvtColor(img, cv2.COLOR_HSV2RGB)
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

def step(frame, output_file = None):
    global g_adjacency, g_max, g_prev, g_colors, g_feat, g_gray, g_rgb, g_rgbfeat, g_blobidx, g_blobidxset, g_myrect, g_myhist
    global current_frame_time, xml_root
    global g_last_depth, g_last_depth_change
    #global g_frameidx
    #ni.show_frame(frame)
    num_kinects = frame['num_kinects']
    if num_kinects == 0:
        return
    depth = frame['depths'][0]
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
    adj = cv2.erode(adj, None)
    
    
##    depth_array = numpy.array(depth, dtype=numpy.uint8)
##    
    #print 'depth_array', depth_array[200]

    
##    if g_last_depth is not None:
##        depth_change = numpy.absolute(depth_array - g_last_depth)
##    else:
##        depth_change = numpy.ones(depth.shape, dtype=numpy.uint8)
##        g_last_depth_change = numpy.ones(depth.shape, dtype=numpy.uint8)
##
##    g_last_depth = depth_array
##    depth_change = cv2.erode(depth_change, None, iterations=4)
##    depth_change = cv2.dilate(depth_change, None, iterations=2)
    
    
    if not BATCH_MODE and not BATCH_BLOB_MODE:
##        cv2.imshow('Depth', depth_change)
##        cv2.waitKey(10)
        cv2.imshow('ADJ', adj)
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

    # Compute the overlap of blobs with those from the last frame to maintain
    # temporal coherence and assign the same id
##    for i in range(max_idx):
##        new_g_colors.append(get_rcol())
##    for idx in range(1, max_idx+1):
##        bmask = maskblob.getblob(img, idx)
##        pmax = 0
##        pmax_val = 0.
##        for pidx in range(1, g_max+1):
##            pmask = maskblob.getblob(g_prev, pidx)
##            perc = maskblob.overlapblob(bmask, pmask)
##            if perc > pmax_val:
##                pmax_val = perc
##                pmax = pidx
##        if not BATCH_MODE and not BATCH_BLOB_MODE:
##            print idx, pmax, pmax_val
##        if pmax_val > .8:
##            new_g_colors[idx] = g_colors[pmax]
##
##    blobidxset = []
##    for idx in range(1, max_idx+1):
##        blobidxset.append(g_blobidx)
##        g_blobidx += 1

##    cmask = maskblob.drawblobs(img, max_idx, 640, 480, new_g_colors)
    #print 'testtest-%03d.png' % g_frameidx
    #cv2.imwrite('testtest-%03d.png' % g_frameidx, cmask)
    #g_frameidx += 1

##    gmask = cv2.cvtColor(cmask, cv2.COLOR_BGR2GRAY)
##    gorig = cv2.cvtColor(cmask, cv2.COLOR_BGR2GRAY)
##    feat = show_features(gmask, 500)

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
##        hsv = cv2.cvtColor(rgb, cv2.COLOR_BGR2HSV)
##        ycc = cv2.cvtColor(rgb, cv2.COLOR_BGR2YCR_CB)
##        
##        mask = cv2.inRange(hsv, numpy.array((0., 60., 32.)), numpy.array((180., 255., 255.)))
##        mask2 = numpy.array(gorig)

        # From "A New Fast Skin Color Detection Technique" by Mahmoud, Tarek M.
        # Modified a bit
##        skinmask = cv2.inRange(ycc,numpy.array((80., 135., 85.)), numpy.array((220., 150., 135.)))
##        skinmask = cv2.dilate(skinmask,None,iterations=3)
##
##        ret, mask2 = cv2.threshold(mask2, 0, 255, cv2.THRESH_BINARY)
##        prob = cv2.calcBackProject([hsv], [0], g_myhist, [0, 180], 1)
##        prob &= mask
##        prob &= mask2

##        depthmask = cv2.inRange(depth_change, numpy.array((5,)), numpy.array((255,)))
##        last_depthmask = cv2.inRange(g_last_depth_change,numpy.array((5,)), numpy.array((255,)))
##        
        # Weight the skin regions 
##        half_matrix = numpy.ones(prob.shape)
##        numpy.putmask(half_matrix,skinmask,.6)

        # Weight the motion regions
##        half_matrix_depth = numpy.ones(prob.shape)
##        numpy.putmask(half_matrix_depth,last_depthmask,4)
##        numpy.putmask(half_matrix_depth,depthmask, 5)
##

        #Store depth change for next frame
##        g_last_depth_change = depth_change
##        
##        prob = cv2.erode(prob,None,iterations=3)
##        prob = prob * half_matrix * half_matrix_depth

        
        # There's got to be a better way to do this right?
##        prob_vector = numpy.zeros((prob.shape[0],prob.shape[1],3))
##        prob_vector[:,:,0] = prob
##        prob_vector[:,:,1] = prob
##        prob_vector[:,:,2] = prob
##        
        #rgb_mask = numpy.copy(rgb) 
        # Take weighted average of color based on probability map
##        rgb_weighted_average = numpy.sum(numpy.sum(prob_vector * rgb ,axis=0),axis=0) \
##                               / numpy.sum(prob)
                
##        print rgb_weighted_average
##        print 'Confidence: ', numpy.std(prob_vector * rgb)

##        if output_file is not None:
####            output_file.write(str(current_frame_time) + '\t')
####            output_file.write(str(rgb_weighted_average) + '\t' +\
####                              str(numpy.std(prob_vector * rgb)) + '\n')
##            output_file.write('-\n' + str(current_frame_time) + '\n')
##            output_file.write(str(rgb_weighted_average[0]) + '\t' +
##                              str(rgb_weighted_average[1]) + '\t' +
##                              str(rgb_weighted_average[2]) + '\n' +
##                              str(numpy.std(prob_vector * hsv)) + '\n\n')

        frame_element = xml.SubElement(xml_root,'frame')
        frame_element.attrib['timestamp'] = str(current_frame_time)
##        frame_element.attrib['confidence'] = str(numpy.std(prob_vector * hsv))

        skeleton_element = xml.SubElement(frame_element,'skeleton')
        if not (frame['skel'][0]['HEAD']['2d'][0] == 0 and
            frame['skel'][0]['HEAD']['2d'][1] == 0):
            xmlhelper.generate_skeleton_xml(frame, skeleton_element)
                
        #frame_element.append(blobs_element)        
        #rgb_weighted_average_element = xml.Element('weighted_rgb')
##        frame_features_element = xml.SubElement(frame_element,'features')
##        rgb_weighted_element = xml.SubElement(frame_features_element,'rgb_weighted')
##        xml.SubElement(rgb_weighted_element,'r').text = str(rgb_weighted_average[2])
##        xml.SubElement(rgb_weighted_element,'g').text = str(rgb_weighted_average[1])
##        xml.SubElement(rgb_weighted_element,'b').text = str(rgb_weighted_average[0])
####        
##        rgb_weighted_average_element.attrib['b'] = str(rgb_weighted_average[0])
##        rgb_weighted_average_element.attrib['g'] = str(rgb_weighted_average[1])
##        rgb_weighted_average_element.attrib['r'] = str(rgb_weighted_average[2])
        
        blobs_element = xml.SubElement(frame_element,'blobs')        

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

##                num_prob_pixels = numpy.sum(prob * blob_mask, dtype=numpy.int32) / 255
                

                x_1 = rects[blob_num][0]
                y_1 = rects[blob_num][1]
                x_2 = x_1 + rects[blob_num][2]
                y_2 = y_1 + rects[blob_num][3]

                
##                blob_prob = prob * (blob_mask / 255)
##                blob_attention = numpy.sum(blob_prob) / numpy.sum(prob)
                
                #blob_red = numpy.sum(red_channel & blob_mask, dtype=numpy.int32) / num_blob_pixels
                blob_red = red_channel & blob_mask
                red_value = numpy.sum(blob_red, dtype=numpy.int32) / num_blob_pixels
                red_variance = numpy.std(blob_red[blob_red.nonzero()])
##                red_prob_value = numpy.sum(blob_prob * blob_red) / num_prob_pixels
                #blob_red_rect = blob_red[y_1:y_2, x_1:x_2]
                blob_red_rect = blob_red[x_1:x_2, y_1:y_2]

                #blob_green = numpy.sum(green_channel & blob_mask, dtype=numpy.int32) / num_blob_pixels
                blob_green = green_channel & blob_mask
                green_value = numpy.sum(blob_green, dtype=numpy.int32) / num_blob_pixels
                green_variance = numpy.std(blob_green[blob_green.nonzero()])
##                green_prob_value = numpy.sum(blob_prob * blob_green) / num_prob_pixels
                #blob_green_rect = cv2.GetSubRect(blob_green, rects[blob_num])
                #blob_green_rect = blob_green[y_1:y_2, x_1:x_2]
                blob_green_rect = blob_green[x_1:x_2, y_1:y_2]

                #blob_blue = numpy.sum(blue_channel & blob_mask, dtype=numpy.int32) / num_blob_pixels
                blob_blue = blue_channel & blob_mask
                blue_value = numpy.sum(blob_blue, dtype=numpy.int32) / num_blob_pixels
                blue_variance = numpy.std(blob_blue[blob_blue.nonzero()])
##                blue_prob_value = numpy.sum(blob_prob * blob_blue) / num_prob_pixels
                #blob_blue_rect = cv2.GetSubRect(blob_blue, rects[blob_num])
                #blob_blue_rect = blob_blue[y_1:y_2, x_1:x_2]
                blob_blue_rect = blob_blue[x_1:x_2, y_1:y_2]

                blob_depth = depth & blob_mask
                max_depth = numpy.max(blob_depth[blob_depth.nonzero()])
                min_depth = numpy.min(blob_depth[blob_depth.nonzero()])

                blob_color_rect = numpy.zeros((rects[blob_num][2], rects[blob_num][3], 3), numpy.uint8)

                # Needed for segmentation but there is a size error
##                blob_color_rect[:,:,0] = blob_red_rect
##                blob_color_rect[:,:,1] = blob_green_rect
##                blob_color_rect[:,:,2] = blob_blue_rect

##                blob_color_rect_image = cv.GetImage(cv.fromarray(blob_color_rect))
                #dest = cv.CloneImage(blob_color_rect_image)

##                blob_color_rect = cv2.pyrMeanShiftFiltering(blob_color_rect,20,100, maxLevel = 1)
                
                #cv.PyrMeanShiftFiltering(blob_color_rect_image,
                #                                blob_color_rect_image,10,50, max_level = 0)
                #cv2.imshow('blob', blob_color_rect_image)
                #cv2.waitKey(10)

                        #cv.PyrMeanShiftFiltering(blob_color_rect_image,
                #                                blob_color_rect_image,10,50, max_level = 0)

##                unique_colors = get_unique_colors(blob_color_rect)

##                print unique_colors

                #del segments
                #contours, hierarchy = cv2.findContours(blob_mask, mode = cv2.RETR_EXTERNAL,
                #                            method = cv2.CHAIN_APPROX_SIMPLE)
                #moments = cv2.moments(contours[0])
                moments = cv2.moments(blob_mask)
                hu_moments = cv2.HuMoments(moments)

                #zernike_moments = zernike.zernike_moments         
                

                blob_element = xml.Element('blob')
                
                blobs_element.append(blob_element)

##                output_file.write(str(blobidxset[blob_num]) + '\n')
##                output_file.write(str(float(num_prob_pixels) / num_blob_pixels) + '\n')

##                output_file.write(str(rects[blob_num][0]) + '\t' + str(rects[blob_num][1]) + '\n')
##                output_file.write(str(rects[blob_num][2]) + '\t' + str(rects[blob_num][3]) + '\n')

##                blob_element.attrib['id'] = str(blobidxset[blob_num])
##                blob_element.attrib['confidence'] = str(float(num_prob_pixels) / num_blob_pixels)
                blob_element.attrib['x'] = str(rects[blob_num][0])
                blob_element.attrib['y'] = str(rects[blob_num][1])
                blob_element.attrib['width'] = str(rects[blob_num][2])
                blob_element.attrib['height'] = str(rects[blob_num][3])
                rect_center = (rects[blob_num][0] + .5 * rects[blob_num][2],
                               rects[blob_num][1] + .5 * rects[blob_num][3])
                blob_element.attrib['head_dist'] = str(distance(rect_center,
                                                            frame['skel'][0]['HEAD']['2d']))
                blob_element.attrib['right_hand_dist'] = str(distance(rect_center,
                                                            frame['skel'][0]['RIGHT_HAND']['2d']))
                blob_element.attrib['left_hand_dist'] = str(distance(rect_center,
                                                            frame['skel'][0]['LEFT_HAND']['2d']))
                
                features_element = xml.Element('features')
                blob_element.append(features_element)

                size_element = xml.Element('size')
                xml.SubElement(size_element, 'pixels').text = str(num_blob_pixels)

                features_element.append(size_element)
                
                hu_element = xml.Element('hu')
                xml.SubElement(hu_element, 'i_1').text = str(hu_moments[0][0])
                xml.SubElement(hu_element, 'i_2').text = str(hu_moments[1][0])
                xml.SubElement(hu_element, 'i_3').text = str(hu_moments[2][0])
                xml.SubElement(hu_element, 'i_4').text = str(hu_moments[3][0])
                xml.SubElement(hu_element, 'i_5').text = str(hu_moments[4][0])
                xml.SubElement(hu_element, 'i_6').text = str(hu_moments[5][0])
                xml.SubElement(hu_element, 'i_7').text = str(hu_moments[6][0])

                features_element.append(hu_element)


                rgb_element = xml.Element('rgb')
                xml.SubElement(rgb_element, 'r').text = str(red_value)
                xml.SubElement(rgb_element, 'g').text = str(green_value)
                xml.SubElement(rgb_element, 'b').text = str(blue_value)

                features_element.append(rgb_element)
                
                rgb_variance_element = xml.Element('rgb_var')
                xml.SubElement(rgb_variance_element, 'r').text = str(red_variance)
                xml.SubElement(rgb_variance_element, 'g').text = str(green_variance)
                xml.SubElement(rgb_variance_element, 'b').text = str(blue_variance)
                
                features_element.append(rgb_variance_element)
                
##                blob_element.attrib['attention'] = str(blob_attention)

##                rgb_prob_element = xml.Element('rgb_prob')
##                xml.SubElement(rgb_prob_element, 'r').text = str(red_prob_value)
##                xml.SubElement(rgb_prob_element, 'g').text = str(green_prob_value)
##                xml.SubElement(rgb_prob_element, 'b').text = str(blue_prob_value)

##                features_element.append(rgb_prob_element)

##        if not BATCH_MODE and not BATCH_BLOB_MODE:
##            cv2.imshow('prob', prob)
##        

##
##        if not BATCH_MODE and not BATCH_BLOB_MODE:
##            rgb_copy = numpy.copy(rgb)
##            prob_pixels = numpy.greater(prob,numpy.ones(prob.shape))
##            rgb_copy[~prob_pixels] = numpy.array((0.,0.,0.))
##            #rgb_copy = cv2.erode(rgb_copy,None,iterations=2)
##            cv2.imshow('probcolor',rgb_copy)
        
        #cv2.imshow('probcolors',prob_colors)
##        term_crit = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT, 10, 1)
##        track_box, g_myrect = cv2.CamShift(prob, g_myrect, term_crit)
#        cv2.ellipse(cmask, track_box, (0, 0, 255), 2)



    if not BATCH_MODE:
        grayrgb = cv2.cvtColor(rgb, cv2.COLOR_BGR2GRAY)
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
            f = cv2.calcOpticalFlowFarneback(g_rgb, grayrgborig, None, .5, 3, 15, 3, 5, 1.2, 0)
            print f.shape

        g_rgb = grayrgborig
        g_rgbfeat = grayrgbfeat

        if not BATCH_BLOB_MODE:
##            cv2.imshow('MAS', cmask)
##            cv2.waitKey(10)
##            cv2.imshow('GAS', gmask)
##            cv2.waitKey(10)
            cv2.imshow('Gray',g_gray)
            cv2.waitKey(10)


            cv2.imshow('IMG', rgb)
            cv2.waitKey(10)
##    g_max, g_prev = max_idx, img
##    g_gray = gorig
##    g_colors = new_g_colors
##    g_feat = feat
    
    
##    g_blobidxset = blobidxset

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
    global g_adjacency, current_frame_time, xml_root
    #cap, c_exit, c_lock, raw_data = ni.start_capture()
    filelist = laceutils.read_filelist(sys.argv[1])
    directory = os.path.basename(sys.argv[1])
    start_frame = 0
    if len(sys.argv) > 2:
        start_frame = int(sys.argv[2])
    #filelist = laceutils.read_filelist("..\Take02")
    #g_adjacency = numpy.zeros((480-1, 640-1), dtype=numpy.uint8)
    g_adjacency = numpy.zeros((480, 640), dtype=numpy.uint8)
    cvmod.set_adjacency(g_adjacency.ravel())
    filelist_idx = start_frame
    with open('color_output','w') as output_file:
        try:
            win = GLWin()
            #data = ni.process_raw_data(raw_data)
##            output_file.write('frame timestamp 1\n')
##            output_file.write('frame color 3\n')
##            output_file.write('frame confidence 1\n')
##            output_file.write('blob id 1\n')
##            output_file.write('blob attention 1\n')
##            output_file.write('space euclidean hu 7 0 0 0 0 0 0 0 -1 -1 -1 -1 -1 -1 -1\n')
##            output_file.write('space euclidean rect_rgb 3 0 0 0 255 255 255\n')
##            output_file.write('space euclidean variance_rgb 3 0 0 0 255 255 255\n')
##            output_file.write('blob confidence 1\n')
##            output_file.write('space euclidean attention_rgb 3 0 0 0 255 255 255\n')
##            output_file.write('endheader')
            while True:
                #frame = ni.read_frame(c_lock, data)
                #print filelist_idx
                print filelist[filelist_idx]
                frame = laceutils.read_pkl_frame(filelist[filelist_idx])
                current_frame_time = float(filelist[filelist_idx].split('-')[-1].split('.')[0]) / 100.0
                filelist_idx += 1
                ret = win.step()
                process_keys(ret)

                if len(frame['skel']) > 0:
                    #print frame['skel'][0]
                    step(frame, output_file)
                #win.update()
        except KeyboardInterrupt:
            indent(xml_root)
            tree = xml.ElementTree(xml_root)
            tree.write('color_output-' + directory + '.xml')
            pass
        except IndexError:
            print 'IndexError'
            indent(xml_root)
            tree = xml.ElementTree(xml_root)
            tree.write('color_output-' + directory + '.xml')
    #ni.stop_capture(cap, c_exit)

if __name__ == '__main__':
    main()
