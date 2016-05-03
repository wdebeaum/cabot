import cv2
import numpy
import random

def get_rcol():
    r = random.randint(0, 255)
    g = random.randint(0, 255)
    b = random.randint(0, 255)
    return (r, g, b)

def createblobmask(mask, thresh=True):
    if thresh == True:
        ret, mask = cv2.threshold(mask, 0, 255, cv2.THRESH_BINARY)
    # assume binary mask image with 0, 255 values
    h, w = mask.shape[0], mask.shape[1]
    temp = numpy.zeros((h+2, w+2), dtype=numpy.uint8)*255
    blob_idx = 0
    rects = []
    for j in range(0, h, 16):
        for i in range(0, w, 16):
            d = mask[j][i]
            if d != 255:
                continue
            blob_idx += 1
            if blob_idx == 255:
                break
            retval, rect = cv2.floodFill(mask, temp, (i, j), blob_idx)
            rects.append(rect)
    return blob_idx, mask, rects

def getblob(mask, idx):
    mask = numpy.array(mask, copy=True)
    ret, mask = cv2.threshold(mask, idx-1, 255, cv2.THRESH_TOZERO)
    ret, mask = cv2.threshold(mask, idx, 255, cv2.THRESH_TOZERO_INV)
    ret, mask = cv2.threshold(mask, 0, 255, cv2.THRESH_BINARY)
    return mask

def colormask(mask, color):
    cmask = numpy.zeros((mask.shape[0], mask.shape[1], 3), dtype=numpy.uint8)
    cmask[:,:,0] = numpy.array((mask/255.)*color[0], dtype=numpy.uint8)
    cmask[:,:,1] = numpy.array((mask/255.)*color[1], dtype=numpy.uint8)
    cmask[:,:,2] = numpy.array((mask/255.)*color[2], dtype=numpy.uint8)
    return cmask

def drawblobs(mask, max_blob, w=640, h=480, colors=None):
    img = numpy.zeros((h, w, 3), dtype=numpy.uint8)
    for i in range(1, max_blob+1):
        bmask = getblob(mask, i)
        if colors == None:
            r, g, b = get_rcol()
        else:
            r, g, b = colors[i]
        cmask = colormask(bmask, (r, g, b))
        img = numpy.maximum(img, cmask)
    return img

def overlapblob(curr_mask, prev_mask):
    # assuming 0, 255 binary
    csum = numpy.sum(curr_mask)
    psum = numpy.sum(prev_mask)
    t_mask = numpy.minimum(curr_mask, prev_mask)
    tsum = numpy.sum(t_mask)
    return float(tsum)/float(csum)

def main():
    img = numpy.zeros((480, 640), dtype=numpy.uint8)
    cv2.circle(img, (100, 100), 50, 100, -1)
    cv2.circle(img, (200, 100), 30, 1, -1)
    cv2.circle(img, (100, 200), 30, 1, -1)
    cv2.circle(img, (400, 100), 50, 1, -1)
    #cv2.rectangle(img, (100, 100), (102, 102), 255, -1)
    (idx, img, rects) = createblobmask(img)
    #img = getblob(img, 35)
    img = drawblobs(img, idx)
    cv2.imshow('IMG', img)
    cv2.waitKey()

if __name__ == '__main__':
    main()
