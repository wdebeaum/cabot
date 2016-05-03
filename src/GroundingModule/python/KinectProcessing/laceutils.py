import multiprocessing as mp
import numpy
import ctypes
import math
import glob
import cPickle

def mp2np(arr):
    _ctypes_to_numpy = {
            ctypes.c_char : numpy.int8,
            ctypes.c_wchar : numpy.int16,
            ctypes.c_byte : numpy.int8,
            ctypes.c_ubyte : numpy.uint8,
            ctypes.c_short : numpy.int16,
            ctypes.c_ushort : numpy.uint16,
            ctypes.c_int : numpy.int32,
            ctypes.c_uint : numpy.int32,
            ctypes.c_long : numpy.int32,
            ctypes.c_ulong : numpy.int32,
            ctypes.c_float : numpy.float32,
            ctypes.c_double : numpy.float64
    }
    address = arr._wrapper.get_address()
    size = arr._wrapper.get_size()
    dtype = _ctypes_to_numpy[arr._type_]
    class Dummy(object): pass
    d = Dummy()
    d.__array_interface__ = {
            'data': (address, False),
            'typestr': '|u1',
            'shape': (size,),
            'strides': None,
            'version': 3
    }
    return numpy.asarray(d).view(dtype=dtype)

def skel2depth(x, y, z, rad):
    c_a = math.cos(-rad)
    s_a = math.sin(-rad)
    nX = x
    nY = z*s_a+y*c_a
    nZ = z*c_a-y*s_a
    dX = .5+nX*(285.63/nZ)/320.
    dY = .5-nY*(285.63/nZ)/240.
    return int(dX*320), int(dY*240)

def depth2skel(x, y, d, rad):
    c_a = math.cos(rad)
    s_a = math.sin(rad)
    fZ = float(d)/1000.
    fX = (float(x)/320. - .5) * (3.501e-3 * fZ) * 320
    fY = (.5 - float(y)/240.) * (3.501e-3 * fZ) * 240
    nX = fX
    nY = fZ*s_a+fY*c_a
    nZ = fZ*c_a-fY*s_a
    return nX, nY, nZ

def conv(ix, iy, iz, deg1, deg2):
    c_a = math.cos(math.radians(deg1))
    s_a = math.sin(math.radians(deg1))
    fx, fy, fz = -ix, iy, iz
    nx, ny, nz = fz*s_a+fx*c_a, fy, fz*c_a+fx*s_a
    c_b = math.cos(math.radians(deg2))
    s_b = math.sin(math.radians(deg2))
    fx, fy, fz = nx, ny, nz
    nx, ny, nz = fy*s_b+fx*c_b, fy*c_b+fx*s_b, fz
    return nx, ny, nz

def read_filelist(datadir):
    filelist = []
    for name in glob.glob(datadir + "/*.pkl"):
        filelist.append(name)
    filelist.sort()
    return filelist

def read_pkl_frame(filename):
    f = open(filename, 'rb')
    frame = cPickle.load(f)
    f.close()
    return frame
