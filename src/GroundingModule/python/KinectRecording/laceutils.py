import multiprocessing as mp
import numpy
import ctypes

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
