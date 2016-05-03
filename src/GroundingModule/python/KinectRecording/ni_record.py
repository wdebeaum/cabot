from ni2 import *
import cPickle
import time
import sys
from os import path
from os import makedirs

def save_frame(filename, frame):
    f = open(filename, 'w')
    cPickle.dump(frame, f, cPickle.HIGHEST_PROTOCOL)
    f.close()

def capture_data(dir_prefix, c_lock, raw_data):
    if path.isdir(dir_prefix):
        print "Directory already exists. Specify a new one or move the existing one."
        raise IOError
    makedirs(dir_prefix)
    data = process_raw_data(raw_data)
    frame_no = 0
    while True:
        frame = read_frame(c_lock, data)
        num_kinects = frame['num_kinects']
        num_users = frame['num_users']

        if num_kinects > 0:
            if frame_no == 0:
                print "[capture.py] CAPTURE STARTED"
            curr_time = int(time.time() * 100)
            filename = dir_prefix + "/ni2-" + ("%06d" % frame_no)\
                       + "-" + str(curr_time) + ".pkl"
            save_frame(filename, frame)
            frame_no += 1

        show_frame(frame)

def main():
    directory = sys.argv[1]#raw_input("Enter the directory to store the data in: ")
    if path.isdir(directory):
        print "Directory already exists. Specify a new one or move the existing one."
        sys.exit(-1)
    raw_input("Press enter to begin recording")
    cap, c_exit, c_lock, raw_data = start_capture()
    try:
        capture_data(directory, c_lock, raw_data)
    except KeyboardInterrupt:
        pass
    except IOError:
        print "[capture.py] Error: Unable to read directory or file"
    stop_capture(cap, c_exit)

if __name__ == '__main__':
    main()
