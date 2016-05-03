import sys
import socket
import glob
import time
import os

start_time= -1
end_time = -1

def get_files_for_time(directory, start_time, end_time):
    start_filenumber = long(start_time)
    end_filenumber = long(end_time) 
    dir_list = os.listdir(directory)
    final_file_list = []
    for filename in dir_list:
        unix_time = long(filename.split('-')[2].split('.')[0])
        if (unix_time > start_filenumber and
            unix_time < end_filenumber):
            final_file_list.append(filename)

    final_file_list.sort()
    return final_file_list

def get_send_message(list_sock):
    
    sock, addr = list_sock.accept()
    data = sock.recv(1024)
    data_split = data.split('\n')
    start_time = data_split[0]
    end_time = data_split[1]
    sock.close()
    return (start_time, end_time)
    
def initialize_socket(port):
    list_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    list_sock.bind(('',port))
    list_sock.listen(2)
    print list_sock.getsockname()

    return list_sock

def send_files_to_socket(listen_sock, path, file_list):
    
    for filename in file_list:
        data_sock, addr = listen_sock.accept()
        with open(os.path.join(path,filename),'rb') as f:
            data = f.read()
            data_sock.sendall(data)
            data_sock.close()

    data_sock, addr = listen_sock.accept()
    data_sock.sendall('end')
    data_sock.close()

if __name__ == '__main__':
    # 22345 - port for receiving send message
    # 22346 - port for data
    message_port = 22345
    data_port = 22346
    path = sys.argv[1]
    receiver_host_name = sys.argv[2]

    listening_message_socket = initialize_socket(message_port)
    listening_data_socket = initialize_socket(data_port)
    
    while True:
        (start_time, end_time) = get_send_message(listening_message_socket)
        print 'Start: ', start_time, ' End: ', end_time
        file_list = get_files_for_time(path, start_time, end_time)
        print file_list
        truncated_file_list = file_list[0:min(len(file_list),11)]
        send_files_to_socket(listening_data_socket,path,truncated_file_list)
        
