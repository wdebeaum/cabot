import sys
import socket

if __name__ == '__main__':
    port = 22346

    sock = socket.socket()
    sock.connect(("localhost",port))
    result = ''
    while True:
        data = sock.recv(1024)
        if not data:
            break
        result += str(data)
        
    print result
        
    sock.close()
