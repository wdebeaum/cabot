import sys
import socket

if __name__ == '__main__':
    port = 22345
    filenames = ['ni2-000987-135811522284.pkl',
                 'ni2-000988-135811522290.pkl',
                 'ni2-000989-135811522296.pkl',
                 'ni2-000987-135811522284.pkl',
                 'ni2-000988-135811522290.pkl',
                 'ni2-000989-135811522296.pkl']
    for filename in filenames:
        sock = socket.socket()
        sock.connect(("localhost",port))
        with open(filename, "rb") as f:
            data = f.read()
        
        print len(data)
        sock.sendall(data)
        sock.close()
        
    sock = socket.socket()
    sock.connect(("localhost",port))
    sock.send('end')
    sock.close()
    
    
