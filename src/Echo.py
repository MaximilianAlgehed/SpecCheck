# Echo server program
import socket
import sys

HOST = "localhost"               # Symbolic name meaning all available interfaces
PORT = int(sys.argv[1])               # Arbitrary non-privileged port
s = None
for res in socket.getaddrinfo(HOST, PORT, socket.AF_UNSPEC, socket.SOCK_STREAM, 0, socket.AI_PASSIVE):
    af, socktype, proto, canonname, sa = res
    s = socket.socket(af, socktype, proto)
    s.bind(sa)
    s.listen(1)
    break
if s is None:
    sys.exit(1)

conn, addr = s.accept()
msg = ""
while True:
    char = conn.recv(1)
    msg += char
    if char == "\n":
        break

conn.send(msg)
conn.close()
s.close()
