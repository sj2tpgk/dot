#!/usr/bin/env python3

def read1(f):
    b = f.read(1)
    if len(b) == 0: return
    if b == b'i':
        o = b''
        while True:
            c = f.read(1)
            if c == b'e': break
            o += c
        # print("int ", o)
        return int(o)
    elif b == b'l':
        o = []
        while True:
            c = f.read(1)
            if c == b'e': break
            f.seek(-1, 1)
            o.append(read1(f))
        # print("list", o)
        return o
    elif b == b'd':
        o = {}
        while True:
            c = f.read(1)
            if c == b'e': break
            f.seek(-1, 1)
            k = read1(f)
            v = read1(f)
            o[k] = v
        # print("dict", o)
        return o
    else:
        l = b
        while True:
            c = f.read(1)
            if c == b':': break
            l += c
        l = int(l)
        o = b""
        for _ in range(l):
            c = f.read(1)
            # o += chr(c[0])
            o += c
        # print("str ", o)
        # return repr(o) # b'...' is output as "b'...'" (a string starting with b)
        # return o.decode("utf-8", errors="ignore")
        # return o.decode("utf-8", errors="surrogateescape")
        return decode2(o)

def decode2_char(b:bytes) -> tuple[str, bytes]:
    # Returns a str of length 1 and remaining bytes
    for i in [1, 2, 3, 4]:
        try:
            return b[:i].decode("utf-8", "strict"), b[i:]
        except:
            pass
    return chr(b[0]), b[1:]

def decode2(b:bytes):
    out = ""
    while len(b) > 0:
        char, b = decode2_char(b)
        out += char
    return out

def encode2(s:str):
    out = b""
    for c in s:
        n = ord(c)
        if n < 256:
            out += bytes([n])
        else:
            out += c.encode("utf-8", "strict")
    return out

def write1(f, x):
    if type(x) is str:
        # enc = eval(x) # convert "b'...'" (a string containing a representation of a bytes) to b'...' (bytes)
        # enc = bytes([ord(y) for y in x])
        enc = encode2(x)
        assert decode2(enc) == x
        f.write(str(len(enc)).encode() + b":" + enc)
    elif type(x) is int:
        f.write(f"i{x}e".encode())
    elif type(x) is list:
        f.write(b"l")
        for y in x:
            write1(f, y)
        f.write(b"e")
    elif type(x) is dict:
        f.write(b"d")
        for k in sorted(x):
            write1(f, k)
            write1(f, x[k])
        f.write(b"e")

def main():
    import json, sys, os
    argv = sys.argv

    if 2 <= len(argv) <= 3 and argv[1] == "enc":
        jso = json.load(sys.stdin.buffer if len(argv) == 2 else open(argv[2], "rb"))
        write1(sys.stdout.buffer, jso)

    elif 2 <= len(argv) <= 3 and argv[1] == "dec":
        print(json.dumps(read1(sys.stdin.buffer if len(argv) == 2 else open(argv[2], "rb")), ensure_ascii=False, indent=2))

    elif 2 <= len(argv) <= 4 and argv[1] == "decdir":
        if os.path.normpath(argv[2]) == os.path.normpath(argv[3]):
            raise Exception("INDIR and OUTDIR must be different")
        for i in os.listdir(argv[2]):
            print(i)
            with open(os.path.join(argv[2], i), "rb") as f:
                with open(os.path.join(argv[3], i), "w") as g:
                    json.dump(read1(f), g, ensure_ascii=False, indent=2)

    else:
        print("Usage: python bedecode.py enc [FILE]")
        print("       python bedecode.py dec [FILE]")
        print("       python bedecode.py decdir INDIR OUTDIR")
        print("Without FILE, reads from stdin and writes to stdout")
        print("With decdir, parse each file in INDIR and write to OUTDIR")

if __name__ == "__main__":
    main()
