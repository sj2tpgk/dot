#!/usr/bin/python3
import argparse, io, os, pathlib, urllib.parse
from http.server import HTTPServer, BaseHTTPRequestHandler

MAX_UPLOAD_SIZE = 100 * 1024 * 1024
SAFE_CHARS = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-")

UPLOAD_ROOT = pathlib.Path(".").resolve()

class FileHandler(BaseHTTPRequestHandler):
    def do_POST(self) -> None:
        path = self.path.lstrip('/')
        try:
            size = int(self.headers.get("Content-Length", ""))
        except Exception:
            return self.error(411, "Length Required")
        if size > MAX_UPLOAD_SIZE:
            return self.error(413, "Payload Too Large")
        self._save_file(path, self.rfile, size)  # will read body later

    def do_GET(self) -> None:
        parsed = urllib.parse.urlparse(self.path)
        path = parsed.path.lstrip('/')
        query = urllib.parse.parse_qs(parsed.query)
        if len(query.get("b", [])) != 1:
            return self.error(400, "Bad Request: missing or multiple b parameters")
        try:
            body = urllib.parse.unquote(query["b"][0]).encode()
        except Exception:
            return self.error(400, "Bad Request: invalid body encoding")
        if len(body) > MAX_UPLOAD_SIZE:
            return self.error(413, "Payload Too Large")
        self._save_file(path, io.BytesIO(body), len(body))

    def _save_file(self, path:str, reader:io.BufferedIOBase, size:int) -> None:
        target = (UPLOAD_ROOT / path).resolve()
        if not target.is_relative_to(UPLOAD_ROOT):
            return self.error(400, "Bad Request: unsafe path")

        # Write to temp file in same directory for atomic rename
        try:
            os.makedirs(target.parent, exist_ok=True)
            with open(target, 'xb') as f:
                remaining = size
                while remaining > 0:
                    chunk = reader.read(min(8192, remaining))
                    if not chunk:
                        return self.error(400, "Bad Request: incomplete body")
                    f.write(chunk)
                    remaining -= len(chunk)
            # Atomic move (works across filesystems on Unix if same device)
            self.log_message(f"Saved {size} bytes to {target}")
            self.send_response(200)
            self.end_headers()
        except FileExistsError:
            return self.error(409, "Conflict: file already exists")
        except Exception:
            return self.error(500, "Internal Server Error")

    def error(self, code:int, message:str) -> None:
        self.log_error(f"Error {code}: {message}")
        self.send_response(code)
        self.send_header('Content-Type', 'text/plain')
        self.end_headers()
        self.wfile.write(f"{code} {message} \n".encode())

def main() -> None:
    description = """To upload files:

curl 'localhost:8000/path/to/file?b=BODY'
curl 'localhost:8000/path/to/file' --data-binary @FILE"""
    parser = argparse.ArgumentParser(description=description, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("-p", "--port", type=int, default=8000)
    args = parser.parse_args()
    print(f"Server at :{args.port}. {description}")
    server = HTTPServer(('', args.port), FileHandler)
    server.serve_forever()

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        pass
