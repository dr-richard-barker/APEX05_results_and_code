# Tiny static server for local preview of the docs/ site. Honours $PORT.
import os, functools, http.server, socketserver
root = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "docs")
port = int(os.environ.get("PORT", "4173"))
Handler = functools.partial(http.server.SimpleHTTPRequestHandler, directory=root)
with socketserver.TCPServer(("", port), Handler) as httpd:
    print(f"serving docs/ on :{port}")
    httpd.serve_forever()
