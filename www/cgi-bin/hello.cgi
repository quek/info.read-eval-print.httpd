#!/bin/sh

echo "Content-type: text/html"
echo ""
echo "<html><head><title>Hello!</title></head>"
echo "<body>"
echo "<h1>Hello, World!</h1>"
echo "<pre>"
env
echo "</pre>"
echo "</body></html>"
