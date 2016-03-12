Rory is a tool for deploying artifacts to a server.

```bash
# Build and start the server
(cd rory; stack build; stack exec rory-server)

echo "hello" > /tmp/abc

curl -F xyz=@/tmp/abc http://localhost:3000

cat /tmp/upload
# Output: hello
```

The name "Rory" has no meaning; I just happening to be watching
The Gilmore Girls while I created this.
