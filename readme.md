Rory is a tool for deploying artifacts to a server.

```bash
# Build and start the server
(cd rory; stack build; stack exec rory)

echo "hello" > /tmp/abc

curl -F xyz=@/tmp/abc http://localhost:3000

cat /tmp/upload
# Output: hello
```

Rory-server responds to SIGHUP by reloading the config file.

Log output goes to the systemd journal.

The name "Rory" has no meaning; I just happening to be watching
The Gilmore Girls while I created this.
