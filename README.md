# Not a Connect4

A client-server application to compare AI playing Connect4.

[application heroku](http://not-a-connect4.herokuapp.com/)

![](tmp/nac4.gif)

## Network protocol

- using websockets
- text messages terminated by " \n"
- username (no whitespace: ' ', \t, \n, \r)
- BOARD: (. | R | Y)x42                         ; line0, line1...
- PLAYER: R | Y
- MOVE: 0-6                                     ; column to play
- STATUS : WinR | WinY | Tie | PlayR | PlayY
- invalid move -> cancel current game then terminate
- disconnection -> cancel current game then terminate
- s2c/c2s: server-to-client/client-to-server

```
c2s: connect <username> \n
s2c: connected [welcome message] \n
s2c: not-connected [error message] \n

s2c: newgame <user> <user> \n
s2c: genmove BOARD COLOR \n             ; color of the current player
c2s: playmove MOVE \n
s2c: endgame BOARD COLOR STATUS \n
```

## Deploy server to Heroku

- build a docker image:

```
nix-build docker.nix
docker load -i result
```

- upload the image to Heroku:

```
heroku login
heroku container:login
heroku create not-a-connect4

docker tag not-a-connect4:latest registry.heroku.com/not-a-connect4/web
docker push registry.heroku.com/not-a-connect4/web
heroku container:release web --app not-a-connect4
```

- (open, shutdown, logs):

```
heroku open --app not-a-connect4
heroku ps:scale web=0 --app not-a-connect4
heroku logs --app not-a-connect4
```

## Run client:

- using stack:

```
stack run nac4-client not-a-connect4.herokuapp.com 80 myname mc 64
```

- using nix+cabal:

```
nix-shell --run "cabal run nac4-client not-a-connect4.herokuapp.com 80 myname mc 64"
```

