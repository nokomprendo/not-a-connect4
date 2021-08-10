# Not a Connect4

A client-server application to compare AI playing Connect4.

[application heroku](http://not-a-connect4.herokuapp.com/)

![](tmp/nac4.gif)

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
stack run nac4-client not-a-connect4.herokuapp.com 80 player-mc mc 10000
```

- using nix+cabal:

```
nix-shell --run "cabal run nac4-client not-a-connect4.herokuapp.com 80 player-mc mc 10000"
```

## Network protocol

[Augmented Backus–Naur form](https://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_form)

```
message     = message-connect               ; client ----> server
            / message-connected             ; client <---- server
            / message-notconnected          ; client <---- server
            / message-newgame               ; client <---- server
            / message-genmove               ; client <---- server
            / message-playmove              ; client ----> server
            / message-endgame               ; client <---- server


message-connect         = "connect" SP user CRLF

message-connected       = "connected" [SP info] CRLF

message-notconnected    = "not-connected" [SP info] CRLF

message-newgame         = "newgame" SP user SP user CRLF    

message-genmove         = "genmove" SP board SP color SP status SP time CRLF

message-playmove        = "playmove" SP move CRLF

message-endgame         = "endgame" SP board SP color SP status SP battlestatus CRLF


user            = *(ALPHA / DIGIT / "-" / "_")

info            = *(ALPHA / DIGIT / SP)

board           = 42*("." / color)      ; line 0, line 1, etc

color           = "R" / "Y"             ; color of the client receiving the message

time            = *DIGIT "." DIGIT

move            = "0" / "1" / "2" / "3" / "4" / "5" / "6"   ; column index

status          = "WinR" / "WinY" / "Tie" / "PlayR" / "PlayY"

battlestatus    = "Ok" / "Timeout" / "Disconnected"
```

