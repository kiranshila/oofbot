# oofbot - A simple, silly bot for Discord, written in Clojure

## Add to your server!
Use the link [here](https://discord.com/oauth2/authorize?client_id=700527695804629002&permissions=67584&scope=bot) to add oofbot to your server. My uptime has been pretty good.

## Setup
### Building
Install the Clojure CLI and create the uberjar with
```
clojure -A:depstar -m hf.depstar.uberjar Oofbot.jar -C -m oofbot.core
```
### Running
The bot requires an api token located in `token.txt`, located next to the jar. Then just run
```
java -jar Oofbot.jar
```

## Usage
The bot will keep tabs on every user who responds `oof` (with an arbitrary number of `o`'s) and the user for which the oofs were in response to. If multiple users respond `oof`, the bot tallys the first non-oofed message as the oof-ee.

Additionally, adding an `:OOF:` reaction to a post will also count the oof-er and the oof-ee as well. You will have to upload a custom reaction, such as this [one](https://discordemoji.com/emoji/OOF).

To list the commands oofbot can do, menion oofbot or use `<command prefix>oofhelp`.
The defualt command prefix is `!`.

### Commands
To list the current oof tally - `<command prefix>getoofs`
To list your oof tally and rank - `<command prefix>myoofs`
To get the list of commands - `<command prefix>oofhelp` or `@oofbot`

#### Server Owner
To set the oof tally leaderboard size - `<command prefix>topcount <count>`
To change the command prefix - `@oofbot prefix <new command prefix>`
