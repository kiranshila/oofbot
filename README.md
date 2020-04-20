# oofbot - A simple, silly bot for Discord, written in Clojure

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

To display the current oof tally, send the `!getoofs` message.
