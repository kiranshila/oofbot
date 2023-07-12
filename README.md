# oofbot - A simple, silly bot for Discord, written in Clojure

## Add to your server!

Use the link [here](https://discord.com/api/oauth2/authorize?client_id=700527695804629002&permissions=67584&scope=bot%20applications.commands) to add oofbot to your server. My uptime has been pretty good.

## Setup

### Building

Install the Clojure CLI and create the uberjar with

```
clojure -T:build uber
```

This will create an artifact in the `target` folder.

### Running

The bot requires an api token located in `token.txt`, located next to the jar. Then just run

```
java -jar oofbot.jar
```

## Usage

The bot will keep tabs on every user who responds `oof` (with an arbitrary number of `o`'s) and the user for which the oofs were in response to. If multiple users respond `oof`, the bot tallys the first non-oofed message as the oof-ee.

Additionally, adding an `:OOF:` reaction to a post will also count the oof-er and the oof-ee as well. You will have to upload a custom reaction, such as this [one](https://discordemoji.com/emoji/OOF).

### Commands

Oofbot uses slash commands:

To list the current oof tally - `/oof top`
To list your oof tally and rank - `/oof stats`

#### Server Owner

To set the oof tally leaderboard size - `/oof topcount <count>`
