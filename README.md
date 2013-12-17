# Rollex

Goal: Extendable hot swappable IRC bot.

## OTP structure:

```
[ Rollex app ]
  |
  [IRC supervisor]
   |  |     |   |
   i1 |     i2  |
      |
 [Msg worker supervisor]
    |   |
    m1  m2

i = IRC worker, one per server

[ IRC worker ]
        +--------> [ Msg worker supervisor ]
                     |   |   |   |   |
                     m1  m2  m3  m4  mn
```

### IRC workers:

  * parameters: server uri, config

  * config: user info, channels, modules

    - user info: nick, user, real name
    - channels: list of channels to join
    - modules: [{Some.Worker.Module, interested_in}, {...}, ...]
    - interested_in: list of atoms of messages that this module is
      interested in. Ex: [:all], [:ping, :privmsg]

#### Basic flow

  0. On startup the Message worker supervisor is started,
    and all Message workers are created and added to it

  1. IRC process connect, logs in, start reading from server.

  2. Receive line from server, parse it into a IRCCommand struct.
  
  3. Send all modules interested in this command type an async message with
    the command and the IRCState (containing config, server socket etc).

  4. GOTO 2

### Message workers:

parameters: none

Will receive messages of the type that it was interested in together
with IRCState
{IRCCommand, IRCState}
Can do whatever with it...

Example workers:

  * "Pong" worker that is only interested in :ping commands and sends
    "PONG" to the server
  * "Channel logger" worker that is interested in :privmsg, :mode,
    :join, :part, :topic, :kick, :quit commands and appends them for each
    channel to a specific file (queue and database?)
  * "Custom command" worker that is interested in :privmsg commands.
    Checks if the text of the command is prefix by a command character and
    then parses the rest to deduce some action. Can be as advanced as
    needed (starting another supervisor subtree with additional
    custom-command workers for example)
