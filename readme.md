# Skype UI for Emacs Users

This program works under only **Linux** Desktop Environment (d-bus).
Using the d-bus channel, `skype.el` communicates with skype application and
provides some user interface buffers to integrate skype into Emacs work-flow.

Currently, author doesn't use Skype as a primary SMS and has no plan for maintenance.

# Usage

To use this program, locate this file to load-path directory,
and add the following code to your `init.el`.

```el
(require 'skype)
(setq skype--my-user-handle "your skype account")
```

## UI Command


You can start skype action with following commands:

- Select chat and open the chat buffer
    - `skype--open-recent-chat-buffer-command`
    - `skype--open-bookmarked-chat-buffer-command`
    - `skype--open-missed-chat-buffer-command`
- Select a user and open the new chat
    - `skype--open-new-chat-command`
- Change your status: Online, Offline, etc...
    - `skype--set-my-status-command`
- Set your mood text
    - `skype--set-my-mood-text-command`
- Show a list of all users with their status
    - `skype--open-all-users-buffer-command`

You can bind them to some key sequence.

If you have anything.el, bind `skype--anything-command` to key like
`(global-set-key (kbd "M-9") 'skype--anything-command)`, you can
select something from all commands and entries quickly.

## Chat buffer

You can use following key bind:

| Key | Command                 |
|-----|-------------------------|
| n,j | move next message       |
| p,k | move previous message   |
| a   | send new message        |
| c   | copy this message       |
| u   | update buffer           |
| U   | refresh buffer          |
| q   | bury buffer             |
| Q   | kill buffer             |
| A   | reply to this message   |
| B   | toggle bookmark         |
| R   | toggle auto read        |
| +   | add new chat member     |
| L   | leave me from this chat |
| t   | set chat topic          |
| C   | clear missed flag       |
| E   | edit this message       |
| l   | open chat member buffer |
|     |                         |
| m   | open new chat           |
| M   | set my mood text        |

## Message buffer

You can use following key bind:

| Key     | Command                 |
|---------|-------------------------|
| C-c C-c | send                    |
| C-c C-q | quit buffer             |
| C-c C-e | select emoticon         |
| C-c C-p | recall previous message |
| C-c C-n | recall next message     |
| C-S-b   | chat scroll bottom      |
| C-S-p   | chat scroll down        |
| C-S-j   | chat scroll down        |
| C-S-n   | chat scroll up          |
| C-S-k   | chat scroll up          |
|         |                         |
| C-S-q   | chat quit               |
| C-S-c   | chat clear missed       |

# License

GPL v3
