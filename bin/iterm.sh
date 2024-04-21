#!/bin/bash

# This script feeds its STDIN to the most recent iTerm2 pane. Escape
# codes are used to turn on and off paste bracketing; this prevents
# e.g. iPython from wreaking havoc by auto-indenting Python code.

osascript<<END
tell application "iTerm"
    tell current window
        tell current session
            write text "[200~$(</dev/stdin)[201~"
        end tell
    end tell
end tell
END
