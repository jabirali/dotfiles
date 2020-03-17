#!/usr/bin/env bash

# Specify where to find Notmuch config.
export NOTMUCH_CONFIG=~/.config/notmuch.ini

# Exit as soon as one of the commands fail.
set -e

# Check that we have an internet connection.
if ! ping -w 1 -W 1 -c 1 google.com; then
    echo "No internet connection."
    exit
fi

# Update maildir based on database.
afew --move-mails --all -v

# Sync maildir with servers via imap.
offlineimap

# Update database based on maildir.
notmuch new

# Filter and tag any new mail.
afew --tag --new -v
