'''
This file is used by offlineimap to fetch passwords from the desktop keyring.
I'm using secret-tool over the keyring library so I can reuse the same stored
password for offlineimap and msmtp, while avoiding an unnecessary dependency.
'''

from subprocess import Popen, PIPE


def keyring(username):
    '''Locate and decrypt a password stored in the desktop keyring.'''

    try:
        ps = Popen('secret-tool lookup user ' + username, shell=True, stdout=PIPE)
        pw = ps.communicate()[0]
        if ps.returncode or not pw:
            pw = None
    except:
        pw = None
    finally:
        return pw
