'''
This file is used by `offlineimap` to decode usernames and passwords.
'''

import keyring
import codecs


def get_username(code):
    '''
    Decode my username. I don't want my plaintext email address in my dotfiles
    repo due to bots and spam. The first-order workaround is a ROT13 encoding.
    '''

    return codecs.decode(code, 'rot_13')


def get_password(code):
    '''
    Fetch my password. I don't want to store plaintext passwords anywhere ever,
    so just asking the OS keyring to store them for us seems like a good idea.
    '''

    return keyring.get_password('offlineimap', get_username(code))
