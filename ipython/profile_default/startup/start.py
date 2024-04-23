# IPython startup routine.

import os
import sys

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


# Quick switching of styles.
def catppuccin():
    plt.style.use("~/.config/ipython/profile_default/startup/catppuccin.mplstyle")


def octagon():
    plt.style.use("~/.config/ipython/profile_default/startup/octagon.mplstyle")


def revtex():
    plt.style.use("~/.config/ipython/profile_default/startup/revtex.mplstyle")


def dracula():
    plt.style.use("~/.config/ipython/profile_default/startup/dracula.mplstyle")


# Plots should not block ipython.
# plt.ion()

# Pick a default colorscheme.
# dracula()

# Don't show toolbar.
# mpl.rcParams["toolbar"] = "None"

# Fresh start.
# os.system("clear")


# Determine dark mode.
def _dark_mode():
    """Checks whether MacOS is running in dark mode."""
    import subprocess

    cmd = "defaults read -g AppleInterfaceStyle"
    p = subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True
    )
    return bool(p.communicate()[0])


mpl.rcParams.update(mpl.rcParamsDefault)
mpl.style.use("bmh")
if _dark_mode():
    mpl.style.use("dark_background")
