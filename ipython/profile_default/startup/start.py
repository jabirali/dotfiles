# IPython startup routine.

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
plt.ion()

# Pick a default colorscheme.
dracula()

# Don't show toolbar.
mpl.rcParams["toolbar"] = "None"
