# This file is run on startup by Jupyter (VSCode) and IPython (terminal).
# Place settings here that should be used for every interactive session.

# Import common libraries.
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt

# Quick switching of styles.
def catppuccin():
    plt.style.use("~/.config/ipython/profile_default/startup/catppuccin.mplstyle")

def octagon():
    plt.style.use("~/.config/ipython/profile_default/startup/octagon.mplstyle")

def revtex():
    plt.style.use("~/.config/ipython/profile_default/startup/revtex.mplstyle")

# Plots should be vector graphics.
from IPython.display import set_matplotlib_formats
set_matplotlib_formats('svg')

# Plots should not block ipython.
plt.ion()

# Pick a default colorscheme.
catppuccin()
