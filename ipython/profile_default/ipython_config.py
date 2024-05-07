# IPython configuration.

import IPython

c = get_config()
c.TerminalInteractiveShell.prompts_class=IPython.terminal.prompts.ClassicPrompts
c.TerminalIPythonApp.display_banner = False
