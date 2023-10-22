# Configuration of the iPython interpreter.
c = get_config()
c.InteractiveShellApp.exec_lines = [r"%load_ext autoreload", r"%autoreload 2"]
c.TerminalInteractiveShell.extra_open_editor_shortcuts = True
