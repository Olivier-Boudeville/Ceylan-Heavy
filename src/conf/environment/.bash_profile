# This file is sourced by bash when you log in interactively:

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	   . ~/.bashrc
fi


# To disable the 'capslock' key:
setxkbmap -option ctrl:nocaps 2>/dev/null

# To reset it (ex: if stuck in uppercase): setxkbmap -option

# To disable the 'insert' key:
xmodmap -e 'keycode 118 =' 2>/dev/null

# To disable the 'numlock' key:
# (commented out, as was wrong in VMWare)
#xmodmap -e 'keycode 77 = NoSymbol Num_Lock' 2>/dev/null
