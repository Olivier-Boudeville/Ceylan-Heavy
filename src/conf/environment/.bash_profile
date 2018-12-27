#
# ~/.bash_profile
#

echo "(bash profile sourced)"

# To disable the 'capslock' key:
setxkbmap -option ctrl:nocaps 2>/dev/null

# To reset it (ex: if stuck in uppercase): setxkbmap -option

# To disable the 'insert' key:
xmodmap -e 'keycode 118 =' 2>/dev/null


# (also: 'pacman -S numlockx')
NUMLOCK_TOOL=$(which numlockx 2>/dev/null)

if [ -x "${NUMLOCK_TOOL}" ]; then
	${NUMLOCK_TOOL} on
fi

# To disable the 'numlock' key:
#
xmodmap -e 'keycode 77 = NoSymbol Num_Lock' 2>/dev/null


[[ -f ~/.bashrc ]] && . ~/.bashrc
