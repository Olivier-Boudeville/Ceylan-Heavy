# Some color definitions, to be more readable.

# See : Ceylan-x.y/src/code/scripts/termUtils.sh 


# if your terminal does not support color, set term_supports_color to false.
# see also : function updateColorSupport of Ceylan-x.y/src/code/scripts/termUtils.sh 
term_supports_color = true


# Text color codes :

black_text   = 30
red_text     = 31
green_text   = 32
yellow_text  = 33
blue_text    = 34
magenta_text = 35
cyan_text    = 36
white_text   = 37


# Background color codes :

black_back   = 40
red_back     = 41
green_back   = 42
yellow_back  = 43
blue_back    = 44
magenta_back = 45
cyan_back    = 46
white_back   = 47


# Text attributes :

none_att       = 00
bold_att       = 01
underscore_att = 04
blink_att      = 05
reverse_att    = 07
concealed_att  = 08

default_att    = $(none_att)


# Pre defined text styles :


ifeq ($(term_supports_color),true)

default_style   = [$(default_att);$(white_text);$(black_back)m
title_style     = [$(default_att);$(magenta_text);$(black_back)m
item_style      = [$(default_att);$(cyan_text);$(black_back)m
command_style   = [$(default_att);$(cyan_text);$(black_back)m
make_style      = [$(default_att);$(green_text);$(black_back)m
compile_style   = [$(default_att);$(cyan_text);$(black_back)m
link_style	= [$(default_att);$(magenta_text);$(black_back)m
install_style   = [$(default_att);$(blue_text);$(black_back)m
clean_style     = [$(default_att);$(yellow_text);$(black_back)m
error_style     = [$(default_att);$(white_text);$(red_back)m
warning_style   = [$(default_att);$(yellow_text);$(black_back)m

else

default_style   = ""
title_style     = ""
item_style      = ""
command_style   = ""
make_style      = ""
compile_style   = "" 
link_style	= ""
install_style   = ""
clean_style     = ""
error_style     = ""
warning_style   = ""

endif
