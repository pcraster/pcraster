If you create new xpm's you might want to run $DEVENV's postprocessxpms.py
script to convert char* to char const*. This prevents gcc 4.2+ warning
messages.
