#!/bin/bash

# Assinging colors for message output
RED='\033[0;31m'
NC='\033[0m'

# Get directory in which this script is being called
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

echo -e "${RED} Your MF.matR clone is installed locally in${NC} $DIR\n"

# Creating shell function to avoid full path
	echo -e "If you're running this script for the first time in this terminal session using the\n 
	full path for this script, now assinging a shell function with the the name 'register'. \n
	For the rest of this Terminal session you can cd to the directory you'd like\n 
	to register videos and simply use the command 'register'.\n"
	register() {
		$DIR/register_auto.sh "$@"
	}
	export -f register