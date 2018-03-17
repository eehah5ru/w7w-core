#!/bin/bash

exiftool "$1" | grep -e "^Title\\s\+:" -e "^Description\\s\+:" -e "^Creator\\s\+:" | sort
