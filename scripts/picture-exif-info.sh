#!/bin/bash

exiftool "$1" | grep -e "^Title" -e "^Description" -e "^Creator" | sort
