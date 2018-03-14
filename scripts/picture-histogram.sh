#!/bin/bash

convert "$1" +dither -colors 5 -define histogram:unique-colors=true -format "%c" histogram:info: | sort -r
