#!/bin/bash

if [ ! -d static/js ]; then
  mkdir static && mkdir static/js
fi

browserify dev/js/index.js -o static/js/main.js
