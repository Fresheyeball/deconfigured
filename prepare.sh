#!/bin/bash

# This script should prepare our static assets for use.

npm update
npm install
bash ./runBrowserify.sh
