#!/bin/bash
stack run build-site -- json
# (cd site && npm run build && firebase deploy)
(cd site && npm run build)
