#!/bin/sh
~/development/lazarus/lazbuild paymowidget.lpi --bm=Release-macOS64 -B
strip ./bin/x86_64-darwin/paymowidget
mv ./bin/x86_64-darwin/paymowidget ./bin/x86_64-darwin/paymowidget.app/Contents/MacOS/paymowidget