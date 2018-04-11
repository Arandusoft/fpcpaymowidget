#!/bin/sh
~/development/lazarus/lazbuild paymowidget.lpi --bm=Release-macOS64 -B
strip ./bin/x86_64-darwin/paymowidget
cp ./bin/x86_64-darwin/paymowidget ./mac/x86_64-darwin/paymowidget.app/Contents/MacOS/paymowidget