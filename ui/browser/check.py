#!/usr/bin/env python

import os
import sys


## FIGURE OUT NEW MODIFICATION TIME

def mostRecentModification(directory):
	mostRecent = 0

	for dirpath, dirs, files in os.walk(directory):
		for f in files:
			lastModified = os.path.getmtime(dirpath + '/' + f)
			mostRecent = max(int(lastModified), mostRecent)

	return mostRecent


srcTime = mostRecentModification('ui/src')
assetTime = mostRecentModification('ui/assets')
mostRecent = max(srcTime, assetTime)


## FIGURE OUT OLD MODIFICATION TIME

with open('ui/last-modified', 'a') as handle:
	pass


prevMostRecent = 0


with open('ui/last-modified', 'r+') as handle:
	line = handle.read()
	prevMostRecent = int(line) if line else 0


## TOUCH FILES IF NECESSARY

if mostRecent > prevMostRecent:
	print "+------------------------------------------------------------+"
	print "| Some ui/ code changed. Touching src/Reactor/StaticFiles.hs |"
	print "| to trigger a recompilation of the Template Haskell stuff.  |"
	print "+------------------------------------------------------------+"
	os.utime('src/Reactor/StaticFiles.hs', None)
	with open('ui/last-modified', 'w') as handle:
		handle.write(str(mostRecent))
