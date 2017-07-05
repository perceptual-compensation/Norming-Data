import os
import json

try:
    filePath = os.path.split(__file__)[0] + "/"
except:
    filePath = ""

with open(filePath + "normingIbexResults.txt", "rt") as f:
    results = f.read().split("\n")

tableFile = open(filePath + "table.txt", "wt")

tableFile.close()