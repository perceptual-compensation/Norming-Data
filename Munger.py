# -*- coding: utf-8 -*-
"""
Created on Tue Dec 27 16:39:33 2016

@author: PWillis
"""
import os
import json

try:
    filePath = os.path.split(__file__)[0] + "/"
except:
    filePath = ""

with open(filePath + "results", "rt") as f:
    results = f.read().split("\n")
jsonFile = open(filePath + "results.json", "wt")
subjectFile = open(filePath + "subjects.txt", "wt")
tableFile = open(filePath + "table.txt", "wt")

sentenceType = {}
for i in ["we're out of flour", "I put the toolbox in the shed", "I'm on the phone", "another pile of papers to grade"]:
    sentenceType[i] = "decl"
for i in ["you don't know how to tango", "I thought apples were in the sunflower family", "she's never done that before", "I've never heard that before"]:
    sentenceType[i] = "conf"
for i in ["he really likes it", "a package arrived for you" , "you can't be serious right now", "there are a lot of cakes to choose from"]:
    sentenceType[i] = "exclam"

struc = {}
subjectFile.write("Subject\tUpload Time\tCriterion\tValue\n")
tableFile.write("Subject\tItem\tParticle\tType\tStimulus\tEvent\tTime\tPlayback Action\tPlayback Location\tQuestion\tAnswer\tScenario\n")
for line in results:
    if line and line[0] != "#":
        spl = line.split(",")
        if spl[2] == "Form":
            struc[spl[1]] = {"Time" : int(spl[0]), "Criterion" : spl[7], "Value" : spl[8], "Items" : {}}
        else:
            if not struc[spl[1]]["Items"].get(spl[3]):
                struc[spl[1]]["Items"][spl[3]] = {"Particle" : spl[5], "Playback" : [], "Ratings" : []}
            if len(spl) == 10:
                struc[spl[1]]["Items"][spl[3]]["Playback"].append({"Time" : int(spl[9]), "Action" : spl[7], "Location" : float(spl[8])})
            elif len(spl) == 8:
                struc[spl[1]]["Items"][spl[3]]["Stimulus"] = spl[7]
                struc[spl[1]]["Items"][spl[3]]["Type"] = sentenceType.get(" ".join(spl[7].split(" ")[1:]))
            elif len(spl) == 9:
                struc[spl[1]]["Items"][spl[3]]["Scenario"] = spl[7]
            elif len(spl) == 11:
                if spl[8] != "NULL":
                    struc[spl[1]]["Items"][spl[3]]["Ratings"].append({"Time" : int(spl[10]), "Question" : spl[7], "Answer" : spl[8]})

jsonFile.write(json.dumps(struc, sort_keys=True, indent=4))
jsonFile.close()

for subject, subContents in struc.items():
    subjectFile.write("\t".join([subject, str(subContents["Time"]), subContents["Criterion"], subContents["Value"]]) + "\n")
    for item, itemContents in subContents["Items"].items():
        tableFile.write("\t".join([subject, item, itemContents["Particle"], str(itemContents["Type"]), itemContents["Stimulus"], "Scenario", "", "", "", "", "", itemContents["Scenario"]]) + "\n")
        for event in itemContents["Playback"]:
            tableFile.write("\t".join([subject, item, itemContents["Particle"], str(itemContents["Type"]), itemContents["Stimulus"], "Playback", str(event["Time"]), event["Action"], str(event["Location"]), "", "", ""]) + "\n")
        for event in itemContents["Ratings"]:
            tableFile.write("\t".join([subject, item, itemContents["Particle"], str(itemContents["Type"]), itemContents["Stimulus"], "Rating", str(event["Time"]), "", "", event["Question"], event["Answer"], ""]) + "\n")

subjectFile.close()
tableFile.close()

