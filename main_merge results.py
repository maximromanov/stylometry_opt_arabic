"""
_TEMP_c2_SL8700_ST2_MFF500_CUL40_cosine_wardD2.csv
_TEMP_c2_SL8700_ST2_MFF500_CUL40_delta_wardD2.csv
_TEMP_c2_SL8700_ST2_MFF500_CUL40_eder_wardD2.csv
"""

def writeToFile(pathToFile, data):
    if os.path.isfile(pathToFile):
        with open(pathToFile, "a", encoding="utf8") as fT:
            fT.write("\n".join(data[1:]))
    else:
        with open(pathToFile, "w", encoding="utf8") as fT:
            fT.write("\n".join(data))

import os
srcFolder = "./Hindawi_Corpus_65_results_ch_WardD_run01/"
trgFolder = "./Hindawi_MergedResults/"
for entry in os.scandir(srcFolder):
    if (("/_TEMP_" in entry.path) and entry.path.endswith(".csv")) and entry.is_file():
        print(entry.path)
        base = entry.path.split("/")[-1].split("_")
        targetFile = trgFolder + "Hindawi_C65_%s_%s.csv" % (base[2][:1], base[7])
        with open(entry.path, "r", encoding="utf8") as f1:
            data = f1.read().split("\n")
            writeToFile(targetFile, data)
