import os
import glob
import numpy
import shutil

from empath import Empath
lexicon = Empath()
currentwd = os.getcwd()
path = currentwd + "/results/wordlists/"

os.chdir(path)
path_words = [f for f in glob.glob("*.txt")]

wordlist = []
for j in path_words:
    with open(j) as f:
        for line in f:
            inner_list = [elt.strip() for elt in line.split(',')]
    wordlist = inner_list[:-1]
    lexicon.create_category("", wordlist, model="fiction",write=True)

    oldname = '/home/anatale/.local/lib/python3.8/site-packages/empath/data/user/.empath'
    newname = currentwd + "/results/empath_resultfiles/"+j
    shutil.move(oldname,newname)

