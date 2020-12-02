import re

filePath = "./Input/Input02.txt"
with open(filePath) as f:
    lines = f.readlines()

def parseInput(line):
    match = re.match(r"^(\d+)-(\d+) (\w): (.+)$", line)
    return (int(match.group(1)),int(match.group(2)),match.group(3),match.group(4))     

inputs = [parseInput(line) for line in lines ]

def checkText(min, max, char, text):
    def match(c):
        if c == char:
            return 1
        return 0

    count = sum([match(c) for c in text])
    if count >= min and count <= max: 
        return 1
    return 0    

def checkText2(a, b, char, text):
    matchA = a < len(text) + 1 and text[a - 1] == char
    matchB = b < len(text) + 1 and text[b - 1] == char
    if matchA and not matchB or not matchA and matchB:
        return 1
    return 0

res1 = sum([checkText(min, max, char, text) for min, max, char, text in inputs])
res2 = sum([checkText2(min, max, char, text) for min, max, char, text in inputs])
print(res1)
print(res2)
