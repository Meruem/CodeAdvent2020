filePath = "./Input/Input01.txt"
with open(filePath) as f:
    lines = f.readlines()

target = 2020
inputs = [int(line) for line in lines ]

for i in range(len(lines)-1):
    for j in range(i+1, len(lines)):
        if inputs[i] + inputs[j] == target:
            print(inputs[i] * inputs[j])

for i in range(len(lines)-2):
    for j in range(i+1, len(lines)-1):
        for k in range(j+1, len(lines)):
            if inputs[i] + inputs[j] + inputs[k] == target:
                print(inputs[i] * inputs[j] * inputs[k])
