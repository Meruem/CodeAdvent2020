filePath = "./Input/Input03.txt"
with open(filePath) as f:
    lines = f.read().splitlines()


def traverse(lines, stepx, stepy):
    posx,posy = 0,0
    height = len(lines)
    width = len(lines[0])    
    count = 0
    while posy < height:
        if lines[posy][posx] == "#":
            count = count + 1
        posx = (posx + stepx) % width
        posy = posy + stepy
    return count 


res1 = traverse(lines, 3, 1)

steps = [(1,1), (3,1), (5,1), (7,1), (1,2)]
res2 = 1
for (stepx, stepy) in steps:
    res2 = res2 * traverse(lines, stepx, stepy)

print(res1)
print(res2)
