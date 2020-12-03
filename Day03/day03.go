package main

import (
	"meruem/CodeAdvent2020/utils"
)

type vector2 struct {
	X int
	Y int
}

func traverse(forest []string, step vector2) int64 {
	posx, posy := 0, 0
	height := len(forest)
	width := len(forest[0])
	count := 0
	for posy < height {
		if forest[posy][posx] == '#' {
			count = count + 1
		}

		posx = (posx + step.X) % width
		posy = posy + step.Y
	}
	return int64(count)
}

func main() {
	filePath := "./Input/Input03.txt"
	lines := utils.ReadLines(filePath)

	res1 := traverse(lines, vector2{3, 1})

	steps := []vector2{
		vector2{1, 1},
		vector2{3, 1},
		vector2{5, 1},
		vector2{7, 1},
		vector2{1, 2},
	}

	var res2 int64 = 1
	for _, step := range steps {
		res2 = res2 * traverse(lines, step)
	}

	println("3_1: ", res1)
	println("3_2: ", res2)
}
