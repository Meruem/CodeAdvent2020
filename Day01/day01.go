package main

import (
	"fmt"
	"meruem/CodeAdvent2020/utils"
	"strconv"
)

func main() {
	filePath := "./Input/Input01.txt"
	target := 2020
	lines := utils.ReadLines(filePath)
	inputs := make([]int, len(lines))
	for i, line := range lines {
		inputs[i], _ = strconv.Atoi(line)
	}

	for i := 0; i < len(inputs)-1; i++ {
		for j := i + 1; j < len(inputs); j++ {
			if inputs[i]+inputs[j] == target {
				fmt.Println(inputs[i] * inputs[j])
			}
		}
	}

	for i := 0; i < len(inputs)-1; i++ {
		for j := i + 1; j < len(inputs); j++ {
			for k := j + 1; k < len(inputs); k++ {
				if inputs[i]+inputs[j]+inputs[k] == target {
					fmt.Println(inputs[i] * inputs[j] * inputs[k])
				}
			}
		}
	}
}
