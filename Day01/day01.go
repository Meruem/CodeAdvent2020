package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	filePath := "./Input/Input01.txt"
	target := 2020
	lines := readLines(filePath)
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

func readLines(fileName string) []string {
	res := make([]string, 0)
	file, err := os.Open(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		res = append(res, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	return res
}
