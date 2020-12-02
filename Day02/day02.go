package main

import (
	"fmt"
	"meruem/CodeAdvent2020/utils"
	"regexp"
	"strconv"
)

type input struct {
	a    int
	b    int
	ch   rune
	text string
}

func checkText(min int, max int, char rune, text string) int {
	count := 0
	for _, ch := range text {
		if ch == char {
			count++
		}
	}

	if count >= min && count <= max {
		return 1
	}
	return 0
}

func checkText2(a int, b int, char rune, text string) int {
	runes := []rune(text)
	matchA := a < len(runes)+1 && runes[a-1] == char
	matchB := b < len(runes)+1 && (runes[b-1] == char)
	if matchA && !matchB || !matchA && matchB {
		return 1
	}
	return 0
}

func main() {
	filePath := "./Input/Input02.txt"
	lines := utils.ReadLines(filePath)
	inputs := make([]input, len(lines))

	re := regexp.MustCompile(`^(\d+)-(\d+) (\w): (.+)$`)

	for i, line := range lines {
		parts := re.FindStringSubmatch(line)

		a, _ := strconv.Atoi(parts[1])
		b, _ := strconv.Atoi(parts[2])
		ch := parts[3]
		text := parts[4]

		inputs[i] = input{a: a, b: b, ch: rune(ch[0]), text: text}
	}

	count := 0
	for _, input := range inputs {
		count = count + checkText(input.a, input.b, input.ch, input.text)
	}

	count2 := 0
	for _, input := range inputs {
		count2 = count2 + checkText2(input.a, input.b, input.ch, input.text)
	}

	fmt.Println("2_1: ", count)
	fmt.Println("2_2: ", count2)

}
