#include <stdbool.h>
#include <stdio.h>

int getValue(char c) {
	if (c == 'X')
		return 1;
	else if (c == 'Y')
		return 2;
	else
		return 3;
}

int getWin(char c1, char c2) {
	if (c1 == 'X')
		if (c2 == 'B')
			return 0;
		else if (c2 == 'A')
			return 3;
		else
			return 6;
	else if (c1 == 'Y')
		if (c2 == 'C')
			return 0;
		else if (c2 == 'B')
			return 3;
		else
			return 6;
	else
		if (c2 == 'A')
			return 0;
		else if (c2 == 'C')
			return 3;
		else
			return 6;
}

char newMe(char c1, char win) {
	if (win == 'Y') {
		if (c1 == 'A')
			return 'X';
		else if (c1 == 'B')
			return 'Y';
		else
			return 'Z';
	}
	else if (win == 'Z') {
		if (c1 == 'A')
			return 'Y';
		else if (c1 == 'B')
			return 'Z';
		else
			return 'X';
	}
	else
		if (c1 == 'A')
			return 'Z';
		else if (c1 == 'B')
			return 'X';
		else
			return 'Y';
}

int main() {
	//read file
	FILE *file;
	file = fopen("input.txt", "r");
	char line[100]; 
	fgets(line, 100, file);
	char me;
	char opponent;
	int score = 0;
	while (!feof(file)) {
		opponent = line[0];
		me = newMe(opponent, line[2]);
		int itemScore = getValue(me);
		int winScore = getWin(me, opponent);
		score += itemScore;
		score += winScore;
		printf("%s", line);
		printf("%d + %d\n", itemScore, winScore);
		fgets(line, 100, file);
	}
	printf("\nTotal score : %d\n", score);
	return 0;
}
