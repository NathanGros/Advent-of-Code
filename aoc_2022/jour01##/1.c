#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
	//read file
	FILE *file;
	file = fopen("input.txt", "r");
	char line[100]; 
	int maxCalories = 0;
	int elfCalories = 0;
	while (!feof(file)) {
		fgets(line, 100, file);
		if (strcmp(line, "\n") == 0 || feof(file)) {
			if (elfCalories > maxCalories)
				maxCalories = elfCalories;
			elfCalories = 0;
		}
		else
			elfCalories += (int) strtol(line, (char **)NULL, 10);
		printf("%s", line);
	}
	printf("\nMax calories : %d\n", maxCalories);
	return 0;
}
