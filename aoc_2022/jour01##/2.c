#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
	//read file
	FILE *file;
	file = fopen("input.txt", "r");
	char line[100]; 
	int max1Calories = 0;
	int max2Calories = 0;
	int max3Calories = 0;
	int elfCalories = 0;
	while (!feof(file)) {
		fgets(line, 100, file);
		if (strcmp(line, "\n") == 0 || feof(file)) {
			if (elfCalories > max3Calories)
				max3Calories = elfCalories;
			elfCalories = 0;
			if (max3Calories > max2Calories) {
				int temp = max2Calories;
				max2Calories = max3Calories;
				max3Calories = temp;
			}
			if (max2Calories > max1Calories) {
				int temp = max1Calories;
				max1Calories = max2Calories;
				max2Calories = temp;
			}
		}
		else
			elfCalories += (int) strtol(line, (char **)NULL, 10);
		printf("%s", line);
	}
	printf("\nMax calories : %d, %d, %d\n", max1Calories, max2Calories, max3Calories);
	printf("Total: %d\n", max1Calories + max2Calories + max3Calories);
	return 0;
}
