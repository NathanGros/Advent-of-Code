#include <stdbool.h>
#include <stdio.h>

void processLine(char *line, int *res) {
	*res += 1;
}

int main() {
	//read file
	FILE *file;
	file = fopen("input.txt", "r");
	char line[100]; 
	fgets(line, 100, file);

	//variables
	int res = 0;

	//loop
	while (!feof(file)) {
		//process line here
		printf("%s", line);
		processLine(line, &res);

		//new line
		fgets(line, 100, file);
	}

	//print result
	printf("\n\nResult: %d\n", res);
	return 0;
}
