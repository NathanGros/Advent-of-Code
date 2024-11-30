#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

int processLine(char *line) {
  char **elements = splitOnChars(line, "-,");
  int i = 0;
  while (elements[i]) {
    printf("%s;", elements[i]);
    i++;
  }
  printf("\n");
  return 2;
}

int main() {
    //read and store all lines
    char **lines = parseFile("./test_input.txt");
    //get number of lines
    int nbLines = 0;
    while (lines[++nbLines]);

    int result = 0;
    for (int i = 0; i < nbLines; i++) {
      result += processLine(lines[i]);
    }
    printf("Result: %d\n", result);

    //de-init
    for (int i = 0; i < nbLines; i++) free(lines[i]);
    free(lines);
    return 0;
}
