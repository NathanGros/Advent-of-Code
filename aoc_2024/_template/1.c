#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

int processLine(char *line) {
  char **elements = splitOnChars(line, ";-,");
  int i = 0;
  while (elements[i]) {
    printf("%s;", elements[i]);
    i++;
  }
  printf("\n");
  return 1;
}

int main() {
  //init
  //read and store all lines
  char **lines = parseFile("./input.txt");
  //get number of lines
  int nbLines = 0;
  while (lines[++nbLines]);
  //get number of characters on the first line in case of grid input
  int nbCharPerLine = 0;
  while (lines[0][++nbCharPerLine]);
  nbCharPerLine--;

  //actual processing
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

