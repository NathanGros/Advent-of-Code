#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

int processLine(char *line, int *leftColumn, int *rightColumn, int i) {
  char **elements = splitOnChars(line, " ");
  //first element is at the beginning and second element is 3 spaces away (see input)
  leftColumn[i] = atoi(elements[0]);
  rightColumn[i] = atoi(elements[3]);
  return 0;
}

int main() {
  //init
  //read and store all lines
  char **lines = parseFile("./input.txt");
  //get number of lines
  int nbLines = 0;
  while (lines[++nbLines]);

  //actual processing
  //parse file in two columns
  int leftColumn[nbLines];
  int rightColumn[nbLines];
  for (int i = 0; i < nbLines; i++) {
    processLine(lines[i], leftColumn, rightColumn, i);
  }
  //computes the number of similarities of the left column in the right one
  int result = 0;
  int nbSimilarity;
  for (int i = 0; i < nbLines; i++) {
    nbSimilarity = 0;
    for (int j = 0; j < nbLines; j++) {
      if (leftColumn[i] == rightColumn[j]) nbSimilarity++;
    }
    result += leftColumn[i] * nbSimilarity;
  }
  printf("Result: %d\n", result);

  //de-init
  for (int i = 0; i < nbLines; i++) free(lines[i]);
  free(lines);
  return 0;
}
