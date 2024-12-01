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

int difference(int a, int b) {
  return (a < b ? b-a : a-b);
}

int compare(const void* a, const void*b) {
  return (*(int*)a > *(int*)b);
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
  //sort columns
  qsort(leftColumn, nbLines, sizeof(int), compare);
  qsort(rightColumn, nbLines, sizeof(int), compare);
  //computes the difference between each terms of the columns
  int result = 0;
  for (int i = 0; i < nbLines; i++) {
    printf("%d, %d\n", leftColumn[i], rightColumn[i]);
    result += difference(leftColumn[i], rightColumn[i]);
  }
  printf("Result: %d\n", result);

  //de-init
  for (int i = 0; i < nbLines; i++) free(lines[i]);
  free(lines);
  return 0;
}
