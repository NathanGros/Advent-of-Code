#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_LINE_LENGTH 1000

char **parseFile(char *fileName) {
  FILE *input = fopen(fileName, "r");
  char **lines = NULL;
  char buffer[MAX_LINE_LENGTH];
  int nbLines = 0;
  while (fgets(buffer, sizeof(buffer), input)) {
    lines = realloc(lines, (nbLines + 1) * sizeof(char *));
    lines[nbLines] = malloc(strlen(buffer) + 1);
    strcpy(lines[nbLines], buffer);
    nbLines++;
  }
  lines = realloc(lines, (nbLines + 1) * sizeof(char *));
  lines[nbLines] = NULL;
  fclose(input);
  return lines;
}

char **splitOnChars(char *str, char *separators) {
  char **elements = NULL;
  int nbElements = 0;
  char buffer[MAX_LINE_LENGTH];
  int buffersize = 0;
  char charBuffer;
  while (charBuffer = *str++) {
    if (strchr(separators, charBuffer) != NULL || charBuffer == '\n') {
      buffer[buffersize] = '\0';
      elements = realloc(elements, (nbElements + 1) * sizeof(char *));
      elements[nbElements] = malloc(MAX_LINE_LENGTH);
      strcpy(elements[nbElements], buffer);
      nbElements++;
      buffersize = 0;
    }
    else buffer[buffersize++] = charBuffer;
  }
  elements = realloc(elements, (nbElements + 1) * sizeof(char *));
  elements[nbElements] = NULL;
  return elements;
}
