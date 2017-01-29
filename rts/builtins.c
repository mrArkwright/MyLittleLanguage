#include <stdlib.h>
#include <stdio.h>

double printDouble(double x) {
	printf("%f", x);
	return 0;
}

double printChar(double x) {
	printf("%c", (char)x);
	return 0;
}

double exitSuccess() {
	exit(0);
	return 0.0;
}

