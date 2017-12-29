#include <stdlib.h>
#include <stdio.h>

double printInt(int i) {
    printf("%d", i);
    return 0.0;
}

double printDouble(double x) {
	printf("%f", x);
	return 0.0;
}

double printChar(double x) {
	printf("%c", (char)x);
	return 0.0;
}

double exitSuccess() {
	exit(0);
	return 0.0;
}

