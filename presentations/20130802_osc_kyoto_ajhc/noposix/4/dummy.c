#include "jhc_rts_header.h"

void abort() {for (;;);}
char *setlocale(int category, const char *locale) {return NULL;}
int fputc(int c, FILE *stream) {return 0;}
int fputs(const char *s, FILE *stream) {return 0;}
int fprintf(FILE *stream, const char *format, ...) {return 0;}
int fflush(FILE* stream) {return 0;}
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {return 0;}
void jhc_print_profile(void) {}
