// https://www.codewars.com/kata/56a73d2194505c29f600002d

// solution
const char solution[] =
    "^(0|1(((0|1(01*00)*01*011)(((0|11)|10(01*00)*01*01)1)*((0|11)|10(01*00)*"
    "01*01)|1(01*00)*01*01)0)*((0|1(01*00)*01*011)(((0|11)|10(01*00)*01*01)1)*"
    "10|1)(01*00)*1)+$";
// ---

// tests
#include <regex.h>
#include <stdio.h>

void test(int regex_res, const char *err_message) {
  if (regex_res)
    printf("ok\n");
  else
    fprintf(stderr, "%s\n", err_message);
}

int main(void) {
  regex_t re;
  test(!regcomp(&re, solution, REG_EXTENDED | REG_NOSUB),
       "must be valid regular expression");
  test(!regexec(&re, "0", 0, 0, 0), "must accept 0");
  test(!regexec(&re, "111", 0, 0, 0), "must accept 7");
  test(!regexec(&re, "110001000101101111000111011001", 0, 0, 0),
       "must accept 823587289");
  regfree(&re);
}
