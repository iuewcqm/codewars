#include <stdio.h>

void print_spiral(unsigned n, int spiral[n][n], int use_zero) {
  for(unsigned y = 0; y < n; y++) {
    for(unsigned x = 0; x < n; x++) {
      char ch = spiral[y][x] == 1 ? '1' : (use_zero ? '0' : ' ');
      printf("%c", ch);
    }
    printf("\n");
  }
}

void fill_with_zeros(unsigned n, int spiral[n][n]) {
  for(unsigned y = 0; y < n; y++)
    for(unsigned x = 0; x < n; x++)
      spiral[y][x] = 0;
}

void solve_upside(int n, int spiral[n][n], int x_offset, int y_offset) {
  for(int x = (x_offset-1) < 0 ? 0 : x_offset-1; x < n-x_offset; x++)
    spiral[y_offset][x] = 1;
}
void solve_rightside(int n, int spiral[n][n], int x_offset, int y_offset) {
  for (int y = y_offset; y < n-y_offset; y++)
    spiral[y][n-x_offset] = 1;
}
void solve_downside(int n, int spiral[n][n], int x_offset, int y_offset) {
  for(int x = n-x_offset-1; x > x_offset-1; x--)
    spiral[n-y_offset][x] = 1;
}
void solve_leftside(int n, int spiral[n][n], int x_offset, int y_offset) {
  for (int y = n-y_offset; y > y_offset; y--)
    spiral[y][x_offset-1] = 1;
}

void spiralize(int n, int spiral[n][n]) {
  fill_with_zeros(n, spiral);
  int x_offset = 0, y_offset = 0;
  for(int i = 0; i < n; i++) {
    if (i%4 == 0) {
      solve_upside(n, spiral, x_offset, y_offset);
      x_offset += 1;
    }
    else if (i%4 == 1) {
      solve_rightside(n, spiral, x_offset, y_offset);
      y_offset += 1;
    }
    else if (i%4 == 2) {
      solve_downside(n, spiral, x_offset-1, y_offset);
    }
    else if (i%4 == 3) {
      solve_leftside(n, spiral, x_offset, y_offset);
      x_offset += 1;
      y_offset += 1;
    }
  }
}

void do_test(unsigned n) {
  int spiral[n][n];
  spiralize(n, spiral);
  print_spiral(n, spiral, 0);
}

int main(void) { 
  do_test(5);
  printf("\n");
  do_test(7);
  printf("\n");
  do_test(20);
  return 0;
}
