// https://www.codewars.com/kata/56b2abae51646a143400001d

#include <iostream>

ulong mystery(ulong n) { 
  return n ^ (n >> 1);
}

ulong mysteryInv(ulong n) {
  ulong mask = n;
  while (mask) {
    mask >>= 1;
    n ^= mask;
  }
  return n;
}

std::string nameOfMystery() {
  return "Gray code";
}

int main() {
  std::cout << mystery(6) << std::endl;
  return 0;
}
