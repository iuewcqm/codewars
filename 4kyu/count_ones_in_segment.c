// https://www.codewars.com/kata/596d34df24a04ee1e3000a25

#include <stdint.h>

typedef uint64_t ulong;

uint64_t countBits(ulong n) {
  n += 1;
  ulong power_of_2 = 2;
  ulong count = n / 2;
  while (power_of_2 <= n) {
    ulong total_pairs = n / power_of_2;
    count += (total_pairs / 2) * power_of_2;
    count += (total_pairs & 1) * (n % power_of_2);
    power_of_2 <<= 1;
  }

  return count;
}

ulong countOnes(ulong left, ulong right) {
  return countBits(right) - countBits(left-1);
}


// tests

#include <stdio.h>

void test(uint64_t a, uint64_t b, uint64_t expected) {
  uint64_t actual = countOnes(a, b);
  printf("countOnes(%10zu, %10zu) == %12zu | %s\n",
	 a, b, expected, actual == expected ? "true" : "false");
}

int main(void) {
  test(4, 7, 8);
  test(1, 1000000000, 14846928141);
  test(65076971, 796639280, 10818295161);
  test(4250829, 231192380, 3131546184);
  test(193303, 289384, 916107);
  test(63211, 639280, 5548403);
  test(476744280, 477171088, 6676768);

  return 0;
}
