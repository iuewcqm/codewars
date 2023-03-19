// https://www.codewars.com/kata/541af676b589989aed0009e7

#include <stdio.h>
#include <string.h>

int count_change(int money, int n_coins, const int coins[n_coins]) {
    int combs[money + 1];
    memset(combs, 0, (money + 1) * sizeof(int));
    combs[0] = 1;
    for(int i=0; i<n_coins; i++)
      for(int j=coins[i]; j<=money; j++)
        combs[j] += combs[j - coins[i]];
  
    return combs[money];
}

int main(void) {
  printf("%d\n", count_change(4, 2, (int[]){2, 1}));
  printf("%d\n", count_change(10, 3, (int[]){3, 5, 2}));
  printf("%d\n", count_change(11, 2, (int[]){5, 7}));
  printf("%d\n", count_change(10, 4, (int[]){4, 3, 2, 1}));
  printf("%d\n", count_change(10, 4, (int[]){1, 2, 3, 4}));
  printf("%d\n", count_change(4, 2, (int[]){2, 1}));
  printf("%d\n", count_change(300, 7, (int[]){500,5,50,100,20,200,10} ));
  printf("%d\n", count_change(3000, 7, (int[]){500,5,50,100,20,200,10} ));
  printf("%d\n", count_change(30000, 7, (int[]){500,5,50,100,20,200,10}));
}
