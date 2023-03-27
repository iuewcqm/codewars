# https://www.codewars.com/kata/540afbe2dc9f615d5e000425

class Sudoku(object):
    def __init__(self, data):
        self.board = data
        self.BASE  = set(range(1, len(data)+1))
        self.N     = int(len(data)**0.5)
    
    def is_valid(self):
        return self.types() and self.rows() and self.cols() and self.boxes()

    def types(self):
        return all(not isinstance(x, bool) for row in self.board for x in row)

    def rows(self):
        for row in self.board:
            if set(row) != self.BASE:
                return False
        return True
    
    def cols(self):
        for col in zip(*self.board):
            if set(col) != self.BASE:
                return False
        return True
    
    def boxes(self):
        for sy in range(0, len(self.board), self.N):
            for sx in range(0, len(self.board[sy]), self.N):
                square = [self.board[y][x] for x in range(sx, sx+self.N) for y in range(sy, sy+self.N)]
                if set(square) != self.BASE:
                    return False
        return True

if __name__ == '__main__':
    good_sudoku = Sudoku([
      [7,8,4, 1,5,9, 3,2,6],
      [5,3,9, 6,7,2, 8,4,1],
      [6,1,2, 4,3,8, 7,5,9],

      [9,2,8, 7,1,5, 4,6,3],
      [3,5,7, 8,4,6, 1,9,2],
      [4,6,1, 9,2,3, 5,8,7],
      
      [8,7,6, 3,9,4, 2,1,5],
      [2,4,3, 5,6,1, 9,7,8],
      [1,9,5, 2,8,7, 6,3,4]
    ])

    bad_sudoku = Sudoku([
      [1,2,3, 4,5,6, 7,8,9],
      [2,3,1, 5,6,4, 8,9,7],
      [3,1,2, 6,4,5, 9,7,8],
      
      [4,5,6, 7,8,9, 1,2,3],
      [5,6,4, 8,9,7, 2,3,1],
      [6,4,5, 9,7,8, 3,1,2],
      
      [7,8,9, 1,2,3, 4,5,6],
      [8,9,7, 2,3,1, 5,6,4],
      [9,7,8, 3,1,2, 6,4,5]
    ])
    print(good_sudoku.is_valid())
    print(bad_sudoku.is_valid())
