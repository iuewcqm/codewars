# Area of House from Path of Mouse (4kyu)
# https://www.codewars.com/kata/5d2f93da71baf7000fe9f096/

ROTATES_RIGHT = { (0,1):(-1,0), (-1,0):(0,-1), (0,-1):(1,0), (1,0):(0,1) }
ROTATES_LEFT  = { (0,1):(0,-1), (0,-1):(-1,0), (-1,0):(1,0), (1,0):(0,1) } 

def mouse_path(s):
    moves = parse_moves(s)
    house_square = 0
    house_field = [[0]*10 for i in range(10)]
    x, y = 5, 0
    dir_x, dir_y = 1, 0
    for move in moves:
        if   move == 'R':
            dir_y = 1
            dir_x = 0
        elif move == 'L':
            dir_x ^= dir_y
            dir_y ^= dir_x
            dir_x ^= dir_y
        else:
            for j in range(move):
                house_field[y+j*dir_y][x+j*dir_x] = 1
        print_field(house_field)
        print()

    print_field(house_field)
    return house_square

def parse_moves(s):
    moves = []
    num = '';
    for i in s:
        if i == 'R' or i == 'L':
            moves.append(int(num))
            moves.append(i)
            num = ''
        else:
            num += i
    return moves

def print_field(field):
    [print(row) for row in field]

if __name__ == '__main__':
    path = mouse_path("4R2L1R5R9R4R4L3")
    print(path, path == 49)
