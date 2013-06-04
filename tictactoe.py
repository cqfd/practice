#!/usr/bin/env python

BOARD = [[' ', ' ', ' '],
         [' ', ' ', ' '],
         [' ', ' ', ' ']]

WINNING_POSITIONS = [[(0, 0), (0, 1), (0, 2)],
                     [(1, 0), (1, 1), (1, 2)],
                     [(2, 0), (2, 1), (2, 2)]]

X_VALS = []
O_VALS = []

def move():
    row = int(raw_input())
    col = int(raw_input())
    X_VALS.append((row, col))
    BOARD[row][col]= "x"

def show():
    for row in BOARD:
        print '|'.join(row)

def won():
    for triplet in WINNING_POSITIONS:
        if triplet[0] in X_VALS and\
           triplet[1] in X_VALS and\
           triplet[2] in X_VALS:
            return True
    return False


def main():
    while True:
        show()
        move()
        if won():
            show()
            break
    print 'Celebrate!'

if __name__ == '__main__':
    main()
