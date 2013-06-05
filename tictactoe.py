#!/usr/bin/env python

BOARD = [[' ', ' ', ' '],
         [' ', ' ', ' '],
         [' ', ' ', ' ']]

WINNING_POSITIONS = [[(0, 0), (0, 1), (0, 2)],
                     [(1, 0), (1, 1), (1, 2)],
                     [(2, 0), (2, 1), (2, 2)],
                     [(0, 0), (1, 0), (2, 0)],
                     [(0, 1), (1, 1), (2, 1)],
                     [(0, 2), (1, 2), (2, 2)],
                     [(0, 0), (1, 1), (2, 2)],
                     [(2, 0), (1, 1), (0, 2)]]

X_VALS = []
O_VALS = []

def move(player):
    row = int(raw_input())
    col = int(raw_input())

    if (row, col) in X_VALS or\
       (row, col) in O_VALS:
        return None

    if player == 1:
        X_VALS.append((row, col))
        BOARD[row][col] = 'x'
    else:
        O_VALS.append((row, col))
        BOARD[row][col] = 'o'

    return 1

def show():
    print ' ' + ' | '.join(BOARD[0])
    print '---|---|---'
    print ' ' + ' | '.join(BOARD[1])
    print '---|---|---'
    print ' ' + ' | '.join(BOARD[2])

def won(player):
    vals = X_VALS if player == 1 else O_VALS
    for triplet in WINNING_POSITIONS:
        if triplet[0] in vals and\
           triplet[1] in vals and\
           triplet[2] in vals:
            return player
    return None


def main():
    player = 1

    show()
    while True:
        res = move(player)
        if not res:
            show()
            continue
        player = 1 if player == 2 else 2
        res = won(player)
        show()
        if res:
            print 'Celebrate player ' + res + '!'
            break

if __name__ == '__main__':
    main()
