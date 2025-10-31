#ifndef TEEKO_H
#define TEEKO_H

#include <QMainWindow>
#include <QPushButton>

// Minimal version - everything in one class
class Teeko : public QMainWindow {
    Q_OBJECT

public:
    Teeko(QWidget *parent = nullptr);

private slots:
    void cellClicked();
    void newGame();

private:
    QPushButton* cells[5][5];  // Board cells
    int board[5][5];           // 0=empty, 1=red, 2=black
    int currentPlayer;         // 1 or 2
    int piecesPlaced;          // Count total pieces
    bool movingPhase;          // true when in move phase
    int selectedRow, selectedCol; // For movement
    
    void checkWin();
    bool isAdjacent(int r1, int c1, int r2, int c2);
};

#endif