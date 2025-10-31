#include "Teeko.hpp"
#include <QGridLayout>
#include <QMessageBox>
#include <QMenuBar>
#include <QStatusBar>

Teeko::Teeko(QWidget *parent) : QMainWindow(parent) {
    // Basic window setup
    setWindowTitle("Teeko");
    
    // Menu
    QMenu* gameMenu = menuBar()->addMenu("Game");
    QAction* newAction = gameMenu->addAction("New Game");
    connect(newAction, &QAction::triggered, this, &Teeko::newGame);
    
    // Status bar
    statusBar()->showMessage("Red's turn - Place piece");
    
    // Central widget with grid
    QWidget* central = new QWidget(this);
    setCentralWidget(central);
    QGridLayout* layout = new QGridLayout(central);
    
    // Create 5x5 grid of buttons
    for(int r = 0; r < 5; r++) {
        for(int c = 0; c < 5; c++) {
            cells[r][c] = new QPushButton();
            cells[r][c]->setFixedSize(60, 60);
            cells[r][c]->setProperty("row", r);
            cells[r][c]->setProperty("col", c);
            connect(cells[r][c], &QPushButton::clicked, this, &Teeko::cellClicked);
            layout->addWidget(cells[r][c], r, c);
        }
    }
    
    newGame();
}

void Teeko::newGame() {
    // Reset everything
    currentPlayer = 1;
    piecesPlaced = 0;
    movingPhase = false;
    selectedRow = selectedCol = -1;
    
    for(int r = 0; r < 5; r++) {
        for(int c = 0; c < 5; c++) {
            board[r][c] = 0;
            cells[r][c]->setText("");
            cells[r][c]->setStyleSheet("");
        }
    }
    
    statusBar()->showMessage("Red's turn - Place piece");
}

void Teeko::cellClicked() {
    QPushButton* btn = qobject_cast<QPushButton*>(sender());
    int r = btn->property("row").toInt();
    int c = btn->property("col").toInt();
    
    if(!movingPhase) {
        // DROP PHASE - place pieces
        if(board[r][c] == 0) {
            board[r][c] = currentPlayer;
            btn->setText(currentPlayer == 1 ? "R" : "B");
            btn->setStyleSheet(currentPlayer == 1 ? 
                "background-color: red; color: white; font-weight: bold;" : 
                "background-color: black; color: white; font-weight: bold;");
            
            piecesPlaced++;
            checkWin();
            
            if(piecesPlaced == 8) {
                movingPhase = true;
                statusBar()->showMessage(QString("%1's turn - Move piece")
                    .arg(currentPlayer == 1 ? "Red" : "Black"));
            } else {
                currentPlayer = (currentPlayer == 1) ? 2 : 1;
                statusBar()->showMessage(QString("%1's turn - Place piece")
                    .arg(currentPlayer == 1 ? "Red" : "Black"));
            }
        }
    } else {
        // MOVE PHASE
        if(selectedRow == -1) {
            // Select a piece
            if(board[r][c] == currentPlayer) {
                selectedRow = r;
                selectedCol = c;
                cells[r][c]->setStyleSheet(cells[r][c]->styleSheet() + 
                    "border: 3px solid yellow;");
                statusBar()->showMessage("Click adjacent cell to move");
            }
        } else {
            // Try to move
            if(board[r][c] == 0 && isAdjacent(selectedRow, selectedCol, r, c)) {
                // Move the piece
                board[r][c] = board[selectedRow][selectedCol];
                board[selectedRow][selectedCol] = 0;
                
                cells[r][c]->setText(cells[selectedRow][selectedCol]->text());
                cells[r][c]->setStyleSheet(cells[selectedRow][selectedCol]->styleSheet().replace("border: 3px solid yellow;", ""));
                cells[selectedRow][selectedCol]->setText("");
                cells[selectedRow][selectedCol]->setStyleSheet("");
                
                selectedRow = selectedCol = -1;
                checkWin();
                
                currentPlayer = (currentPlayer == 1) ? 2 : 1;
                statusBar()->showMessage(QString("%1's turn - Move piece")
                    .arg(currentPlayer == 1 ? "Red" : "Black"));
            } else {
                // Deselect
                cells[selectedRow][selectedCol]->setStyleSheet(
                    cells[selectedRow][selectedCol]->styleSheet().replace("border: 3px solid yellow;", ""));
                selectedRow = selectedCol = -1;
                statusBar()->showMessage(QString("%1's turn - Move piece")
                    .arg(currentPlayer == 1 ? "Red" : "Black"));
            }
        }
    }
}

bool Teeko::isAdjacent(int r1, int c1, int r2, int c2) {
    int dr = abs(r2 - r1);
    int dc = abs(c2 - c1);
    return (dr <= 1 && dc <= 1 && (dr > 0 || dc > 0));
}

void Teeko::checkWin() {
    // Check horizontal
    for(int r = 0; r < 5; r++) {
        for(int c = 0; c < 2; c++) {
            int count = 0;
            for(int i = 0; i < 4; i++) {
                if(board[r][c+i] == currentPlayer) count++;
            }
            if(count == 4) {
                QMessageBox::information(this, "Winner!", 
                    QString("%1 wins!").arg(currentPlayer == 1 ? "Red" : "Black"));
                newGame();
                return;
            }
        }
    }
    
    // Check vertical
    for(int c = 0; c < 5; c++) {
        for(int r = 0; r < 2; r++) {
            int count = 0;
            for(int i = 0; i < 4; i++) {
                if(board[r+i][c] == currentPlayer) count++;
            }
            if(count == 4) {
                QMessageBox::information(this, "Winner!", 
                    QString("%1 wins!").arg(currentPlayer == 1 ? "Red" : "Black"));
                newGame();
                return;
            }
        }
    }
    
    // Check diagonals
    for(int r = 0; r < 2; r++) {
        for(int c = 0; c < 2; c++) {
            int count = 0;
            for(int i = 0; i < 4; i++) {
                if(board[r+i][c+i] == currentPlayer) count++;
            }
            if(count == 4) {
                QMessageBox::information(this, "Winner!", 
                    QString("%1 wins!").arg(currentPlayer == 1 ? "Red" : "Black"));
                newGame();
                return;
            }
        }
    }
    
    // Check 2x2 square
    for(int r = 0; r < 4; r++) {
        for(int c = 0; c < 4; c++) {
            if(board[r][c] == currentPlayer &&
               board[r][c+1] == currentPlayer &&
               board[r+1][c] == currentPlayer &&
               board[r+1][c+1] == currentPlayer) {
                QMessageBox::information(this, "Winner!", 
                    QString("%1 wins!").arg(currentPlayer == 1 ? "Red" : "Black"));
                newGame();
                return;
            }
        }
    }
}