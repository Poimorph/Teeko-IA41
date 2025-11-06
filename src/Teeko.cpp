#include "Teeko.hpp"
#include "ui_Teeko.h"

#include <QDebug>
#include <QMessageBox>
#include <QSignalMapper>

Teeko::Teeko(QWidget *parent)
    : QMainWindow(parent),
      ui(new Ui::Teeko),
      m_player(Player::player(Player::Red)),
      m_phase(Teeko::DropPhase),
      m_dropCount(0),
      m_eatDone(true),
      m_selectedHole(nullptr) {

    ui->setupUi(this);

    // Connect menu actions
    QObject::connect(ui->actionNew, SIGNAL(triggered(bool)), this, SLOT(reset()));
    QObject::connect(ui->actionQuit, SIGNAL(triggered(bool)), qApp, SLOT(quit()));

    // Set up the board with signal mapper
    QSignalMapper* map = new QSignalMapper(this);
    for (int row = 0; row < 5; ++row) {
        for (int col = 0; col < 5; ++col) {
            QString holeName = QString("hole%1%2").arg(row).arg(col);
            Hole* hole = this->findChild<Hole*>(holeName);
            Q_ASSERT(hole != nullptr);

            m_board[row][col] = hole;
            hole->setRow(row);
            hole->setCol(col);

            int id = row * 5 + col;
            map->setMapping(hole, id);
            QObject::connect(hole, SIGNAL(clicked(bool)), map, SLOT(map()));
        }
    }
    
#if QT_VERSION < QT_VERSION_CHECK(6,0,0)
    QObject::connect(map, SIGNAL(mapped(int)), this, SLOT(play(int)));
#else
    QObject::connect(map, SIGNAL(mappedInt(int)), this, SLOT(play(int)));
#endif

    // Connect game signals
    QObject::connect(this, SIGNAL(turnEnded()), this, SLOT(switchPlayer()));
    QObject::connect(this, SIGNAL(gameOver()), this, SLOT(showGameOver()));
    QObject::connect(this, SIGNAL(newGame()), this, SLOT(eat()));
    QObject::connect(this, SIGNAL(newGame()), this, SLOT(reset()));

    this->reset();
    this->adjustSize();
    this->setFixedSize(this->size());
}

Teeko::~Teeko() {
    delete ui;
}

void Teeko::setPhase(Teeko::Phase phase) {
    if (m_phase != phase) {
        m_phase = phase;
        emit phaseChanged(m_phase);
    }
}

void Teeko::play(int id) {
    int row = id / 5;
    int col = id % 5;

    Hole* hole = m_board[row][col];

    if (m_phase == DropPhase) {
        // Placement phase: click empty hole to place piece
        if (hole->isEmpty()) {
            hole->setPlayer(m_player);

            if (checkPosition()) {
                emit gameOver();
                emit newGame();
                return;
            }

            ++m_dropCount;
            if (m_dropCount == 8) {
                setPhase(Teeko::MovePhase);
            }

            emit turnEnded();
        }

    } else if (m_phase == MovePhase) {
        // Movement phase
        if (hole->player() == m_player) {
            // Clear previous selections
            for (int r = 0; r < 5; r++) {
                for (int c = 0; c < 5; c++) {
                    if (m_board[r][c]->isSelected()) 
                        m_board[r][c]->setState(Hole::Used);
                    if (m_board[r][c]->isPlayable()) 
                        m_board[r][c]->setState(Hole::Empty);
                }
            }

            hole->setState(Hole::Selected);
            m_selectedHole = hole;

            // Mark adjacent empty cells as playable
            if (col + 1 < 5 && m_board[row][col + 1]->isEmpty())
                m_board[row][col + 1]->setState(Hole::Playable);
            if (col + 1 < 5 && row + 1 < 5 && m_board[row + 1][col + 1]->isEmpty())
                m_board[row + 1][col + 1]->setState(Hole::Playable);
            if (row + 1 < 5 && m_board[row + 1][col]->isEmpty())
                m_board[row + 1][col]->setState(Hole::Playable);
            if (row + 1 < 5 && col - 1 >= 0 && m_board[row + 1][col - 1]->isEmpty())
                m_board[row + 1][col - 1]->setState(Hole::Playable);
            if (col - 1 >= 0 && m_board[row][col - 1]->isEmpty())
                m_board[row][col - 1]->setState(Hole::Playable);
            if (row - 1 >= 0 && col - 1 >= 0 && m_board[row - 1][col - 1]->isEmpty())
                m_board[row - 1][col - 1]->setState(Hole::Playable);
            if (row - 1 >= 0 && m_board[row - 1][col]->isEmpty())
                m_board[row - 1][col]->setState(Hole::Playable);
            if (row - 1 >= 0 && col + 1 < 5 && m_board[row - 1][col + 1]->isEmpty())
                m_board[row - 1][col + 1]->setState(Hole::Playable);
        }

        if (hole->isPlayable()) {
            // Move piece to this playable cell
            for (int r = 0; r < 5; r++) {
                for (int c = 0; c < 5; c++) {
                    if (m_board[r][c]->isSelected()) {
                        m_board[r][c]->setPlayer(nullptr);
                        m_board[r][c]->setState(Hole::Empty);
                    }
                    if (m_board[r][c]->isPlayable()) {
                        m_board[r][c]->setState(Hole::Empty);
                    }
                }
            }

            hole->setPlayer(m_player);

            if (checkPosition()) {
                emit gameOver();
                emit newGame();
                return;
            }
            
            emit turnEnded();
        }
    }

    if (!m_eatDone) {
        m_eatDone = true;
        reset();
    }
}

void Teeko::switchPlayer() {
    m_player = m_player->other();
    this->updateStatusBar();
}

void Teeko::reset() {
    // Reset board
    for (int row = 0; row < 5; ++row) {
        for (int col = 0; col < 5; ++col) {
            Hole* hole = m_board[row][col];
            hole->setPlayer(nullptr);
            hole->setState(Hole::Empty);
        }
    }

    // Reset to initial state
    m_player = Player::player(Player::Red);
    m_phase = DropPhase;
    m_dropCount = 0;
    m_selectedHole = nullptr;

    this->updateStatusBar();
}

bool Teeko::checkPosition() {
    int combo = 0, row, col, attmpt;

    // Check rows
    for (row = 0; row < 5; row++) {
        for (attmpt = 0; attmpt < 2; attmpt++) {
            for (col = 0; col < 4; col++) {
                Hole* hole = m_board[row][col + attmpt];
                if (hole->player() == m_player) {
                    combo++;
                    if (combo == 4) return true;
                }
            }
            combo = 0;
        }
    }

    // Check columns
    for (col = 0; col < 5; col++) {
        for (attmpt = 0; attmpt < 2; attmpt++) {
            for (row = 0; row < 4; row++) {
                Hole* hole = m_board[row + attmpt][col];
                if (hole->player() == m_player) {
                    combo++;
                    if (combo == 4) return true;
                }
            }
            combo = 0;
        }
    }

    // Check secondary diagonals
    for (row = 0; row < 2; row++) {
        for (col = 3; col < 5; col++) {
            for (attmpt = 0; attmpt < 4; attmpt++) {
                Hole* hole = m_board[row + attmpt][col - attmpt];
                if (hole->player() == m_player) {
                    combo++;
                    if (combo == 4) return true;
                }
            }
            combo = 0;
        }
    }

    // Check main diagonals
    for (row = 0; row < 2; row++) {
        for (col = 0; col < 2; col++) {
            for (attmpt = 0; attmpt < 4; attmpt++) {
                Hole* hole = m_board[row + attmpt][col + attmpt];
                if (hole->player() == m_player) {
                    combo++;
                    if (combo == 4) return true;
                }
            }
            combo = 0;
        }
    }

    // Check 2x2 squares
    for (row = 0; row < 4; row++) {
        for (col = 0; col < 4; col++) {
            Hole* hole;
            hole = m_board[row][col];
            if (hole->player() == m_player) combo++;
            hole = m_board[row][col + 1];
            if (hole->player() == m_player) combo++;
            hole = m_board[row + 1][col + 1];
            if (hole->player() == m_player) combo++;
            hole = m_board[row + 1][col];
            if (hole->player() == m_player) combo++;

            if (combo == 4) return true;
            combo = 0;
        }
    }

    return false;
}

void Teeko::showGameOver() {
    QMessageBox::information(this, tr("Winner"),
        tr("Congratulations! %1 wins!").arg(m_player->name()));
}

void Teeko::updateStatusBar() {
    QString phase(m_phase == Teeko::DropPhase ? tr("placement") : tr("movement"));
    ui->statusbar->showMessage(tr("%1 phase: %2's turn")
                               .arg(phase)
                               .arg(m_player->name()));
}

void Teeko::eat() {
    m_eatDone = false;
}