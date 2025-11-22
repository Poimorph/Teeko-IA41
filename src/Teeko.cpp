#include "Teeko.hpp"
#include "ui_Teeko.h"

#include <QDebug>
#include <QMessageBox>
#include <QSignalMapper>
#include <QTimer>
#include <QFileInfo>
#include <QCoreApplication>

Teeko::Teeko(QWidget *parent)
    : QMainWindow(parent),
      ui(new Ui::Teeko),
      m_player(Player::player(Player::Red)),
      m_phase(Teeko::DropPhase),
      m_dropCount(0),
      m_eatDone(true),
      m_selectedHole(nullptr),
      m_ai(nullptr),
      m_gameMode(PlayerVsPlayer),
      m_waitingForAI(false) {

    ui->setupUi(this);

    // Connect menu actions
    QObject::connect(ui->actionNew, SIGNAL(triggered(bool)), this, SLOT(reset()));
    QObject::connect(ui->actionQuit, SIGNAL(triggered(bool)), qApp, SLOT(quit()));
    QObject::connect(ui->actionAI, &QAction::toggled, this, &Teeko::toggleAIMode);

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

    // Initialize AI
    m_ai = new PrologAI(this);
    QObject::connect(m_ai, &PrologAI::aiReady, this, &Teeko::onAIReady);
    QObject::connect(m_ai, &PrologAI::aiDropReceived, this, &Teeko::onAIDropReceived);
    QObject::connect(m_ai, &PrologAI::aiMoveReceived, this, &Teeko::onAIMoveReceived);
    QObject::connect(m_ai, &PrologAI::aiError, this, &Teeko::onAIError);
    QObject::connect(m_ai, &PrologAI::connectionLost, this, &Teeko::onAIConnectionLost);

    this->reset();
    this->adjustSize();
    this->setFixedSize(this->size());
}

Teeko::~Teeko() {
    if (m_ai) {
        m_ai->stop();
    }
    delete ui;
}

void Teeko::setPhase(Teeko::Phase phase) {
    if (m_phase != phase) {
        m_phase = phase;
        emit phaseChanged(m_phase);
    }
}

void Teeko::setGameMode(GameMode mode) {
    m_gameMode = mode;
    reset();
}

void Teeko::toggleAIMode(bool enabled) {
    if (enabled) {
        startAI();
    } else {
        if (m_ai->isRunning()) {
            m_ai->stop();
        }
        m_gameMode = PlayerVsPlayer;
        m_waitingForAI = false;
        reset();
        updateStatusBar();
    }
}

void Teeko::startAI() {
    // Look for AI file in various locations
    QStringList searchPaths;
    searchPaths << QCoreApplication::applicationDirPath() + "/TeekoProlog.pl"
                << "./TeekoProlog.pl"
                << "../TeekoProlog.pl"
                << QCoreApplication::applicationDirPath() + "/../TeekoProlog.pl";
    
    QString aiFilePath;
    for (const QString& path : searchPaths) {
        if (QFileInfo::exists(path)) {
            aiFilePath = path;
            break;
        }
    }
    
    if (aiFilePath.isEmpty()) {
        QMessageBox::warning(this, tr("AI Error"),
            tr("TeekoProlog.pl file not found.\n\n"
               "This version includes the AI communication interface,\n"
               "but the Prolog AI implementation is not included.\n\n"
               "To enable AI:\n"
               "1. Implement your own AI backend following the protocol\n"
               "2. Place it next to the executable as 'TeekoProlog.pl'\n\n"
               "See PrologAI.h for protocol documentation."));
        ui->actionAI->setChecked(false);
        return;
    }
    
    if (!m_ai->start("swipl", aiFilePath)) {
        QMessageBox::warning(this, tr("AI Error"),
            tr("Could not start Prolog process.\n"
               "Verify that SWI-Prolog is installed and accessible."));
        ui->actionAI->setChecked(false);
        return;
    }
    
    m_gameMode = PlayerVsAI;
    m_waitingForAI = false;
    reset();
}

void Teeko::onAIReady() {
    qDebug() << "AI is ready";
    updateStatusBar();
    m_ai->sendNewGame();
}

void Teeko::onAIDropReceived(int row, int col) {
    qDebug() << "AI drop at" << row << col;
    
    if (!m_waitingForAI) {
        qWarning() << "Received AI move but not waiting for one";
        return;
    }
    
    m_waitingForAI = false;
    
    // Verify the cell is empty
    Hole* hole = m_board[row][col];
    if (!hole->isEmpty()) {
        qWarning() << "AI tried to drop on non-empty cell";
        onAIError(tr("AI attempted an invalid move"));
        return;
    }
    
    // Apply AI's move
    hole->setPlayer(Player::player(Player::Black));
    
    ++m_dropCount;
    if (m_dropCount == 8) {
        setPhase(Teeko::MovePhase);
    }
    
    // Check if AI won
    Player* savedPlayer = m_player;
    m_player = Player::player(Player::Black);
    if (checkPosition()) {
        emit gameOver();
        emit newGame();
        return;
    }
    m_player = savedPlayer;
    
    // Switch to player's turn
    m_player = Player::player(Player::Red);
    updateStatusBar();
}

void Teeko::onAIMoveReceived(int fromRow, int fromCol, int toRow, int toCol) {
    qDebug() << "AI move from" << fromRow << fromCol << "to" << toRow << toCol;
    
    if (!m_waitingForAI) {
        qWarning() << "Received AI move but not waiting for one";
        return;
    }
    
    m_waitingForAI = false;
    
    // Validate the move
    Hole* fromHole = m_board[fromRow][fromCol];
    Hole* toHole = m_board[toRow][toCol];
    
    if (fromHole->player() != Player::player(Player::Black)) {
        qWarning() << "AI tried to move a piece that isn't its own";
        onAIError(tr("AI attempted an invalid move"));
        return;
    }
    
    if (!toHole->isEmpty()) {
        qWarning() << "AI tried to move to non-empty cell";
        onAIError(tr("AI attempted an invalid move"));
        return;
    }
    
    // Check adjacency
    int rowDiff = qAbs(toRow - fromRow);
    int colDiff = qAbs(toCol - fromCol);
    if (rowDiff > 1 || colDiff > 1) {
        qWarning() << "AI tried to move more than one cell";
        onAIError(tr("AI attempted an invalid move"));
        return;
    }
    
    // Apply the move
    fromHole->setPlayer(nullptr);
    fromHole->setState(Hole::Empty);
    toHole->setPlayer(Player::player(Player::Black));
    
    // Check if AI won
    Player* savedPlayer = m_player;
    m_player = Player::player(Player::Black);
    if (checkPosition()) {
        emit gameOver();
        emit newGame();
        return;
    }
    m_player = savedPlayer;
    
    // Switch to player's turn
    m_player = Player::player(Player::Red);
    updateStatusBar();
}

void Teeko::onAIError(const QString& error) {
    qWarning() << "AI Error:" << error;
    QMessageBox::warning(this, tr("AI Error"), error);
    m_waitingForAI = false;
}

void Teeko::onAIConnectionLost() {
    qWarning() << "AI connection lost";
    m_waitingForAI = false;
    m_gameMode = PlayerVsPlayer;
    
    ui->actionAI->setChecked(false);
    
    QMessageBox::warning(this, tr("AI Connection Lost"),
        tr("The connection with the Prolog AI has been lost.\n"
           "The game will continue in player vs player mode."));
    
    updateStatusBar();
}

void Teeko::play(int id) {
    // Ignore clicks if waiting for AI
    if (m_waitingForAI) {
        return;
    }

    int row = id / 5;
    int col = id % 5;

    Hole* hole = m_board[row][col];

    if (m_phase == DropPhase) {
        if (hole->isEmpty()) {
            hole->setPlayer(m_player);

            // Inform AI of the move
            if (m_gameMode == PlayerVsAI && m_player == Player::player(Player::Red)) {
                m_ai->sendPlayerDrop(row, col);
            }

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

            // If it's AI's turn after player's move
            if (m_gameMode == PlayerVsAI && m_player == Player::player(Player::Black)) {
                m_waitingForAI = true;
                updateStatusBar();
                // Request AI move with a small delay for UX
                QTimer::singleShot(500, [this]() {
                    m_ai->requestAIMove();
                });
            }
        }

    } else if (m_phase == MovePhase) {

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
            int fromRow = -1, fromCol = -1;
            
            // Find and clear selected piece
            for (int r = 0; r < 5; r++) {
                for (int c = 0; c < 5; c++) {
                    if (m_board[r][c]->isSelected()) {
                        fromRow = r;
                        fromCol = c;
                        m_board[r][c]->setPlayer(nullptr);
                        m_board[r][c]->setState(Hole::Empty);
                    }
                    if (m_board[r][c]->isPlayable()) {
                        m_board[r][c]->setState(Hole::Empty);
                    }
                }
            }

            hole->setPlayer(m_player);

            // Inform AI of the move
            if (m_gameMode == PlayerVsAI && m_player == Player::player(Player::Red) && fromRow >= 0) {
                m_ai->sendPlayerMove(fromRow, fromCol, row, col);
            }

            if (checkPosition()) {
                emit gameOver();
                emit newGame();
                return;
            }
            
            emit turnEnded();

            // If it's AI's turn
            if (m_gameMode == PlayerVsAI && m_player == Player::player(Player::Black)) {
                m_waitingForAI = true;
                updateStatusBar();
                QTimer::singleShot(500, [this]() {
                    m_ai->requestAIMove();
                });
            }
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
    m_waitingForAI = false;

    // Inform AI of new game
    if (m_gameMode == PlayerVsAI && m_ai->isRunning()) {
        m_ai->sendNewGame();
    }

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
    QString modeStr;
    
    if (m_gameMode == PlayerVsAI) {
        if (m_waitingForAI) {
            modeStr = tr(" [AI thinking...]");
        } else {
            modeStr = tr(" [vs AI]");
        }
    }

    ui->statusbar->showMessage(tr("%1 phase: %2's turn%3")
                               .arg(phase)
                               .arg(m_player->name())
                               .arg(modeStr));
}

void Teeko::eat() {
    m_eatDone = false;
}