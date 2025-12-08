#ifndef TEEKO_H
#define TEEKO_H

#include <QMainWindow>
#include "Hole.hpp"
#include "PrologAI.hpp"

QT_BEGIN_NAMESPACE
namespace Ui {
    class Teeko;
}
QT_END_NAMESPACE

class Teeko : public QMainWindow {
    Q_OBJECT

public:
    enum Phase {
        DropPhase,
        MovePhase
    };
    Q_ENUM(Phase)

    enum GameMode {
        PlayerVsPlayer,
        PlayerVsAI
    };
    Q_ENUM(GameMode)

    Teeko(QWidget *parent = nullptr);
    virtual ~Teeko();

    Teeko::Phase phase() const { return m_phase; }
    Teeko::GameMode gameMode() const { return m_gameMode; }

signals:
    void phaseChanged(Teeko::Phase phase);
    void turnEnded();
    void gameOver();
    void newGame();

private:
    Ui::Teeko *ui;
    Player* m_player;
    Phase m_phase;
    Hole* m_board[5][5];
    int m_dropCount;
    bool m_eatDone;
    Hole* m_selectedHole;
    
    // AI support
    PrologAI* m_ai;
    GameMode m_gameMode;
    bool m_waitingForAI;

private slots:
    void setPhase(Teeko::Phase phase);
    void play(int id);
    void switchPlayer();
    void reset();
    bool checkPosition();
    void eat();
    void showGameOver();
    void updateStatusBar();
    
    // AI slots
    void setGameMode(GameMode mode);
    void onAIReady();
    void onAIDropReceived(int row, int col);
    void onAIMoveReceived(int fromRow, int fromCol, int toRow, int toCol);
    void onAIError(const QString& error);
    void onAIConnectionLost();
    
    // Menu actions
    void toggleAIMode(bool enabled);
    void startAI();
};

#endif // TEEKO_H