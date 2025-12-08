#ifndef PROLOGAI_H
#define PROLOGAI_H

#include <QObject>
#include <QProcess>
#include <QString>
#include <QStringList>

/**
 * Communication interface with Prolog AI via simple text protocol
 * 
 * Command format (Qt -> Prolog):
 *   INIT
 *   NEWGAME
 *   DROP row col
 *   MOVE fromRow fromCol toRow toCol
 *   GETMOVE
 *   QUIT
 * 
 * Response format (Prolog -> Qt):
 *   OK READY
 *   OK ACK
 *   OK DROP row col
 *   OK MOVE fromRow fromCol toRow toCol
 *   ERROR message
 * 
 */

class PrologAI : public QObject {
    Q_OBJECT

public:
    explicit PrologAI(QObject *parent = nullptr);
    virtual ~PrologAI();

    // Start/stop the Prolog process
    bool start(const QString& prologPath = "swipl", const QString& aiFile = "TeekoProlog.pl");
    void stop();
    bool isRunning() const;

    // Communication with AI
    void sendInit();
    void sendNewGame();
    void sendPlayerDrop(int row, int col);
    void sendPlayerMove(int fromRow, int fromCol, int toRow, int toCol);
    void requestAIMove();

signals:
    void aiReady();
    void aiAck();
    void aiDropReceived(int row, int col);
    void aiMoveReceived(int fromRow, int fromCol, int toRow, int toCol);
    void aiError(const QString& errorMessage);
    void connectionLost();

private slots:
    void onReadyRead();
    void onProcessError(QProcess::ProcessError error);
    void onProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);

private:
    QProcess* m_process;
    QString m_buffer;

    void sendCommand(const QString& command);
    void parseResponse(const QString& response);
};

#endif // PROLOGAI_H