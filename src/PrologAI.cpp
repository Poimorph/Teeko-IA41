#include "PrologAI.hpp"
#include <QDebug>

PrologAI::PrologAI(QObject *parent)
    : QObject(parent),
      m_process(nullptr) {
}

PrologAI::~PrologAI() {
    stop();
}

bool PrologAI::start(const QString& prologPath, const QString& aiFile) {
    if (m_process && m_process->state() != QProcess::NotRunning) {
        qWarning() << "Prolog process already running";
        return false;
    }

    m_process = new QProcess(this);
    m_buffer.clear();
    
    // Connect process signals
    connect(m_process, &QProcess::readyReadStandardOutput,
            this, &PrologAI::onReadyRead);
    connect(m_process, &QProcess::errorOccurred,
            this, &PrologAI::onProcessError);
    connect(m_process, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),
            this, &PrologAI::onProcessFinished);

    // Arguments for SWI-Prolog
    QStringList args;
    args << "-q"           // Quiet mode
         << "-f" << aiFile // Load AI file
         << "-g" << "main" // Execute main predicate
         << "-t" << "halt"; // Terminate cleanly

    qDebug() << "Starting Prolog process:" << prologPath << args;
    m_process->start(prologPath, args);
    
    if (!m_process->waitForStarted(5000)) {
        qWarning() << "Failed to start Prolog process:" << m_process->errorString();
        delete m_process;
        m_process = nullptr;
        return false;
    }

    qDebug() << "Prolog AI started successfully";
    
    // Send initialization command
    sendInit();
    
    return true;
}

void PrologAI::stop() {
    if (m_process) {
        if (m_process->state() != QProcess::NotRunning) {
            sendCommand("QUIT");
            m_process->waitForFinished(2000);
            
            if (m_process->state() != QProcess::NotRunning) {
                m_process->terminate();
                m_process->waitForFinished(1000);
                
                if (m_process->state() != QProcess::NotRunning) {
                    m_process->kill();
                }
            }
        }
        delete m_process;
        m_process = nullptr;
    }
}

bool PrologAI::isRunning() const {
    return m_process && m_process->state() == QProcess::Running;
}

void PrologAI::sendCommand(const QString& command) {
    if (!isRunning()) {
        emit aiError("Prolog process not running");
        return;
    }
    
    QString msg = command + "\n";
    qDebug() << "Sending to Prolog:" << command;
    m_process->write(msg.toUtf8());
    m_process->waitForBytesWritten();
}

void PrologAI::sendInit() {
    sendCommand("INIT");
}

void PrologAI::sendNewGame() {
    sendCommand("NEWGAME");
}

void PrologAI::sendPlayerDrop(int row, int col) {
    sendCommand(QString("DROP %1 %2").arg(row).arg(col));
}

void PrologAI::sendPlayerMove(int fromRow, int fromCol, int toRow, int toCol) {
    sendCommand(QString("MOVE %1 %2 %3 %4").arg(fromRow).arg(fromCol).arg(toRow).arg(toCol));
}

void PrologAI::requestAIMove() {
    sendCommand("GETMOVE");
}

void PrologAI::onReadyRead() {
    QByteArray data = m_process->readAllStandardOutput();
    m_buffer += QString::fromUtf8(data);
    
    // Process complete lines
    int newlineIndex;
    while ((newlineIndex = m_buffer.indexOf('\n')) != -1) {
        QString line = m_buffer.left(newlineIndex).trimmed();
        m_buffer = m_buffer.mid(newlineIndex + 1);
        
        if (!line.isEmpty()) {
            qDebug() << "Received from Prolog:" << line;
            parseResponse(line);
        }
    }
}

void PrologAI::parseResponse(const QString& response) {
    QStringList parts = response.split(' ', Qt::SkipEmptyParts);
    
    if (parts.isEmpty()) {
        return;
    }
    
    QString status = parts[0].toUpper();
    
    if (status == "ERROR") {
        // ERROR message...
        QString errorMsg = parts.mid(1).join(' ');
        emit aiError(errorMsg);
        return;
    }
    
    if (status != "OK") {
        qWarning() << "Unexpected response status:" << status;
        return;
    }
    
    if (parts.size() < 2) {
        qWarning() << "Missing response type";
        return;
    }
    
    QString type = parts[1].toUpper();
    
    if (type == "READY") {
        emit aiReady();
    }
    else if (type == "ACK") {
        emit aiAck();
    }
    else if (type == "DROP") {
        // OK DROP row col
        if (parts.size() >= 4) {
            int row = parts[2].toInt();
            int col = parts[3].toInt();
            emit aiDropReceived(row, col);
        } else {
            qWarning() << "Invalid DROP response:" << response;
        }
    }
    else if (type == "MOVE") {
        // OK MOVE fromRow fromCol toRow toCol
        if (parts.size() >= 6) {
            int fromRow = parts[2].toInt();
            int fromCol = parts[3].toInt();
            int toRow = parts[4].toInt();
            int toCol = parts[5].toInt();
            emit aiMoveReceived(fromRow, fromCol, toRow, toCol);
        } else {
            qWarning() << "Invalid MOVE response:" << response;
        }
    }
    else if (type == "BYE") {
        qDebug() << "Prolog said goodbye";
    }
    else {
        qDebug() << "Unknown response type:" << type;
    }
}

void PrologAI::onProcessError(QProcess::ProcessError error) {
    QString errorMsg;
    switch (error) {
        case QProcess::FailedToStart:
            errorMsg = "Failed to start Prolog process";
            break;
        case QProcess::Crashed:
            errorMsg = "Prolog process crashed";
            break;
        case QProcess::Timedout:
            errorMsg = "Prolog process timed out";
            break;
        case QProcess::WriteError:
            errorMsg = "Write error to Prolog process";
            break;
        case QProcess::ReadError:
            errorMsg = "Read error from Prolog process";
            break;
        default:
            errorMsg = "Unknown Prolog process error";
    }
    
    qWarning() << errorMsg;
    emit aiError(errorMsg);
    emit connectionLost();
}

void PrologAI::onProcessFinished(int exitCode, QProcess::ExitStatus exitStatus) {
    qDebug() << "Prolog process finished with exit code:" << exitCode;
    
    if (exitStatus == QProcess::CrashExit) {
        emit aiError("Prolog process crashed unexpectedly");
        emit connectionLost();
    }
}