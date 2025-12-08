#include "Player.hpp"

Player* Player::m_players[] = { nullptr, nullptr };
QMutex& Player::m_mutex = *(new QMutex());

Player::Player(Player::Type type)
    : QObject(nullptr), m_type(type) {
    switch (type) {
        case Player::Red:
            m_name = tr("Red Player");
            m_color = QColor(220, 60, 60);           // Red
            m_selectedColor = QColor(255, 120, 120); // Light red (selected)
            break;
        case Player::Black:
            m_name = tr("Black Player");
            m_color = QColor(60, 60, 60);            // Black
            m_selectedColor = QColor(100, 100, 100); // Gray (selected)
            break;
        default:
            Q_UNREACHABLE();
            break;
    }
}

Player::~Player() {
}

Player* Player::player(Player::Type type) {
    if (!m_players[type]) {
        QMutexLocker locker(&m_mutex);
        Q_UNUSED(locker);

        if (!m_players[type])
            m_players[type] = new Player(type);
    }

    return m_players[type];
}

Player* Player::other() const {
    switch (m_type) {
        case Player::Red:
            return Player::player(Player::Black);
        case Player::Black:
            return Player::player(Player::Red);
        default:
            Q_UNREACHABLE();
            return nullptr;
    }
}