#ifndef PLAYER_H
#define PLAYER_H

#include <QObject>
#include <QMutex>
#include <QColor>

class Player : public QObject {
    Q_OBJECT

public:
    enum Type {
        Red,
        Black
    };
    Q_ENUM(Type)

    virtual ~Player();
    static Player* player(Player::Type type);

    Player::Type type() const { return m_type; }
    const QString& name() const { return m_name; }
    const QColor& color() const { return m_color; }
    const QColor& selectedColor() const { return m_selectedColor; }

    Player* other() const;

private:
    static Player* m_players[2];
    static QMutex& m_mutex;

    Player::Type m_type;
    QString m_name;
    QColor m_color;
    QColor m_selectedColor;

    explicit Player(Player::Type type);
    Q_DISABLE_COPY(Player);
};

#endif // PLAYER_H