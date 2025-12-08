#include "Hole.hpp"
#include <QPainter>
#include <QPaintEvent>

Hole::Hole(QWidget *parent)
    : QPushButton(parent),
      m_row(-1), m_col(-1),
      m_state(State::Empty),
      m_player(nullptr) {
    setMinimumSize(60, 60);
    setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
}

Hole::~Hole() {
}

QSize Hole::sizeHint() const {
    return QSize(60, 60);
}

void Hole::setState(Hole::State state) {
    if (m_state != state) {
        switch (state) {
            case Hole::Empty:
            case Hole::Playable:
                Q_ASSERT(m_player == nullptr);
                break;
            case Hole::Used:
            case Hole::Selected:
                Q_ASSERT(m_player != nullptr);
                break;
            default:
                Q_UNREACHABLE();
        }

        m_state = state;
        emit stateChanged(m_state);
        update();
    }
}

void Hole::setPlayer(Player* player) {
    if (m_player != player) {
        m_player = player;
        this->setState(player ? State::Used : State::Empty);
        emit playerChanged(player);
    }
}

void Hole::reset() {
    m_state = State::Empty;
    m_player = nullptr;
    update();
}

void Hole::paintEvent(QPaintEvent *event) {
    Q_UNUSED(event);
    
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing, true);
    
    QRect rect = this->rect();
    int size = qMin(rect.width(), rect.height()) - 4;
    int x = (rect.width() - size) / 2;
    int y = (rect.height() - size) / 2;
    QRect circleRect(x, y, size, size);
    
    // Background
    painter.fillRect(rect, QColor(240, 230, 200));
    
    switch (m_state) {
        case State::Empty: {
            // Empty hole: dark gray circle
            painter.setPen(QPen(QColor(100, 90, 80), 3));
            painter.setBrush(QColor(180, 170, 150));
            painter.drawEllipse(circleRect.adjusted(6, 6, -6, -6));
            break;
        }
        
        case State::Playable: {
            // Playable cell: semi-transparent green
            painter.setPen(QPen(QColor(50, 150, 50), 2));
            painter.setBrush(QColor(100, 200, 100, 150));
            painter.drawEllipse(circleRect.adjusted(4, 4, -4, -4));
            break;
        }
        
        case State::Used: {
            // Player piece: solid circle with gradient
            QColor color = m_player->color();
            
            // Shadow
            painter.setPen(Qt::NoPen);
            painter.setBrush(QColor(0, 0, 0, 50));
            painter.drawEllipse(circleRect.adjusted(6, 8, -4, -2));
            
            // Radial gradient for 3D effect
            QRadialGradient gradient(circleRect.center() - QPoint(size/6, size/6), size/2);
            gradient.setColorAt(0, color.lighter(140));
            gradient.setColorAt(0.7, color);
            gradient.setColorAt(1, color.darker(130));
            
            painter.setBrush(gradient);
            painter.setPen(QPen(color.darker(150), 2));
            painter.drawEllipse(circleRect.adjusted(4, 4, -4, -4));
            break;
        }
        
        case State::Selected: {
            // Selected piece: with glowing halo
            QColor color = m_player->selectedColor();
            
            // Glowing halo
            painter.setPen(Qt::NoPen);
            QRadialGradient halo(circleRect.center(), size/2 + 4);
            halo.setColorAt(0.5, QColor(255, 255, 100, 150));
            halo.setColorAt(1, QColor(255, 255, 100, 0));
            painter.setBrush(halo);
            painter.drawEllipse(circleRect.adjusted(-2, -2, 2, 2));
            
            // Radial gradient for 3D effect
            QRadialGradient gradient(circleRect.center() - QPoint(size/6, size/6), size/2);
            gradient.setColorAt(0, color.lighter(130));
            gradient.setColorAt(0.7, color);
            gradient.setColorAt(1, color.darker(110));
            
            painter.setBrush(gradient);
            painter.setPen(QPen(color.darker(130), 3));
            painter.drawEllipse(circleRect.adjusted(4, 4, -4, -4));
            break;
        }
        
        default:
            Q_UNREACHABLE();
    }
}