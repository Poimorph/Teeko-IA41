#include "Teeko.h"
#include <QApplication>

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    Teeko game;
    game.show();
    return app.exec();
}