QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

CONFIG += c++17

SOURCES += $$files(src/*.cpp)

HEADERS += $$files(src/*.hpp)

FORMS += Teeko.ui

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target

# Copy Prolog AI file to build directory
DISTFILES += \
    TeekoProlog.pl

# Post-build: copy TeekoProlog.pl next to executable
win32 {
    QMAKE_POST_LINK += $$QMAKE_COPY $$shell_path($$PWD/TeekoProlog.pl) $$shell_path($$OUT_PWD/TeekoProlog.pl)
}
unix {
    QMAKE_POST_LINK += cp $$PWD/TeekoProlog.pl $$OUT_PWD/TeekoProlog.pl
}