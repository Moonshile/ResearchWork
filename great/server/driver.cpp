/**
 * Pseudo tty driver
 *
 * @author Yongjian Li <lyj238@gmail.com>
 * @author Kaiqiang Duan <duankq@ios.ac.cn>
 */

#include "pty.h"
#include "utils.h"

#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/socket.h>

#include <cstdlib>
#include <cstdio>
#include <cstring>

#define OPTSTR "+d:einv"

static void set_noecho(int);

int main(int argc, char *argv[]) {
    int fdm, c, ignoreoef, interactive, noecho, verbose;
    pid_t pid;
    char *driver;
    char slave_name[20];
    struct termios orig_termios;
    struct winsize size;

    interactive = isatty(STDIN_FILENO);
    ignoreoef = 0;
    noecho = 0;
    verbose = 0;
    driver = nullptr;

    opterr = 0;

    while((c = getopt(argc, argv, OPTSTR)) != EOF) {
        switch (c) {
        case 'd':
            driver = optarg;
            break;
        case 'e':
            noecho = 1;
            break;
        case 'i':
            ignoreoef = 1;
            break;
        case 'n':
            interactive = 0;
            break;
        case 'v':
            verbose = 1;
            break;
        case '?':
            err_exit("Unrecognized option");
        }
    }

    if (optind >= argc) {
        err_exit("Usage: driver [-d driver -einv] program [arg ... ]");
    }

    if (interactive) {
        if (tcgetattr(STDIN_FILENO, &orig_termios) < 0) {
            err_exit("tcgetattr error on stdin");
        }
        if (ioctl(STDIN_FILENO, TIOCGWINSZ, (char*)&size) < 0) {
            err_exit("TIOCGWINSZ error");
        }
        pid = pty_fork(&fdm, slave_name, sizeof(slave_name), &orig_termios, &size);
    } else {
        pid = pty_fork(&fdm, slave_name, sizeof(slave_name), nullptr, nullptr);
    }

    if (pid < 0) {
        err_exit("Failed to fork");
    } else if (pid == 0) {
        if (noecho) {
            set_noecho(STDIN_FILENO);
        }
        if (execvp(argv[optind], &argv[optind]) < 0) {
            err_exit("Can't execute");
        }
    }

    if (verbose) {
        fprintf(stderr, "slave name = %s\n", slave_name);
        if (driver != nullptr) {
            fprintf(stderr, "driver = %s\n", driver);
        }
    }

    if (interactive && driver == nullptr) {
        // TODO
    }

    if (driver) {
        do_driver(driver);
    }

    loop(fdm, ignoreoef);

    exit(0);
}

static void set_noecho(int fd) {
    struct termios stermios;

    if (tcgetattr(fd, &stermios) < 0) {
        err_exit("tcgetattr error");
    }

    stermios.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL);

    stermios.c_oflag &= ~(ONLCR);

    if (tcsetattr(fd, TCSANOW, &stermios) < 0) {
        err_exit("tcsetattr error");
    }
}
