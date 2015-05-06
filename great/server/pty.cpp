/**
 * Pseudo tty
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

int fd_pipe(int fd[2]) {
    return socketpair(AF_UNIX, SOCK_STREAM, 0, fd);
}

ssize_t writen(int fd, const char *ptr, size_t n) {
    size_t nleft = n;
    ssize_t nwritten;

    while (nleft > 0) {
        if ((nwritten = write(fd, ptr, nleft)) < 0) {
            if (nleft == n) {
                return -1; // error
            } else {
                break; // error, return amount written so far
            }
        } else if (nwritten == 0) {
            break;
        }
        nleft -= nwritten;
        ptr += nwritten;
    }
    return n - nleft;
}

Sigfunc signal_intr(int signo, Sigfunc func) {
    struct sigaction act, oact;
    act.sa_handler = func;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if (sigaction(signo, &act, &oact) < 0) {
        return(SIG_ERR);
    }
    return oact.sa_handler;
}

int ptym_open(char *pts_name, int pts_namesz) {
    char *ptr;
    int fdm;
    if ((fdm = posix_openpt(O_RDWR)) < 0) {
        return -1;
    }
    if (grantpt(fdm) < 0 || unlockpt(fdm) < 0 || (ptr = ptsname(fdm)) == nullptr) {
        close(fdm);
        return -1;
    }

    strncpy(pts_name, ptr, pts_namesz);
    pts_name[pts_namesz - 1] = '\0';
    return fdm;
}

int ptys_open(char *pts_name) {
    int fds;
    if((fds = open(pts_name, O_RDWR)) < 0) {
        return -1;
    }
    return fds;
}

pid_t pty_fork(int *ptrfdm, char *slave_name, int slave_namesz,
    const struct termios *slave_termios,
    const struct winsize *slave_winsize) {
    int fdm, fds;
    pid_t pid;
    char pts_name[20];

    if((fdm = ptym_open(pts_name, sizeof(pts_name))) < 0) {
        err_exit("Can't open master pseudo tty");        
    }

    if(slave_name != nullptr) {
        strncpy(slave_name, pts_name, slave_namesz);
        slave_name[slave_namesz - 1] = '\0';
    }

    if((pid = fork()) < 0) {
        err_exit("Failed to fork");
    } else if (pid == 0) {
        if (setsid() < 0) {
            err_exit("Error in setsid");
        }
        if ((fds = ptys_open(pts_name)) < 0) {
            err_exit("Can't open slave pseudo tty");
        }
        close(fdm);

        if (slave_termios != nullptr) {
            if (tcsetattr(fds, TCSANOW, slave_termios) < 0) {
                err_exit("tcsetattr error on slave pseudo tty");
            }
        }
        if (slave_winsize != nullptr) {
            if(ioctl(fds, TIOCSWINSZ, slave_winsize) < 0) {
                err_exit("TIOCSWINSZ error on slave pseudo tty");
            }
        }

        if (dup2(fds, STDIN_FILENO) != STDIN_FILENO) {
            err_exit("dup2 error to stdin");
        }
        if (dup2(fds, STDOUT_FILENO) != STDOUT_FILENO) {
            err_exit("dup2 error to stdout");
        }
        if (dup2(fds, STDERR_FILENO) != STDERR_FILENO) {
            err_exit("dup2 error to stderr");
        }
        if (fds != STDIN_FILENO && fds != STDOUT_FILENO && fds != STDERR_FILENO) {
            close(fds);
        }
        return 0;
    } else {
        *ptrfdm = fdm;
        return pid;
    }
    return -1;
}

void do_driver(char *driver) {
    pid_t child;
    int pipe[2];

    if (fd_pipe(pipe) < 0) {
        err_exit("Can't create stream pipe");
    }

    if ((child = fork()) < 0) {
        err_exit("Failed to fork");
    } else if (child == 0) {
        close(pipe[1]);

        if (dup2(pipe[0], STDIN_FILENO) != STDIN_FILENO) {
            err_exit("dup2 error to stdin");
        }
        if (dup2(pipe[0], STDOUT_FILENO) != STDOUT_FILENO) {
            err_exit("dup2 error to stdout");
        }
        if (dup2(pipe[0], STDERR_FILENO) != STDERR_FILENO) {
            err_exit("dup2 error to stderr");
        }
        if (pipe[0] != STDIN_FILENO && pipe[0] != STDOUT_FILENO && pipe[0] != STDERR_FILENO) {
            close(pipe[0]);
        }

        execlp(driver, driver, (char*)0);
        err_exit("execlp error");
    } else {
        close(pipe[0]);

        if (dup2(pipe[1], STDIN_FILENO) != STDIN_FILENO) {
            err_exit("dup2 error to stdin");
        }
        if (dup2(pipe[1], STDOUT_FILENO) != STDOUT_FILENO) {
            err_exit("dup2 error to stdout");
        }
        if (dup2(pipe[1], STDERR_FILENO) != STDERR_FILENO) {
            err_exit("dup2 error to stderr");
        }
        if (pipe[1] != STDIN_FILENO && pipe[1] != STDOUT_FILENO && pipe[1] != STDERR_FILENO) {
            close(pipe[1]);
        }

        // parent returns, but with stdin/stdout/stderr connected to the driver
    }
}

#define BUFFSIZE (512)

static volatile sig_atomic_t sigcaught;

static void sig_term(int signo) {
    sigcaught = 1;
}

void loop(int ptym, int ignoreeof) {
    pid_t child;
    int nread;
    char buf[BUFFSIZE];

    if((child = fork()) < 0) {
        err_exit("Failed to fork");
    } else if (child == 0) {
        while (true) {
            if ((nread = read(STDIN_FILENO, buf, BUFFSIZE)) < 0) {
                err_exit("Read error from stdin");
            } else if (nread == 0) {
                break;
            }
            if (writen(ptym, buf, nread) != nread) {
                err_exit("Writen error to master pseudo tty");
            }
        }

        if (ignoreeof == 0) {
            kill(getppid(), SIGTERM);
        }
        exit(0);
    } else {
        if (signal_intr(SIGTERM, sig_term) == SIG_ERR) {
            err_exit("signal_intr error for SIGTERM");
        }

        while (true) {
            if ((nread = read(ptym, buf, BUFFSIZE)) <= 0) {
                break;
            }
            if (writen(STDOUT_FILENO, buf, nread) != nread) {
                err_exit("Writen error to stdout");
            }
        }

        if (sigcaught == 0) {
            kill(child, SIGTERM);
        }
    }
}
