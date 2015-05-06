/**
 * Pseudo tty
 *
 * @author Yongjian Li <lyj238@gmail.com>
 * @author Kaiqiang Duan <duankq@ios.ac.cn>
 */

#ifndef PTY_H_LCS
#define PTY_H_LCS

#include <sys/types.h>
#include <sys/termios.h>

int fd_pipe(int *);

ssize_t writen(int, const char *, size_t);

using Sigfunc = void (*)(int);

Sigfunc signal_intr(int, Sigfunc);

int ptym_open(char *pts_name, int pts_namesz);

int ptys_open(char *pts_name);

pid_t pty_fork(int *ptrfdm, char *slave_name, int slave_namesz,
    const struct termios *slave_termios,
    const struct winsize *slave_winsize);

void do_driver(char *);

void loop(int, int);

#endif // PTY_H_LCS
