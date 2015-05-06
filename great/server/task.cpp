/**
 * Process framework for execute a task
 *
 * @author Yongjian Li <lyj238@gmail.com>
 * @author Kaiqiang Duan <duankq@ios.ac.cn>
 */

#include "task.h"
#include "utils.h"

#include <fstream>

#include <sys/types.h>
#include <unistd.h>

#ifdef DEBUG
#include <iostream>
#endif // DEBUG

using namespace std;

// Task with key k and SMV code c
Task::Task(const std::string &k, const std::string &c, const std::string &path_base,
        const std::string &smv_p) {
    key = k;
    smv_path = smv_p;
    // TODO should encrypt k or escape path intervals
    full_filename = path_base + k + ".smv";
    ofstream smvfile(full_filename);
    smvfile << c << endl;
}

// Task with key k, suppose the SMV code has been already stored
Task::Task(const std::string &k, const std::string &path_base,
        const std::string &smv_p) {
    key = k;
    smv_path = smv_p;
    full_filename = path_base + k + ".smv";
    // TODO check if the file exists
}

// Run smv in another child process
void Task::run() const {
    pipe(tosmv);
    pipe(fromsmv);
    pipe(errinsmv);

    int pid;
    // double fork
    if ((pid = fork()) < 0) {
        err_exit("Failed to fork");
    } else if (pid == 0) { // child
        if((pid = fork()) < 0) {
            err_exit("Failed to fork");
        } else if (pid > 0) {
            cout << "Pid of smv is: " << pid << endl;
            exit(0);
        } else {
            close(tosmv[1]);
            close(fromsmv[0]);
            close(errinsmv[0]);

            dup2(tosmv[0], STDIN_FILENO);
            dup2(fromsmv[1], STDOUT_FILENO);
            dup2(errinsmv[1], STDERR_FILENO);

            close(tosmv[0]);
            close(fromsmv[1]);
            close(errinsmv[1]);

            if (execl("./driver", "driver", "-e", smv_path.c_str(),
                "-dcx", "-int", "-old", full_filename.c_str(), (char*)nullptr) < 0) {
                err_exit("Failed to execute NuSMV");
            }
        }
    // end child
    // parent
    } else {
        close(tosmv[0]);
        close(fromsmv[1]);
        close(errinsmv[1]);
        char s[100];
        write(tosmv[1], "go\ncompute_reachable\n", 100);
        while(read(fromsmv[0], s, 100))
            cout << s << endl;
    }
}


// Execute a command
std::string Task::exec(const std::string cmd) const {
    string res;
    return res;
}

// Execute a command asychronously
void Task::exec_asyc(const std::string cmd, delegator callback) const {

}

// Check if code of the task is same as c
bool Task::same_code_as(std::string &c) const {
    return true;
}