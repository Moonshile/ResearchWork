/**
 * Process framework for execute a task
 *
 * @author Yongjian Li <lyj238@gmail.com>
 * @author Kaiqiang Duan <duankq@ios.ac.cn>
 */

#ifndef TASK_H_LCS
#define TASK_H_LCS

#include <string>
#include <queue>

class Task {
public:
    using delegator = void (*)(const std::string &, const std::string &);

    // Task with key k and SMV code c
    Task(const std::string &k, const std::string &c, const std::string &path_base,
        const std::string &smv_p);

    // Task with key k, suppose the SMV code has been already stored
    Task(const std::string &k, const std::string &path_base,
        const std::string &smv_p);

    // Run smv in another child process
    void run() const;

    // Execute a command
    std::string exec(const std::string cmd) const;

    // Execute a command asychronously
    void exec_asyc(const std::string cmd, delegator callback) const;

private:
    // Check if code of the task is same as c
    bool same_code_as(std::string &c) const;

    std::string key;
    std::string full_filename;
    std::string smv_path;
    std::queue<std::string> cmds;
    mutable int tosmv[2];
    mutable int fromsmv[2];
    mutable int errinsmv[2];
};

#endif // TASK_H_LCS
