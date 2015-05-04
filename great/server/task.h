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
    // Task with key k and SMV code c
    Task(const std::string &k, const std::string &c);

    // Task with key k, suppose the SMV code has been already stored
    Task(const std::string &k);

    // Execute a command
    std::string exec(const std::string cmd) const;

    // Execute a command asychronously
    void exec_asyc(const std::string cmd, void callback(const std::string)) const;

private:
    // Check if code of the task is same as c
    bool same_code_as(std::string &c) const;

    std::string key;
    std::string full_filename;
    std::queue<std::string> cmds;
};

#endif // TASK_H_LCS
