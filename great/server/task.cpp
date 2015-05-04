/**
 * Process framework for execute a task
 *
 * @author Yongjian Li <lyj238@gmail.com>
 * @author Kaiqiang Duan <duankq@ios.ac.cn>
 */

#include "task.h"

 using namespace std;

// Task with key k and SMV code c
Task::Task(const std::string &k, const std::string &c) {

}

// Task with key k, suppose the SMV code has been already stored
Task::Task(const std::string &k) {

}

// Execute a command
std::string Task::exec(const std::string cmd) const {
    string res;
    return res;
}

// Execute a command asychronously
void Task::exec_asyc(const std::string cmd, void callback(const std::string)) const {

}

// Check if code of the task is same as c
bool Task::same_code_as(std::string &c) const {
    return true;
}
