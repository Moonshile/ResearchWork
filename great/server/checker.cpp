/**
 * Checker is to check invariant in NuSMV asynchronous and concurrently
 *
 * @author Yongjian Li <lyj238@gmail.com>
 * @author Kaiqiang Duan <duankq@ios.ac.cn>
 */

#include "uniserv.h"
#include "task.h"

#include <sys/types.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
    Task task("test", "test", "./", "/home/duan/Downloads/NuSMV/bin/NuSMV");
    Task task2("mutualEx", "./", "/home/duan/Downloads/NuSMV/bin/NuSMV");
    task2.run();
    return 0;
}
