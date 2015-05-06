/**
 * Utility functions and constants
 *
 * @author Yongjian Li <lyj238@gmail.com>
 * @author Kaiqiang Duan <duankq@ios.ac.cn>
 */

#include <iostream>

#define DEBUG (1)

using namespace std;

inline void err_exit(const char *err_msg) {
    cout << err_msg << endl;
    exit(1);
}

