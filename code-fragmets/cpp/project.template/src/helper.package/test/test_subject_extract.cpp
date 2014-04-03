
// _____________________________________________________________________________
//                                                               Includes/Using

#include "../subject_extract.h"

#include <iostream>
#include <gtest/gtest.h>

using helper_package::get_subject_txt;

// TEST has two parameters: the test case name and the test name.
// After using the macro, you should define your test logic between a
// pair of braces.  You can use a bunch of macros to indicate the
// success or failure of a test.  EXPECT_TRUE and EXPECT_EQ are
// examples of such macros.  For a complete list, see gtest.h.

// _____________________________________________________________________________
//                                                                        TEST

// Tests some trivial cases.
TEST(TestSuitName, testCaseName) {
  std::cout << "Running tests..." << std::endl;

  EXPECT_EQ("", get_subject_txt("blah"));
  EXPECT_EQ("test messages", get_subject_txt("Subject: test messages"));
}
