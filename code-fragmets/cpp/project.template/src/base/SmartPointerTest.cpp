#include "SmartPointer.h"

/**
 * Simple class that is used to illustrate using of smart pointer
 */
class Nothing
{
 public:
  Nothing() { sNumAllocations++; }
  ~Nothing() { sNumDeletions++; }

  static int sNumAllocations;
  static int sNumDeletions;
};

int Nothing::sNumAllocations = 0;
int Nothing::sNumDeletions = 0;


// _____________________________________________________________________________
//                                                                         Main

int main(int argc, char** argv)
{
  Nothing* myNothing = new Nothing();

  {
    SmartPointer<Nothing> ptr1(myNothing);
    SmartPointer<Nothing> ptr2(myNothing);
  }

  if (Nothing::sNumAllocations != Nothing::sNumDeletions) {
    std::cout << "TEST FAILED: " << Nothing::sNumAllocations <<
        " allocations and " << Nothing::sNumDeletions <<
        " deletions" << std::endl;
  } else {
    std::cout << "TEST PASSED" << std::endl;
  }
}
