#include "ObjectPool.h"

/**
 * Simple class that is used to illustrate using of object pool
 */
class UserRequest
{
 public:
  UserRequest() {}
  ~UserRequest() {}

  // Methods to populate the request with specific information.
  // Methods to retrieve the request data.
  // (not shown)

 protected:
  // data members (not shown)
};

// _____________________________________________________________________________
//                                                              Utility methods

UserRequest& obtainUserRequest(ObjectPool<UserRequest>& pool)
{
  // Obtain a UserRequest object from the pool
  UserRequest& request = pool.acquireObject();

  // populate the request with user input
  // (not shown)

  return (request);
}

void processUserRequest(ObjectPool<UserRequest>& pool, UserRequest& req)
{
  // process the request
  // (not shown)

  // return the request to the pool
  pool.releaseObject(req);
}

// _____________________________________________________________________________
//                                                                         Main

int main(int argc, char** argv)
{
  /* 
   * User of the object pool specifies through the template parameter the name of the class
   * from which objects can be created, and through the constructor the allocation
   * "chunk size"
   */
  ObjectPool<UserRequest> requestPool(1000);

  // Set up program
  // (not shown)

  while (true /* program is running */) {
    UserRequest& req = obtainUserRequest(requestPool);
    processUserRequest(requestPool, req);
  }

  return (0);
}
