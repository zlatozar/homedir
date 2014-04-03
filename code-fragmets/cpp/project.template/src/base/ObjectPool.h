/**
 * Copyright (C). Copyright Year(s). License.
 *
 * @file   ObjectPool.h
 *
 * @author
 * @date
 *
 * @brief
 *
 *
 */
#ifndef _OBJECTPOOL_H_
#define _OBJECTPOOL_H_

// _____________________________________________________________________________
//                                                                     Includes

#include <queue>
#include <vector>
#include <stdexcept>
#include <memory>      // contains for_each

using std::queue;
using std::vector;

/**
 * @class ObjectPool
 * @brief Provides an object pool that can be used with any class that provides a
 * default constructor.
 *
 * The object pool constructor @b creates a pool of objects, which it hands out
 * to clients when requested via the @c acquireObject() method.
 * When a client is finished with the object it calls @c releaseObject() to put the
 * object back into the object pool.
 * The constructor and destructor on each object in the pool will be called only
 * once each for the lifetime of the program, not once per acquisition and release.
 *
 * The primary use of an object pool is to avoid creating and deleting objects
 * repeatedly. The object pool is most suited to applications that use large
 * numbers of objects for short periods of time.
 *
 * For efficiency, the object pool doesn't perform sanity checks.
 * Expects the user to release every acquired object exactly once.
 * Expects the user to avoid using any objects that he or she has released.
 *
 * Expects the user not to delete the object pool until every object
 * that was acquired has been released. Deleting the object pool invalidates
 * any objects that the user had acquired, even if they had not yet been released.
 *
 * @author
 * @version 1.0.0
 */
template <typename T>
class ObjectPool
{
 public:
  /**
   * @brief Creates an object pool with @p chunkSize objects.
   *
   * Whenever the object pool runs out of objects, @p chunkSize
   * more objects will be added to the pool. The pool only grows:
   * objects are never removed from the pool (freed), until
   * the pool is destroyed.
   *
   * @param chunkSize the size of chunks
   * @throw invalid_argument if @p chunkSize is <= 0.
   */
  ObjectPool(int chunkSize = kDefaultChunkSize)
    throw(std::invalid_argument, std::bad_alloc);

  /**
   * @brief Frees all the allocated objects.
   *
   * Invalidates any objects that have been acquired for use.
   */
  ~ObjectPool();

  /**
   * @brief Reserve an object for use.
   *
   * The reference to the object is invalidated if the object pool itself is freed.
   * @note Clients must not free the object!
   */
  T& acquireObject();

  /**
   * @brief Return the object to the pool.
   *
   * Clients must not use the object after it has been returned to the pool.
   */
  void releaseObject(T& obj);

 protected:
  /// Default chunk size if user do not pass another one.
  static const int kDefaultChunkSize = 10;

  /// Stores the objects that are not currently in use by clients.
  queue<T*> _freeList;

  /**
   * @brief Stores pointers to all the objects, in use or not.
   *
   * This vector is needed in order to ensure that all objects are freed properly
   * in the destructor.
   */
  vector<T*> _allObjects;

  /// Controls the number of objects created at one time
  int _chunkSize;

  /**
   * @brief Allocates @e _chunkSize new objects and adds them to the @e _freeList.
   *
   * Allocates an array of @e _chunkSize objects because that's
   * more efficient than allocating each of them individually.
   * Stores a pointer to the first element of the array in the @e _allObjects
   * vector. Adds a pointer to each new object to the @e _freeList.
   *
   * @see _chunkSize, _freeList
   */
  void allocateChunk();

  /**
   * @brief Freeing function for use in the @b for_each algorithm in the
   * destructor.
   */
  static void arrayDeleteObject(T* obj);

 private:
  //@{
  /** Prevent assignment and pass-by-value */
  ObjectPool(const ObjectPool<T>& src);
  ObjectPool<T>& operator=(const ObjectPool<T>& rhs);
  //@}
};

// _____________________________________________________________________________
//                                                      Template Implementation

template<typename T>
const int ObjectPool<T>::kDefaultChunkSize;

template <typename T>
ObjectPool<T>::ObjectPool(int chunkSize)
throw(std::invalid_argument, std::bad_alloc) : _chunkSize(chunkSize)
{
  if (_chunkSize <= 0) {
    throw std::invalid_argument("chunk size must be positive");
  }
  // Create _chunkSize objects to start
  allocateChunk();
}

template <typename T>
void ObjectPool<T>::allocateChunk()
{
  T* newObjects = new T[_chunkSize];
  _allObjects.push_back(newObjects);

  for (int i = 0; i < _chunkSize; i++) {
    _freeList.push(&newObjects[i]);
  }
}

template<typename T>
void ObjectPool<T>::arrayDeleteObject(T* obj)
{
  delete [] obj;
}

template <typename T>
ObjectPool<T>::~ObjectPool()
{
  // free each of the allocation chunks
  for_each(_allObjects.begin(), _allObjects.end(), arrayDeleteObject);
}

template <typename T>
T& ObjectPool<T>::acquireObject()
{
  if (_freeList.empty()) {
    allocateChunk();
  }
  T* obj = _freeList.front();
  _freeList.pop();
  return (*obj);
}

template <typename T>
void ObjectPool<T>::releaseObject(T& obj)
{
  _freeList.push(&obj);
}

#endif /* _OBJECTPOOL_H_ */
