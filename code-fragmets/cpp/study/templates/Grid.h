/**
 * Copyright (C). Copyright Year(s). License.
 *
 * @file   Grid.h
 *
 * @author
 * @date
 *
 * When the compiler encounters template method definitions, it performs syntax checking, but
 * doesn’t actually compile the templates.
 *
 */

/**
 * @brief Example templete class with a lot useful comments
 *
 * Within a class definition, the compiler will interpret Grid as Grid<T> where needed.
 * However, it’s best to get in the habit of specifying Grid<T> explicitly because that’s
 * the syntax you use outside the class to refer to types generated from the template.
 *
 * Templates allow nontype parameters to be values of only "simple" types:
 * ints, enums, pointers, and references.
 *
 */
template <typename T>
class Grid
{
 public:
  Grid(int inWidth = kDefaultWidth, int inHeight = kDefaultHeight);
  Grid(const Grid<T>& src);
  ~Grid();
  Grid<T>& operator=(const Grid<T>& rhs);  // lhs/rhs - left/right hand side

  void setElementAt(int x, int y, const T& inElem);
  T& getElementAt(int x, int y);
  const T& getElementAt(int x, int y) const;

  int getHeight() const { return mHeight; }
  int getWidth() const { return mWidth; }

  /// It is a good habit to start all constants with "k"
  static const int kDefaultWidth = 10;
  static const int kDefaultHeight = 10;

 protected:
  void copyFrom(const Grid<T>& src);
  T** mCells;
  int mWidth, mHeight;
};

// _____________________________________________________________________________
//                                                         Class Implementation

// put implementation in separate header file: GridDefinitions.h if too big

/**
 * Note that the class name before the :: is Grid<T>, not Grid.
 */
template <typename T>
Grid<T>::Grid(int inWidth, int inHeight) : mWidth(inWidth), mHeight(inHeight)
{
  mCells = new T* [mWidth];
  for (int i = 0; i < mWidth; i++) {
    mCells[i] = new T[mHeight];
  }
}

template <typename T>
const int Grid<T>::kDefaultWidth;

template <typename T>
const int Grid<T>::kDefaultHeight;

template <typename T>
Grid<T>::Grid(const Grid<T>& src)
{
  copyFrom(src);
}

template <typename T>
Grid<T>::~Grid()
{
  // free the old memory
  for (int i = 0; i < mWidth; i++) {
    delete [] mCells[i];
  }
  delete [] mCells;
}

template <typename T>
void Grid<T>::copyFrom(const Grid<T>& src)
{
  int i, j;
  mWidth = src.mWidth;
  mHeight = src.mHeight;

  mCells = new T* [mWidth];
  for (i = 0; i < mWidth; i++) {
    mCells[i] = new T[mHeight];
  }

  for (i = 0; i < mWidth; i++) {
    for (j = 0; j < mHeight; j++) {
      mCells[i][j] = src.mCells[i][j];
    }
  }
}

template <typename T>
Grid<T>& Grid<T>::operator=(const Grid<T>& rhs)
{
  // check for self-assignment
  if (this == &rhs) {
    return (*this);
  }
  // free the old memory
  for (int i = 0; i < mWidth; i++) {
    delete [] mCells[i];
  }
  delete [] mCells;

  // copy the new memory
  copyFrom(rhs);

  return (*this);
}

template <typename T>
void Grid<T>::setElementAt(int x, int y, const T& inElem)
{
  mCells[x][y] = inElem;
}

template <typename T>
T& Grid<T>::getElementAt(int x, int y)
{
  return (mCells[x][y]);
}

template <typename T>
const T& Grid<T>::getElementAt(int x, int y) const
{
  return (mCells[x][y]);
}
