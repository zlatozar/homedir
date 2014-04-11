#include "Grid.h"

#include <string>
#include <vector>

using namespace std;

/**
 * If you want to declare a function or method that takes a Grid object,
 * you must specify the type stored in that grid as part of the Grid type.
 *
 */
void processIntGrid(Grid<int>& inGrid)
{
  // body omitted for brevity
}

int main(int argc, char** argv)
{
  Grid<int> myIntGrid; // declares a grid that stores ints
  myIntGrid.setElementAt(0, 0, 10);
  int x = myIntGrid.getElementAt(0, 0);

  Grid<int> grid2(myIntGrid);
  Grid<int> anotherIntGrid = grid2;

  /// You can store pointer types as well
  Grid<char*> myStringGrid;
  myStringGrid.setElementAt(2, 2, "hello");

  /// The type specified can even be another template type
  Grid<vector<int> > gridOfVectors; // Note the extra space!
  vector<int> myVector;
  gridOfVectors.setElementAt(5, 6, myVector);

  //Grid<vector<int>> gridOfVectors; // INCORRECT SYNTAX

  /// You can also dynamically allocate Grid template instantiations on the heap
  Grid<int>* myGridp = new Grid<int>();
  myGridp->setElementAt(0, 0, 10);
  x = myGridp->getElementAt(0, 0);

  delete myGridp;

  return (0);
}
