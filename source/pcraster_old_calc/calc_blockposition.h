#ifndef INCLUDED_CALC_BLOCKPOSITION
#define INCLUDED_CALC_BLOCKPOSITION

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace calc {

class InfoScript;

//! OBSOLETE, NOT USED
class BlockPosition {
 private:
	//! the root (script) has a key of size 0
	std::vector<size_t> d_key;
	size_t              d_nrChildren;
 public:
	//! constructor for script (root)
	BlockPosition();
	//! constructor for child, incr parent's nrChildren
	BlockPosition(BlockPosition& parent);
	// ACCESORS
	//! compute the grade of the common ancestor.
	/*! e.g. 0 means I am the common,
	 *  1 means parent is com. ancestor
	 *  2 means grandparent is com. ancestor
	 */
	size_t grade(const BlockPosition& other) const; 
	std::string asString() const;
};

}

#endif
